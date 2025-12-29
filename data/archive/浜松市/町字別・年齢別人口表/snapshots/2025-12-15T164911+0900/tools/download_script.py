#
# -*- coding: utf-8 -*-
"""
浜松市「町字別・年齢別人口表」ダウンロード（研究用アーカイブ凍結版）

- data/archive/浜松市/町字別・年齢別人口表/snapshots/<timestamp>/ 以下へ保存
- スナップショット内に以下を同梱
  - _manifest_download.csv（URL/取得日時/ETag/Last-Modified/sha256 等）
  - _run_meta.json（実行メタ、スクリプトsha256、Python/依存など）
  - tools/（実行時点のスクリプトコピー、requirements.txt、run_command.txt）
  - _README.txt（運用ルール）
"""

from __future__ import annotations

import csv
import hashlib
import json
import os
import re
import subprocess
import sys
import time
import random
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Tuple
from urllib.parse import urljoin, urlparse

import requests
from bs4 import BeautifulSoup


# ===============================================================
# 設定
# ===============================================================

JST = timezone(timedelta(hours=9))

# 出力フォルダ名（後工程互換のため日本語を使用）
ERA_LABEL_MAP = {
    "old": "市町村合併前",
    "before": "再編前",
    "after": "再編後",
    "other": "other",
}


DATASET_NAME = "町字別・年齢別人口表"

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.environ.get("MIKKABI_PROJECT_ROOT", os.path.abspath(os.path.join(BASE_DIR, "..")))

# アーカイブ（凍結）ルート
ARCHIVE_ROOT = os.path.join(PROJECT_ROOT, "data", "archive", "浜松市", DATASET_NAME)

# 取得対象URL（浜松市 年齢別人口表）
BASE_URLS = [
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_nenreibetsu.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu2.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu1.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu.html",
]

# 対象拡張子（必要なら ".pdf" も追加可）
TARGET_EXTS = (".xls", ".xlsx")

# UA（市サイトで稀に弾かれる対策）
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    )
}

TIMEOUT = 60

MAX_RETRIES = 4


def polite_sleep(base: float = 1.0) -> None:
    # 固定1秒より少しだけ揺らす（連続アクセス感が減る）
    time.sleep(base + random.random() * 0.4)

CHUNK_SIZE = 8192

# テスト用：件数制限（Noneで全件）
MAX_FILES = None  # 例: 20


# ===============================================================
# 町字別・年齢別人口表：区分ロジック（既存スクリプトを踏襲）
# ===============================================================

def _normalize_digits(s: str) -> str:
    return s.translate(str.maketrans("０１２３４５６７８９", "0123456789"))

def extract_era_year(filename: str) -> Tuple[str, int | None]:
    """
    ファイル名から Hxx / Rxx を抽出して返す（全角数字対応）
    戻り: ("H", 19) / ("R", 6) / ("other", None)
    """
    name = _normalize_digits(filename).lower()

    m = re.search(r"h(\d{1,2})", name)
    if m:
        return "H", int(m.group(1))

    m = re.search(r"r(\d{1,2})", name)
    if m:
        return "R", int(m.group(1))

    return "other", None

def classify_era(filename: str) -> str:
    """
    before / after / old / other
    - before: 行政区再編前（政令市区割時代）
    - after : 再編後（2024〜）
    - old   : 市町村合併前（旧自治体時代）
    """
    era, yy = extract_era_year(filename)
    if era == "H" and yy is not None:
        if 19 <= yy <= 31:
            return "before"
        if yy <= 18:
            return "old"
        return "other"
    if era == "R" and yy is not None:
        if 1 <= yy <= 5:
            return "before"
        if yy >= 6:
            return "after"
        return "other"
    return "other"

def classify_month(filename: str) -> str:
    """
    町字別・年齢別人口表は「4月」「10月」が基本（既存運用を踏襲）
    取れなければ other

    NOTE:
      年号（例: H04）に含まれる "04" を月と誤認しないため、
      「区切り文字（-/_）直後の 04/10」を最優先で判定する。
    """
    name = _normalize_digits(filename).lower()

    # 1) 最優先：区切り文字の直後の月（例: *_04*, *_10*, *-04*, *-10*）
    m = re.search(r'(?:-|_)(0?4|10)(?!\d)', name)
    if m:
        mm = m.group(1)
        return "04" if mm in ("4", "04") else "10"

    # 2) 次点：「4月/10月」「4gatsu/10gatsu」など明示的な月表現
    if re.search(r'(?:^|[^0-9])0?4(?:gatsu|月)(?!\d)', name):
        return "04"
    if re.search(r'(?:^|[^0-9])10(?:gatsu|月)(?!\d)', name):
        return "10"

    return "other"



def parse_g_year(filename: str) -> int | None:
    # 例: r07_04 / R7-10 / h17-06 などを拾う（大文字小文字無視）
    m = re.search(r'(?i)(?:^|[^a-z0-9])([rh])0?(\d{1,2})(?=[^0-9]|$)', filename)
    if not m:
        return None
    era = m.group(1).upper()
    yy = int(m.group(2))
    if era == "H":
        return 1988 + yy
    # R
    return 2018 + yy

def classify_district_from_filename(filename: str) -> str:
    """
    ファイル名（URL末尾）に含まれる英字キーから行政区を推定する。

    - ここでは「英字のみ（hamanaku, chuuoku, nakaku...）」を前提にする。
    - "hamakitaku" が "kitaku" を含むため、浜北区を先に判定する。
    - 取れない場合は "other"
    """
    name_l = _normalize_digits(filename).lower()

    # 再編後（2024-01以降の新3区）
    if re.search(r"chu+o+ku", name_l):
        return "中央区"
    if "hamanaku" in name_l:
        return "浜名区"
    if re.search(r"ten+ryu+ku", name_l):
        return "天竜区"

    # 再編前（旧7区）
    if "nakaku" in name_l:
        return "中区"
    if "higashiku" in name_l:
        return "東区"
    if "nishiku" in name_l:
        return "西区"
    if "minamiku" in name_l:
        return "南区"
    if "hamakitaku" in name_l:
        return "浜北区"
    if re.search(r"(?<!hama)kitaku", name_l):
        return "北区"

    # 全市（全区版）
    if "hamamatsushi" in name_l:
        return "浜松市"

    return "other"


# ===============================================================
# 凍結（スナップショット）ユーティリティ
# ===============================================================

def now_jst() -> datetime:
    return datetime.now(JST)

def ensure_dir(path: str) -> None:
    os.makedirs(path, exist_ok=True)

def sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()

def sha256_text(s: str) -> str:
    return hashlib.sha256(s.encode("utf-8")).hexdigest()

def pip_freeze() -> str:
    try:
        out = subprocess.check_output(
            [sys.executable, "-m", "pip", "freeze"],
            stderr=subprocess.STDOUT,
            text=True
        )
        return out.strip() + "\n"
    except Exception as e:
        return f"# pip freeze failed: {e}\n"

def copy_self(to_path: str) -> None:
    with open(__file__, "r", encoding="utf-8") as f:
        code = f.read()
    ensure_dir(os.path.dirname(to_path))
    with open(to_path, "w", encoding="utf-8") as g:
        g.write(code)

def write_text(path: str, content: str) -> None:
    ensure_dir(os.path.dirname(path))
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

def download_stream(url: str, save_path: str) -> Tuple[int, Dict[str, str]]:
    ensure_dir(os.path.dirname(save_path))
    with requests.get(url, headers=HEADERS, stream=True, timeout=TIMEOUT) as r:
        status = r.status_code

        hdr = {
            "ETag": r.headers.get("ETag", ""),
            "Last-Modified": r.headers.get("Last-Modified", ""),
            "Content-Length": r.headers.get("Content-Length", ""),
            "Content-Type": r.headers.get("Content-Type", ""),
            "Retry-After": r.headers.get("Retry-After", ""),
        }

        # 失敗時はファイルを書かずに返す（外側で retry 判断する）
        if status != 200:
            return status, hdr

        with open(save_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=CHUNK_SIZE):
                if chunk:
                    f.write(chunk)

        return status, hdr


# ===============================================================
# メイン
# ===============================================================

def main() -> None:
    started_at = now_jst()
    snapshot_id = started_at.strftime("%Y-%m-%dT%H%M%S%z")
    snapshot_root = os.path.join(ARCHIVE_ROOT, "snapshots", snapshot_id)

    ensure_dir(snapshot_root)

    # tools 同梱
    tools_dir = os.path.join(snapshot_root, "tools")
    ensure_dir(tools_dir)

    # 実行時点のスクリプトをコピー
    script_copy_path = os.path.join(tools_dir, "download_script.py")
    copy_self(script_copy_path)

    # requirements（pip freeze）
    requirements_path = os.path.join(tools_dir, "requirements.txt")
    write_text(requirements_path, pip_freeze())

    # 実行コマンド
    run_command_path = os.path.join(tools_dir, "run_command.txt")
    write_text(run_command_path, f"{sys.executable} {os.path.basename(__file__)}\n")

    # README
    readme_path = os.path.join(snapshot_root, "_README.txt")
    write_text(readme_path, "\n".join([
        f"Dataset: {DATASET_NAME}",
        f"Publisher: 浜松市",
        f"Snapshot: {snapshot_id} (JST)",
        "",
        "This folder is an immutable archive snapshot of the official distributed files.",
        "DO NOT edit/overwrite files in this snapshot.",
        "Use a separate 'raw' (working copy) layer for any processing.",
        "",
        "Integrity can be verified by sha256 hashes recorded in _manifest_download.csv.",
        "",
    ]))

    # スクリプトsha256
    with open(__file__, "r", encoding="utf-8") as f:
        script_text = f.read()
    script_sha = sha256_text(script_text)

    # 1) リンク収集
    found: List[Dict[str, str]] = []
    for page_url in BASE_URLS:
        print(f"取得中: {page_url}")
        resp = requests.get(page_url, headers=HEADERS, timeout=TIMEOUT)
        resp.raise_for_status()
        resp.encoding = resp.apparent_encoding
        soup = BeautifulSoup(resp.text, "html.parser")

        for a in soup.find_all("a"):
            href = a.get("href")
            if not href:
                continue
            href_l = href.lower()
            if href_l.endswith(TARGET_EXTS):
                abs_url = urljoin(page_url, href)
                found.append({"url": abs_url, "source_page": page_url})

    # 重複除去
    seen = set()
    links = []
    for it in found:
        if it["url"] in seen:
            continue
        seen.add(it["url"])
        links.append(it)

    if MAX_FILES is not None:
        links = links[:MAX_FILES]

    print(f"総発見ファイル数: {len(links)}")

    # 2) ダウンロード
    manifest_path = os.path.join(snapshot_root, "_manifest_download.csv")
    rows: List[List[str]] = []

    # カウント
    download_count: Dict[str, Dict[str, int]] = {
        "before": {},
        "after": {},
        "old": {},
        "other": {},
    }

    for it in links:
        url = it["url"]
        source_page = it["source_page"]
        filename = os.path.basename(urlparse(url).path)

        era = classify_era(filename)
        district = classify_district_from_filename(filename)
        month = classify_month(filename)

        # 出力先：era / district / month
        era_dir = ERA_LABEL_MAP.get(era, era)
        save_dir = os.path.join(snapshot_root, era_dir, district, month)
        ensure_dir(save_dir)
        save_path = os.path.join(save_dir, filename)

        downloaded_at = now_jst().isoformat()

        for attempt in range(1, MAX_RETRIES + 1):
            status = None
            hdr = {}
            try:
                status, hdr = download_stream(url, save_path)

                if status == 200:
                    polite_sleep(1.0)  # 成功後だけ待機

                    file_sha = sha256_file(save_path)

                    download_count.setdefault(era_dir, {})
                    download_count[era_dir][district] = download_count[era_dir].get(district, 0) + 1

                    rows.append([
                        era_dir, district, month,
                        filename, filename,
                        url, source_page,
                        downloaded_at,
                        str(status),
                        hdr.get("Content-Length", ""),
                        hdr.get("Last-Modified", ""),
                        hdr.get("ETag", ""),
                        hdr.get("Content-Type", ""),
                        file_sha,
                        "downloaded",
                    ])
                    print(f"OK: {era}/{district}/{month} {filename}")
                    break

                # 一時的な混雑・障害はリトライ
                if status == 429 or status >= 500:
                    backoff = min(30, 2 ** (attempt - 1))
                    ra = hdr.get("Retry-After", "")
                    if ra.isdigit():
                        backoff = max(backoff, int(ra))
                    print(f"RETRY: status={status} attempt={attempt}/{MAX_RETRIES} wait={backoff}s {filename}")
                    time.sleep(backoff)
                    continue

                # 404等はリトライしても無駄
                raise RuntimeError(f"HTTP status={status}")

            except Exception as e:
                if attempt == MAX_RETRIES:
                    rows.append([
                        era_dir, district, month,
                        filename, filename,
                        url, source_page,
                        downloaded_at,
                        "" if status is None else str(status),
                        hdr.get("Content-Length", ""),
                        hdr.get("Last-Modified", ""),
                        hdr.get("ETag", ""),
                        hdr.get("Content-Type", ""),
                        "",
                        f"error: {e}",
                    ])
                    print(f"FAILED: {filename} ({e})")
                else:
                    backoff = min(30, 2 ** (attempt - 1))
                    print(f"RETRY(EX): attempt={attempt}/{MAX_RETRIES} wait={backoff}s {filename} err={e}")
                    time.sleep(backoff)

    # ===============================================================
    # 追加: 検証（other検出 / 固定件数 / 最新到達） ※検証以外の機構は不変更
    # ===============================================================
    EXPECTED_FIXED_TOTAL_OLD_BEFORE = 329
    PUBLISH_GRACE_DAY = 10  # 『翌月初旬』の目安

    warnings: List[str] = []
    def warn(msg: str) -> None:
        print(f"[WARNING] {msg}")
        warnings.append(msg)

    def now_jst_ref() -> datetime:
        """検証用参照時刻（NTP優先→HTTP Date→失敗時PC時計）。検証以外は変更しない。"""
        # 1) NTP（NICT）
        try:
            import ntplib
            c = ntplib.NTPClient()
            resp = c.request("ntp.nict.jp", version=3, timeout=5)
            utc = datetime.fromtimestamp(resp.tx_time, tz=timezone.utc)
            return utc.astimezone(JST)
        except Exception as e:
            warn(f"NTP時刻の取得に失敗（ntp.nict.jp）: {e}")

        # 2) NTP（pool）
        try:
            import ntplib
            c = ntplib.NTPClient()
            resp = c.request("pool.ntp.org", version=3, timeout=5)
            utc = datetime.fromtimestamp(resp.tx_time, tz=timezone.utc)
            return utc.astimezone(JST)
        except Exception as e:
            warn(f"NTP時刻の取得に失敗（pool.ntp.org）: {e}")

        # 3) HTTP Date（軽量フォールバック）
        try:
            import email.utils
            r = requests.head(BASE_URLS[0], headers=HEADERS, allow_redirects=True, timeout=15)
            d = r.headers.get("Date")
            if d:
                dt = email.utils.parsedate_to_datetime(d)
                if dt.tzinfo is None:
                    dt = dt.replace(tzinfo=timezone.utc)
                return dt.astimezone(JST)
        except Exception as e:
            warn(f"ネット時刻取得に失敗（HTTP Date）。PC時刻で検証します: {e}")

        warn("参照時刻をPC時計で代替しました（NTP/HTTP Date 取得不可）")
        return now_jst()

    # 1) other 検出
    other_rows: List[List[str]] = []
    for r in rows:
        if not r or len(r) < 3:
            continue
        era = (r[0] or '').strip()
        dist = (r[1] or '').strip()
        mon = (r[2] or '').strip()
        if era == 'other' or dist == 'other' or mon == 'other':
            other_rows.append(r)
    if other_rows:
        ex = [rr[3] for rr in other_rows[:10] if len(rr) > 3]
        warn(f"'other'（分類不可）が {len(other_rows)}件あります。例: {', '.join(ex)}{' ...' if len(other_rows) > 10 else ''}")

    # 2) 固定件数（合併前＋再編前＝ old + before）
    fixed_total = 0
    try:
        fixed_total += sum(download_count.get(ERA_LABEL_MAP['old'], {}).values())
        fixed_total += sum(download_count.get(ERA_LABEL_MAP['before'], {}).values())
    except Exception:
        pass
    if fixed_total != EXPECTED_FIXED_TOTAL_OLD_BEFORE:
        warn(f"固定範囲（old+before）のダウンロード件数が想定と違います: {fixed_total}件（想定 {EXPECTED_FIXED_TOTAL_OLD_BEFORE}件）")

    # 3) 最新到達チェック（4月/10月）
    latest_ym: int | None = None
    latest_fn: str | None = None
    for r in rows:
        if not r or len(r) < 15:
            continue
        status_flag = r[14] if len(r) > 14 else ''
        if status_flag != 'downloaded':
            continue
        mon = (r[2] or '').strip()
        if mon not in ('04', '10'):
            continue
        fn = r[3] if len(r) > 3 else ''
        y = parse_g_year(fn)
        if y is None:
            continue
        ym = y * 100 + int(mon)
        if latest_ym is None or ym > latest_ym:
            latest_ym, latest_fn = ym, fn

    now = now_jst_ref()
    y, m, d = now.year, now.month, now.day
    # 公開遅延を考慮して、月初（PUBLISH_GRACE_DAY未満）は1か月余裕を見る
    if d < PUBLISH_GRACE_DAY:
        # 期待値を1か月戻したうえで4/10へ丸める
        m2 = m - 1
        y2 = y
        if m2 == 0:
            y2 -= 1
            m2 = 12
        y, m = y2, m2

    # 期待される最新（4/10）を計算
    if m <= 4:
        expected_y, expected_m = (y - 1), 10
    elif m <= 10:
        expected_y, expected_m = y, 4
    else:
        expected_y, expected_m = y, 10
    expected_ym = expected_y * 100 + expected_m

    if latest_ym is None:
        warn("最新ファイル（年月）の判定に失敗しました（ファイル名から年を抽出できない可能性）。")
    elif latest_ym < expected_ym:
        warn(f"最新データまで到達できていない可能性: 取得最新={latest_fn}（{latest_ym}） / 期待={expected_y}-{expected_m:02d}（{expected_ym}）")

    # WARNINGS.txt（警告があれば出力）
    if warnings:
        warn_path = os.path.join(snapshot_root, "_WARNINGS.txt")
        write_text(warn_path, "\n".join(warnings) + "\n")
        print("\n[WARNING] _WARNINGS.txt を出力しました（snapshot_root 直下）。\n")

    # 3) マニフェスト出力
    with open(manifest_path, "w", newline="", encoding="utf-8-sig") as f:
        w = csv.writer(f)
        w.writerow([
            "era", "district", "month",
            "filename_original", "filename_saved",
            "url", "source_page",
            "downloaded_at",
            "http_status",
            "content_length",
            "last_modified",
            "etag",
            "content_type",
            "sha256",
            "status",
        ])
        w.writerows(rows)

    finished_at = now_jst()

    # 4) run_meta
    meta = {
        "dataset": DATASET_NAME,
        "publisher": "浜松市",
        "snapshot_id": snapshot_id,
        "started_at": started_at.isoformat(),
        "finished_at": finished_at.isoformat(),
        "script_name": os.path.basename(__file__),
        "script_sha256": script_sha,
        "python_version": sys.version,
        "base_urls": BASE_URLS,
        "target_exts": list(TARGET_EXTS),
        "outputs": {
            "snapshot_root": snapshot_root,
            "manifest_csv": os.path.basename(manifest_path),
            "tools_dir": "tools",
        },
        "counts_by_era": download_count,
    }
    meta_path = os.path.join(snapshot_root, "_run_meta.json")
    with open(meta_path, "w", encoding="utf-8") as f:
        json.dump(meta, f, ensure_ascii=False, indent=2)

    # 5) 結果表示
    print("\n--- ダウンロード結果（era -> district） ---")
    for era_key, d in download_count.items():
        print(f"\n[{era_key}]")
        for dist, cnt in sorted(d.items(), key=lambda x: x[0]):
            print(f"  {dist}: {cnt}件")

    print("\n--- 出力 ---")
    print(f"snapshot_root: {snapshot_root}")
    print(f"manifest: {manifest_path}")
    print(f"run_meta: {meta_path}")

    print("\nNOTE: 取得後は data/archive 配下を読み取り専用(ACL)にして凍結してください。")


if __name__ == "__main__":
    main()
