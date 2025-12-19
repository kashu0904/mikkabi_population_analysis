
# -*- coding: utf-8 -*-
"""
浜松市「区別・町字別世帯数人口」ダウンロード（研究用アーカイブ凍結版）

- data/浜松市/区別・町字別世帯数人口/archive/snapshots/<timestamp>/ 以下へ保存
- 行政区分類が無い（1ファイルに全区）前提:
  - 合併前/合併後（2005-06 まで=合併前, 2005-07 から=合併後）で分ける
  - 年月はファイル名の和暦/西暦から抽出（例: r07-01, h17-06, 2024-01 等）
- 月次（1〜12月）に対応
- スナップショット内に tools/、_manifest_download.csv、_run_meta.json、_README.txt を同梱
"""

from __future__ import annotations

import csv
import hashlib
import json
import os
import shutil
import zipfile
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
# 時刻（厳格：NTP必須 / フォールバック禁止）
# ===============================================================
try:
    import ntplib  # type: ignore
except Exception as e:
    print(f"[ERROR] ntplib が必要です。次を実行してください: pip install ntplib ({e})")
    raise SystemExit(2)


def get_ntp_jst() -> Tuple[datetime, str]:
    """厳格：NTPでのみ時刻を取得する（NICT→pool）。失敗したら例外。"""
    c = ntplib.NTPClient()
    servers = ["ntp.nict.jp", "pool.ntp.org"]
    last_err: Exception | None = None
    for s in servers:
        try:
            resp = c.request(s, version=3, timeout=5)
            utc = datetime.fromtimestamp(resp.tx_time, tz=timezone.utc)
            return utc.astimezone(JST), s
        except Exception as e:
            last_err = e
    raise RuntimeError(f"NTP時刻が取得できないため中止します（フォールバック禁止）。最後のエラー: {last_err}")



# ===============================================================
# 設定
# ===============================================================

JST = timezone(timedelta(hours=9))

DATASET_NAME = "区別・町字別世帯数人口"

BASE_DIR = os.path.dirname(os.path.abspath(__file__))

def _default_project_root(base_dir: str) -> str:
    """上位階層をたどって data/ が見つかる場所を PROJECT_ROOT とする。
    見つからない場合はエラー（誤った場所へ保存しないため）。
    """
    cur = os.path.abspath(base_dir)
    while True:
        if os.path.isdir(os.path.join(cur, "data")):
            return cur
        parent = os.path.abspath(os.path.join(cur, ".."))
        if parent == cur:
            raise RuntimeError("PROJECT_ROOT を推定できません（上位階層に data/ が見つかりません）。MIKKABI_PROJECT_ROOT を指定してください。")
        cur = parent

_env_root = os.environ.get("MIKKABI_PROJECT_ROOT")
if _env_root:
    if not os.path.isdir(os.path.join(_env_root, "data")):
        raise RuntimeError("MIKKABI_PROJECT_ROOT 配下に data/ が見つかりません。誤保存防止のため中止します。")
    PROJECT_ROOT = _env_root
else:
    PROJECT_ROOT = _default_project_root(BASE_DIR)
ARCHIVE_ROOT = os.path.join(PROJECT_ROOT, "data", "浜松市", DATASET_NAME , "archive")

BASE_URLS = [
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_kubetsu.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu2.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu1.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu.html",
]

TARGET_EXTS = (".xls", ".xlsx")

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    )
}
TIMEOUT = 60
CHUNK_SIZE = 8192

MAX_FILES = None  # テスト用

# ダウンロード再試行回数（429/5xx 等の一時障害に備える）
MAX_RETRIES = 1


# 合併判定（ページ説明に合わせる：平成17年6月末以前は旧浜松市）
# -> 2005-06 まで = 合併前、2005-07〜2023-12 = 再編前、2024-01〜 = 再編後
MERGER_CUTOFF_YM = (2005, 7)

# 行政区再編（2024-01以降の新3区）に合わせて、合併後をさらに分割
# -> 2005-07〜2023-12 = 再編前、2024-01〜 = 再編後
REORG_CUTOFF_YM = (2024, 1)

# 固定件数（合併前 + 再編前）
EXPECTED_FIXED_TOTAL_OLD_BEFORE = 287



# ===============================================================
# ユーティリティ
# ===============================================================

def now_jst() -> datetime:
    raise RuntimeError("now_jst は使用禁止（厳格NTPモード）。get_ntp_jst() を使用してください。")

def ensure_dir(path: str) -> None:
    os.makedirs(path, exist_ok=True)

def sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()



def zip_snapshot_folder(snapshot_root: str) -> tuple[str, str]:
    """スナップショットフォルダを snapshot.zip に固め、snapshot.zip.sha256 を出力する。

    - snapshot_root/snapshot.zip を生成
    - snapshot_root/snapshot.zip.sha256 を生成（中身: "<sha256>  snapshot.zip"）

    返り値: (zip_path, zip_sha256)
    """
    zip_path = os.path.join(snapshot_root, "snapshot.zip")
    sha_path = zip_path + ".sha256"
    tmp_zip = zip_path + ".part"

    # 既存があれば作り直す（同名衝突を避ける）
    for p in (tmp_zip, zip_path, sha_path):
        try:
            if os.path.exists(p):
                os.remove(p)
        except Exception:
            pass

    with zipfile.ZipFile(tmp_zip, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        for root, _dirs, files in os.walk(snapshot_root):
            for fn in files:
                full = os.path.join(root, fn)
                rel = os.path.relpath(full, snapshot_root)
                # zip 本体/生成途中/sha256 を巻き込まない
                if rel in ("snapshot.zip", "snapshot.zip.part", "snapshot.zip.sha256"):
                    continue
                zf.write(full, arcname=rel)

    os.replace(tmp_zip, zip_path)
    h = hashlib.sha256()
    with open(zip_path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    zip_sha = h.hexdigest()
    with open(sha_path, "w", encoding="utf-8") as f:
        f.write(f"{zip_sha}  snapshot.zip\n")

    return zip_path, zip_sha


def resolve_save_path_on_collision(save_path: str, new_sha256: str) -> str:
    """同一保存先に同名が既に存在する場合の衝突回避。
    - 既存と同一内容（sha256一致）ならそのまま保存先は変えない
    - 内容が違うなら __sha256_<先頭12桁> を付けた別名へ保存
    """
    try:
        if not os.path.exists(save_path):
            return save_path
        existing_sha = sha256_file(save_path)
        if existing_sha == new_sha256:
            return save_path
        base, ext = os.path.splitext(save_path)
        return f"{base}__sha256_{new_sha256[:12]}{ext}"
    except Exception:
        return save_path


def validate_excel_magic(save_path: str, filename: str) -> None:
    """保存したファイルが本当に xls/xlsx かを簡易検証する（200でHTMLが返る事故対策）。
    - .xlsx: ZIP（PK..）
    - .xls : OLE2（D0 CF 11 E0 A1 B1 1A E1）
    不一致なら例外。
    """
    ext = os.path.splitext(filename)[1].lower()
    with open(save_path, "rb") as f:
        head = f.read(8)

    if ext == ".xlsx":
        if not (len(head) >= 2 and head[0:2] == b"PK"):
            raise ValueError("xlsx マジックバイト不一致（ZIPではありません）")
    elif ext == ".xls":
        if head != bytes.fromhex("D0 CF 11 E0 A1 B1 1A E1"):
            raise ValueError("xls マジックバイト不一致（OLE2ではありません）")
    else:
        # ここには来ない想定（リンク収集で弾く）が安全のため
        raise ValueError(f"未知の拡張子: {ext}")

def extract_excel_filename_from_url(url: str) -> str | None:
    """URL から Excel ファイル名（.xls/.xlsx）を推定する。
    - path に拡張子がある場合はそれを優先
    - query 等に含まれる場合は最後に出現する *.xls(x) を拾う
    """
    try:
        path = urlparse(url).path
        base = os.path.basename(path)
        if base and os.path.splitext(base)[1].lower() in TARGET_EXTS:
            return base
    except Exception:
        base = ""

    m = re.findall(r"[^/?#&=]+\.xlsx?", url, flags=re.IGNORECASE)
    if m:
        fn = m[-1]
        if os.path.splitext(fn)[1].lower() in TARGET_EXTS:
            return fn
    return None


def sha256_text(s: str) -> str:
    return hashlib.sha256(s.encode("utf-8")).hexdigest()

def _normalize_digits(s: str) -> str:
    return s.translate(str.maketrans("０１２３４５６７８９", "0123456789"))

def era_to_western_year(era: str, n: int) -> int:
    era = era.upper()
    if era == "H":  # 平成1=1989
        return 1988 + n
    if era == "R":  # 令和1=2019
        return 2018 + n
    raise ValueError(f"Unknown era: {era}")

def parse_year_month_from_filename(filename: str) -> Tuple[int | None, int | None, str | None]:
    """
    例:
      setaisu-jinkousu_area_r07-01.xlsx -> (2025, 1, "R07")
      setaisu-jinkousu_area_h17-06.xls  -> (2005, 6, "H17")
      ...2024-01... -> (2024, 1, None)

    取れない場合: (None, None, None)
    """
    name = _normalize_digits(filename).lower()

    m = re.search(r'([hr])(\d{1,2})[-_](\d{1,2})', name)
    if m:
        era = m.group(1).upper()
        era_year = int(m.group(2))
        month = int(m.group(3))
        if 1 <= month <= 12:
            year = era_to_western_year(era, era_year)
            return year, month, f"{era}{era_year:02d}"

    m = re.search(r'((?:19|20)\d{2})[-_](\d{1,2})', name)
    if m:
        year = int(m.group(1))
        month = int(m.group(2))
        if 1 <= month <= 12:
            return year, month, None

    return None, None, None

def merger_bucket(year: int | None, month: int | None) -> str:
    """保存バケット（フォルダ）を返す。

    - 2005-06 まで: 合併前
    - 2005-07〜2023-12: 再編前
    - 2024-01〜: 再編後
    """
    if year is None:
        return "other"

    my, mm = MERGER_CUTOFF_YM
    ry, rm = REORG_CUTOFF_YM

    # 月が取れないケースは年だけで粗く判定（保険）
    if month is None:
        if year < my:
            return "合併前"
        if year < ry:
            return "再編前"
        return "再編後"

    if (year, month) < (my, mm):
        return "合併前"
    if (year, month) < (ry, rm):
        return "再編前"
    return "再編後"

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




def _count_errors_from_rows(rows: list[list[str]], status_index: int) -> int:
    cnt = 0
    for r in rows:
        try:
            s = (r[status_index] or "").strip()
        except Exception:
            continue
        if not s:
            continue
        if s != "downloaded":
            cnt += 1
    return cnt


def _write_status_and_markers(
    snapshot_root: str,
    *,
    result: str,
    started_at_iso: str,
    finished_at_iso: str | None,
    warnings: list[str],
    errors_count: int,
    extra: dict | None = None,
) -> None:
    """結果を“見た瞬間に分かる形”で残す。"""
    status = {
        "result": result,  # OK / WARN / ERROR / FATAL
        "started_at": started_at_iso,
        "finished_at": finished_at_iso or "",
        "warnings_count": len(warnings),
        "errors_count": int(errors_count),
    }
    if extra:
        status.update(extra)

    # 機械用
    status_path = os.path.join(snapshot_root, "_STATUS.json")
    try:
        with open(status_path, "w", encoding="utf-8") as f:
            json.dump(status, f, ensure_ascii=False, indent=2)
    except Exception:
        pass

    # 人間用（旗ファイル）
    if result == "OK":
        p = os.path.join(snapshot_root, "_SUCCESS.txt")
        body = "RESULT=OK\nwarnings=0\nerrors=0\n"
        try:
            write_text(p, body)
        except Exception:
            pass
    elif result == "WARN":
        p = os.path.join(snapshot_root, "_WARN.txt")
        body = "RESULT=WARN\nwarnings>0\nerrors=0\n"
        try:
            write_text(p, body)
        except Exception:
            pass
    elif result == "ERROR":
        p = os.path.join(snapshot_root, "_ERROR.txt")
        body = f"RESULT=ERROR\nwarnings={len(warnings)}\nerrors={errors_count}\n"
        try:
            write_text(p, body)
        except Exception:
            pass
    else:
        p = os.path.join(snapshot_root, "_FATAL.txt")
        body = f"RESULT=FATAL\nwarnings={len(warnings)}\nerrors={errors_count}\n"
        try:
            write_text(p, body)
        except Exception:
            pass


def _move_to_failed(snapshot_root: str, archive_root: str, snapshot_id: str) -> str:
    failed_base = os.path.join(archive_root, "snapshots_failed")
    ensure_dir(failed_base)
    failed_root = os.path.join(failed_base, snapshot_id)

    # 既存があれば避ける（基本起きないが保険）
    if os.path.exists(failed_root):
        i = 1
        while os.path.exists(failed_root + f"__{i}"):
            i += 1
        failed_root = failed_root + f"__{i}"

    try:
        shutil.move(snapshot_root, failed_root)
        return failed_root
    except Exception:
        return snapshot_root


def polite_sleep(base: float = 1.0) -> None:
    """連続アクセスの負荷を下げるための待機（少し揺らす）"""
    time.sleep(base + random.random() * 0.4)

def download_stream(url: str, save_path: str) -> Tuple[int, Dict[str, str], str, str]:
    """URLをストリームで保存し、(status_code, headers) を返す。
    - status != 200 の場合はファイルを書かずに返す（外側でリトライ判断する）
    - 途中失敗で中途半端なファイルが残らないよう、.part へ書いてから置換する
    """
    ensure_dir(os.path.dirname(save_path))
    tmp_path = save_path + ".part"

    # 万一前回の残骸があれば消す
    if os.path.exists(tmp_path):
        try:
            os.remove(tmp_path)
        except Exception:
            pass

    with requests.get(url, headers=HEADERS, stream=True, timeout=TIMEOUT) as r:
        status = r.status_code
        hdr = {
            "ETag": r.headers.get("ETag", ""),
            "Last-Modified": r.headers.get("Last-Modified", ""),
            "Content-Length": r.headers.get("Content-Length", ""),
            "Content-Type": r.headers.get("Content-Type", ""),
            "Retry-After": r.headers.get("Retry-After", ""),
        }

        if status != 200:
            return status, hdr, "", ""
        try:
            with open(tmp_path, "wb") as f:
                for chunk in r.iter_content(chunk_size=CHUNK_SIZE):
                    if chunk:
                        f.write(chunk)
            validate_excel_magic(tmp_path, os.path.basename(save_path))
            file_sha = sha256_file(tmp_path)
            final_path = resolve_save_path_on_collision(save_path, file_sha)
            os.replace(tmp_path, final_path)
            return status, hdr, final_path, file_sha
        except Exception:
            # 失敗したら中途半端ファイルを残さない
            try:
                if os.path.exists(tmp_path):
                    os.remove(tmp_path)
            except Exception:
                pass
            try:
                if os.path.exists(save_path):
                    os.remove(save_path)
            except Exception:
                pass
            # 既存の save_path が壊れて上書きされることはないが念のため
            raise
# ===============================================================
# メイン
# ===============================================================

def main() -> None:
    warnings: List[str] = []
    def warn(msg: str) -> None:
        print(f"[WARNING] {msg}")
        warnings.append(msg)

    started_at, ntp_server_started = get_ntp_jst()
    t0 = time.monotonic()
    snapshot_id = started_at.strftime("%Y-%m-%dT%H%M%S%z")
    snapshot_root = os.path.join(ARCHIVE_ROOT, "snapshots", snapshot_id)
    ensure_dir(snapshot_root)

    try:
        # tools 同梱
        tools_dir = os.path.join(snapshot_root, "tools")
        ensure_dir(tools_dir)
    
        script_copy_path = os.path.join(tools_dir, "download_script.py")
        copy_self(script_copy_path)
    
        requirements_path = os.path.join(tools_dir, "requirements.txt")
        write_text(requirements_path, pip_freeze())
    
        run_command_path = os.path.join(tools_dir, "run_command.txt")
        write_text(run_command_path, "python tools/download_script.py\n")

        # 元スクリプト名と再実行対象を明記（再現性メモ）
        origin_path = os.path.join(tools_dir, "origin.txt")
        write_text(origin_path, "\n".join([
            f"original_script_filename: {os.path.basename(__file__)}",
            "executable_script: tools/download_script.py",
            "repro_command: python tools/download_script.py",
        ]) + "\n")
    
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
            "Bucket rule: 2005-06 and earlier = 合併前, 2005-07..2023-12 = 再編前, 2024-01 and later = 再編後.",
            "",
        ]))
    
        with open(__file__, "r", encoding="utf-8") as f:
            script_text = f.read()
        script_sha = sha256_text(script_text)
        # 1) リンク収集（＋ source page HTML 保存）
        found: List[Dict[str, str]] = []
        source_pages_dir = os.path.join(snapshot_root, "source_pages")
        ensure_dir(source_pages_dir)
        source_pages_rows: List[List[str]] = []
        source_pages_rows.append([
            "index", "requested_url", "final_url", "saved_html",
            "status_code", "content_type", "content_length", "date",
            "last_modified", "etag", "sha256",
        ])
        for idx, page_url in enumerate(BASE_URLS, start=1):
            print(f"取得中: {page_url}")
            resp = requests.get(page_url, headers=HEADERS, timeout=TIMEOUT)
            resp.raise_for_status()
            # source page HTML をそのまま保存（証拠保全）
            try:
                html_bytes = resp.content
                html_sha = hashlib.sha256(html_bytes).hexdigest()
                html_name = f"{idx:02d}_{sha256_text(page_url)[:12]}.html"
                html_path = os.path.join(source_pages_dir, html_name)
                with open(html_path, "wb") as f_html:
                    f_html.write(html_bytes)

                source_pages_rows.append([
                    str(idx),
                    page_url,
                    resp.url,
                    os.path.join("source_pages", html_name),
                    str(resp.status_code),
                    resp.headers.get("Content-Type", ""),
                    resp.headers.get("Content-Length", ""),
                    resp.headers.get("Date", ""),
                    resp.headers.get("Last-Modified", ""),
                    resp.headers.get("ETag", ""),
                    html_sha,
                ])
            except Exception:
                # HTML 保存は補助機能。ここで落ちて収集全体が止まるのは避ける。
                pass
            resp.encoding = resp.apparent_encoding
            soup = BeautifulSoup(resp.text, "html.parser")
    
            for a in soup.find_all("a"):
                href = a.get("href")
                if not href:
                    continue
                abs_url = urljoin(page_url, href)
                # URL全体から *.xls(x) を検出（path endswith 依存を避ける）
                if extract_excel_filename_from_url(abs_url):
                    found.append({"url": abs_url, "source_page": page_url})
    
        # source page の一覧を出力（HTML/ヘッダ/sha256）
        source_pages_csv = os.path.join(snapshot_root, "_source_pages.csv")
        with open(source_pages_csv, "w", encoding="utf-8", newline="") as f_sp:
            w = csv.writer(f_sp)
            w.writerows(source_pages_rows)

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
        counts: Dict[str, int] = {"合併前": 0, "再編前": 0, "再編後": 0, "other": 0}
    
        for it in links:
            url = it["url"]
            source_page = it["source_page"]
            filename = os.path.basename(urlparse(url).path)
            if (not filename) or (os.path.splitext(filename)[1].lower() not in TARGET_EXTS):
                fn2 = extract_excel_filename_from_url(url)
                if fn2:
                    filename = fn2
    
            year, month, era_tag = parse_year_month_from_filename(filename)
            bucket = merger_bucket(year, month)
    
            # 年月が取れない場合は bucket も other に揃える（マニフェストと保存先の整合性）
            if year is None or month is None:
                bucket = "other"
    
            # 保存先：bucket/YYYY（年月取れないものは other）
            if year is None or month is None:
                save_dir = os.path.join(snapshot_root, "other")
            else:
                # 月フォルダは作らない（1年12ファイルを年フォルダ直下に置く）
                save_dir = os.path.join(snapshot_root, bucket, f"{year:04d}")
    
            ensure_dir(save_dir)
            save_path = os.path.join(save_dir, filename)
            downloaded_at = ""
            for attempt in range(1, MAX_RETRIES + 1):
                status = None
                hdr: Dict[str, str] = {}
                try:
                    status, hdr, saved_path, file_sha = download_stream(url, save_path)
    
                    # 成功
                    if status == 200:
                        downloaded_at = (started_at + timedelta(seconds=(time.monotonic() - t0))).isoformat()
                        polite_sleep(1.0)  # ★成功後だけ待機
                        try:
                            validate_excel_magic(saved_path, filename)
                        except Exception:
                            try:
                                if os.path.exists(save_path):
                                    os.remove(save_path)
                            except Exception:
                                pass
                            raise
                        counts[bucket] = counts.get(bucket, 0) + 1

                        rows.append([
                            bucket,
                            "" if year is None else str(year),
                            "" if month is None else str(month),
                            era_tag or "",
                            filename,
                            os.path.basename(saved_path),
                            url,
                            source_page,
                            downloaded_at,
                            str(status),
                            hdr.get("Content-Length", ""),
                            hdr.get("Last-Modified", ""),
                            hdr.get("ETag", ""),
                            hdr.get("Content-Type", ""),
                            file_sha,
                            "downloaded",
                        ])
                        print(f"OK: {bucket} {filename}")
                        break
    
                    # 一時的な混雑・障害はリトライ（429 / 5xx）
                    if status == 429 or status >= 500:
                        backoff = min(30, 2 ** (attempt - 1))  # 1,2,4,8... 上限30秒
                        ra = hdr.get("Retry-After", "")
                        if ra.isdigit():
                            backoff = max(backoff, int(ra))
                        print(f"RETRY: status={status} attempt={attempt}/{MAX_RETRIES} wait={backoff}s {filename}")
                        time.sleep(backoff)
                        if attempt == MAX_RETRIES:  # [CHANGED]
                            raise RuntimeError(f"HTTP status={status}")  # [CHANGED]
                        continue
    
                    # 404 など：リトライしても無駄
                    raise RuntimeError(f"HTTP status={status}")
    
                except Exception as e:
                    if attempt == MAX_RETRIES:
                        downloaded_at = downloaded_at or started_at.isoformat()  # [CHANGED]
                        rows.append([
                            bucket,
                            "" if year is None else str(year),
                            "" if month is None else str(month),
                            era_tag or "",
                            filename,
                            filename,
                            url,
                            source_page,
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
                        warn(f"FAILED: {filename} ({e})")
                    else:
                        backoff = min(30, 2 ** (attempt - 1))
                        print(f"RETRY(EX): attempt={attempt}/{MAX_RETRIES} wait={backoff}s {filename} err={e}")
                        time.sleep(backoff)
    
        # 3) マニフェスト出力
        with open(manifest_path, "w", newline="", encoding="utf-8-sig") as f:
            w = csv.writer(f)
            w.writerow([
                "bucket",
                "year",
                "month",
                "era_tag",
                "filename_original",
                "filename_saved",
                "url",
                "source_page",
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


        # ===============================================================
        # 追加: other 一覧CSV（全件）出力（v19相当） ※他の機構は不変更
        # ===============================================================
        other_rows: List[List[str]] = []
        for r in rows:
            if not r:
                continue
            bucket = (r[0] or "").strip() if len(r) > 0 else ""
            if bucket == "other":
                other_rows.append(r)
        if other_rows:
            other_list_path = os.path.join(snapshot_root, "_OTHER_FILES_ALL.csv")
            with open(other_list_path, "w", newline="", encoding="utf-8-sig") as f:
                w = csv.writer(f)
                w.writerow([
                    "bucket",
                    "year",
                    "month",
                    "era_tag",
                    "filename_original",
                    "filename_saved",
                    "url",
                    "source_page",
                    "downloaded_at",
                    "http_status",
                    "content_length",
                    "last_modified",
                    "etag",
                    "content_type",
                    "sha256",
                    "status",
                ])
                w.writerows(other_rows)

        # ===============================================================
        # 警告（town と同様に収集・保存する）
        # ===============================================================
        fixed_total = counts.get("合併前", 0) + counts.get("再編前", 0)
        if fixed_total != EXPECTED_FIXED_TOTAL_OLD_BEFORE:
            warn(f"固定範囲（合併前+再編前）のダウンロード件数が想定と違います: {fixed_total}件（想定 {EXPECTED_FIXED_TOTAL_OLD_BEFORE}件）")
        if counts.get("other", 0) > 0:
            warn("bucket=other が存在します（年月抽出できない/想定外のファイル名の可能性）。_manifest_download.csv を確認してください。")

        finished_at, ntp_server_finished = get_ntp_jst()
        elapsed_monotonic_sec = time.monotonic() - t0
    
        # 4) run_meta
        meta = {
            "dataset": DATASET_NAME,
            "publisher": "浜松市",
            "snapshot_id": snapshot_id,
            "started_at": started_at.isoformat(),
            "finished_at": finished_at.isoformat(),
            "ntp_server_started": ntp_server_started,
            "ntp_server_finished": ntp_server_finished,
            "elapsed_monotonic_sec": round(elapsed_monotonic_sec, 6),
            "script_name": os.path.basename(__file__),
            "script_sha256": script_sha,
            "python_version": sys.version,
            "base_urls": BASE_URLS,
            "target_exts": list(TARGET_EXTS),
            "merger_cutoff_ym": {"year": MERGER_CUTOFF_YM[0], "month": MERGER_CUTOFF_YM[1]},
            "reorg_cutoff_ym": {"year": REORG_CUTOFF_YM[0], "month": REORG_CUTOFF_YM[1]},
            "expected_fixed_total_old_before": EXPECTED_FIXED_TOTAL_OLD_BEFORE,
            "outputs": {
                "snapshot_root": snapshot_root,
                "manifest_csv": os.path.basename(manifest_path),
                "tools_dir": "tools",
            },
            "counts": counts,
        }
        meta_path = os.path.join(snapshot_root, "_run_meta.json")
        with open(meta_path, "w", encoding="utf-8") as f:
            json.dump(meta, f, ensure_ascii=False, indent=2)

        # ===============================================================
        # 追加: snapshot_root を ZIP 凍結（snapshot.zip + snapshot.zip.sha256）
        # ===============================================================
        # warnings をファイルに出力（ZIP凍結に含める）
        if warnings:
            warnings_path = os.path.join(snapshot_root, "_WARNINGS.txt")
            with open(warnings_path, "w", encoding="utf-8") as f_w:
                for m in warnings:
                    f_w.write(m + "\n")

        # ===============================================================
        # 追加: 結果判定（OK/WARN/ERROR）とステータス出力（必ず残す）
        # ===============================================================
        errors_count = _count_errors_from_rows(rows, status_index=15)  # status列
        if errors_count > 0:
            result = "ERROR"
        elif warnings:
            result = "WARN"
        else:
            result = "OK"

        _write_status_and_markers(
            snapshot_root,
            result=result,
            started_at_iso=started_at.isoformat(),
            finished_at_iso=finished_at.isoformat(),
            warnings=warnings,
            errors_count=errors_count,
        )

        zip_path, zip_sha = zip_snapshot_folder(snapshot_root)

    
        # 5) 結果表示
        print("\n--- ダウンロード結果 ---")
        for k, v in counts.items():
            print(f"{k}: {v}件")

        # 固定件数チェック（合併前 + 再編前）
        print("\n--- 出力 ---")
        print(f"snapshot_root: {snapshot_root}")
        print(f"snapshot_zip: {zip_path}")
        print(f"snapshot_zip_sha256: {zip_sha}")
        print(f"manifest: {manifest_path}")
        print(f"run_meta: {meta_path}")
    
        print("\nNOTE: スナップショットは ZIP + sha256 により凍結されました。これから attrib +R により読み取り専用属性を付与します。")  # [CHANGED]
        if result in ("OK", "WARN"):
            subprocess.run(f'attrib +R /S /D "{snapshot_root}\\*"', shell=True, check=True)
        else:
            warn("結果=ERROR のため attrib +R は適用しません（再処理・掃除を優先）。")
    
    


        # ERROR のときは snapshots_failed へ退避して“失敗回”を明確化
        if result == "ERROR":
            moved_from = snapshot_root
            snapshot_root = _move_to_failed(snapshot_root, ARCHIVE_ROOT, snapshot_id)
            _write_status_and_markers(
                snapshot_root,
                result=result,
                started_at_iso=started_at.isoformat(),
                finished_at_iso=finished_at.isoformat(),
                warnings=warnings,
                errors_count=errors_count,
                extra={"moved_from": moved_from, "moved_to": snapshot_root},
            )
            print(f"\n[ERROR] 一部のダウンロードが失敗したため snapshots_failed に退避しました: {snapshot_root}")
        elif result == "OK":
            print("\n[OK] 警告なし・失敗なしで完了しました。")
        else:
            print("\n[WARN] 警告はありますが、致命的失敗なく完了しました。")
    except Exception as e:
        # 失敗時に“証拠を消さない”。削除はしない。必ず失敗を明示して退避する。
        try:
            warn(f"RUN FAILED: {e}")
        except Exception:
            pass

        try:
            try:
                if warnings:
                    warn_path = os.path.join(snapshot_root, "_WARNINGS.txt")
                    write_text(warn_path, "\n".join(warnings) + "\n")
            except Exception:
                pass

            _write_status_and_markers(
                snapshot_root,
                result="FATAL",
                started_at_iso=started_at.isoformat(),
                finished_at_iso="",
                warnings=warnings,
                errors_count=0,
                extra={"exception": str(e)},
            )

            try:
                subprocess.run(f'attrib -R /S /D "{snapshot_root}\\*"', shell=True, check=True)
            except Exception:
                pass

            moved_from = snapshot_root
            snapshot_root = _move_to_failed(snapshot_root, ARCHIVE_ROOT, snapshot_id)

            _write_status_and_markers(
                snapshot_root,
                result="FATAL",
                started_at_iso=started_at.isoformat(),
                finished_at_iso="",
                warnings=warnings,
                errors_count=0,
                extra={"exception": str(e), "moved_from": moved_from, "moved_to": snapshot_root},
            )

            print(f"[FATAL] 実行に失敗しました（削除せず snapshots_failed に退避）: {snapshot_root}")
        except Exception:
            print(f"[FATAL] 実行に失敗しました: {e}")
        raise SystemExit(1)

if __name__ == "__main__":
    main()
