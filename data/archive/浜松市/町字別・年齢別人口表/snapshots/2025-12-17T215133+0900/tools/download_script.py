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

def _find_project_root_by_data(start_dir: str) -> str:
    """上位階層をたどって data/ を目印に PROJECT_ROOT を推定する。"""
    cur = os.path.abspath(start_dir)
    for _ in range(20):
        if os.path.isdir(os.path.join(cur, "data")):
            return cur
        parent = os.path.abspath(os.path.join(cur, ".."))
        if parent == cur:
            break
        cur = parent
    raise RuntimeError("PROJECT_ROOT を推定できません（上位階層に data/ が見つかりません）。MIKKABI_PROJECT_ROOT を指定してください。")

_env_root = os.environ.get("MIKKABI_PROJECT_ROOT")
if _env_root:
    if not os.path.isdir(os.path.join(_env_root, "data")):
        raise RuntimeError("MIKKABI_PROJECT_ROOT を指定していますが、その配下に data/ が見つかりません。誤った場所へ保存しないため中止します。")
    PROJECT_ROOT = _env_root
else:
    PROJECT_ROOT = _find_project_root_by_data(BASE_DIR)


# アーカイブ（凍結）ルート
ARCHIVE_ROOT = os.path.join(PROJECT_ROOT, "data", "archive", "浜松市", DATASET_NAME)

# 取得対象URL（浜松市 年齢別人口表）
BASE_URLS = [
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_nenreibetsu.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu2.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu1.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu.html",
]

# 対象拡張子
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

MAX_RETRIES = 1


def polite_sleep(base: float = 1.0) -> None:
    # 固定1秒より少しだけ揺らす（連続アクセス感が減る）
    time.sleep(base + random.random() * 0.4)

CHUNK_SIZE = 8192

# テスト用：件数制限（Noneで全件）
MAX_FILES = None  # 例: 20


# ===============================================================
# 固定件数検証ベースライン（メモ）
#
# これは 2025-12-15 時点で浜松市HPから取得できた「合併前(old)＋再編前(before)」の
# 公開ファイル一式の件数を基準として固定検証するためのメモです。
# 将来この件数が増減した場合は、サイト側で公開データの追加/差し替えが起きた可能性が高いです。
# ===============================================================
BASELINE_FIXED_COUNTS = {
    "as_of": "2025-12-15",
    "scope": "old+before",
    "expected_fixed_total_old_before": 329,
    "note": "2025-12-15時点で浜松市HPから取得できた公開ファイル数。以後この件数は変わらない前提で固定検証する。",
}


# ===============================================================
# 町字別・年齢別人口表：区分ロジック（既存スクリプトを踏襲）
# ===============================================================

def _normalize_digits(s: str) -> str:
    return s.translate(str.maketrans("０１２３４５６７８９", "0123456789"))

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



# ===============================================================
# 行政区分類（再編後）
# ===============================================================
def classify_reorg_after(name):
    name_l = name.lower()

    if "chuouku" in name_l:
        return "中央区"
    if "hamanaku" in name_l:
        return "浜名区"
    if re.search(r"ten+ryu+ku", name_l):
        return "天竜区"
    if "hamamatsushi" in name_l:
        return "浜松市"

    return "other"

# 天竜区についてはファイル名の表記が、本来「tenryuku」との表記であるが、「tenryuuku」や「tennryuuku」などのけしからん表記揺れもある。

# ===============================================================
# 行政区分類（再編前：2005〜2023）
# ===============================================================
def classify_reorg_before(name):
    name_l = name.lower()

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
    if re.search(r'(?<!hama)kitaku', name_l):
        return "北区"
    if re.search(r"ten+ryu+ku", name_l):
        return "天竜区"

    if "hamamatsushi" in name_l:
        return "浜松市"

    return "other"


# 天竜区についてはファイル名の表記が、本来「tenryuku」との表記であるが、「tenryuuku」や「tennryuuku」などのけしからん表記揺れもある。

# ===============================================================
# 市町村合併前（old）の行政区分類
# ===============================================================
def classify_reorg_old(name):
    """
    市町村合併前（old 時代）の行政区分類を行う。

    特徴:
        ・浜松市が公開していた旧 Excel ファイルの命名揺れに合わせて判定する。
        ・特に "cyu" は行政側が旧データで実際に使用していた表記であり、
          一般的な "chuo"（中央）では判定しない。
        ・"hama" は浜名〜龍山エリア全般を広く包含するため、意図的に広めにマッチさせる。
        ・"hamamatsu" を含む場合は例外として必ず other とする（行政仕様に基づく）。
    """
    name_l = name.lower()

    # hamamatsu を含む場合は必ず other とする（行政仕様による例外）
    if "hamamatsu" in name_l:
        return "other"

    # 旧浜松市（中央〜江東）
    # 通常は "chuo" と表記されるが、旧公開データでは "cyu" という特殊表記が使用されていたため、
    # それに合わせて判定している。
    if "cyu" in name_l:
        return "浜松市、中央～江東"

    # 白脇〜可美エリア
    if "shiro" in name_l:
        return "白脇～可美"

    # 浜名〜龍山エリア
    # hamakita / hamana / hama... 等をまとめて包含する。
    if "hama" in name_l:
        return "浜名～龍山"

    return "other"


# ----------------------------------------------------------------
# 市町村合併前の名称および分類について
#
# 参考URL：
#   https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu.html
#
# 対象期間：
#   平成18年10月1日まで（市町村合併直前の旧地域区分）
#
# 【浜松市（旧市内）】
#   浜松市計、中央、西、城北、北、東、駅南、県居、入野、佐鳴台、
#   富塚、萩丘、曳馬、江東
#
# 【白脇〜可美（旧郊外エリア）】
#   白脇、江西、新津、篠原、庄内、和地、伊佐見、神久呂、都田、新都田、
#   三方原、花川、積志、長上、笠井、中ノ町、和田、蒲、飯田、芳川、
#   河輪、五島、可美
#
# 【浜名〜龍山（市町村合併直前区分）】
#   対象期間：平成17年10月1日〜平成18年10月1日
#   浜名、北浜、中瀬、赤佐、麁玉（以上 旧浜北市）、
#   天竜、舞阪、雄踏、細江、引佐、三ヶ日、
#   春野、佐久間、水窪、龍山
# ----------------------------------------------------------------



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
    """ファイル名（URL末尾）から行政区（または旧地域区分）を推定する。

    - era（old/before/after）を filename から判定し、時代に応じた分類関数へ委譲する。
    - 取れない場合は "other"
    """
    era = classify_era(filename)

    if era == "after":
        return classify_reorg_after(filename)
    if era == "before":
        return classify_reorg_before(filename)
    if era == "old":
        return classify_reorg_old(filename)

    # era が判定できない場合は、念のため順に試す（他機能への影響を避ける）
    d = classify_reorg_after(filename)
    if d != "other":
        return d
    d = classify_reorg_before(filename)
    if d != "other":
        return d
    d = classify_reorg_old(filename)
    if d != "other":
        return d

    return "other"


# ===============================================================
# 凍結（スナップショット）ユーティリティ
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

def _detect_excel_signature(path: str) -> str:
    """ダウンロード後の実体が Excel かを簡易判定する（中身判定）。"""
    try:
        with open(path, "rb") as f:
            head8 = f.read(8)
    except Exception:
        return "unknown"

    if head8.startswith(b"PK"):
        return "xlsx"
    if head8.startswith(b"\xD0\xCF\x11\xE0\xA1\xB1\x1A\xE1"):
        return "xls"
    return "unknown"


def _validate_excel_file(path: str, expected_ext: str) -> None:
    expected_ext = (expected_ext or "").lower()
    kind = _detect_excel_signature(path)

    if expected_ext == ".xlsx" and kind != "xlsx":
        raise ValueError(f"Downloaded content is not .xlsx (signature={kind})")
    if expected_ext == ".xls" and kind != "xls":
        raise ValueError(f"Downloaded content is not .xls (signature={kind})")


def download_stream(url: str, save_path: str) -> Tuple[int, Dict[str, str], str, str]:
    ensure_dir(os.path.dirname(save_path))
    expected_ext = os.path.splitext(save_path)[1].lower()
    tmp_path = save_path + ".part"

    # 既存の中途半端ファイルがあれば除去
    try:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)
    except Exception:
        pass

    try:
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
                return status, hdr, "", ""
            with open(tmp_path, "wb") as f:
                for chunk in r.iter_content(chunk_size=CHUNK_SIZE):
                    if chunk:
                        f.write(chunk)

        # (C) 中身で検証（Excel でないなら失敗扱い）
        _validate_excel_file(tmp_path, expected_ext)

        file_sha = sha256_file(tmp_path)

        final_path = resolve_save_path_on_collision(save_path, file_sha)
        os.replace(tmp_path, final_path)
        return status, hdr, final_path, file_sha

    except Exception:
        # 失敗時：中途半端ファイルを残さない
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
        raise


# ===============================================================
# メイン
# ===============================================================

def main() -> None:
    warnings: List[str] = []
    def warn(msg: str) -> None:
        print(f"[WARNING] {msg}")
        warnings.append(msg)

    # ===============================================================
    # 厳格モード: NTP時刻の取得は必須（フォールバック禁止）
    # - ntplib が無い / NTPが取れない → 即終了
    # - 参照時刻は「開始NTP」「終了NTP」の2回のみ取得
    # ===============================================================
    try:
        import ntplib  # type: ignore
    except Exception as e:
        print(f"[ERROR] ntplib が必要です。`pip install ntplib` を実行してください: {e}")
        raise SystemExit(2)

    def _get_ntp_jst() -> Tuple[datetime, str]:
        """JSTの信頼時刻をNTPから取得する（NICT→pool）。失敗したら例外。"""
        c = ntplib.NTPClient()
        last_err = None
        for host in ("ntp.nict.jp", "pool.ntp.org"):
            try:
                resp = c.request(host, version=3, timeout=5)
                utc = datetime.fromtimestamp(resp.tx_time, tz=timezone.utc)
                return utc.astimezone(JST), host
            except Exception as e:
                last_err = e
                warn(f"NTP時刻の取得に失敗（{host}）: {e}")
        raise RuntimeError(f"NTP時刻が取得できないため中止します（フォールバック禁止）: {last_err}")

    def now_jst_ref() -> datetime:
        """厳格モードの参照時刻: NTP必須（NICT→pool）。失敗したら例外で中止。"""
        dt, _host = _get_ntp_jst()
        return dt

    run_ts, ntp_server_started = _get_ntp_jst()  # 厳格: NTP必須（フォールバック禁止）
    started_at = run_ts
    t0_monotonic = time.monotonic()  # 経過時間計測（補助情報）
    snapshot_id = run_ts.strftime("%Y-%m-%dT%H%M%S%z")
    snapshot_root = os.path.join(ARCHIVE_ROOT, "snapshots", snapshot_id)

    ensure_dir(snapshot_root)

    try:

    
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
        write_text(run_command_path, "python tools/download_script.py\n")

        # 元スクリプト名と再実行対象を明記（再現性メモ）
        origin_path = os.path.join(tools_dir, "origin.txt")
        write_text(origin_path, "\n".join([
            f"original_script_filename: {os.path.basename(__file__)}",
            "executable_script: tools/download_script.py",
            "repro_command: python tools/download_script.py",
        ]) + "\n")
    
        # README
        readme_path = os.path.join(snapshot_root, "_README.txt")
        write_text(readme_path, "\n".join([
            f"Dataset: {DATASET_NAME}",
            f"Publisher: 浜松市",
            f"Snapshot: {snapshot_id} (JST)",
            f"BaselineFixedCounts(as_of={BASELINE_FIXED_COUNTS['as_of']} / scope={BASELINE_FIXED_COUNTS['scope']} / expected_total={BASELINE_FIXED_COUNTS['expected_fixed_total_old_before']})",
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
                abs_url = urljoin(page_url, href)
                if extract_excel_filename_from_url(abs_url):
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
            ERA_LABEL_MAP["before"]: {},
            ERA_LABEL_MAP["after"]: {},
            ERA_LABEL_MAP["old"]: {},
            ERA_LABEL_MAP["other"]: {},
        }
    
        for it in links:
            url = it["url"]
            source_page = it["source_page"]
            filename = os.path.basename(urlparse(url).path)
            if (not filename) or (os.path.splitext(filename)[1].lower() not in TARGET_EXTS):
                fn2 = extract_excel_filename_from_url(url)
                if fn2:
                    filename = fn2

    
            era = classify_era(filename)
            district = classify_district_from_filename(filename)
            month = classify_month(filename)
    
            # 出力先：era / district / month
            era_dir = ERA_LABEL_MAP.get(era, era)
            save_dir = os.path.join(snapshot_root, era_dir, district, month)
            ensure_dir(save_dir)
            save_path = os.path.join(save_dir, filename)
            for attempt in range(1, MAX_RETRIES + 1):
                status = None
                hdr = {}
                try:
                    status, hdr, saved_path, file_sha = download_stream(url, save_path)
    
                    if status == 200:
                        downloaded_at = (started_at + timedelta(seconds=(time.monotonic() - t0_monotonic))).isoformat()
                        polite_sleep(1.0)  # 成功後だけ待機
    
                        download_count.setdefault(era_dir, {})
                        download_count[era_dir][district] = download_count[era_dir].get(district, 0) + 1
    
                        rows.append([
                            era_dir, district, month,
                            filename, os.path.basename(saved_path),
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
                        if attempt == MAX_RETRIES:  # [CHANGED]
                            raise RuntimeError(f"HTTP status={status}")  # [CHANGED]
                        continue
    
                    # 404等はリトライしても無駄
                    raise RuntimeError(f"HTTP status={status}")
    
                except Exception as e:
                    if attempt == MAX_RETRIES:
                        downloaded_at = (started_at + timedelta(seconds=(time.monotonic() - t0_monotonic))).isoformat()

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
        # 固定件数（合併前＋再編前）ベースライン
        EXPECTED_FIXED_TOTAL_OLD_BEFORE = BASELINE_FIXED_COUNTS["expected_fixed_total_old_before"]
        PUBLISH_GRACE_DAY = 10  # 『翌月初旬』の目安
    
    
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
            ex = [rr[3] for rr in other_rows[:100] if len(rr) > 3]
            warn(f"'other'（分類不可）が {len(other_rows)}件あります。例: {', '.join(ex)}{' ...' if len(other_rows) > 100 else ''}")
            # 全件保存（マニフェストと同じ列）
            other_list_path = os.path.join(snapshot_root, "_OTHER_FILES_ALL.csv")
            with open(other_list_path, "w", newline="", encoding="utf-8-sig") as f:
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
                w.writerows(other_rows)
    
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
    
        now = run_ts + timedelta(seconds=(time.monotonic() - t0_monotonic))  # 推定現在時刻（基準NTP＋経過）
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
    
        finished_at, ntp_server_finished = _get_ntp_jst()  # 厳格: 終了時刻もNTPで取得（失敗→中止）
        # 4) run_meta
        meta = {
            "dataset": DATASET_NAME,
            "publisher": "浜松市",
            "snapshot_id": snapshot_id,
            "started_at": started_at.isoformat(),
            "finished_at": finished_at.isoformat(),
            "ntp_server_started": ntp_server_started,
            "ntp_server_finished": ntp_server_finished,
            "elapsed_monotonic_sec": round(time.monotonic() - t0_monotonic, 6),
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
            "baseline_fixed_counts": BASELINE_FIXED_COUNTS,
        }
        meta_path = os.path.join(snapshot_root, "_run_meta.json")
        with open(meta_path, "w", encoding="utf-8") as f:
            json.dump(meta, f, ensure_ascii=False, indent=2)

        # ===============================================================
        # 追加: snapshot_root を ZIP 凍結（snapshot.zip + snapshot.zip.sha256）
        # ===============================================================
        zip_path, zip_sha = zip_snapshot_folder(snapshot_root)

    
        # 5) 結果表示
        print("\n--- ダウンロード結果（era -> district） ---")
        for era_key, d in download_count.items():
            print(f"\n[{era_key}]")
            for dist, cnt in sorted(d.items(), key=lambda x: x[0]):
                print(f"  {dist}: {cnt}件")
    
        print("\n--- 出力 ---")
        print(f"snapshot_root: {snapshot_root}")
        print(f"snapshot_zip: {zip_path}")
        print(f"snapshot_zip_sha256: {zip_sha}")
        print(f"manifest: {manifest_path}")
        print(f"run_meta: {meta_path}")
    
        print("\nNOTE: スナップショットは ZIP + sha256 により凍結されました。これから snapshot_root 配下へ attrib +R を適用します。")  # [CHANGED]
        subprocess.run(f'attrib +R /S /D "{snapshot_root}\\*"', shell=True, check=True)  # [CHANGED]
    
    

    except Exception as e:
        print(f"[ERROR] 実行に失敗しました: {e}")
        print(f"[ERROR] 途中生成物を残さないため snapshot を削除します: {snapshot_root}")
        try:  # [CHANGED]
            subprocess.run(f'attrib -R /S /D "{snapshot_root}\\*"', shell=True, check=True)  # [CHANGED]
        except Exception:  # [CHANGED]
            pass  # [CHANGED]
        try:
            shutil.rmtree(snapshot_root)
        except Exception:
            pass
        raise SystemExit(1)

if __name__ == "__main__":
    main()