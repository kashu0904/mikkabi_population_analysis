
# -*- coding: utf-8 -*-
"""
浜松市「区別・町字別世帯数人口」ダウンロード（研究用アーカイブ凍結版）

- data/archive/浜松市/区別・町字別世帯数人口/snapshots/<timestamp>/ 以下へ保存
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
import re
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Tuple
from urllib.parse import urljoin, urlparse

import requests
from bs4 import BeautifulSoup


# ===============================================================
# 設定
# ===============================================================

JST = timezone(timedelta(hours=9))

DATASET_NAME = "区別・町字別世帯数人口"

PROJECT_ROOT = r"C:\Users\pirat\Documents\MikkabiLab_population_analysis"
ARCHIVE_ROOT = os.path.join(PROJECT_ROOT, "data", "archive", "浜松市", DATASET_NAME)


BASE_URLS = [
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_kubetsu.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu2.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu1.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu.html",
]

TARGET_EXTS = (".xls", ".xlsx")  # 必要なら ".pdf" を追加

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


# 合併判定（ページ説明に合わせる：平成17年6月末以前は旧浜松市）
# -> 2005-06 まで = 合併前、2005-07 から = 合併後
MERGER_CUTOFF_YM = (2005, 7)


# ===============================================================
# ユーティリティ
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
    if year is None:
        return "other"
    cy, cm = MERGER_CUTOFF_YM
    if month is None:
        return "合併前" if year < cy else "合併後"
    return "合併前" if (year, month) < (cy, cm) else "合併後"

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
        r.raise_for_status()
        with open(save_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=CHUNK_SIZE):
                if chunk:
                    f.write(chunk)
        hdr = {
            "ETag": r.headers.get("ETag", ""),
            "Last-Modified": r.headers.get("Last-Modified", ""),
            "Content-Length": r.headers.get("Content-Length", ""),
            "Content-Type": r.headers.get("Content-Type", ""),
        }
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

    script_copy_path = os.path.join(tools_dir, "download_script.py")
    copy_self(script_copy_path)

    requirements_path = os.path.join(tools_dir, "requirements.txt")
    write_text(requirements_path, pip_freeze())

    run_command_path = os.path.join(tools_dir, "run_command.txt")
    write_text(run_command_path, f"{sys.executable} {os.path.basename(__file__)}\n")

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
        "Merger bucket rule: 2005-06 and earlier = 合併前, 2005-07 and later = 合併後.",
        "",
    ]))

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
                found.append({"url": urljoin(page_url, href), "source_page": page_url})

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
    counts: Dict[str, int] = {"合併前": 0, "合併後": 0, "other": 0}

    for it in links:
        url = it["url"]
        source_page = it["source_page"]
        filename = os.path.basename(urlparse(url).path)

        year, month, era_tag = parse_year_month_from_filename(filename)
        bucket = merger_bucket(year, month)

        # 保存先：bucket/YYYY/MM（年月取れないものは other）
        if year is None or month is None:
            save_dir = os.path.join(snapshot_root, "other")
        else:
            save_dir = os.path.join(snapshot_root, bucket, f"{year:04d}", f"{month:02d}")
        ensure_dir(save_dir)
        save_path = os.path.join(save_dir, filename)

        downloaded_at = now_jst().isoformat()

        try:
            status, hdr = download_stream(url, save_path)
            file_sha = sha256_file(save_path)
            counts[bucket] = counts.get(bucket, 0) + 1

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
                str(status),
                hdr.get("Content-Length", ""),
                hdr.get("Last-Modified", ""),
                hdr.get("ETag", ""),
                hdr.get("Content-Type", ""),
                file_sha,
                "downloaded",
            ])
            print(f"OK: {bucket} {filename}")

        except Exception as e:
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
                "", "", "", "", "", "",
                f"error: {e}",
            ])
            print(f"FAILED: {filename} ({e})")

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
        "merger_cutoff_ym": {"year": MERGER_CUTOFF_YM[0], "month": MERGER_CUTOFF_YM[1]},
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

    # 5) 結果表示
    print("\n--- ダウンロード結果 ---")
    for k, v in counts.items():
        print(f"{k}: {v}件")

    print("\n--- 出力 ---")
    print(f"snapshot_root: {snapshot_root}")
    print(f"manifest: {manifest_path}")
    print(f"run_meta: {meta_path}")

    print("\nNOTE: 取得後は data/archive 配下を読み取り専用(ACL)にして凍結してください。")


if __name__ == "__main__":
    main()
