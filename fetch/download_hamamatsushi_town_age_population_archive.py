
# -*- coding: utf-8 -*-
"""
浜松市「町字別・年齢別人口表」ダウンロード（既存ロジック完全踏襲＋研究用アーカイブ凍結版）

- 既存の分類ロジック（表記揺れ吸収含む）・保存階層設計は変更しません。
- 変更点は「保存先を archive のスナップショットにする」ことと、
  「取得日時・manifest・run_meta・tools（コード/依存/実行コマンド）を同梱する」だけです。
"""

from __future__ import annotations

import os
import csv
import json
import hashlib
import subprocess
import sys
from datetime import datetime, timedelta, timezone

JST = timezone(timedelta(hours=9))

def _now_jst():
    return datetime.now(JST)

def _ensure_dir(p: str):
    os.makedirs(p, exist_ok=True)

def _sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()

def _sha256_text(s: str) -> str:
    return hashlib.sha256(s.encode("utf-8")).hexdigest()

def _pip_freeze() -> str:
    try:
        out = subprocess.check_output([sys.executable, "-m", "pip", "freeze"], stderr=subprocess.STDOUT, text=True)
        return out.strip() + "\n"
    except Exception as e:
        return f"# pip freeze failed: {e}\n"

def _write_text(path: str, content: str):
    _ensure_dir(os.path.dirname(path))
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

def _copy_file(src: str, dst: str):
    _ensure_dir(os.path.dirname(dst))
    with open(src, "r", encoding="utf-8") as f:
        code = f.read()
    with open(dst, "w", encoding="utf-8") as g:
        g.write(code)

_started_at = _now_jst()
_snapshot_id = _started_at.strftime("%Y-%m-%dT%H%M%S%z")

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.environ.get("MIKKABI_PROJECT_ROOT", os.path.abspath(os.path.join(BASE_DIR, "..")))

ARCHIVE_ROOT = os.path.join(PROJECT_ROOT, "data", "archive", "浜松市", "町字別・年齢別人口表")
snapshot_root = os.path.join(ARCHIVE_ROOT, _snapshot_id)

# tools 同梱
tools_dir = os.path.join(snapshot_root, "tools")
_ensure_dir(tools_dir)
_copy_file(__file__, os.path.join(tools_dir, "download_script.py"))
_write_text(os.path.join(tools_dir, "requirements.txt"), _pip_freeze())
_write_text(os.path.join(tools_dir, "run_command.txt"), f"{sys.executable} {os.path.basename(__file__)}\n")

_write_text(os.path.join(snapshot_root, "_README.txt"), "\n".join([
    "浜松市「町字別・年齢別人口表」公式配布物の原本アーカイブ（スナップショット）",
    f"取得日時（JST）: {_started_at.isoformat()}",
    "注意: このフォルダ内は編集・上書き禁止。加工は raw へコピーして実施。",
    "真正性: _manifest_download.csv の sha256 により検証可能。",
    "",
]))

with open(__file__, "r", encoding="utf-8") as _f:
    _script_text = _f.read()
_script_sha256 = _sha256_text(_script_text)

_manifest_rows = []
_manifest_path = os.path.join(snapshot_root, "_manifest_download.csv")

import os
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import re

# -----------------------------------
# 保存ディレクトリの定義と作成（凍結版：snapshot_root を使用）
# -----------------------------------
root_dir = snapshot_root
os.makedirs(root_dir, exist_ok=True)

# -----------------------------------
# 取得対象URL（浜松市 年齢別人口表）
# -----------------------------------
base_urls = [
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_nenreibetsu.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu2.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu1.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/007_past_nenreibetsu.html",
]

# ===============================================================
# 年度判定ヘルパー
# ===============================================================
def to_year(name):
    """Hxx / Rxx などから西暦へ変換（※今回は未使用）"""
    if match := re.search(r"H(\d{1,2})", name):
        return 1988 + int(match.group(1))  # 平成H1 = 1989
    if match := re.search(r"R(\d{1,2})", name):
        return 2018 + int(match.group(1))  # 令和R1 = 2019
    return "unknown"

# ===============================================================
# Hxx/Rxx（時代 + 年度）抽出
# ===============================================================
def extract_era_year(filename):
    """
    ファイル名から Hxx / Rxx を robust に抽出して返す。
    小文字化・全角数字対応済み。
    """
    name = filename.lower()

    # 全角数字 → 半角へ変換
    name = name.translate(str.maketrans(
        "０１２３４５６７８９",
        "0123456789"
    ))

    # 平成
    m = re.search(r"h(\d{1,2})", name)
    if m:
        return ("H", int(m.group(1)))

    # 令和
    m = re.search(r"r(\d{1,2})", name)
    if m:
        return ("R", int(m.group(1)))

    return (None, None)

# ===============================================================
# 月判定（4月 / 10月） 
# ===============================================================
def classify_month(filename):
    """
    ファイル名から 4月 / 10月 を判定する。

    ルール:
    - 「区切り文字（- or _）」に挟まれた 04 / 4 / 10 だけを月とみなす
      例："-04-" / "_04_" / "-10-" / "_10_"
    - R04 / H04 など、英字にくっついた 04 は無視される
    - 判定不能な場合は "other" を返す
    """
    name = filename.lower()

    # 全角数字 → 半角数字（念のため）
    name = name.translate(str.maketrans(
        "０１２３４５６７８９",
        "0123456789"
    ))

    # 4月判定：[-_]0?4[-_]
    if re.search(r'[-_](0?4)[-_]', name):
        return 4

    # 10月判定：[-_]10[-_]
    if re.search(r'[-_]10[-_]', name):
        return 10

    # どちらでもなければ other
    return "other"



# 月 → フォルダ名マッピング
month_to_folder = {
    4:       "4月",
    10:      "10月",
    "other": "other",
}


# ===============================================================
# 年代区分：before / after / old / other
# ===============================================================

def classify_era(filename):
    era, year = extract_era_year(filename)

    if era == "H":
        if 19 <= year <= 31:
            return "before"   # 行政区再編前（政令市区割時代）
        elif year <= 18:
            return "old"      # 市町村合併前（旧自治体時代）
        else:
            return "other"

    if era == "R":
        if 1 <= year <= 5:
            return "before"   # 再編前
        elif year >= 6:
            return "after"    # 再編後（2024〜）
        else:
            return "other"

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


# ===============================================================
# Excelリンク抽出
# ===============================================================
excel_links = []

for base_url in base_urls:
    print(f"取得中：{base_url}")

    resp = requests.get(base_url)
    resp.encoding = resp.apparent_encoding
    soup = BeautifulSoup(resp.text, "html.parser")

    for a in soup.find_all("a"):
        href = a.get("href")
        if not href:
            continue
        if href.lower().endswith((".xls", ".xlsx")):
            excel_links.append(urljoin(base_url, href))

print(f"総発見Excelファイル数: {len(excel_links)}")


# ===============================================================
# ダウンロード件数のカウンタ
# ===============================================================
other_files = {
    "before_other": [],
    "after_other": [],
    "old_other": [],
    "era_other": [],
}

download_count = {
    "before": {
        "浜松市": 0, "中区": 0, "東区": 0, "西区": 0, "南区": 0,
        "北区": 0, "浜北区": 0, "天竜区": 0, "other": 0,
    },
    "after": {
        "浜松市": 0, "中央区": 0, "浜名区": 0, "天竜区": 0, "other": 0,
    },
    "old": {
        "浜松市、中央～江東": 0, "白脇～可美": 0, "浜名～龍山": 0, "other": 0,
    },
    "other": {
        "other": 0
    }
}

# === ここから追加: 月判定 other のログ用 ================================
month_other_files = {
    "before": {},
    "after":  {},
    "old":    {},
    "other":  {},
}

# ===============================================================
# ダウンロード処理
# ===============================================================

# era を日本語フォルダ名に変換
era_to_jp = {
    "before": "再編前",
    "after": "再編後",
    "old": "市町村合併前",
    "other": "other",
}

for link in excel_links:
    filename = os.path.basename(link)

    #  年代判定
    era = classify_era(filename)

    #  行政区分類
    if era == "after":
        district = classify_reorg_after(filename)
    elif era == "before":
        district = classify_reorg_before(filename)
    elif era == "old":
        district = classify_reorg_old(filename)
    else:
        print(f"年度判定不可 → other として保存: {filename}")
        era = "other"
        district = "other"
        
    # district = other の場合のみ記録
    if district == "other":
        other_key = f"{era}_other"   # before_other / after_other / old_other / other_other
        other_files.setdefault(other_key, []).append(filename)
        print(f"地区名判定不可 → other として保存: {filename}")

    month = classify_month(filename)
    month_folder = month_to_folder.get(month, "other")

    if month == "other":
        era_dict = month_other_files.setdefault(era, {})
        era_dict.setdefault(district, []).append(filename)

    # カウンタ初期化
    download_count.setdefault(era, {})
    download_count[era].setdefault(district, 0)
    
    # era の英語識別子 → 日本語フォルダ名
    jp_era = era_to_jp.get(era, "other")

    # 保存先ディレクトリ（月フォルダを追加）
    save_dir = os.path.join(root_dir, jp_era, district, month_folder)
    os.makedirs(save_dir, exist_ok=True)

    save_path = os.path.join(save_dir, filename)
    print(f"Downloading {filename} → {save_path}")

    downloaded_at = _now_jst().isoformat()

    # ダウンロード
    try:
        with requests.get(link, stream=True) as r:
            r.raise_for_status()
            with open(save_path, 'wb') as f:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)
        download_count[era][district] += 1
        # --- archive manifest 記録（凍結版付帯） ---
        try:
            file_sha = _sha256_file(save_path)
        except Exception:
            file_sha = ""

        _manifest_rows.append([
            _snapshot_id,
            era,
            jp_era,
            district,
            month_folder,
            filename,
            save_path,
            link,
            downloaded_at,
            str(getattr(r, "status_code", "")),
            r.headers.get("Content-Length", "") if hasattr(r, "headers") else "",
            r.headers.get("Last-Modified", "") if hasattr(r, "headers") else "",
            r.headers.get("ETag", "") if hasattr(r, "headers") else "",
            r.headers.get("Content-Type", "") if hasattr(r, "headers") else "",
            file_sha,
            "downloaded",
        ])

    except Exception as e:
        print("Failed:", link, e)
        _manifest_rows.append([
            _snapshot_id,
            era,
            jp_era,
            district,
            month_folder,
            filename,
            save_path,
            link,
            downloaded_at,
            "", "", "", "", "",
            "",
            f"error: {e}",
        ])


# ===============================================================
# ダウンロード結果表示
# ===============================================================
print("\n--- ダウンロード結果 ---")

print("[再編前(before)]")
for cat, count in download_count["before"].items():
    print(f"  {cat}: {count}件")

print("\n[再編後(after)]")
for cat, count in download_count["after"].items():
    print(f"  {cat}: {count}件")

print("\n[市町村合併前(old)]")
for cat, count in download_count["old"].items():
    print(f"  {cat}: {count}件")

print("\n[年代判定不能（era = other）]")
for cat, count in download_count["other"].items():
    print(f"  {cat}: {count}件")

print("-------------------------")


# ===============================================================
# otherとなったファイルのファイル名
# ===============================================================

print("\n--- other になったファイル一覧 ---")

print("\n[年代判定 other]")
for fn in other_files["era_other"]:
    print("  ", fn)

print("\n[再編前 before → district = other]")
for fn in other_files["before_other"]:
    print("  ", fn)

print("\n[再編後 after → district = other]")
for fn in other_files["after_other"]:
    print("  ", fn)

print("\n[合併前 old → district = other]")
for fn in other_files["old_other"]:
    print("  ", fn)

print("\n[年代=other かつ district=other → other_other]")
for fn in other_files.get("other_other", []):
    print("  ", fn)

print("\n--- 4月・10月 判定できなかったファイル（月 = other） ---")
for era_key, districts in month_other_files.items():
    if not districts:
        continue
    print(f"\nera = {era_key}")
    for dist, files in districts.items():
        print(f"  district = {dist}")
        for fn in files:
            print("    ", fn)

print("----------------------------------------------------------")
# === ここまで追加 =====================================================


# ===============================================================
# 凍結版付帯：manifest / run_meta 出力
# ===============================================================
with open(_manifest_path, "w", newline="", encoding="utf-8-sig") as f:
    w = csv.writer(f)
    w.writerow([
        "snapshot_id",
        "era_key",
        "era_folder_jp",
        "district",
        "month_folder",
        "filename",
        "saved_path",
        "url",
        "downloaded_at",
        "http_status",
        "content_length",
        "last_modified",
        "etag",
        "content_type",
        "sha256",
        "status",
    ])
    w.writerows(_manifest_rows)

_finished_at = _now_jst()
_run_meta = {
    "dataset": "町字別・年齢別人口表",
    "publisher": "浜松市",
    "snapshot_id": _snapshot_id,
    "started_at": _started_at.isoformat(),
    "finished_at": _finished_at.isoformat(),
    "script_name": os.path.basename(__file__),
    "script_sha256": _script_sha256,
    "python_version": sys.version,
    "base_urls": base_urls,
    "outputs": {
        "snapshot_root": snapshot_root,
        "manifest_csv": os.path.basename(_manifest_path),
        "tools_dir": "tools",
    },
    "note": "分類・表記揺れ吸収などの処理機構は既存スクリプトを変更していない。保存先を archive のスナップショットに切替え、凍結用付帯物を追加した。",
}

with open(os.path.join(snapshot_root, "_run_meta.json"), "w", encoding="utf-8") as f:
    json.dump(_run_meta, f, ensure_ascii=False, indent=2)

print("\n--- archive outputs ---")
print("snapshot_root:", snapshot_root)
print("manifest:", _manifest_path)
print("run_meta:", os.path.join(snapshot_root, "_run_meta.json"))
