import os
import re
import csv
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin, urlparse

# ===============================================================
# 設定
# ===============================================================

BASE_DIR = os.path.dirname(os.path.abspath(__file__))

DATASET_NAME = "区別・町字別世帯数人口"
root_dir = os.path.abspath(os.path.join(
    BASE_DIR, ".", "data", "raw", "浜松市", DATASET_NAME
))
os.makedirs(root_dir, exist_ok=True)

# 浜松市の掲載ページ（ユーザー提示URL）
base_urls = [
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_kubetsu.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu2.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu1.html",
    "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu.html",
]

# 合併前/後の境界（ページ記載: 平成17年6月末以前は旧浜松市）
# -> 2005年6月まで = 合併前、2005年7月から = 合併後
MERGER_CUTOFF_YM = (2005, 7)

# まずはExcelのみ（必要ならPDFも追加可能）
TARGET_EXTS = (".xls", ".xlsx")

# 403など避けるためにUAを明示（市サイトで稀に必要）
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                  "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
}

TIMEOUT = 60
CHUNK_SIZE = 8192

# テスト用（Noneで全件）
MAX_FILES = None  # 例: 10 にすると最初の10件だけ


# ===============================================================
# ユーティリティ
# ===============================================================

def normalize_digits(s: str) -> str:
    """全角数字→半角数字に寄せる（念のため）"""
    return s.translate(str.maketrans("０１２３４５６７８９", "0123456789"))

def era_to_western_year(era: str, n: int) -> int:
    """Hxx/Rxx -> 西暦"""
    era = era.upper()
    if era == "H":  # 平成1年=1989
        return 1988 + n
    if era == "R":  # 令和1年=2019
        return 2018 + n
    raise ValueError(f"Unknown era: {era}")

def parse_year_month_from_filename(filename: str):
    """
    例:
      setaisu-jinkousu_area_r07-01.xlsx -> (2025, 1, "R07")
      setaisu-jinkousu_area_h17-06.xls  -> (2005, 6, "H17")

    返り値: (year, month, era_tag) / 取れない場合は (None, None, None)
    """
    name = normalize_digits(filename.lower())

    # 1) 和暦パターン: r07-01 / h17-06（区切りは - or _ を許容）
    m = re.search(r'([hr])(\d{1,2})[-_](\d{1,2})', name)
    if m:
        era = m.group(1).upper()
        era_year = int(m.group(2))
        month = int(m.group(3))
        if 1 <= month <= 12:
            year = era_to_western_year(era, era_year)
            return year, month, f"{era}{era_year:02d}"

    # 2) 西暦パターン: 2024-01 / 2024_01 等（保険）
    m = re.search(r'((?:19|20)\d{2})[-_](\d{1,2})', name)
    if m:
        year = int(m.group(1))
        month = int(m.group(2))
        if 1 <= month <= 12:
            return year, month, None

    return None, None, None

def merger_bucket(year: int, month: int | None):
    """合併前/合併後の2択に落とす。判定不能なら 'other'."""
    if year is None:
        return "other"
    cutoff_y, cutoff_m = MERGER_CUTOFF_YM
    # year-month 比較
    if month is None:
        return "合併前" if year < cutoff_y else "合併後"
    if (year, month) < (cutoff_y, cutoff_m):
        return "合併前"
    return "合併後"

def safe_mkdir(path: str):
    os.makedirs(path, exist_ok=True)

def download_file(url: str, save_path: str):
    safe_mkdir(os.path.dirname(save_path))
    with requests.get(url, headers=HEADERS, stream=True, timeout=TIMEOUT) as r:
        r.raise_for_status()
        with open(save_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=CHUNK_SIZE):
                if chunk:
                    f.write(chunk)


# ===============================================================
# 1) Excelリンク収集
# ===============================================================

items = []  # {url, source_page}

for page_url in base_urls:
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
            items.append({"url": abs_url, "source_page": page_url})

# 重複除去（URL単位）
seen = set()
deduped = []
for it in items:
    if it["url"] in seen:
        continue
    seen.add(it["url"])
    deduped.append(it)

items = deduped
print(f"総発見Excelファイル数: {len(items)}")


# ===============================================================
# 2) ダウンロード & 保存
# ===============================================================

counts = {"合併前": 0, "合併後": 0, "other": 0}
errors = []
manifest_rows = []

if MAX_FILES is not None:
    items = items[:MAX_FILES]

for it in items:
    url = it["url"]
    source_page = it["source_page"]

    path = urlparse(url).path
    filename = os.path.basename(path)

    year, month, era_tag = parse_year_month_from_filename(filename)
    bucket = merger_bucket(year, month)

    # 年月が取れない場合は other に逃がす（フォルダも単純化）
    if year is None or month is None:
        save_dir = os.path.join(root_dir, "other")
        save_name = filename
    else:
        save_dir = os.path.join(root_dir, bucket, f"{year:04d}", f"{month:02d}")
        # 保存名は「元のファイル名」を基本維持（トレーサビリティ優先）
        save_name = filename

    save_path = os.path.join(save_dir, save_name)

    # 既にあればスキップ（再実行が軽い）
    if os.path.exists(save_path) and os.path.getsize(save_path) > 0:
        print(f"SKIP: {save_path}")
        manifest_rows.append([bucket, year, month, era_tag, filename, save_name, url, source_page, "skipped"])
        continue

    try:
        print(f"Downloading: {filename} -> {save_path}")
        download_file(url, save_path)
        counts[bucket] += 1
        manifest_rows.append([bucket, year, month, era_tag, filename, save_name, url, source_page, "downloaded"])
    except Exception as e:
        errors.append((url, str(e)))
        manifest_rows.append([bucket, year, month, era_tag, filename, save_name, url, source_page, f"error: {e}"])
        print(f"FAILED: {url} ({e})")

# ===============================================================
# 3) マニフェスト（CSV）出力
# ===============================================================

manifest_path = os.path.join(root_dir, "_manifest_download.csv")
with open(manifest_path, "w", newline="", encoding="utf-8-sig") as f:
    w = csv.writer(f)
    w.writerow(["bucket", "year", "month", "era_tag", "filename_original", "filename_saved", "url", "source_page", "status"])
    w.writerows(manifest_rows)

print("\n--- ダウンロード結果 ---")
for k, v in counts.items():
    print(f"{k}: {v}件")

print(f"\nmanifest: {manifest_path}")

if errors:
    print("\n--- エラー ---")
    for url, msg in errors[:50]:
        print(f"- {url}\n  {msg}")
    if len(errors) > 50:
        print(f"(…省略: {len(errors)}件中 50件のみ表示)")
