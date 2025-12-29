import os
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import re

base_url = "https://www.city.hamamatsu.shizuoka.jp/gyousei/library/1_jinkou-setai/005_past_kubetsu.html"

# -----------------------------
# 年度変換（H → 平成, R → 令和）
# -----------------------------
def to_year(name):
    if match := re.search(r"H(\d{1,2})", name):
        return 1988 + int(match.group(1))  # 平成H1 = 1989
    if match := re.search(r"R(\d{1,2})", name):
        return 2018 + int(match.group(1))  # 令和R1 = 2019
    return "unknown"


# -----------------------------
# カテゴリ分類
# -----------------------------
def classify(name):
    name_l = name.lower()
    if "hamamatsushi" in name_l:
        return "浜松市"
    if "chuouku" in name_l:
        return "中央区"
    if "hamanaku" in name_l:
        return "浜名区"
    if "tenryuku" in name_l:
        return "天竜区"
    return "other"


# -----------------------------
# HTML取得
# -----------------------------
resp = requests.get(base_url)
resp.encoding = resp.apparent_encoding
soup = BeautifulSoup(resp.text, "html.parser")

# -----------------------------
# Excelリンク抽出
# -----------------------------
excel_links = []

for a in soup.find_all("a"):
    href = a.get("href")
    if not href:
        continue
    if href.lower().endswith((".xls", ".xlsx")):
        excel_links.append(urljoin(base_url, href))

print(f"発見したExcelファイル数: {len(excel_links)}")

# -----------------------------
# ダウンロード処理
# -----------------------------
root_dir = "C:\Users\pirat\Documents\MikkabiLab_population_analysis\data\raw\浜松市\町字別・年齢別人口表\行政区（新）2024-"
os.makedirs(root_dir, exist_ok=True)

for link in excel_links:
    filename = os.path.basename(link)
    category = classify(filename)
    # year = to_year(filename)

    save_dir = os.path.join(root_dir, category)
    os.makedirs(save_dir, exist_ok=True)

    save_path = os.path.join(save_dir, filename)

    print(f"Downloading {filename} → {save_path}")

    try:
        with requests.get(link, stream=True) as r:
            r.raise_for_status()
            with open(save_path, 'wb') as f:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)
    except Exception as e:
        print("Failed:", link, e)
