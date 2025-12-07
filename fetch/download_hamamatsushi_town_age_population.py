import os
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import re

# -----------------------------------
# 保存ディレクトリの定義と作成
# -----------------------------------
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
root_dir = os.path.join(
    BASE_DIR,
    "..",
    "data",
    "raw",
    "浜松市",
    "町字別・年齢別人口表"
)
root_dir = os.path.abspath(root_dir)  # 正規化
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

# 天竜区についてはファイル名の表記が、本来「tenryuku」との表記であるが、「tenryuuku」や「tennryuuku」などのけしからん表記もある。

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


# 天竜区についてはファイル名の表記が、本来「tenryuku」との表記であるが、「tenryuuku」や「tennryuuku」などのけしからん表記もある。

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

    # カウンタ初期化
    download_count.setdefault(era, {})
    download_count[era].setdefault(district, 0)
    
    # era の英語識別子 → 日本語フォルダ名
    jp_era = era_to_jp.get(era, "other")

    # 保存先ディレクトリ
    save_dir = os.path.join(root_dir, jp_era, district)
    os.makedirs(save_dir, exist_ok=True)

    save_path = os.path.join(save_dir, filename)
    print(f"Downloading {filename} → {save_path}")

    # ダウンロード
    try:
        with requests.get(link, stream=True) as r:
            r.raise_for_status()
            with open(save_path, 'wb') as f:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)
        download_count[era][district] += 1

    except Exception as e:
        print("Failed:", link, e)


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

print("------------------------------")

