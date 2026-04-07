# -*- coding: utf-8 -*-
"""archive_hamajitiren_jichikai_households_v7_6.py

浜松市自治会連合会「地区別単位自治会」ページから、自治会別世帯数を取得して研究用にアーカイブする。

v7_6 追加/改善（ユーザー要望）
- すべての出力フォルダを存在前提にせず、必ず mkdir(parents=True, exist_ok=True) で作成
- 予期しない例外は traceback を表示して exit code 2

（v4由来機能）
- 実行の最後に「完全成功 / 成功(警告あり) / 失敗(エラーあり)」を明示
- 警告・エラーが出た場合、地域別の要約と上位メッセージを表示
- 出力先は常に「このスクリプト(fetch/)の1つ上（=プロジェクト直下）の data/」に固定
- 年フォルダは --year で指定可能。未指定ならページの「令和..年..月..日現在」から推定（取れない場合は実行年）

出力（例: 年=2025）
- data/自治会/2025/自治会世帯数__2025.csv
- data/自治会/2025/regions/<region_key>/自治会世帯数__2025__<region_key>.csv
- data/自治会/2025/regions/<region_key>/districts/<district>.csv
- data/自治会/archive/2025/runs/<RUN_ID>/snapshots/<region_key>.html
- data/自治会/archive/2025/runs/<RUN_ID>/logs/_WARNINGS.txt, _ERRORS.txt, run_summary.txt
- data/自治会/archive/2025/LATEST_RUN.txt
"""

from __future__ import annotations
import os
import subprocess

import argparse
import csv
import hashlib
import datetime
import json
import re
import sys
import traceback
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import requests
from bs4 import BeautifulSoup


# =========================
# 基本設定（パスは __file__ 起点で固定）
# =========================

SCRIPT_PATH = Path(__file__).resolve()
SCRIPT_DIR = SCRIPT_PATH.parent

# ===============================================================
# PROJECT_ROOT 推定（download_hamamatsushi_* の方針を踏襲）
#  - 環境変数 MIKKABI_PROJECT_ROOT があれば最優先（配下に data/ が必須）
#  - なければ、このスクリプトの場所から上位へたどって data/ を探す
# ===============================================================

def _find_project_root_by_data(start_dir: Path) -> Path:
    cur = start_dir.resolve()
    for _ in range(30):
        if (cur / "data").is_dir():
            return cur
        parent = cur.parent
        if parent == cur:
            break
        cur = parent
    raise RuntimeError("PROJECT_ROOT を推定できません（上位階層に data/ が見つかりません）。MIKKABI_PROJECT_ROOT を指定してください。")

_env_root = os.environ.get("MIKKABI_PROJECT_ROOT")
if _env_root:
    _env_root_p = Path(_env_root).expanduser().resolve()
    if not (_env_root_p / "data").is_dir():
        raise RuntimeError("MIKKABI_PROJECT_ROOT を指定していますが、その配下に data/ が見つかりません。誤った場所へ保存しないため中止します。")
    PROJECT_ROOT = _env_root_p
else:
    PROJECT_ROOT = _find_project_root_by_data(SCRIPT_DIR)

DATA_ROOT = PROJECT_ROOT / "data"
BASE_DIR = DATA_ROOT / "自治会"

TZ_JST = datetime.timezone(datetime.timedelta(hours=9))
RUN_DT = datetime.datetime.now(TZ_JST)
RUN_ID = RUN_DT.strftime("%Y%m%d_%H%M%S")  # Windowsでも安全（":"なし）

UA = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/120.0 Safari/537.36"
)

ERROR_LOG: List[str] = []
WARN_LOG: List[str] = []


def log_error(region_key: str, msg: str) -> None:
    ERROR_LOG.append(f"{region_key}: {msg}")


def log_warn(region_key: str, msg: str) -> None:
    WARN_LOG.append(f"{region_key}: {msg}")



def print_region_summary(st: RunStat) -> None:
    """地域単位の簡易サマリを標準出力に出す（失敗しても本処理を止めないための補助表示）"""
    try:
        print(f"[REGION] {st.region_key} ({st.ward}) records={st.records} errors={st.errors} warnings={st.warnings}")
    except Exception:
        # ここで落ちると本末転倒なので黙って無視
        return

# =========================
# 対象URL
# =========================

TARGETS = [
    {"key": "hamakita", "ward": "浜名区", "url": "https://www.hamajitiren.jp/chiiki/hamakita02.html"},
    {"key": "kita", "ward": "浜名区", "url": "https://www.hamajitiren.jp/chiiki/kita02.html"},
    {"key": "tenryu", "ward": "天竜区", "url": "https://www.hamajitiren.jp/chiiki/tenryu02.html"},
    {"key": "naka", "ward": "中央区", "url": "https://www.hamajitiren.jp/chiiki/naka02.html"},
    {"key": "higashi", "ward": "中央区", "url": "https://www.hamajitiren.jp/chiiki/higashi02.html"},
    {"key": "nishi", "ward": "中央区", "url": "https://www.hamajitiren.jp/chiiki/nishi02.html"},
    {"key": "minami", "ward": "中央区", "url": "https://www.hamajitiren.jp/chiiki/minami02.html"},
]


# =========================
# ユーティリティ
# =========================


def fetch_html(url: str, timeout: int = 30) -> str:
    r = requests.get(url, timeout=timeout, headers={"User-Agent": UA})
    r.raise_for_status()
    # サイト側のcharset宣言がUTF-8でも、requestsが誤判定するケース対策
    r.encoding = r.apparent_encoding or "utf-8"
    return r.text


def parse_asof_date(soup: BeautifulSoup) -> Optional[str]:
    text = soup.get_text(" ", strip=True)
    m = re.search(r"令和(\d+)年(\d+)月(\d+)日現在", text)
    if not m:
        return None
    r_year, month, day = map(int, m.groups())
    year = 2018 + r_year  # 令和1年=2019
    return f"{year:04d}-{month:02d}-{day:02d}"

def extract_year_from_asof(asof: str) -> Optional[int]:
    """基準日表記から西暦年だけを取り出す。
    想定: 'YYYY-MM-DD'（本スクリプト内では基本これ）。
    保険で '令和7年4月1日現在' のような表記も受ける。
    """
    if not asof:
        return None
    m = re.match(r"^(\d{4})-(\d{2})-(\d{2})$", asof.strip())
    if m:
        return int(m.group(1))
    m = re.search(r"令和(\d+)年", asof)
    if m:
        return 2018 + int(m.group(1))
    m = re.search(r"(\d{4})年", asof)
    if m:
        return int(m.group(1))
    return None


def parse_expected_counts(soup: BeautifulSoup) -> Tuple[Optional[int], Optional[int]]:
    """例: '5地区 95単位自治会' を拾う"""
    text = soup.get_text(" ", strip=True)
    m = re.search(r"(\d+)地区\s+(\d+)単位自治会", text)
    if not m:
        return None, None
    return int(m.group(1)), int(m.group(2))


def normalize_int(text: str) -> Optional[int]:
    text = text.replace(",", "").strip()
    if not text:
        return None
    try:
        return int(text)
    except ValueError:
        return None


def clean_district_label(label: str) -> str:
    """'都田地区（14自治会）' -> '都田地区'"""
    label = re.sub(r"\s+", " ", label.strip())
    label = re.sub(r"（.*?）", "", label)
    return label.strip()


def safe_filename(s: str) -> str:
    s = re.sub(r"\s+", "_", s.strip())
    s = re.sub(r"[\\/:*?\"<>|]", "_", s)
    return s


def ensure_dir(path: Path) -> None:
    """Create directory if missing (parents=True)."""
    path.mkdir(parents=True, exist_ok=True)

def write_csv(path: Path, rows: List[Dict], fieldnames: List[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8-sig", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rows)


def sort_rows_by_fieldnames(rows: List[Dict], fieldnames: List[str]) -> List[Dict]:
    """Return a stably sorted copy of rows.
    Sorting by fieldnames eliminates nondeterminism from scraping/order and makes
    'CSV更新' warnings meaningful (only real data changes trigger).
    """
    def _key(r: Dict):
        return tuple("" if r.get(fn) is None else str(r.get(fn)) for fn in fieldnames)
    return sorted(rows, key=_key)




def write_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")

def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()

def pip_freeze_text() -> str:
    try:
        out = subprocess.check_output([sys.executable, "-m", "pip", "freeze"], stderr=subprocess.STDOUT, text=True)
        return out.strip() + "\n"
    except Exception as e:
        return f"# pip freeze failed: {e}\n"

def copy_self(to_path: Path) -> None:
    to_path.parent.mkdir(parents=True, exist_ok=True)
    to_path.write_text(Path(__file__).read_text(encoding="utf-8"), encoding="utf-8")

def zip_folder(folder: Path, zip_path: Path) -> str:
    import zipfile
    zip_path.parent.mkdir(parents=True, exist_ok=True)
    tmp = zip_path.with_suffix(zip_path.suffix + ".part")
    if tmp.exists():
        tmp.unlink()
    if zip_path.exists():
        zip_path.unlink()
    with zipfile.ZipFile(tmp, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        for p in folder.rglob("*"):
            if p.is_dir():
                continue
            rel = p.relative_to(folder).as_posix()
            if rel in (zip_path.name, tmp.name, zip_path.name + ".sha256"):
                continue
            zf.write(p, arcname=rel)
    tmp.replace(zip_path)
    h = sha256_file(zip_path)
    write_text(zip_path.with_suffix(zip_path.suffix + ".sha256"), f"{h}  {zip_path.name}\n")
    return h

# =========================
# 読み取り専用（凍結）
# =========================

def set_readonly(path: Path, readonly: bool) -> None:
    if not path.exists():
        return
    if os.name == "nt":
        flag = "+R" if readonly else "-R"
        try:
            subprocess.run(f'attrib {flag} /S /D "{str(path)}\\*"', shell=True, check=True)
        except Exception:
            pass
    else:
        # POSIX fallback
        try:
            if path.is_file():
                mode = path.stat().st_mode
                if readonly:
                    path.chmod(mode & ~stat.S_IWUSR & ~stat.S_IWGRP & ~stat.S_IWOTH)
                else:
                    path.chmod(mode | stat.S_IWUSR)
                return
            for p in path.rglob("*"):
                if not p.exists() or p.is_dir():
                    continue
                mode = p.stat().st_mode
                if readonly:
                    p.chmod(mode & ~stat.S_IWUSR & ~stat.S_IWGRP & ~stat.S_IWOTH)
                else:
                    p.chmod(mode | stat.S_IWUSR)
        except Exception:
            pass



def sha256_text(s: str) -> str:
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


def read_text_if_exists(path: Path) -> Optional[str]:
    try:
        if path.exists():
            return path.read_text(encoding="utf-8-sig")
    except Exception:
        return None
    return None


def write_csv_dual(
    *,
    latest_path: Path,
    run_outputs_root: Path,
    rel_path: Path,
    rows: List[Dict],
    fieldnames: List[str],
    region_key_for_log: str,
) -> None:
    """Write CSV to both:
    - latest_path (overwritten, used for analysis)
    - run_outputs_root/rel_path (frozen snapshot per run)

    Also logs a WARN if the latest file changed compared to previous content.
    """
    # --- frozen (run) ---
    run_path = run_outputs_root / rel_path
    rows_sorted = sort_rows_by_fieldnames(rows, fieldnames)
    write_csv(run_path, rows_sorted, fieldnames)

    # --- latest (may overwrite) ---
    before = read_text_if_exists(latest_path)
    write_csv(latest_path, rows_sorted, fieldnames)
    after = read_text_if_exists(latest_path)

    if before is not None and after is not None and sha256_text(before) != sha256_text(after):
        log_warn(region_key_for_log, f"CSV更新: {latest_path.name}（前回と内容が変わりました）")


@dataclass
class RegionStats:
    region_key: str
    ward: str
    url: str
    asof: Optional[str]
    exp_districts: Optional[int]
    exp_jichikai: Optional[int]
    detected_districts: int
    detected_jichikai: int
    total_calc: int
    total_html: Optional[int]
    subtotal_warns: int


# =========================
# パース
# =========================


def parse_region(html: str, meta: Dict) -> Tuple[List[Dict], RegionStats]:
    soup = BeautifulSoup(html, "html.parser")

    asof = parse_asof_date(soup)
    exp_districts, exp_jichikai = parse_expected_counts(soup)

    table = soup.find("table", class_="meibo")
    if not table:
        log_error(meta["key"], "meibo table not found")
        stats = RegionStats(
            region_key=meta["key"],
            ward=meta["ward"],
            url=meta["url"],
            asof=asof,
            exp_districts=exp_districts,
            exp_jichikai=exp_jichikai,
            detected_districts=0,
            detected_jichikai=0,
            total_calc=0,
            total_html=None,
            subtotal_warns=0,
        )
        return [], stats

    records: List[Dict] = []
    districts_seen = set()

    current_district: Optional[str] = None
    district_sum = 0
    total_calc = 0
    total_html: Optional[int] = None

    # 「※」列がrowspanで省略された行があるので、直前のcorpを保持（列が欠けた時だけ継承）
    last_corp_in_district: Optional[bool] = None

    subtotal_warnings = 0

    for tr in table.find_all("tr"):
        tds = tr.find_all("td")
        if not tds:
            continue  # th行など

        # --- 地区セル(rowspan)がある場合は、その行も自治会データを含むので捨てない ---
        i = 0
        if tds[0].has_attr("rowspan"):
            current_district = clean_district_label(tds[0].get_text(" ", strip=True))
            districts_seen.add(current_district)
            district_sum = 0
            last_corp_in_district = None
            i = 1

        if i >= len(tds):
            continue

        row_texts = [td.get_text(" ", strip=True) for td in tds]

        # --- 小計/合計（colspan等の揺れ対策: 行内に「小計」「合計」があれば拾う） ---
        if any(t.strip() == "小計" for t in row_texts):
            nums = [normalize_int(t) for t in row_texts]
            nums = [n for n in nums if n is not None]
            html_val = nums[0] if nums else None

            if current_district is None:
                log_warn(meta["key"], "小計行が district 未確定（表構造が想定外）")
                subtotal_warnings += 1
            else:
                if html_val is None:
                    log_warn(meta["key"], f"{current_district}: 小計の数値が読めない")
                    subtotal_warnings += 1
                elif html_val != district_sum:
                    log_warn(meta["key"], f"{current_district}: 小計不一致 (calc={district_sum}, html={html_val})")
                    subtotal_warnings += 1
            continue

        if any(t.strip() == "合計" for t in row_texts):
            nums = [normalize_int(t) for t in row_texts]
            nums = [n for n in nums if n is not None]
            total_html = nums[0] if nums else None
            continue

        # --- 自治会行 ---
        name = tds[i].get_text(" ", strip=True)

        households: Optional[int] = None
        if i + 1 < len(tds):
            households = normalize_int(tds[i + 1].get_text(" ", strip=True))

        corp: Optional[bool] = None
        if i + 2 < len(tds):
            corp = "●" in tds[i + 2].get_text(" ", strip=True)
            last_corp_in_district = corp
        else:
            corp = last_corp_in_district

        if households is not None:
            district_sum += households
            total_calc += households

        records.append(
            {
                "ward": meta["ward"],
                "region_key": meta["key"],
                "district": current_district,
                "jichikai": name,
                "households": households,
                "corporate": corp,
                "asof": asof,
                "source_url": meta["url"],
            }
        )

    stats = RegionStats(
        region_key=meta["key"],
        ward=meta["ward"],
        url=meta["url"],
        asof=asof,
        exp_districts=exp_districts,
        exp_jichikai=exp_jichikai,
        detected_districts=len(districts_seen),
        detected_jichikai=len(records),
        total_calc=total_calc,
        total_html=total_html,
        subtotal_warns=subtotal_warnings,
    )

    # 検証（警告はログへ）
    if exp_jichikai is not None and len(records) != exp_jichikai:
        log_warn(meta["key"], f"自治会数が期待値と不一致 (detected={len(records)}, expected={exp_jichikai})")

    if total_html is None:
        log_warn(meta["key"], "合計行が見つからない/合計数値が読めない")
    elif total_calc != total_html:
        log_warn(meta["key"], f"合計不一致 (calc={total_calc}, html={total_html})")

    return records, stats


def choose_dataset_year(stats_list: List[RegionStats], forced_year: Optional[int]) -> int:
    if forced_year is not None:
        return forced_year

    years: List[int] = []
    for st in stats_list:
        if st.asof and re.match(r"\d{4}-\d{2}-\d{2}$", st.asof):
            years.append(int(st.asof[:4]))

    if not years:
        log_warn("_GLOBAL", "基準日(asof)を取得できないため、出力年は実行年を使用")
        return RUN_DT.year

    uniq = sorted(set(years))
    if len(uniq) == 1:
        return uniq[0]

    # 複数年が混在 → 最頻値（同数なら最大）
    c = Counter(years)
    top_count = max(c.values())
    top_years = sorted([y for y, cnt in c.items() if cnt == top_count])
    picked = top_years[-1]
    log_warn("_GLOBAL", f"基準日の年が複数 ({uniq}) のため、最頻値/最大の {picked} を採用")
    return picked


def build_paths(dataset_year: int) -> Dict[str, Path]:
    # 最新キャッシュ（分析で参照する“正”）
    out_dir = BASE_DIR / str(dataset_year)

    # 研究用アーカイブ（run単位で凍結する“副”）
    archive_year_dir = BASE_DIR / "archive" / str(dataset_year)
    archive_run_dir = archive_year_dir / "runs" / RUN_ID
    snapshot_run_dir = archive_run_dir / "snapshots"
    log_run_dir = archive_run_dir / "logs"
    tools_dir = archive_run_dir / "tools"
    run_outputs_root = archive_run_dir / "outputs"

    # create dirs (never assume existence)
    ensure_dir(out_dir)
    ensure_dir(archive_year_dir)
    ensure_dir(snapshot_run_dir)
    ensure_dir(log_run_dir)
    ensure_dir(tools_dir)
    ensure_dir(run_outputs_root)

    latest_run_file = archive_year_dir / "LATEST_RUN.txt"
    ensure_dir(latest_run_file.parent)

    return {
        "out_dir": out_dir,
        "archive_year_dir": archive_year_dir,
        "archive_year_root": archive_year_dir,
        "archive_run_dir": archive_run_dir,
        "snapshot_run_dir": snapshot_run_dir,
        "log_run_dir": log_run_dir,
        "tools_dir": tools_dir,
        "run_outputs_root": run_outputs_root,
        "latest_run_file": latest_run_file,
    }
def summarize_logs() -> Tuple[Dict[str, List[str]], Dict[str, List[str]]]:
    err_by: Dict[str, List[str]] = defaultdict(list)
    warn_by: Dict[str, List[str]] = defaultdict(list)

    for e in ERROR_LOG:
        k = e.split(":", 1)[0].strip() if ":" in e else "_UNKNOWN"
        err_by[k].append(e)
    for w in WARN_LOG:
        k = w.split(":", 1)[0].strip() if ":" in w else "_UNKNOWN"
        warn_by[k].append(w)

    return err_by, warn_by


def print_region_validation(st: RegionStats) -> None:
    print("---- VALIDATION ----")
    print(
        f"地区数(検出)   : {st.detected_districts}" + (f" / 期待={st.exp_districts}" if st.exp_districts else "")
    )
    print(
        f"自治会数(検出) : {st.detected_jichikai}" + (f" / 期待={st.exp_jichikai}" if st.exp_jichikai else "")
    )
    print(f"世帯数合計(計) : {st.total_calc}")
    print(f"世帯数合計(H)  : {st.total_html}")

    if st.total_html is None:
        print("合計検証       : WARN（HTML 合計なし/読めず）")
    elif st.total_calc == st.total_html:
        print("合計検証       : OK")
    else:
        print("合計検証       : WARN（不一致）")

    if st.subtotal_warns == 0:
        print("小計検証       : OK")
    else:
        print(f"小計検証       : WARN（{st.subtotal_warns} 箇所）")

    print(f"基準日(asof)   : {st.asof}")
    print("--------------------")


# =============================================================================
# Diff report (previous run vs current run)
# =============================================================================

def _load_csv_records(path: Path) -> Dict[Tuple[str, str, str, str], Dict[str, str]]:
    """Load CSV into a dict keyed by (ward, region_key, district, jichikai)."""
    data: Dict[Tuple[str, str, str, str], Dict[str, str]] = {}
    if not path.exists():
        return data
    with path.open("r", encoding="utf-8-sig", newline="") as f:
        r = csv.DictReader(f)
        for row in r:
            key = (
                (row.get("ward") or "").strip(),
                (row.get("region_key") or "").strip(),
                (row.get("district") or "").strip(),
                (row.get("jichikai") or "").strip(),
            )
            data[key] = row
    return data


def _to_int_safe(x: Optional[str]) -> Optional[int]:
    if x is None:
        return None
    s = str(x).strip()
    if s == "":
        return None
    try:
        return int(s)
    except Exception:
        try:
            return int(float(s))
        except Exception:
            return None


def make_diff_report(
    *,
    prev_csv: Path,
    curr_csv: Path,
    out_txt: Path,
    out_csv: Path,
    prev_run_id: str,
    curr_run_id: str,
) -> Dict[str, object]:
    """Create diff report and write to out_txt/out_csv. Returns a summary dict."""
    old = _load_csv_records(prev_csv)
    new = _load_csv_records(curr_csv)

    old_keys = set(old.keys())
    new_keys = set(new.keys())

    added = sorted(new_keys - old_keys)
    removed = sorted(old_keys - new_keys)
    common = sorted(old_keys & new_keys)

    diff_rows: List[Dict[str, str]] = []

    total_old = 0
    total_new = 0

    # totals
    for k, row in old.items():
        v = _to_int_safe(row.get("households"))
        if v is not None:
            total_old += v
    for k, row in new.items():
        v = _to_int_safe(row.get("households"))
        if v is not None:
            total_new += v

    changed = []
    changed_households_sum = 0

    for k in common:
        o = old[k]
        n = new[k]
        o_h = _to_int_safe(o.get("households")) or 0
        n_h = _to_int_safe(n.get("households")) or 0
        o_c = (o.get("corporate") or "").strip()
        n_c = (n.get("corporate") or "").strip()
        if o_h != n_h or o_c != n_c:
            changed.append(k)
            changed_households_sum += (n_h - o_h)
            diff_rows.append({
                "change_type": "CHANGED",
                "ward": k[0], "region_key": k[1], "district": k[2], "jichikai": k[3],
                "households_old": str(o_h),
                "households_new": str(n_h),
                "diff": str(n_h - o_h),
                "corporate_old": o_c,
                "corporate_new": n_c,
            })

    for k in added:
        n = new[k]
        n_h = _to_int_safe(n.get("households")) or 0
        n_c = (n.get("corporate") or "").strip()
        diff_rows.append({
            "change_type": "ADDED",
            "ward": k[0], "region_key": k[1], "district": k[2], "jichikai": k[3],
            "households_old": "",
            "households_new": str(n_h),
            "diff": str(n_h),
            "corporate_old": "",
            "corporate_new": n_c,
        })

    for k in removed:
        o = old[k]
        o_h = _to_int_safe(o.get("households")) or 0
        o_c = (o.get("corporate") or "").strip()
        diff_rows.append({
            "change_type": "REMOVED",
            "ward": k[0], "region_key": k[1], "district": k[2], "jichikai": k[3],
            "households_old": str(o_h),
            "households_new": "",
            "diff": str(-o_h),
            "corporate_old": o_c,
            "corporate_new": "",
        })

    # write csv
    ensure_dir(out_csv.parent)
    with out_csv.open("w", encoding="utf-8-sig", newline="") as f:
        fns = [
            "change_type","ward","region_key","district","jichikai",
            "households_old","households_new","diff","corporate_old","corporate_new"
        ]
        w = csv.DictWriter(f, fieldnames=fns)
        w.writeheader()
        for row in diff_rows:
            w.writerow(row)

    # write txt
    lines: List[str] = []
    lines.append("==== DIFF REPORT ====")
    lines.append(f"prev_run_id: {prev_run_id}")
    lines.append(f"curr_run_id: {curr_run_id}")
    lines.append(f"prev_csv   : {prev_csv}")
    lines.append(f"curr_csv   : {curr_csv}")
    lines.append("")
    lines.append(f"added_count   : {len(added)}")
    lines.append(f"removed_count : {len(removed)}")
    lines.append(f"changed_count : {len(changed)}")
    lines.append("")
    lines.append(f"total_households_old : {total_old}")
    lines.append(f"total_households_new : {total_new}")
    lines.append(f"total_diff           : {total_new - total_old}")
    lines.append(f"changed_diff_sum      : {changed_households_sum}")
    lines.append("")
    # include a compact list of changes (top)
    max_list = 200
    def fmt_key(k: Tuple[str,str,str,str]) -> str:
        return f"{k[0]} / {k[1]} / {k[2]} / {k[3]}"
    if added:
        lines.append("---- ADDED (top) ----")
        for k in added[:max_list]:
            lines.append(f"+ {fmt_key(k)}")
        if len(added) > max_list:
            lines.append(f"... ({len(added)-max_list} more)")
        lines.append("")
    if removed:
        lines.append("---- REMOVED (top) ----")
        for k in removed[:max_list]:
            lines.append(f"- {fmt_key(k)}")
        if len(removed) > max_list:
            lines.append(f"... ({len(removed)-max_list} more)")
        lines.append("")
    if changed:
        lines.append("---- CHANGED (top) ----")
        for k in changed[:max_list]:
            o = old[k]; n = new[k]
            o_h = _to_int_safe(o.get("households")) or 0
            n_h = _to_int_safe(n.get("households")) or 0
            lines.append(f"* {fmt_key(k)} : {o_h} -> {n_h} (diff {n_h-o_h})")
        if len(changed) > max_list:
            lines.append(f"... ({len(changed)-max_list} more)")
        lines.append("")

    ensure_dir(out_txt.parent)
    out_txt.write_text("\n".join(lines) + "\n", encoding="utf-8")

    return {
        "added": len(added),
        "removed": len(removed),
        "changed": len(changed),
        "total_old": total_old,
        "total_new": total_new,
        "total_diff": total_new - total_old,
        "changed_diff_sum": changed_households_sum,
        "out_txt": str(out_txt),
        "out_csv": str(out_csv),
    }



def main() -> int:
    ap = argparse.ArgumentParser(description="Archive Hamajitiren jichikai households")
    ap.add_argument("--year", type=int, default=None, help="出力フォルダの年（未指定ならページの基準日から推定）")
    ap.add_argument("--timeout", type=int, default=30, help="HTTP timeout seconds")
    args = ap.parse_args()

    # まず取得・解析（年を推定するため、ここではまだ出力フォルダを確定しない）
    all_records: List[Dict] = []
    region_records: Dict[str, List[Dict]] = {}
    html_map: Dict[str, str] = {}
    stats_list: List[RegionStats] = []

    fieldnames = [
        "ward",
        "region_key",
        "district",
        "jichikai",
        "households",
        "corporate",
        "asof",
        "source_url",
    ]

    print("\n---- RUN ----")
    print(f"script      : {SCRIPT_PATH}")
    print(f"project_root: {PROJECT_ROOT}")
    print(f"run_id      : {RUN_ID}")
    print("------------")

    # Fetch & parse
    for meta in TARGETS:
        print(f"\n=== {meta['key']} ({meta['ward']}) ===")
        print(meta["url"])
        try:
            html = fetch_html(meta["url"], timeout=args.timeout)
            html_map[meta["key"]] = html

            records, st = parse_region(html, meta)
            stats_list.append(st)
            region_records[meta["key"]] = records
            all_records.extend(records)

            print_region_summary(st)
        except Exception as e:
            log_error(meta["key"], f"fetch/parse failed: {e}")
            print(f"[ERROR] {meta['key']}: {e}")

    # dataset year
    years = [extract_year_from_asof(s.asof) for s in stats_list if s.asof]
    years = [y for y in years if y]
    dataset_year = args.year if args.year else (max(years) if years else datetime.datetime.now().year)

    # Build paths (fixed, not depending on CWD)
    paths = build_paths(dataset_year)
    out_dir: Path = paths["out_dir"]
    archive_run_dir: Path = paths["archive_run_dir"]
    snapshot_run_dir: Path = paths["snapshot_run_dir"]
    log_run_dir: Path = paths["log_run_dir"]
    run_outputs_root: Path = paths["run_outputs_root"]
    latest_run_file: Path = paths["latest_run_file"]
    tools_dir: Path = paths["tools_dir"]
    archive_year_root: Path = paths["archive_year_root"]

    # Defensive: ensure all required directories exist
    for _p in [out_dir, archive_year_root, archive_run_dir, snapshot_run_dir, log_run_dir, tools_dir, run_outputs_root]:
        ensure_dir(_p)

    # previous run id (for diff)
    prev_run_id = None
    try:
        t = read_text_if_exists(latest_run_file)
        if t:
            prev_run_id = t.strip()
            if prev_run_id == RUN_ID:
                prev_run_id = None
    except Exception:
        prev_run_id = None

    # If the pointer exists but the run folder is missing (user may have deleted archives),
    # just disable diff gracefully.
    if prev_run_id:
        prev_run_dir = archive_year_root / "runs" / prev_run_id
        if not prev_run_dir.exists():
            log_warn("_GLOBAL", f"前回runフォルダが見つからないため差分をスキップ: {prev_run_dir}")
            prev_run_id = None

    print("\n---- PATHS ----")
    print(f"dataset_year: {dataset_year}")
    print(f"out_dir     : {out_dir}")
    print(f"archive_run : {archive_run_dir}")
    if prev_run_id:
        print(f"prev_run_id : {prev_run_id}")
    print("----------------")

    # Make sure latest cache can be overwritten: temporarily writable
    try:
        set_readonly(out_dir, False)
    except Exception:
        pass

    # Tools bundle + README (following the reference archive script)
    try:
        copy_self(tools_dir / "archive_script.py")
        write_text(tools_dir / "requirements.txt", pip_freeze_text())
        write_text(tools_dir / "run_command.txt", f"python {SCRIPT_PATH.name}\n")
        write_text(tools_dir / "origin.txt", f"original_script_filename: {SCRIPT_PATH.name}\n")
        write_text(
            archive_run_dir / "_README.txt",
            "\n".join(
                [
                    "This folder is an immutable per-run archive snapshot.",
                    "DO NOT edit/overwrite files under this run directory.",
                    "Latest cache: " + str(out_dir),
                    "Frozen outputs: " + str(run_outputs_root),
                    "",
                ]
            )
            + "\n",
        )
    except Exception:
        pass

    # snapshots
    for key, html in html_map.items():
        write_text(snapshot_run_dir / f"{key}.html", html)

    # region / district CSV
    for meta in TARGETS:
        key = meta["key"]
        if key not in region_records:
            continue

        records = region_records[key]
        region_dir = out_dir / "regions" / key
        # region summary CSV
        write_csv_dual(
            latest_path=region_dir / f"自治会世帯数__{dataset_year}__{key}.csv",
            run_outputs_root=run_outputs_root,
            rel_path=Path("regions") / key / f"自治会世帯数__{dataset_year}__{key}.csv",
            rows=records,
            fieldnames=fieldnames,
            region_key_for_log=key,
        )

        # district split
        by_district: Dict[str, List[Dict]] = {}
        for r in records:
            d = r.get("district") or ""
            by_district.setdefault(d, []).append(r)

        dist_dir = region_dir / "districts"
        for d, rows in by_district.items():
            fn = safe_filename(d)
            write_csv_dual(
                latest_path=dist_dir / f"{fn}.csv",
                run_outputs_root=run_outputs_root,
                rel_path=Path("regions") / key / "districts" / f"{fn}.csv",
                rows=rows,
                fieldnames=fieldnames,
                region_key_for_log=key,
            )

    # all (latest + frozen)
    all_rel = Path(f"自治会世帯数__{dataset_year}.csv")
    write_csv_dual(
        latest_path=out_dir / all_rel.name,
        run_outputs_root=run_outputs_root,
        rel_path=all_rel,
        rows=all_records,
        fieldnames=fieldnames,
        region_key_for_log="_GLOBAL",
    )

    # logs
    write_text(out_dir / "_ERRORS.txt", "\n".join(ERROR_LOG) + "\n")
    write_text(out_dir / "_WARNINGS.txt", "\n".join(WARN_LOG) + "\n")
    write_text(log_run_dir / "_ERRORS.txt", "\n".join(ERROR_LOG) + "\n")
    write_text(log_run_dir / "_WARNINGS.txt", "\n".join(WARN_LOG) + "\n")

    # update latest run pointer (after write)
    write_text(latest_run_file, RUN_ID + "\n")

    # Diff report (prev run vs current run)
    diff_summary = None
    if prev_run_id:
        prev_csv = archive_year_root / "runs" / prev_run_id / "outputs" / all_rel
        curr_csv = run_outputs_root / all_rel
        if prev_csv.exists() and curr_csv.exists():
            try:
                diff_summary = make_diff_report(
                    prev_csv=prev_csv,
                    curr_csv=curr_csv,
                    out_txt=(out_dir / f"_DIFF__{prev_run_id}__to__{RUN_ID}.txt"),
                    out_csv=(out_dir / f"_DIFF__{prev_run_id}__to__{RUN_ID}.csv"),
                    prev_run_id=prev_run_id,
                    curr_run_id=RUN_ID,
                )
                # also freeze diff into run logs
                make_diff_report(
                    prev_csv=prev_csv,
                    curr_csv=curr_csv,
                    out_txt=(log_run_dir / f"diff__{prev_run_id}__to__{RUN_ID}.txt"),
                    out_csv=(log_run_dir / f"diff__{prev_run_id}__to__{RUN_ID}.csv"),
                    prev_run_id=prev_run_id,
                    curr_run_id=RUN_ID,
                )
            except Exception as e:
                log_warn("_GLOBAL", f"差分レポート生成に失敗: {e}")

    # manifest
    manifest = {
        "timestamp": datetime.datetime.now().isoformat(),
        "script_path": str(SCRIPT_PATH),
        "project_root": str(PROJECT_ROOT),
        "dataset_year": dataset_year,
        "out_dir": str(out_dir),
        "archive_run_dir": str(archive_run_dir),
        "targets": TARGETS,
        "python": sys.version,
        "stats": [asdict(s) for s in stats_list],
        "errors": ERROR_LOG,
        "warnings": WARN_LOG,
        "diff_summary": diff_summary,
    }
    write_text(archive_run_dir / "run_manifest.json", json.dumps(manifest, ensure_ascii=False, indent=2) + "\n")

    # ===== Final summary =====
    err_by, warn_by = summarize_logs()

    def status_label() -> str:
        if not ERROR_LOG and not WARN_LOG:
            return f"完全成功（エラー0 / 警告0）"
        if not ERROR_LOG and WARN_LOG:
            return f"成功（警告 {len(WARN_LOG)} 件）"
        return f"失敗（エラー {len(ERROR_LOG)} 件 / 警告 {len(WARN_LOG)} 件）"

    summary_lines: List[str] = []
    summary_lines.append("==== RESULT ====")
    summary_lines.append(f"STATUS: {status_label()}")
    summary_lines.append(f"dataset_year: {dataset_year}")
    summary_lines.append(f"records_total: {len(all_records)}")
    summary_lines.append(f"output: {out_dir}")
    summary_lines.append(f"snapshot: {snapshot_run_dir}")
    summary_lines.append(f"frozen_outputs: {run_outputs_root}")
    if prev_run_id:
        summary_lines.append(f"prev_run_id: {prev_run_id}")
    if diff_summary:
        summary_lines.append(f"diff_report_txt: {diff_summary.get('out_txt')}")
        summary_lines.append(f"diff_report_csv: {diff_summary.get('out_csv')}")

    summary_lines.append("\n---- ISSUES (by region) ----")
    for meta in TARGETS:
        k = meta["key"]
        summary_lines.append(f"- {k}: errors={len(err_by.get(k, []))} warnings={len(warn_by.get(k, []))}")
    if "_GLOBAL" in err_by or "_GLOBAL" in warn_by:
        summary_lines.append(f"- _GLOBAL: errors={len(err_by.get('_GLOBAL', []))} warnings={len(warn_by.get('_GLOBAL', []))}")

    if ERROR_LOG:
        summary_lines.append("\n---- ERROR (top) ----")
        for e in ERROR_LOG[:20]:
            summary_lines.append(f"- {e}")
        if len(ERROR_LOG) > 20:
            summary_lines.append(f"... ({len(ERROR_LOG) - 20} more)")

    if WARN_LOG:
        summary_lines.append("\n---- WARNING (top) ----")
        for w in WARN_LOG[:30]:
            summary_lines.append(f"- {w}")
        if len(WARN_LOG) > 30:
            summary_lines.append(f"... ({len(WARN_LOG) - 30} more)")

    summary_text = "\n".join(summary_lines) + "\n"
    print("\n" + summary_text)
    write_text(log_run_dir / "run_summary.txt", summary_text)

    # --- ZIP freeze (proof) ---
    try:
        zip_folder(archive_run_dir, archive_run_dir / "run_snapshot.zip")
    except Exception:
        pass

    # --- Read-only freeze ---
    # Run archive: freeze only when OK/WARN
    if len(ERROR_LOG) == 0:
        try:
            set_readonly(archive_run_dir, True)
        except Exception:
            pass
    # Latest cache: always freeze to prevent accidental edits
    try:
        set_readonly(out_dir, True)
    except Exception:
        pass

    return 0 if not ERROR_LOG else 1


if __name__ == "__main__":
    try:
        rc = main()
    except SystemExit:
        # Keep explicit exits as-is
        raise
    except Exception:
        # Fatal: print traceback and exit with code 2
        traceback.print_exc()
        rc = 2

    if rc != 0:
        # Help the user find logs even if they deleted old runs.
        try:
            print("\n[HINT] 実行ログは data/自治会/archive/<年>/runs/<RUN_ID>/logs/ にあります。")
            print("       直近RUN_IDは data/自治会/archive/<年>/LATEST_RUN.txt を参照してください。")
            print(f"       BASE_DIR = {BASE_DIR}")
        except Exception:
            pass

    raise SystemExit(rc)
