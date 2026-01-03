# =============================================================================
# 自治会データ集計（相対パス対応 / プロジェクト直下起点 / 日本語列名）
#
# 入力:
#   data/自治会/2025/自治会世帯数__2025.csv
#
# 出力（列名は日本語）:
#   1) 区×地域×地区（自治会名一覧つき）
#   2) 区×地域（地区名一覧 + 自治会名一覧つき）
#   3) 区（地域一覧 + 地区名一覧つき）
#   4) 地区のみ横断（自治会名一覧つき）
#
# 後から追加するCSV（任意。data/自治会/ 配下に置く想定）:
#   - 人口:        population_by_*.csv など
#   - 住民票世帯:  resident_households_by_*.csv など
#
# 乖離率 = (自治会世帯数合計 - 住民票世帯数) / 住民票世帯数
#
# ---- 返り値（list）について（日本語の説明） ----------------------------------
# このスクリプトを source() すると最後に list を返します。
# list の中身:
#   - PROJECT_ROOT:
#       プロジェクトルート（data/ と scripts/ のあるフォルダ）を自動検出した結果
#   - 区_地域_地区_集計:
#       区×地域×地区 単位の集計（自治会名一覧付き）
#   - 区_地域_集計:
#       区×地域 単位の集計（地区名一覧 + 自治会名一覧付き）
#   - 区_集計:
#       区 単位の集計（地域一覧 + 地区名一覧付き）
#   - 地区_横断_集計:
#       地区 単位の横断集計（自治会名一覧付き）
#   - attach_population():
#       後から人口CSVを join して「人口あたり自治会数」を計算する関数（日本語列でもOK）
#   - attach_resident_households():
#       後から住民票世帯数CSVを join して「乖離率」を計算する関数（日本語列でもOK）
#
# ---- コンソール表示件数 -------------------------------------------------------
# tibble の表示行数を「最低100行」表示するように設定します（環境により上限あり）
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

options(
  tibble.print_min = 100,
  tibble.print_max = 1000,
  pillar.print_min = 100,
  pillar.print_max = 1000,
  max.print = 10000
)

# ---- 0) プロジェクトルート検出（相対パス起点） -------------------------------
# 優先順位:
#   1) 環境変数 PROJECT_ROOT があればそれ
#   2) このスクリプトの場所から親方向に探索（RStudioで実行してもOK）
#   3) getwd() から親方向に探索
#
# ルート判定条件（どれか満たせばOK）:
#   - data/ と scripts/ が両方存在
#   - *.Rproj が存在
#
# ※ rstudioapi が無い環境でも動くように、依存は optional 扱い

find_project_root <- function(start_dir = NULL, max_up = 6) {
  is_root <- function(p) {
    has_data <- dir.exists(file.path(p, "data"))
    has_scripts <- dir.exists(file.path(p, "scripts"))
    has_rproj <- length(list.files(p, pattern = "\\.Rproj$", ignore.case = TRUE)) > 0
    (has_data && has_scripts) || has_rproj
  }

  env_root <- Sys.getenv("PROJECT_ROOT", unset = NA_character_)
  if (!is.na(env_root) && nzchar(env_root) && dir.exists(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  if (is.null(start_dir) || !nzchar(start_dir)) start_dir <- getwd()
  p <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  for (i in 0:max_up) {
    if (dir.exists(p) && is_root(p)) return(p)
    parent <- normalizePath(file.path(p, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, p)) break
    p <- parent
  }

  stop("プロジェクトルートを自動検出できませんでした。PROJECT_ROOT を環境変数で指定してください。", call. = FALSE)
}

guess_start_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) NA_character_)
    if (!is.na(p) && nzchar(p)) return(dirname(p))
  }
  getwd()
}

PROJECT_ROOT <- find_project_root(guess_start_dir())
message("[INFO] PROJECT_ROOT = ", PROJECT_ROOT)

# ---- 1) 入力パス（相対） ------------------------------------------------------
csv_jichikai_rel <- file.path("data", "自治会", "2025", "自治会世帯数__2025.csv")
csv_jichikai <- file.path(PROJECT_ROOT, csv_jichikai_rel)

if (!file.exists(csv_jichikai)) {
  stop(paste0("入力CSVが見つかりません: ", csv_jichikai), call. = FALSE)
}

# ---- 2) region_key -> 日本語（確定版） ---------------------------------------
REGION_KEY_JP <- c(
  higashi  = "東地域",
  nishi    = "西地域",
  minami   = "南地域",
  kita     = "北地域",
  hamakita = "浜北地域",
  tenryu   = "天竜地域",
  naka     = "中地域"
)

region_key_to_jp <- function(x) {
  x_chr <- as.character(x)
  out <- unname(REGION_KEY_JP[x_chr])
  ifelse(is.na(out), x_chr, out)
}

collapse_unique <- function(x, sep = "｜") {
  x <- unique(na.omit(as.character(x)))
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

# ---- 3) 読み込み & 必須列チェック -------------------------------------------
df <- read_csv(csv_jichikai, show_col_types = FALSE)

required_cols <- c("ward", "region_key", "district", "jichikai", "households", "corporate")
missing <- setdiff(required_cols, names(df))
if (length(missing) > 0) {
  stop(paste0("CSVに必要列がありません: ", paste(missing, collapse = ", ")), call. = FALSE)
}

df <- df %>%
  mutate(
    ward       = as.character(ward),
    region_key = as.character(region_key),
    district   = as.character(district),
    jichikai   = as.character(jichikai),
    households = as.numeric(households),
    corporate  = as.logical(corporate),
    region_jp  = region_key_to_jp(region_key)
  )

# =============================================================================
# 4) 名称一覧（要件どおり）
# =============================================================================

# district単位: 自治会名一覧 + 自治会数
name_district <- df %>%
  group_by(ward, region_key, district) %>%
  summarise(
    jichikai_list = collapse_unique(jichikai),
    .groups = "drop"
  )

# region_key単位: district名一覧 + district数 + 自治会数 + 自治会名一覧
name_region <- df %>%
  group_by(ward, region_key) %>%
  summarise(
    district_list = collapse_unique(district),
    n_district    = n_distinct(district),
    jichikai_list = collapse_unique(jichikai),
    region_jp     = first(region_key_to_jp(region_key)),
    .groups = "drop"
  )

# ward単位: 地域(日本語)一覧 + 地域数 + district名一覧 + district数 + 自治会数
name_ward <- df %>%
  group_by(ward) %>%
  summarise(
    region_list_jp = collapse_unique(region_jp),
    n_region       = n_distinct(region_jp),
    district_list  = collapse_unique(district),
    n_district     = n_distinct(district),

    .groups = "drop"
  )

# district横断（任意）: districtごとに自治会名一覧 + 自治会数
name_district_only <- df %>%
  group_by(district) %>%
  summarise(
    jichikai_list = collapse_unique(jichikai),
    .groups = "drop"
  )

# =============================================================================
# 5) 指標集計（共通）
# =============================================================================

summarise_indicators <- function(df, keys) {
  df %>%
    group_by(across(all_of(keys))) %>%
    summarise(
      n_jichikai = n(),
      households_sum = sum(households, na.rm = TRUE),
      households_per_jichikai = if_else(n_jichikai > 0, households_sum / n_jichikai, NA_real_),

      corporate_true_n  = sum(corporate %in% TRUE,  na.rm = TRUE),
      corporate_false_n = sum(corporate %in% FALSE, na.rm = TRUE),
      corporate_true_rate  = if_else(n_jichikai > 0, (corporate_true_n / n_jichikai * 100), NA_real_),
      corporate_false_rate = if_else(n_jichikai > 0, (corporate_false_n / n_jichikai * 100), NA_real_),

      # 後から join で埋める
      population = NA_real_,
      jichikai_per_population = NA_real_,

      resident_households = NA_real_,
      divergence_rate = NA_real_,

      .groups = "drop"
    )
}

ind_district <- summarise_indicators(df, c("ward", "region_key", "district"))
ind_region   <- summarise_indicators(df, c("ward", "region_key"))
ind_ward     <- summarise_indicators(df, c("ward"))
ind_district_only <- summarise_indicators(df, c("district"))

# =============================================================================
# 6) 指標 + 名称一覧 の合体（英列の中間オブジェクト）
# =============================================================================

summary_en_district <- ind_district %>%
  left_join(name_district, by = c("ward","region_key","district")) %>%
  mutate(region_jp = region_key_to_jp(region_key)) %>%
  relocate(region_jp, .after = region_key)

summary_en_region <- ind_region %>%
  left_join(name_region, by = c("ward","region_key")) %>%
  relocate(region_jp, .after = region_key)

summary_en_region_only <- df %>%
  dplyr::group_by(region_jp) %>%
  dplyr::summarise(
    district_list = collapse_unique(district),
    n_district    = dplyr::n_distinct(district),
    n_jichikai    = dplyr::n(),
    jichikai_list = collapse_unique(jichikai),
    .groups = "drop"
  )

summary_en_ward <- ind_ward %>%
  left_join(name_ward, by = "ward")

summary_en_district_only <- ind_district_only %>%
  left_join(name_district_only, by = "district")

# =============================================================================
# 7) 列名を日本語へ（出力用）
# =============================================================================

to_jp_cols <- function(df) {
  rename_map <- c(
    ward = "区",
    region_key = "地域キー",
    region_jp = "地域",
    district = "地区",

    n_region = "地域数",
    region_list_jp = "地域一覧",

    n_district = "地区数",
    district_list = "地区名一覧",

    n_jichikai = "自治会数",
    jichikai_list = "自治会名一覧",

    households_sum = "自治会世帯数合計",
    households_per_jichikai = "1自治会あたり世帯数",

    corporate_true_n = "法人格あり数",
    corporate_false_n = "法人格なし数",
    corporate_true_rate = "法人格あり率",
    corporate_false_rate = "法人格なし率",

    population = "人口",
    jichikai_per_population = "人口あたり自治会数",

    resident_households = "住民票世帯数",
    divergence_rate = "乖離率"
  )

  for (old in names(rename_map)) {
    new <- unname(rename_map[old])
    if (old %in% names(df)) names(df)[names(df) == old] <- new
  }
  df
}

# 出力用（日本語列）
区_地域_地区_集計 <- to_jp_cols(summary_en_district)
# 乖離率（住民票世帯数・乖離率）は後からjoinで埋まるが、列として常に保持する
区_地域_地区_集計 <- 区_地域_地区_集計 %>%
  dplyr::relocate(住民票世帯数, 乖離率, .after = `自治会世帯数合計`)

地域_地区_集計 <- to_jp_cols(summary_en_region_only)

区_地域_集計      <- to_jp_cols(summary_en_region)
区_集計           <- to_jp_cols(summary_en_ward)
地区_横断_集計     <- to_jp_cols(summary_en_district_only)

# =============================================================================
# 8) 後から人口CSVを追加する（人口あたり自治会数）
# =============================================================================

jp_to_en_cols_min <- function(df) {
  map <- c(
    "区"="ward",
    "地域キー"="region_key",
    "地区"="district",
    "自治会数"="n_jichikai",
    "自治会世帯数合計"="households_sum",
    "人口"="population",
    "人口あたり自治会数"="jichikai_per_population",
    "住民票世帯数"="resident_households",
    "乖離率"="divergence_rate"
  )
  for (jp in names(map)) {
    en <- unname(map[jp])
    if (jp %in% names(df)) names(df)[names(df) == jp] <- en
  }
  df
}

attach_population <- function(summary_df, pop_df, keys_jp_or_en, pop_col = "population") {
  keys <- keys_jp_or_en
  keys <- ifelse(keys %in% c("区","地域キー","地区"),
                 c("ward","region_key","district")[match(keys, c("区","地域キー","地区"))],
                 keys)

  is_jp <- any(c("区","自治会数","自治会世帯数合計") %in% names(summary_df))
  work <- if (is_jp) jp_to_en_cols_min(summary_df) else summary_df

  pop_df <- pop_df %>%
    mutate(across(all_of(keys), as.character)) %>%
    rename(population = all_of(pop_col))

  out <- work %>%
    select(-population, -jichikai_per_population) %>%
    left_join(pop_df %>% select(all_of(keys), population), by = keys) %>%
    mutate(
      jichikai_per_population = if_else(!is.na(population) & population > 0, n_jichikai / population, NA_real_)
    )

  if (is_jp) return(to_jp_cols(out))
  out
}

# =============================================================================
# 9) 後から住民票ベース世帯数CSVを追加する（乖離率）
# =============================================================================

attach_resident_households <- function(summary_df, rh_df, keys_jp_or_en, rh_col = "resident_households") {
  keys <- keys_jp_or_en
  keys <- ifelse(keys %in% c("区","地域キー","地区"),
                 c("ward","region_key","district")[match(keys, c("区","地域キー","地区"))],
                 keys)

  is_jp <- any(c("区","自治会数","自治会世帯数合計") %in% names(summary_df))
  work <- if (is_jp) jp_to_en_cols_min(summary_df) else summary_df

  rh_df <- rh_df %>%
    mutate(across(all_of(keys), as.character)) %>%
    rename(resident_households = all_of(rh_col))

  out <- work %>%
    select(-resident_households, -divergence_rate) %>%
    left_join(rh_df %>% select(all_of(keys), resident_households), by = keys) %>%
    mutate(
      divergence_rate = if_else(
        !is.na(resident_households) & resident_households > 0,
        ((households_sum - resident_households) / resident_households * 100),
        NA_real_
      )
    )

  if (is_jp) return(to_jp_cols(out))
  out
}

## =============================================================================
# 10) フォント警告について（重要）
# =============================================================================
# 警告:
#   "Windows のフォントデータベースにフォントファミリが見付かりません"
# は、主に ggplot2/grid が「存在しないフォント」を指定されている時に出ます。
# このスクリプト自体はプロットを生成しないため、通常この警告は出ません。
# もし出る場合は、他のスクリプトで theme_set(base_family=...) 等を設定している可能性が高いです。
# 対処（例）:
#   ggplot2::theme_set(ggplot2::theme_grey(base_family = ""))
#   par(family = "")
# =============================================================================

# ---- 返り値 ------------------------------------------------------------------
list(
  PROJECT_ROOT = PROJECT_ROOT,
  区_地域_地区_集計 = 区_地域_地区_集計,
  区_地域_集計      = 区_地域_集計,
  区_集計           = 区_集計,
  地域_地区_集計     = 地域_地区_集計,
  地区_横断_集計     = 地区_横断_集計,
  attach_population = attach_population,
  attach_resident_households = attach_resident_households
)
