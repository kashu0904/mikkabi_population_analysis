# =============================================================================
# run_自治会_plots.R
#  - CSV（自治会世帯数__YYYY.csv）を読み込み、region_key → 地域別フォルダに分類して
#    district ごとに bar/pie をSVG出力します。
#  - ここだけいじる：target_year / csv_path / out_root / district_filter / region_filter
#                    / non_corporate_mark / bar_color / source_date_reiwa
# =============================================================================

options(stringsAsFactors = FALSE)

# ---- style 読み込み（lib：絶対パス固定） --------------------------------------
style_path <- "C:/Users/pirat/Documents/MikkabiLab_population_analysis/lib/plot_自治会_style.R"
if (!file.exists(style_path)) stop("style が見つかりません: ", style_path)
source(style_path, encoding = "UTF-8")

if (!exists("read_or_dummy", mode = "function")) {
  stop("read_or_dummy() が見つかりません。styleファイルが古い／別物です。", call. = FALSE)
}

# ---- ここだけ書き換える ------------------------------------------------------
target_year <- 2025

csv_path <- "C:/Users/pirat/Documents/MikkabiLab_population_analysis/data/自治会/2025/自治会世帯数__2025.csv"

# 出力ルート（指定：figures/自治会/2025/out_svg）
out_root <- "C:/Users/pirat/Documents/MikkabiLab_population_analysis/figures/自治会"

# NULLなら全region_keyを処理。ベクトルで絞る（例: c("kita","tenryu")）
region_filter <- NULL

# NULLなら各region内の全districtを処理。ベクトルで絞る（例: c("中ノ町地区","細江地区")）
district_filter <- NULL

# corporate==FALSE の自治会名に付ける印（後でここを変えるだけ）
non_corporate_mark <- "×"

# 棒グラフのバー色
bar_color <- "#005686"

# 出典の日付（令和表記）。NULLならCSVの asof(YYYY-MM-DD) から自動生成（ユニーク1件のとき）
source_date_reiwa <- NULL

# 出典日付（CSV列名 / 手動上書き）
source_date_col <- "asof"          # CSVの列名（YYYY-MM-DD）
source_date_iso_override <- NULL   # 手で上書きする場合は "2025-04-01" など
# -----------------------------------------------------------------------------


# ---- region_key → 地域名（日本語） -------------------------------------------
region_name_jp <- function(region_key) {
  dplyr::case_when(
    region_key == "higashi"  ~ "東",
    region_key == "nishi"    ~ "西",
    region_key == "minami"   ~ "南",
    region_key == "kita"     ~ "北",
    region_key == "naka"     ~ "中",
    region_key == "tenryu"   ~ "天竜",
    region_key == "hamakita" ~ "浜北",
    TRUE ~ as.character(region_key)
  )
}

# ---- YYYY-MM-DD → 令和X年M月D日 ----------------------------------------------
to_reiwa_date <- function(x) {
  # x: Date or character (YYYY-MM-DD)
  d <- as.Date(x)
  if (is.na(d)) return(NA_character_)

  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  dd <- as.integer(format(d, "%d"))

  # 令和は2019-05-01〜（年は2019を令和1年として換算）
  if (y < 2019) {
    # 令和以前はとりあえず西暦表記で返す（必要なら拡張）
    return(sprintf("%d年%d月%d日", y, m, dd))
  }
  ry <- y - 2018
  sprintf("令和%d年%d月%d日", ry, m, dd)
}

# ---- 読み込み ----------------------------------------------------------------
df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

if (isTRUE(attr(df, "used_dummy"))) {
  warning("used_dummy = TRUE：SVGを配布・公開する前に実データで再生成してください。", call. = FALSE)
}

# region_key が無いなら止める（今回の要件）
if (!("region_key" %in% names(df))) {
  stop("CSVに region_key 列がありません。", call. = FALSE)
}

# 出典日付の決定（令和表記）
if (!is.null(source_date_iso_override)) {
  source_date_reiwa <- to_reiwa_date(source_date_iso_override)
} else if (!is.null(source_date_col) && (source_date_col %in% names(df))) {
  u <- unique(na.omit(df[[source_date_col]]))
  if (length(u) == 1) {
    source_date_reiwa <- to_reiwa_date(u[[1]])
  }
}
if (is.null(source_date_reiwa) || is.na(source_date_reiwa) || source_date_reiwa == "") {
  stop("出典日付を決められません。CSVに asof 列（YYYY-MM-DD）を入れるか、source_date_iso_override に YYYY-MM-DD を指定してください。", call. = FALSE)
}


# ---- 対象region一覧 -----------------------------------------------------------
regions <- sort(unique(as.character(df$region_key)))
if (!is.null(region_filter)) {
  regions <- intersect(regions, region_filter)
  if (length(regions) == 0) stop("region_filter に該当する region_key がCSVにありません。", call. = FALSE)
}

# ---- 実行 --------------------------------------------------------------------
results <- list()

for (rk in regions) {
  rjp <- region_name_jp(rk)

  # 地域別フォルダ（指定の out_svg の下に地域別で分類保存）
  out_dir <- file.path(out_root, as.character(target_year), "out_svg", rjp)

  # 出典（地域名を反映）
  source_caption <- paste0(
    "出典：浜松市自治連合会「", rjp, "地域の地区別単位自治会」（", source_date_reiwa, "）を基に作成"
  )

  df_r <- df |> dplyr::filter(.data$region_key == rk)

  districts <- sort(unique(df_r$district))
  if (!is.null(district_filter)) {
    districts <- intersect(districts, district_filter)
    if (length(districts) == 0) {
      message("region_key=", rk, "（", rjp, "）：district_filter に該当が無いのでスキップ")
      next
    }
  }

  message("\n=== region: ", rk, " (", rjp, ") / districts: ", length(districts), " ===")

  for (d in districts) {
    message("  - district: ", d)

    p1 <- plot_bar_horizontal_desc(
      df_r, d,
      bar_color = bar_color,
      non_corp_mark = non_corporate_mark,
      plot_year = target_year,
      caption = source_caption
    )

    p2 <- plot_pie_share(
      df_r, d,
      non_corp_mark = non_corporate_mark,
      plot_year = target_year,
      caption = source_caption
    )

    prefix <- paste0("jichikai_", safe_filename(rjp), "__", safe_filename(d))

    results[[paste0(rk, "::", d)]] <- save_svg_pair(
      p1, p2,
      out_dir = out_dir,
      prefix  = prefix,
      w = 11, h = 6
    )
  }
}

invisible(results)

