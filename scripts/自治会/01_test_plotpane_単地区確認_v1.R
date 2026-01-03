# =============================================================================
# 01_test_plotpane_単地区確認_v1.R
#   - Plot pane で bar/pie を確認するためのテスト
#   - 出力は行わない
# =============================================================================

# ---- project_root を検出（ブートストラップ） -------------------------------
find_project_root_boot <- function(start = getwd(), max_up = 10) {
  start <- normalizePath(start, winslash = "/", mustWork = TRUE)
  cur <- start
  for (i in 0:max_up) {
    if (dir.exists(file.path(cur, "scripts")) && dir.exists(file.path(cur, "lib"))) return(cur)
    parent <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = TRUE)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("project_root を検出できません（scripts/ と lib/ が見つかりません）。setwd(project_root) してから再実行してください。", call. = FALSE)
}

project_root <- find_project_root_boot()

# ---- source -----------------------------------------------------------------
source(file.path(project_root, "lib", "自治会", "自治会_デザイン設定_v1.R"), encoding = "UTF-8")
source(file.path(project_root, "lib", "自治会", "自治会_図表関数_v1.R"), encoding = "UTF-8")
source(file.path(project_root, "lib", "自治会", "自治会_書き出し_4形式_v1.R"), encoding = "UTF-8")

cfg <- JICHAIKAI_PLOT_CFG_V1

# ---- 入力 -------------------------------------------------------------------
# 年度（CSVの格納先 data/自治会/<year>/ を想定）
TARGET_YEAR <- 2025

# 入力CSV（project_root基準の相対パス）
csv_path <- file.path(project_root, "data", "自治会", as.character(TARGET_YEAR), paste0("自治会世帯数__", TARGET_YEAR, ".csv"))
# ここを試したい地区名に変える
TARGET_DISTRICT <- "赤佐地区"

# ---- 読み込み ----------------------------------------------------------------
df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

if (!("region_key" %in% names(df))) stop("CSVに region_key 列がありません。", call. = FALSE)

# 同名自治会をユニーク化（市場（1）, 市場（2）...）
df <- add_unique_jichikai(df)

# ---- 地域/出典 ---------------------------------------------------------------
region_keys <- unique(df$region_key[df$district == TARGET_DISTRICT])
region_keys <- region_keys[!is.na(region_keys)]
if (length(region_keys) == 0) stop("district が見つかりません: ", TARGET_DISTRICT, call. = FALSE)
if (length(region_keys) > 1) warning("同一 district に複数 region_key が見つかりました。先頭を使用します: ", paste(region_keys, collapse = ", "), call. = FALSE)

region_key <- region_keys[1]
region_jp <- resolve_region_name_v1(region_key, cfg)

# 出典日付（候補列のうち単一値を採用）
asof_reiwa <- to_reiwa_date(pick_asof_value_v1(df))
caption <- make_source_caption(region_jp, asof_reiwa)

# ---- barプロフィール ----------------------------------------------------------
df_d <- df |> filter(district == TARGET_DISTRICT)
n_jichikai <- nrow(df_d)
bar_profile <- get_bar_profile_v1(n_jichikai, cfg)
cfg_bar <- apply_bar_profile_to_cfg_v1(cfg, bar_profile)

message("district=", TARGET_DISTRICT, " / 自治会数=", n_jichikai, " / bar_profile=", bar_profile$id)

# ---- プロット -----------------------------------------------------------------
p_bar <- plot_bar_horizontal_desc_v1(
  df, TARGET_DISTRICT,
  n_jichikai = n_jichikai,
  cfg = cfg_bar,
  non_corp_mark = cfg$label$non_corp_mark,
  mark_non_corporate = cfg$label$mark_non_corporate,
  caption = caption
)

p_pie <- plot_pie_share_topN_v1(
  df, TARGET_DISTRICT,
  n_jichikai = n_jichikai,
  cfg = cfg,
  non_corp_mark = cfg$label$non_corp_mark,
  mark_non_corporate = cfg$label$mark_non_corporate,
  caption = caption
)

print(p_bar)
#print(p_pie)
