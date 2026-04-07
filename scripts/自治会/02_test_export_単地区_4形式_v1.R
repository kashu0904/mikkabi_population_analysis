# =============================================================================
# 02_test_export_単地区_4形式_v1.R
#   - 1地区だけを 4形式（PDF/PNG/SVG/EPS）で書き出すテスト
#   - bar は自治会数レンジ別プロフィール、pie は固定 7×7
# =============================================================================

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

source(file.path(project_root, "lib", "自治会", "自治会_デザイン設定_v1.R"), encoding = "UTF-8")
source(file.path(project_root, "lib", "自治会", "自治会_図表関数_v1.R"), encoding = "UTF-8")
source(file.path(project_root, "lib", "自治会", "自治会_書き出し_4形式_v1.R"), encoding = "UTF-8")

cfg <- JICHAIKAI_PLOT_CFG_V1

# ---- 入力 -------------------------------------------------------------------
TARGET_YEAR <- 2025

# 入力CSV（project_root基準の相対パス）
csv_path <- file.path(project_root, "data", "自治会", as.character(TARGET_YEAR), paste0("自治会世帯数__", TARGET_YEAR, ".csv"))
TARGET_DISTRICT <- "天竜地区"

# ---- 読み込み ----------------------------------------------------------------
df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)
if (!("region_key" %in% names(df))) stop("CSVに region_key 列がありません。", call. = FALSE)

df <- add_unique_jichikai(df)

# ---- 地域/出典 ---------------------------------------------------------------
region_key <- unique(df$region_key[df$district == TARGET_DISTRICT])[1]
region_jp <- resolve_region_name_v1(region_key, cfg)

asof_reiwa <- to_reiwa_date(pick_asof_value_v1(df))
caption <- make_source_caption(region_jp, asof_reiwa)

# ---- barプロフィール ----------------------------------------------------------
df_d <- df |> filter(district == TARGET_DISTRICT)
n_jichikai <- nrow(df_d)
bar_profile <- get_bar_profile_v1(n_jichikai, cfg)
cfg_bar <- apply_bar_profile_to_cfg_v1(cfg, bar_profile)

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

# ---- 出力 -------------------------------------------------------------------
out_root <- file.path(project_root, "figures", "自治会")
run_id <- make_run_id_v1("test")
run_root <- init_run_root_v1(out_root, TARGET_YEAR, run_id)
log <- init_logger_v1(run_root)
write_gs_info_v1(log)

prefix <- paste0("自治会世帯数__", safe_filename(TARGET_DISTRICT))

save_pair_all_formats_v1(
  p_bar = p_bar,
  p_pie = p_pie,
  run_root = run_root,
  region_jp = region_jp,
  prefix = prefix,
  bar_profile = bar_profile,
  cfg = cfg,
  log = log
)

message("DONE: ", run_root)
