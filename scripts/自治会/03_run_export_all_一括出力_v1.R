# =============================================================================
# 03_run_export_all_一括出力_v1.R
#   - 全地区を 4形式（PDF/PNG/SVG/EPS）で一括書き出し
#   - 地域フォルダは日本語（例：北地域）
#   - bar は自治会数レンジ別プロフィール、pie は固定 7×7（ラベル上位5）
#   - PDF/EPSは embedFonts を必ず試行（失敗しても続行、ログへ記録）
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
#  - ここが存在するなら必ずこれを使う
#  - 存在しない場合は project_root 配下の相対パスへフォールバック
csv_path <- file.path(project_root, "data", "自治会", as.character(TARGET_YEAR), paste0("自治会世帯数__", TARGET_YEAR, ".csv"))
# ---- 読み込み ----------------------------------------------------------------
df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)
if (!("region_key" %in% names(df))) stop("CSVに region_key 列がありません。", call. = FALSE)

# 同名自治会をユニーク化（市場（1）, 市場（2）...）
df <- add_unique_jichikai(df)

# 出典日付（列 asof が単一なら採用）
asof <- pick_asof_value_v1(df)
asof_reiwa <- to_reiwa_date(asof)

# ---- 出力ルート --------------------------------------------------------------
out_root <- file.path(project_root, "figures", "自治会")
run_id <- make_run_id_v1("jichikai")
run_root <- init_run_root_v1(out_root, TARGET_YEAR, run_id)
log <- init_logger_v1(run_root)
write_gs_info_v1(log)

append_text_v1(log$warnings_txt, paste0("[INFO] run_root=", run_root))

# ---- ループ -----------------------------------------------------------------
# 地域順: cfg$region$map の順を優先（存在しないキーは後ろ）
keys_in_data <- unique(na.omit(df$region_key))
keys_in_map  <- names(cfg$region$map)
region_keys <- c(intersect(keys_in_map, keys_in_data), setdiff(keys_in_data, keys_in_map))

for (rk in region_keys) {
  region_jp <- resolve_region_name_v1(rk, cfg, log)
  df_r <- df |> filter(region_key == rk)
  districts <- unique(df_r$district)
  districts <- districts[!is.na(districts)]

  for (d in districts) {
    # district内の自治会数
    df_d <- df_r |> filter(district == d)
    n_jichikai <- nrow(df_d)

    # barプロフィール決定 → cfgへ反映
    bar_profile <- get_bar_profile_v1(n_jichikai, cfg)
    cfg_bar <- apply_bar_profile_to_cfg_v1(cfg, bar_profile)

    # 出典（既存準拠）
    caption <- make_source_caption(region_jp, asof_reiwa)

    # 描画と書き出し（エラーはログして続行）
    tryCatch({
      p_bar <- plot_bar_horizontal_desc_v1(
        df_r, d,
        n_jichikai = n_jichikai,
        cfg = cfg_bar,
        non_corp_mark = cfg$label$non_corp_mark,
        mark_non_corporate = cfg$label$mark_non_corporate,
        caption = caption
      )

      p_pie <- plot_pie_share_topN_v1(
        df_r, d,
        n_jichikai = n_jichikai,
        cfg = cfg,
        non_corp_mark = cfg$label$non_corp_mark,
        mark_non_corporate = cfg$label$mark_non_corporate,
        caption = caption
      )

      prefix <- paste0("自治会世帯数__", safe_filename(d))

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

    }, error = function(e) {
      append_text_v1(log$warnings_txt, paste0("[ERROR] ", region_jp, " / ", d, " :: ", conditionMessage(e)))
    })
  }
}

message("DONE: ", run_root)
message("export_manifest: ", log$export_csv)
message("warnings: ", log$warnings_txt)
