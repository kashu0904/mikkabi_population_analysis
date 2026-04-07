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
    # scripts/ と lib/ に加え、本スクリプトが依存する lib ファイル群が揃う階層を project_root とみなす。
    # （別プロジェクトの scripts/lib を誤検出すると、古い cfg/関数を掴んで描画が崩れる原因になる）
    ok_dirs <- dir.exists(file.path(cur, "scripts")) && dir.exists(file.path(cur, "lib"))
    ok_libs <- file.exists(file.path(cur, "lib", "自治会", "自治会_デザイン設定_v1.R")) &&
      file.exists(file.path(cur, "lib", "自治会", "自治会_図表関数_v1.R")) &&
      file.exists(file.path(cur, "lib", "自治会", "自治会_書き出し_4形式_v1.R"))
    if (isTRUE(ok_dirs && ok_libs)) return(cur)
    parent <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = TRUE)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("project_root を検出できません（scripts/ と lib/ が見つかりません）。setwd(project_root) してから再実行してください。", call. = FALSE)
}

project_root <- find_project_root_boot()

# ---- SAFETY: 古い cfg/関数がグローバルに残っていると、source に失敗しても古い定義で続行してしまう。
#              その結果「同じ cfg を渡したはずなのに 03/04 だけ描画が違う」などが起きるため、まず掃除する。
rm_global_if_exists_v1 <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    rm(list = name, envir = .GlobalEnv)
  }
}
for (nm in c(
  "JICHAIKAI_PLOT_CFG_V1",
  "plot_pie_share_topN_v1", "plot_bar_horizontal_desc_v1",
  "save_pair_all_formats_v1", "save_one_plot_v1",
  "read_or_dummy", "add_unique_jichikai"
)) rm_global_if_exists_v1(nm)

source(file.path(project_root, "lib", "自治会", "自治会_デザイン設定_v1.R"), encoding = "UTF-8")
source(file.path(project_root, "lib", "自治会", "自治会_図表関数_v1.R"), encoding = "UTF-8")
source(file.path(project_root, "lib", "自治会", "自治会_書き出し_4形式_v1.R"), encoding = "UTF-8")

cfg <- JICHAIKAI_PLOT_CFG_V1

# ---- SAFETY: cfg 検査・補完・警告捕捉（03/04 共通の事故対策） ------------------
cfg_log_v1 <- function(log, level, msg) {
  # level: INFO/WARN/ALERT/CFG/FATAL など
  append_text_v1(log$warnings_txt, paste0("[", level, "] ", msg))
}

ensure_cfg_defaults_v1 <- function(cfg, log) {
  # 「コード側は参照するが cfg に値が無い」ケースを明示し、既定値を入れてログに残す。
  # 既定値は“現状の見た目を変えない”方向（=既存値のコピー）を優先。
  if (is.null(cfg$pie$label_x_01_10)) {
    cfg$pie$label_x_01_10 <- cfg$pie$label_x %||% 0.7
    cfg_log_v1(log, "CFG", "cfg$pie$label_x_01_10 が未定義のため既定値を補完しました（現状維持）。必要ならデザイン設定で上書きしてください。")
  }
  if (is.null(cfg$pie$slice_border_lwd_40_50)) {
    cfg$pie$slice_border_lwd_40_50 <- cfg$pie$slice_border_lwd %||% 0.5
    cfg_log_v1(log, "CFG", "cfg$pie$slice_border_lwd_40_50 が NULL のため既定値を補完しました（現状維持）。")
  }
  if (is.null(cfg$pie$slice_border_lwd_50_plus)) {
    cfg$pie$slice_border_lwd_50_plus <- cfg$pie$slice_border_lwd %||% 0.5
    cfg_log_v1(log, "CFG", "cfg$pie$slice_border_lwd_50_plus が NULL のため既定値を補完しました（現状維持）。")
  }
  cfg
}

validate_cfg_v1 <- function(cfg, log) {
  # 致命的に欠けている場合は止める（“外側配置に落ちる”等の再発を予防）
  if (is.null(cfg$pie) || is.null(cfg$export) || is.null(cfg$region)) {
    cfg_log_v1(log, "FATAL", "cfg の必須ブロック（pie/export/region）が欠落しています。デザイン設定の読み込みに失敗しています。")
    stop("cfg の必須ブロックが欠落しています（pie/export/region）。", call. = FALSE)
  }
  if (is.null(cfg$pie$label_x)) {
    cfg_log_v1(log, "FATAL", "cfg$pie$label_x が NULL です。NULL の場合フォールバックで label_x=1.15（外側配置）になり得ます。")
    stop("cfg$pie$label_x が NULL です（円グラフのラベル位置が不定）。", call. = FALSE)
  }
  invisible(TRUE)
}

calc_pie_label_x_used_v1 <- function(n_jichikai, cfg) {
  lx <- cfg$pie$label_x %||% 1.15
  if (n_jichikai <= 10) lx <- cfg$pie$label_x_01_10 %||% lx
  lx
}

capture_warnings_v1 <- function(expr, log, ctx) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      cfg_log_v1(log, "WARN", paste0(ctx, " :: ", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
}

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
cfg_log_v1(log, "INFO", "script=03_run_export_all_一括出力_v1.R")
cfg_log_v1(log, "INFO", paste0("getwd=", normalizePath(getwd(), winslash = "/", mustWork = TRUE)))
cfg_log_v1(log, "INFO", paste0("project_root=", project_root))

# どの cfg / lib を読んだかをログに残す（再発時に“どの時点で壊れたか”を即判定できる）
cfg_paths <- c(
  design = file.path(project_root, "lib", "自治会", "自治会_デザイン設定_v1.R"),
  plots  = file.path(project_root, "lib", "自治会", "自治会_図表関数_v1.R"),
  export = file.path(project_root, "lib", "自治会", "自治会_書き出し_4形式_v1.R")
)
for (nm in names(cfg_paths)) {
  p <- cfg_paths[[nm]]
  if (file.exists(p)) {
    fi <- file.info(p)
    cfg_log_v1(log, "INFO", paste0("lib_", nm, "=", normalizePath(p, winslash = "/", mustWork = TRUE), " mtime=", fi$mtime))
  } else {
    cfg_log_v1(log, "WARN", paste0("lib_", nm, " NOT FOUND: ", p))
  }
}

# cfg の補完と必須キー検査
cfg <- ensure_cfg_defaults_v1(cfg, log)
validate_cfg_v1(cfg, log)

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

    # SAFETY: pie ラベル位置が「外側（例: 1.15）」に落ちていないかを毎地区ログに残す
    ctx <- paste0(region_jp, " / ", d)
    pie_lx <- calc_pie_label_x_used_v1(n_jichikai, cfg)
    cfg_log_v1(log, "INFO", paste0(ctx, " :: n_jichikai=", n_jichikai, " pie_label_x_used=", pie_lx))
    if (!is.na(pie_lx) && pie_lx > 1.01) {
      cfg_log_v1(log, "ALERT", paste0(ctx, " :: pie_label_x_used が 1.0 を超えています（外側配置になり得ます）。cfg 読み込み失敗/上書きキー欠落を疑ってください。"))
    }

    # 描画と書き出し（エラーはログして続行）
    tryCatch({
      capture_warnings_v1({
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
      }, log = log, ctx = ctx)
    }, error = function(e) {
      append_text_v1(log$warnings_txt, paste0("[ERROR] ", region_jp, " / ", d, " :: ", conditionMessage(e)))
    })
  }
}

message("DONE: ", run_root)
message("export_manifest: ", log$export_csv)
message("warnings: ", log$warnings_txt)
