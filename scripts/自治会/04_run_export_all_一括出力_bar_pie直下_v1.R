# =============================================================================
# 04_run_export_all_一括出力_bar_pie直下_v1.R
#   - 全地区を 4形式（PDF/PNG/SVG/EPS）で一括書き出し
#   - 地域フォルダは日本語（例：北地域）
#   - 出力階層を「地域/（bar|pie）」までに簡略化し、図表ファイルは bar/pie 直下へ配置
#     例）run_root/pdf/北地域/bar/自治会世帯数__三ヶ日地区__bar.pdf
#         run_root/pdf/北地域/pie/自治会世帯数__三ヶ日地区__pie.pdf
#   - bar のサイズは「自治会数レンジ別プロフィール」を引き続き適用する（ただしフォルダ分けはしない）
#   - pie は固定 7×7（ラベル上位5）
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
append_text_v1(log$warnings_txt, "[INFO] export_layout=region/(bar|pie)/file (flat)")

cfg_log_v1(log, "INFO", "script=04_run_export_all_一括出力_bar_pie直下_v1.R")
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


# ---- 04専用：地区並び順（YAML） ---------------------------------------------
# config/hamamatsu_district_order.yaml を編集することで、地域フォルダ内の地区順（番号）を制御できます。
# ※district 名はCSVの district 列と完全一致が必要です。

DISTRICT_ORDER_YAML <- file.path(project_root, "config", "hamamatsu_district_order.yaml")

load_district_order_yaml_v1 <- function(path, log) {
  out <- list(
    filename = list(pad = 2L, sep = "_"),
    order_by_region = list()
  )
  
  if (!file.exists(path)) {
    append_text_v1(log$warnings_txt, paste0("[WARN] district_order_yaml not found: ", path, " (データ出現順で番号付与します)"))
    return(out)
  }
  
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("パッケージ 'yaml' が必要です。install.packages('yaml') を実行してください。", call. = FALSE)
  }
  
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  y <- yaml::yaml.load(paste(txt, collapse = "\n"))
  if (is.null(y)) return(out)
  
  # filename settings
  if (!is.null(y$filename$pad)) out$filename$pad <- as.integer(y$filename$pad)
  if (!is.null(y$filename$sep)) out$filename$sep <- as.character(y$filename$sep)
  
  # order settings
  if (!is.null(y$order_by_region)) out$order_by_region <- y$order_by_region
  
  out
}

order_districts_v1 <- function(districts_data, rk, order_by_region, log) {
  districts_data <- as.character(districts_data)
  districts_data <- districts_data[!is.na(districts_data) & nzchar(trimws(districts_data))]
  
  ord <- order_by_region[[rk]]
  if (is.null(ord)) return(districts_data)
  
  ord_vec <- as.character(unlist(ord, use.names = FALSE))
  ord_vec <- ord_vec[!is.na(ord_vec) & nzchar(trimws(ord_vec))]
  ord_vec <- ord_vec[!duplicated(ord_vec)]
  
  missing_in_data <- setdiff(ord_vec, districts_data)
  if (length(missing_in_data) > 0) {
    append_text_v1(log$warnings_txt, paste0("[WARN] district_order_yaml: region_key=", rk, " にデータ不存在の地区があります: ", paste(missing_in_data, collapse = ", ")))
  }
  
  not_in_order <- setdiff(districts_data, ord_vec)
  if (length(not_in_order) > 0) {
    append_text_v1(log$warnings_txt, paste0("[WARN] district_order_yaml: region_key=", rk, " にYAML未定義の地区があります（末尾に追加）: ", paste(not_in_order, collapse = ", ")))
  }
  
  c(ord_vec[ord_vec %in% districts_data], not_in_order)
}

# 読み込み（logger 初期化後に実行）
district_order_cfg <- load_district_order_yaml_v1(DISTRICT_ORDER_YAML, log)
district_order_by_region <- district_order_cfg$order_by_region %||% list()
file_pad <- district_order_cfg$filename$pad %||% 2L
file_sep <- district_order_cfg$filename$sep %||% "_"
if (is.na(file_pad) || file_pad < 1) file_pad <- 2L
if (is.null(file_sep) || !nzchar(file_sep)) file_sep <- "_"

append_text_v1(log$warnings_txt, paste0("[INFO] district_order_yaml=", DISTRICT_ORDER_YAML))
append_text_v1(log$warnings_txt, paste0("[INFO] numbering_prefix=on pad=", file_pad, " sep='", file_sep, "'"))


# ---- 04専用：bar/pie 直下へ保存（フォルダ分けしない） ------------------------
save_pair_all_formats_bar_pie_flat_v1 <- function(
    p_bar, p_pie,
    run_root,
    region_jp,
    prefix,
    bar_profile,
    cfg,
    log
) {
  formats <- cfg$export$formats
  
  # bar/pie サイズ
  bar_w <- bar_profile$w %||% cfg$export$bar_w_default
  bar_h <- bar_profile$h %||% cfg$export$bar_h_default
  pie_w <- cfg$export$pie_w
  pie_h <- cfg$export$pie_h
  
  # pie の仕様は現状維持（メタ情報としてのみ使用）
  pie_folder <- get_pie_folder_v1(cfg)
  
  # manifest
  t <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  for (fmt in formats) {
    base_fmt_dir <- file.path(run_root, fmt, region_jp)
    
    # ★ここが04の要件：bar/pie 直下
    out_bar_dir <- file.path(base_fmt_dir, "bar")
    out_pie_dir <- file.path(base_fmt_dir, "pie")
    
    dir.create(out_bar_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(out_pie_dir, recursive = TRUE, showWarnings = FALSE)
    
    bar_path <- file.path(out_bar_dir, paste0(prefix, "__bar.", fmt))
    pie_path <- file.path(out_pie_dir, paste0(prefix, "__pie.", fmt))
    
    ok_bar <- TRUE
    ok_pie <- TRUE
    
    # 1) 保存（失敗しても次形式へ）
    tryCatch(
      save_one_plot_v1(p_bar, bar_path, bar_w, bar_h, fmt, cfg),
      error = function(e) {
        ok_bar <<- FALSE
        append_text_v1(log$warnings_txt, paste0("[WARN] save(bar) failed: fmt=", fmt, " path=", bar_path, " msg=", conditionMessage(e)))
      }
    )
    tryCatch(
      save_one_plot_v1(p_pie, pie_path, pie_w, pie_h, fmt, cfg),
      error = function(e) {
        ok_pie <<- FALSE
        append_text_v1(log$warnings_txt, paste0("[WARN] save(pie) failed: fmt=", fmt, " path=", pie_path, " msg=", conditionMessage(e)))
      }
    )
    
    # 2) PDF/EPS は embedFonts を必ず試行（失敗しても続行、ログへ記録）
    if (ok_bar && fmt %in% c("pdf", "eps")) {
      apply_embedfonts_v1(bar_path, fmt, log, region_jp, prefix, "bar")
    }
    if (ok_pie && fmt %in% c("pdf", "eps")) {
      apply_embedfonts_v1(pie_path, fmt, log, region_jp, prefix, "pie")
    }
    
    # 3) export manifest（bar/pie 両方成功した場合のみ）
    if (ok_bar && ok_pie) {
      append_csv_row_v1(
        path = log$export_csv,
        header = c("timestamp", "format", "region", "prefix", "type", "profile_or_pie", "path", "w_in", "h_in"),
        row = c(t, fmt, region_jp, prefix, "bar", bar_profile$folder, bar_path, bar_w, bar_h)
      )
      append_csv_row_v1(
        path = log$export_csv,
        header = c("timestamp", "format", "region", "prefix", "type", "profile_or_pie", "path", "w_in", "h_in"),
        row = c(t, fmt, region_jp, prefix, "pie", pie_folder, pie_path, pie_w, pie_h)
      )
    }
  }
}


# ---- ループ -----------------------------------------------------------------
# 地域順: cfg$region$map の順を優先（存在しないキーは後ろ）
keys_in_data <- unique(na.omit(df$region_key))
keys_in_map  <- names(cfg$region$map)
region_keys <- c(intersect(keys_in_map, keys_in_data), setdiff(keys_in_data, keys_in_map))

for (rk in region_keys) {
  region_jp <- resolve_region_name_v1(rk, cfg, log)
  df_r <- df |> filter(region_key == rk)
  districts_data <- unique(df_r$district)
  districts_data <- districts_data[!is.na(districts_data)]
  districts <- order_districts_v1(districts_data, rk, district_order_by_region, log)
  
  for (i in seq_along(districts)) {
    d <- districts[[i]]
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
      
      num <- sprintf(paste0("%0", file_pad, "d"), i)
      prefix <- paste0(num, file_sep, "自治会世帯数__", safe_filename(d))
      
      save_pair_all_formats_bar_pie_flat_v1(
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
