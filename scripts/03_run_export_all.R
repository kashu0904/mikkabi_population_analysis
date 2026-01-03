# =============================================================================
# 03_run_export_all.R
#   全地区を一括出力する runner（現行 run_自治会_plots.R の置き換え版）
#
# 【このスクリプトがやること】
#   - CSV読込
#   - region_key / district でフィルタ（任意）
#   - district ごとに bar/pie を生成
#   - save_pair_multi() で複数形式を書き出し（研究目的なら showtext も混ぜる）
#
# 【触る場所】
#   - lib/plot_自治会_design_config.R で見た目調整
#   - このファイルの "フィルタ" と "formats" と "出力先" だけ触る
# =============================================================================

project_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = FALSE)

config_path <- file.path(project_root, "lib", "plot_自治会_design_config.R")
style_path  <- file.path(project_root, "lib", "plot_自治会_style_v2.R")
csv_path    <- file.path(project_root, "data", "自治会", "2025", "自治会世帯数__2025.csv")

source(config_path, encoding = "UTF-8")
source(style_path,  encoding = "UTF-8")

df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

# ---- フィルタ（必要なら） ----------------------------------------------------
# 例: "kita" のみ
region_filter <- NULL  # 例: "kita"
district_filter <- NULL # 例: c("三ヶ日地区","細江地区")

if (!is.null(region_filter) && "region_key" %in% names(df)) {
  df <- df |> filter(.data$region_key == region_filter)
}
if (!is.null(district_filter)) {
  df <- df |> filter(.data$district %in% district_filter)
}

districts <- sort(unique(df$district))

# ---- caption（出典） ---------------------------------------------------------
caption <- NULL
asof_u <- if ("asof" %in% names(df)) unique(na.omit(df$asof)) else character(0)
url_u  <- if ("source_url" %in% names(df)) unique(na.omit(df$source_url)) else character(0)

if (length(asof_u) == 1) caption <- paste0("出典: ", asof_u)
if (length(url_u) == 1)  caption <- paste0(caption %||% "", if (!is.null(caption)) " / " else "", url_u)

# ---- 出力先 ------------------------------------------------------------------
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(project_root, "figures", "自治会", "2025", "exports_all", ts)

# ---- 形式（研究用途） --------------------------------------------------------
formats <- c("svg", "pdf_cairo", "png_ragg")  # 通常運用の最小
# formats <- c("svg","pdf_cairo","eps_cairo","png_ragg","svg_showtext","pdf_showtext")  # 研究フルセット

# ---- 実行 --------------------------------------------------------------------
results <- list()

for (d in districts) {
  message("\n=== ", d, " ===")

  p1 <- plot_bar_horizontal_desc(df, district_name = d, cfg = JICHAIKAI_PLOT_CFG, caption = caption)
  p2 <- plot_pie_share(df, district_name = d, cfg = JICHAIKAI_PLOT_CFG, caption = caption)

  prefix <- paste0("jichikai_", safe_filename(d))
  results[[d]] <- save_pair_multi(p1, p2, out_dir = out_dir, prefix = prefix, formats = formats, cfg = JICHAIKAI_PLOT_CFG)
}

message("\nDONE: ", out_dir)
invisible(results)
