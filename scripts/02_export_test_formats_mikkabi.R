# =============================================================================
# 02_export_test_formats_mikkabi.R
#   SVG/PDF/EPS/PNG を同条件で書き出す「研究ハーネス」
#
# 【使い方】
#   - まず scripts/01_test_plotpane_mikkabi.R で見た目を詰める
#   - 次にこのスクリプトを Source して書き出し
#   - 出力フォルダを Illustrator で開いて差分を確認
# =============================================================================

project_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = FALSE)

config_path <- file.path(project_root, "lib", "plot_自治会_design_config.R")
style_path  <- file.path(project_root, "lib", "plot_自治会_style_v2.R")
csv_path    <- file.path(project_root, "data", "自治会", "2025", "自治会世帯数__2025.csv")

source(config_path, encoding = "UTF-8")
source(style_path,  encoding = "UTF-8")

df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

district_name <- "神久呂地区"

# caption: できるだけ「出典日付」を入れる
caption <- NULL
if ("asof" %in% names(df)) {
  u <- unique(na.omit(df$asof))
  if (length(u) == 1) caption <- paste0("出典: ", u)
}

p1 <- plot_bar_horizontal_desc(df, district_name = district_name, cfg = JICHAIKAI_PLOT_CFG, caption = caption)
p2 <- plot_pie_share(df, district_name = district_name, cfg = JICHAIKAI_PLOT_CFG, caption = caption)

# ---- 出力先（タイムスタンプで分ける） ---------------------------------------
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path(project_root, "figures", "自治会", "2025", "export_test", ts)

# ---- 形式一覧（必要に応じて増減） --------------------------------------------
formats <- c("svg", "pdf_cairo", "eps_cairo", "png_ragg", "svg_showtext", "pdf_showtext")  # アウトライン比較も含む

prefix <- paste0("mikkabi_", safe_filename(district_name))

res <- save_pair_multi(p1, p2, out_dir = out_dir, prefix = prefix, formats = formats, cfg = JICHAIKAI_PLOT_CFG)

message("written: ", out_dir)
print(res)

# jsonlite が無いと manifest を書けないので先にチェック
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite が必要です。install.packages('jsonlite')")
}

# ---- 研究ログ（環境情報） ----------------------------------------------------
# Illustrator で崩れた時に「どの環境で吐いたか」を追跡するため
log <- list(
  time = as.character(Sys.time()),
  R = R.version.string,
  platform = R.version$platform,
  packages = list(
    ggplot2 = as.character(utils::packageVersion("ggplot2")),
    svglite = if (requireNamespace("svglite", quietly=TRUE)) as.character(utils::packageVersion("svglite")) else NA,
    ragg    = if (requireNamespace("ragg", quietly=TRUE))    as.character(utils::packageVersion("ragg"))    else NA
  ),
  cfg = JICHAIKAI_PLOT_CFG,
  outputs = res
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
json_path <- file.path(out_dir, "manifest.json")
writeLines(jsonlite::toJSON(log, auto_unbox = TRUE, pretty = TRUE), json_path, useBytes = TRUE)

message("manifest: ", json_path)
