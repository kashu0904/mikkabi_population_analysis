# =============================================================================
# 01_test_plotpane_mikkabi.R
#   RStudio の Plot pane だけで「三ヶ日地区」を即試せるテストランナー
#
# 【使い方】
#   - RStudio でこのファイルを開いて Source（Ctrl+Shift+Enter）
#   - Plot pane に bar と pie が順に表示される
#   - lib/plot_自治会_design_config.R をいじって → もう一回 Source
# =============================================================================

# ---- パス（プロジェクトに合わせて調整） --------------------------------------
project_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = FALSE)

# もし「このスクリプトの場所」を基準にしたいなら下を有効化して下さい
# this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile, winslash="/"), error=function(e) NULL)
# if (!is.null(this_file)) project_root <- normalizePath(file.path(dirname(this_file), ".."), winslash="/")

config_path <- file.path(project_root, "lib", "plot_自治会_design_config.R")
style_path  <- file.path(project_root, "lib", "plot_自治会_style_v2.R")
csv_path    <- file.path(project_root, "data", "自治会", "2025", "自治会世帯数__2025.csv")

# ---- 読み込み ----------------------------------------------------------------
source(config_path, encoding = "UTF-8")
source(style_path,  encoding = "UTF-8")

df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

# ---- テスト対象 --------------------------------------------------------------
district_name <- "三ヶ日地区"

# 研究の都合で caption を「出典日付」「URL」込みにするならここ
caption <- NULL
if ("asof" %in% names(df)) {
  u <- unique(na.omit(df$asof))
  if (length(u) == 1) caption <- paste0("出典: ", u)
}

# ---- プロット（Plot pane） ---------------------------------------------------
p1 <- plot_bar_horizontal_desc(df, district_name = district_name, cfg = JICHAIKAI_PLOT_CFG, caption = caption)
p2 <- plot_pie_share(df, district_name = district_name, cfg = JICHAIKAI_PLOT_CFG, caption = caption)

print(p1)
print(p2)
