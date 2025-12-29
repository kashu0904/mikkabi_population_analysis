# =============================================================================
# run_jichikai_plots.R
#  - ここだけいじる：csv_path と district_name
# =============================================================================

source("plot_自治会_style.R")

# ---- ここだけ書き換える ------------------------------------------------------
csv_path <- NULL  # 例: "C:/path/to/jichikai_households.csv"
district_name <- "三ヶ日町"
out_dir <- "out_svg"
# -----------------------------------------------------------------------------


df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

# ダミーを使ったかを明示したいなら、ここで追い打ちも可能
if (isTRUE(attr(df, "used_dummy"))) {
  warning("used_dummy = TRUE：SVGを配布・公開する前に実データで再生成してください。", call. = FALSE)
}

p1 <- plot_bar_horizontal_desc(df, district_name, bar_color = "#005686")
p2 <- plot_pie_share(df, district_name)

print(p1)
print(p2)

save_svg_pair(
  p1, p2,
  out_dir = out_dir,
  prefix = paste0("jichikai_", district_name),
  w = 11, h = 6
)
