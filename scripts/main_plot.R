# =============================================================================
# run_自治会_plots.R
#  - ここだけいじる：csv_path / out_dir / district_filter / non_corporate_mark
#  - CSV内の district を自動で列挙して、地区ごとに bar/pie を出力する
# =============================================================================

source("plot_自治会_style.R")

# ---- ここだけ書き換える ------------------------------------------------------
csv_path <- NULL  # 例: "C:/Users/pirat/Documents/MikkabiLab_population_analysis/data/自治会/2025/自治会世帯数__2025.csv"
out_dir <- "C:/Users/pirat/Documents/MikkabiLab_population_analysis/figure/自治会/2025/out_svg"

# NULLならCSV内の全 district を一括処理。ベクトルを入れるとその地区だけ。
district_filter <- NULL  # 例: c("三ヶ日町", "中ノ町地区")

# corporate==FALSE の自治会名に付ける印（後でここを変えるだけ）
non_corporate_mark <- "×"

# 棒グラフのバー色
bar_color <- "#005686"
# -----------------------------------------------------------------------------


df <- read_or_dummy(csv_path, warn_on_dummy = TRUE)

# ダミーを使ったかを明示したいなら、ここで追い打ちも可能
if (isTRUE(attr(df, "used_dummy"))) {
  warning("used_dummy = TRUE：SVGを配布・公開する前に実データで再生成してください。", call. = FALSE)
}

districts <- sort(unique(df$district))

if (!is.null(district_filter)) {
  districts <- intersect(districts, district_filter)
  if (length(districts) == 0) {
    stop("district_filter に該当する district がCSVにありません。", call. = FALSE)
  }
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

results <- list()

for (d in districts) {
  message("\n=== district: ", d, " ===")
  
  p1 <- plot_bar_horizontal_desc(
    df, d,
    bar_color = bar_color,
    non_corp_mark = non_corporate_mark
  )
  
  p2 <- plot_pie_share(
    df, d,
    non_corp_mark = non_corporate_mark
  )
  
  print(p1)
  print(p2)
  
  prefix <- paste0("jichikai_", safe_filename(d))
  
  results[[d]] <- save_svg_pair(
    p1, p2,
    out_dir = out_dir,
    prefix = prefix,
    w = 11, h = 6
  )
}

invisible(results)
