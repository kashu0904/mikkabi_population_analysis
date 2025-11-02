
# ======================================================================
# batch_p0005_trend.R
# main_plot.R と同じ前提で、指定の area_name（例: "mikkabi"）に対して
# 各「町字」の 0〜5歳人口（p0004 + p05）の推移グラフを一括出力します。
# さらに年次×町字のCSVサマリも保存します。
# ----------------------------------------------------------------------
# 使い方:
#   1) area_name を設定（例: area_name <- "mikkabi"）
#   2) 本スクリプトを実行（source でも Rscript でもOK）
#   3) 出力:
#       - figures/p0005/<area_name>/<area>_p0005_trend.png
#       - figures/p0005/<area_name>/p0005_summary_<area_name>.csv
# ======================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(here)
  library(stringi)
})

# ----------------------------------------------------------------------
# ① データ対象（Excelの接頭）を指定： 'mikkabi' / 'hosoe' / 'inasa' など
#    ★ 必要に応じて書き換えてください
# ----------------------------------------------------------------------
area_name <- "mikkabi"

# ----------------------------------------------------------------------
# ② 共通モジュールを読み込み（main_plot.R と同じ）
#    ※ このモジュール内で、対象 Excel を自動で読み込み、
#      years, NAMES_area, p0004, p05, theme_std, width_std などが定義されます。
# ----------------------------------------------------------------------
source(here::here("lib", "plot_population_by_area_common.R"), encoding = "UTF-8")

# ----------------------------------------------------------------------
# デザイン定数（共通が無い場合のフォールバック）
# ----------------------------------------------------------------------
if (!exists("width_std"))      width_std      <- 0.75
if (!exists("linewidth_std"))  linewidth_std  <- 0.3
if (!exists("expand_x_std"))   expand_x_std   <- expansion(mult = c(0.04, 0.04))
if (!exists("expand_y_std"))   expand_y_std   <- expansion(mult = c(0.02, 0.06))
if (!exists("theme_std"))      theme_std      <- theme_minimal(base_family = "sans")

# ----------------------------------------------------------------------
# 便利: ファイル名用の正規化（日本語OK、空白等を安全な文字へ）
# ----------------------------------------------------------------------
norm_fname <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)   # 予約文字をアンダースコアに
  x <- gsub("\\s+", "_", x)             # 空白をアンダースコアに
  x
}

# ----------------------------------------------------------------------
# 指定した町字の時系列を取り出す（0〜5歳: p0004 + p05）
# - 共通モジュールで定義済みのベクトル（p0004, p05）は
#   長さ = years * n_areas で、各年×各地区の順で並ぶ。
# - 行列化して area 列だけを抜き出す。
# ----------------------------------------------------------------------
extract_p0005_series <- function(area_label) {
  aidx <- match(area_label, NAMES_area)
  if (is.na(aidx)) stop(sprintf("'%s' は NAMES_area に存在しません。", area_label))
  if (!all(c("p0004","p05","years") %in% ls())) stop("共通モジュールの読み込みに失敗（p0004/p05/yearsが未定義）。")

  # 年×地区の行列へ（byrow=TRUE で年ごとに地区が並ぶ想定）
  m0004 <- matrix(p0004, nrow = length(years), byrow = TRUE)
  m05   <- matrix(p05,   nrow = length(years), byrow = TRUE)

  tibble(
    year  = years,
    p0004 = as.numeric(m0004[, aidx]),
    p05   = as.numeric(m05[,   aidx])
  ) %>% mutate(p0005 = p0004 + p05)
}

# ----------------------------------------------------------------------
# 描画（棒グラフ）
# ----------------------------------------------------------------------
plot_p0005_trend_single <- function(df_area, area_label, base_area_label) {
  ggplot(df_area, aes(x = factor(year, levels = years), y = p0005)) +
    geom_col(width = width_std, color = "black", linewidth = linewidth_std, fill = "#6BC7F1") +
    scale_x_discrete(expand = expand_x_std) +
    scale_y_continuous(expand = expand_y_std, labels = scales::comma) +
    labs(title = paste0(area_label, "：0〜5歳の人口の推移 ", min(years), "〜", max(years), "（", base_area_label, "）"),
         x = "年", y = "人口（人）") +
    theme_std +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# ----------------------------------------------------------------------
# 実行：全町字を一括処理して保存
# ----------------------------------------------------------------------
out_dir <- here::here("figures", "p0005", area_name)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

summary_list <- list()

for (area in NAMES_area) {
  df_one <- extract_p0005_series(area)
  p      <- plot_p0005_trend_single(df_one, area_label = area, base_area_label = NAMES_area[1])

  fname  <- file.path(out_dir, paste0(norm_fname(area), "_p0005_trend.png"))
  ggsave(filename = fname, plot = p, width = 8, height = 4.5, dpi = 300)

  summary_list[[area]] <- df_one %>% mutate(area = area) %>% select(area, year, p0005)
}

# サマリCSVの保存
summary_df <- bind_rows(summary_list)
csv_path   <- file.path(out_dir, paste0("p0005_summary_", area_name, ".csv"))
readr::write_csv(summary_df, csv_path)

message("完了: 画像とCSVを出力しました => ", out_dir)
