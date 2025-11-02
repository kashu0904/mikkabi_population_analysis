
# ======================================================================
# batch_p0005_trend_v2.R  （診断強化版）
# - area_name を設定して実行すると、共通モジュールを複数パスから探索して読み込み
# - p0004 / p05 / years / NAMES_area が揃っているかを起動時に検証し、
#   未定義なら “どのパスを見に行ったか” を含むエラーメッセージを出す
# - その後、各町字の 0〜5歳人口（p0004+p05）の推移図とCSVを一括出力
# ======================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(here)
})

# ----------------------------------------------------------------------
# ① エリア指定（例: "mikkabi", "hosoe", "inasa"）
# ----------------------------------------------------------------------
area_name <- "mikkabi"

# ----------------------------------------------------------------------
# ② 共通モジュールの探索と読み込み
# ----------------------------------------------------------------------
candidate_paths <- c(
  here::here("lib", "plot_population_by_area_common.R"),
  file.path(getwd(), "lib", "plot_population_by_area_common.R"),
  file.path(getwd(), "plot_population_by_area_common.R"),
  file.path(getwd(), "..", "lib", "plot_population_by_area_common.R")
)

loaded_path <- NA_character_
for (p in candidate_paths) {
  if (file.exists(p)) {
    message("共通モジュールを読み込み: ", p)
    source(p, encoding = "UTF-8")
    loaded_path <- p
    break
  }
}

if (is.na(loaded_path)) {
  stop(paste0(
    "共通モジュールが見つかりません。\n",
    "- 探索したパス:\n  ", paste(candidate_paths, collapse = "\n  "), "\n",
    "- 提案: 本スクリプトと同じプロジェクト直下に 'lib/plot_population_by_area_common.R' を置いてください。"
  ))
}

# ----------------------------------------------------------------------
# ③ 必須オブジェクトの検証
# ----------------------------------------------------------------------
missing <- setdiff(c("p0004","p05","years","NAMES_area"), ls())
if (length(missing) > 0) {
  stop(paste0(
    "共通モジュールの読み込みに失敗（必須オブジェクト未定義）: ",
    paste(missing, collapse = ", "), "\n",
    "読み込んだファイル: ", loaded_path, "\n",
    "対処: area_name をこのスクリプト内で '", area_name, "' に設定済みか確認し、",
    "main_plot.R が正常に動く配置・フォルダ構成（data, lib 等）になっているか確認してください。"
  ))
}

# ----------------------------------------------------------------------
# ④ デザイン定数（存在しない場合のフォールバック）
# ----------------------------------------------------------------------
if (!exists("width_std"))      width_std      <- 0.75
if (!exists("linewidth_std"))  linewidth_std  <- 0.3
if (!exists("expand_x_std"))   expand_x_std   <- expansion(mult = c(0.04, 0.04))
if (!exists("expand_y_std"))   expand_y_std   <- expansion(mult = c(0.02, 0.06))
if (!exists("theme_std"))      theme_std      <- theme_minimal(base_family = "sans")

# ----------------------------------------------------------------------
# ⑤ 0〜5歳（p0004 + p05）シリーズ抽出
# ----------------------------------------------------------------------
extract_p0005_series <- function(area_label) {
  aidx <- match(area_label, NAMES_area)
  if (is.na(aidx)) stop(sprintf("'%s' は NAMES_area に存在しません。", area_label))

  m0004 <- matrix(p0004, nrow = length(years), byrow = TRUE)
  m05   <- matrix(p05,   nrow = length(years), byrow = TRUE)

  tibble(
    year  = years,
    p0004 = as.numeric(m0004[, aidx]),
    p05   = as.numeric(m05[,   aidx])
  ) %>% mutate(p0005 = p0004 + p05)
}

# ----------------------------------------------------------------------
# ⑥ 描画
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

# 事前にこのスクリプト全体をsourceしてある前提
df_one <- extract_p0005_series("三ヶ日地区")
p <- plot_p0005_trend_single(df_one, area_label = "三ヶ日町", base_area_label = NAMES_area[1])
p   # ← これでPlotsに表示（print(p)でもOK）


# ----------------------------------------------------------------------
# ⑦ 実行：全町字一括出力
# ----------------------------------------------------------------------
out_dir <- here::here("figures", "p0005", area_name)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

summary_list <- list()

for (area in NAMES_area) {
  df_one <- extract_p0005_series(area)
  p      <- plot_p0005_trend_single(df_one, area_label = area, base_area_label = NAMES_area[1])

  fname  <- file.path(out_dir, paste0(gsub('[\\\\/:*?"<>|\\s]+', "_", area), "_p0005_trend.png"))
  ggsave(filename = fname, plot = p, width = 8, height = 4.5, dpi = 300)

  summary_list[[area]] <- df_one %>% mutate(area = area) %>% select(area, year, p0005)
}

summary_df <- bind_rows(summary_list)
csv_path   <- file.path(out_dir, paste0("p0005_summary_", area_name, ".csv"))
readr::write_csv(summary_df, csv_path)

message("完了: 画像とCSVを出力しました => ", out_dir)
