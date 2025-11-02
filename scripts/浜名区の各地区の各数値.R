# ============================================================
# plot_latest_ratio_overlay_hamanaku_multi.R
# 浜名区の10地区それぞれが別Excel（<key>_population_combined.xlsx）にある前提で、
# 「最新（例：2025年）の5比率」を横並びにオーバーレイ表示する独立スクリプト。
#   対象比率：高齢化率 / 後期高齢化率 / 従属人口比率 / 年少人口従属比率 / 老年人口従属比率
#   デザイン：色・ポイント形状・線種を metric_styles で簡単に変更可能
#   既存「比率まとめ（オーバーレイ）」の設計思想を踏襲
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(here)
})

# ------------------------------------------------------------
# 1) 基本設定 ——— 必要に応じてここだけ直す
# ------------------------------------------------------------
# 10地区のキー（= ファイル名の先頭）。
area_keys <- c(
  "mikkabi","inasa","hosoe","miyakoda","shinmiyakoda",
  "aratama","akasa","nakaze","hamana","kitahama"
)

# 表示名（任意）。未指定なら area_keys をそのまま使う。
# 例：display_names <- c(mikkabi="三ヶ日", hosoe="細江", inasa="引佐", miyakoda="都田", shinmiyakoda="新都田",
#                        aratama="有玉", akasa="赤佐", nakaze="中瀬", hamana="浜名", kitahama="北浜")
display_names <- NULL

# データ置き場とファイルパターン
processed_dir <- here::here("data","processed")
file_of <- function(key) file.path(processed_dir, paste0(key, "_population_combined.xlsx"))

# 出力PNG
out_file <- here::here("figures","ratio","hamanaku","ratio_overlay_latest.png")
width  <- 12; height <- 6; dpi <- 300

# デザイン（色・形・線種）。未指定は既定を使用。
metric_styles <- list(
  colors    = c(
    "高齢化率"         = "#BB0000",
    "後期高齢化率"     = "#7E3AF2",
    "従属人口比率"     = "#333333",
    "年少人口従属比率" = "#2CA02C",
    "老年人口従属比率" = "#006EBB"
  ),
  shapes    = c(
    "高齢化率"=21, "後期高齢化率"=22, "従属人口比率"=16,
    "年少人口従属比率"=24, "老年人口従属比率"=25
  ),
  linetypes = c(
    "高齢化率"="solid", "後期高齢化率"="solid", "従属人口比率"="solid",
    "年少人口従属比率"="dashed", "老年人口従属比率"="dashed"
  )
)

# ------------------------------------------------------------
# 2) ユーティリティ
# ------------------------------------------------------------
norm_area <- function(x){
  x <- as.character(x); x <- trimws(x)
  gsub("[[:space:]\u00A0\u3000]+", "", x, perl = TRUE)
}

ext_cell_num <- function(dat, row, col){
  as.numeric(dat[row, col][[1]])
}

safe_div <- function(num, den){ ifelse(den==0 | is.na(den), NA_real_, num/den) }

get_latest_year <- function(path){
  sh <- readxl::excel_sheets(path)
  yrs <- grep("^[0-9]{4}-04$", sh, value = TRUE)
  if (length(yrs) == 0) stop("YYYY-04 シートがありません: ", basename(path))
  yrs <- as.integer(sub("-04$", "", yrs))
  max(yrs)
}

# 単一ファイル（1地区）から最新年の5比率を抽出（“先頭ブロック=地区全体”の集計を採用）
read_latest_metrics_from_file <- function(path){
  latest_year  <- get_latest_year(path)
  latest_sheet <- sprintf("%d-04", latest_year)
  
  # 生データで地区ブロック数を検出
  wb  <- openxlsx::loadWorkbook(path)
  raw <- openxlsx::readWorkbook(wb, sheet = latest_sheet, colNames = FALSE)
  
  header_positions   <- seq(1, nrow(raw), by = 45)
  header_candidates  <- as.character(raw[header_positions, 11])
  header_stripped    <- stringr::str_sub(header_candidates, 2, -2)
  NAMES_area_raw     <- header_stripped[!is.na(header_stripped) & header_stripped != ""]
  NAMES_area         <- norm_area(NAMES_area_raw)
  n_areas            <- length(NAMES_area)
  if (n_areas == 0) stop("地区ヘッダーが検出できません: ", basename(path))
  
  # 通常データとして数値抽出（先頭ブロック index=0 を“地区全体”とみなす）
  dat       <- readxl::read_excel(path, sheet = latest_sheet, col_names = TRUE)
  index_vec <- 0  # 先頭ブロックのみ
  
  p0014  <- ext_cell_num(dat, 44 + 45*index_vec,  2)
  p1564  <- ext_cell_num(dat, 44 + 45*index_vec,  6)
  p6500  <- ext_cell_num(dat, 44 + 45*index_vec, 10)
  pTotal <- ext_cell_num(dat, 43 + 45*index_vec, 10)
  
  # 65-74 / 75+ は5歳階級からも取れるが、先頭ブロックのみ必要最小限で計算
  p6569 <- ext_cell_num(dat, 38 + 45*index_vec,  6)
  p7074 <- ext_cell_num(dat,  2 + 45*index_vec, 10)
  p6574 <- p6569 + p7074
  p7500 <- p6500 - p6574
  
  tibble(
    latest_year         = latest_year,
    `高齢化率`         = safe_div(p6500, pTotal) * 100,
    `後期高齢化率`     = safe_div(p7500, pTotal) * 100,
    `従属人口比率`     = safe_div(p0014 + p6500, p1564) * 100,
    `年少人口従属比率` = safe_div(p0014, p1564) * 100,
    `老年人口従属比率` = safe_div(p6500, p1564) * 100
  )
}

# ------------------------------------------------------------
# 3) 10ファイルを読み込んで結合
# ------------------------------------------------------------
rows <- list(); latest_years <- c()
for (key in area_keys) {
  path <- file_of(key)
  if (!file.exists(path)) stop("ファイルが見つかりません: ", path)
  met <- read_latest_metrics_from_file(path)
  rows[[key]] <- cbind(area_key = key, met)
  latest_years <- c(latest_years, met$latest_year[[1]])
}
latest_year <- max(latest_years, na.rm = TRUE)  # タイトル用（各ファイルで同年を想定）

wide <- dplyr::bind_rows(rows)

# 表示名付与
disp <- if (is.null(display_names)) setNames(area_keys, area_keys) else display_names

long <- wide |>
  select(area_key, latest_year, `高齢化率`, `後期高齢化率`, `従属人口比率`, `年少人口従属比率`, `老年人口従属比率`) |>
  tidyr::pivot_longer(cols = -c(area_key, latest_year), names_to = "metric", values_to = "value") |>
  mutate(area = factor(area_key, levels = area_keys, labels = unname(disp[area_keys])))

# ------------------------------------------------------------
# 4) 描画（オーバーレイ）
# ------------------------------------------------------------
colors    <- metric_styles$colors
shapes    <- metric_styles$shapes
linetypes <- metric_styles$linetypes

p <- ggplot(long, aes(x = area, y = value, group = metric,
                      color = metric, shape = metric, linetype = metric)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 3.0, stroke = 1.0, fill = "white") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
    labels = function(x) number(x, accuracy = 0.1, suffix = "%"),
    limits = c(0, NA), expand = expansion(mult = c(0.04,0.04))
  ) +
  labs(
    title    = sprintf("浜名区：各地区の主要比率（%d年 最新）", latest_year),
    subtitle = "高齢化率 / 後期高齢化率 / 従属人口比率 / 年少人口従属比率 / 老年人口従属比率",
    x = "地区", y = "比率（%）", color = NULL, shape = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

print(p)

if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
ggsave(out_file, p, width = width, height = height, dpi = dpi)
message("Saved: ", out_file)
