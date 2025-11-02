# ============================================================
# plot_latest_ratio_overlay_hamanaku_multi.R
# 浜名区の10地区それぞれが別Excel（<key>_population_combined.xlsx）にある前提で、
# 「最新（例：2025年）の5比率」を横並びにオーバーレイ表示する独立スクリプト。
# さらに各地区の「総人口」と「平均年齢」（最新年）もグラフ出力。
#   指標：高齢化率 / 後期高齢化率 / 従属人口比率 / 年少人口従属比率 / 老年人口従属比率
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
# 1) 基本設定
# ------------------------------------------------------------
area_keys <- c(
  "mikkabi","inasa","hosoe","miyakoda","shinmiyakoda",
  "aratama","akasa","nakaze","hamana","kitahama"
)

# 表示名（任意）。未指定なら area_keys をそのまま使う。
# display_names <- c(mikkabi="三ヶ日", hosoe="細江", inasa="引佐", miyakoda="都田", shinmiyakoda="新都田",
#                    aratama="有玉", akasa="赤佐", nakaze="中瀬", hamana="浜名", kitahama="北浜")
display_names <- c(
  mikkabi="三ヶ日",
  inasa="引佐",
  hosoe="細江",
  miyakoda="都田",
  shinmiyakoda="新都田",
  aratama="麁玉",
  akasa="赤佐",
  nakaze="中瀬",
  hamana="浜名",
  kitahama="北浜"
)


processed_dir <- here::here("data","processed")
file_of <- function(key) file.path(processed_dir, paste0(key, "_population_combined.xlsx"))

# 出力ディレクトリ & ファイル名
out_dir  <- here::here("figures","ratio","hamanaku")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
file_overlay <- file.path(out_dir, "ratio_overlay_latest.png")
file_total   <- file.path(out_dir, "total_population_latest.png")
file_meanage <- file.path(out_dir, "mean_age_latest.png")
width  <- 12; height <- 6; dpi <- 300

# デザイン（色・形・線種）
metric_styles <- list(
  colors    = c(
    "高齢化率"         = "#BB0000",
    "後期高齢化率"     = "#7E3AF2",
    "従属人口比率"     = "#333333",
    "年少人口従属比率" = "#2CA02C",
    "老年人口従属比率" = "#006EBB"
  ),
  shapes    = c(
    "高齢化率"=21, "後期高齢化率"=22, "従属人口比率"=23,
    "年少人口従属比率"=24, "老年人口従属比率"=25
  ),
  linetypes = c(
    "高齢化率"="solid", "後期高齢化率"="solid", "従属人口比率"="solid",
    "年少人口従属比率"="solid", "老年人口従属比率"="solid"
  )
)

# ------------------------------------------------------------
# 2) ユーティリティ
# ------------------------------------------------------------
norm_area <- function(x){
  x <- as.character(x); x <- trimws(x)
  gsub("[[:space:] 　]+", "", x, perl = TRUE)
}

ext_cell_num <- function(dat, row, col){
  as.numeric(dat[row, col][[1]])
}

ext_vec <- function(dat, rows, col){
  as.numeric(dat[rows, col][[1]])
}

safe_div <- function(num, den){ ifelse(den==0 | is.na(den), NA_real_, num/den) }

get_latest_year <- function(path){
  sh <- readxl::excel_sheets(path)
  yrs <- grep("^[0-9]{4}-04$", sh, value = TRUE)
  if (length(yrs) == 0) stop("YYYY-04 シートがありません: ", basename(path))
  yrs <- as.integer(sub("-04$", "", yrs))
  max(yrs)
}

# 指定ファイル（1地区）から最新年の5比率＋総人口＋平均年齢を取得
read_latest_from_file <- function(path){
  latest_year  <- get_latest_year(path)
  latest_sheet <- sprintf("%d-04", latest_year)
  
  # 生データでブロック数検出（念のため）
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
  dat <- readxl::read_excel(path, sheet = latest_sheet, col_names = TRUE)
  i <- 0
  # 集計行
  p0014  <- ext_cell_num(dat, 44 + 45*i,  2)
  p1564  <- ext_cell_num(dat, 44 + 45*i,  6)
  p6500  <- ext_cell_num(dat, 44 + 45*i, 10)
  pTotal <- ext_cell_num(dat, 43 + 45*i, 10)
  
  # 65-74 / 75+
  p6569 <- ext_cell_num(dat, 38 + 45*i,  6)
  p7074 <- ext_cell_num(dat,  2 + 45*i, 10)
  p6574 <- p6569 + p7074
  p7500 <- p6500 - p6574
  
  # 平均年齢（ご指定の抽出手順を忠実に実装）
  v1 <- ext_vec(dat, rows = 3:43 + 45*i, col = 2)
  v2 <- ext_vec(dat, rows = 3:43 + 45*i, col = 6)
  v3 <- ext_vec(dat, rows = 3:42 + 45*i, col = 10)  # ここだけ 3:42
  # 6の倍数行を除外（必ずカッコ）
  v1 <- v1[-(6 * (1:6))]
  v2 <- v2[-(6 * (1:6))]
  v3 <- v3[-(6 * (1:5))]
  num <- sum(c(v1 * (0:34), v2 * (35:69), v3 * (70:104)))
  den <- pTotal
  mean_age <- num / den + 0.5
  
  tibble(
    latest_year         = latest_year,
    total_pop           = as.numeric(pTotal),
    mean_age            = as.numeric(mean_age),
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
  met <- read_latest_from_file(path)
  rows[[key]] <- cbind(area_key = key, met)
  latest_years <- c(latest_years, met$latest_year[[1]])
}
latest_year <- max(latest_years, na.rm = TRUE)
wide <- dplyr::bind_rows(rows)

# 表示名
disp <- if (is.null(display_names)) setNames(area_keys, area_keys) else display_names

# 比率のロング
long_ratio <- wide |>
  select(area_key, latest_year, `高齢化率`, `後期高齢化率`, `従属人口比率`, `年少人口従属比率`, `老年人口従属比率`) |>
  tidyr::pivot_longer(cols = -c(area_key, latest_year), names_to = "metric", values_to = "value") |>
  mutate(area = factor(area_key, levels = area_keys, labels = unname(disp[area_keys])))

# 合計人口・平均年齢
stats_df <- wide |>
  transmute(area = factor(area_key, levels = area_keys, labels = unname(disp[area_keys])),
            latest_year = latest_year, total_pop = total_pop, mean_age = mean_age)

# ------------------------------------------------------------
# 4) 描画
# ------------------------------------------------------------
# 4-1) 比率まとめ（オーバーレイ）
colors    <- metric_styles$colors
shapes    <- metric_styles$shapes
linetypes <- metric_styles$linetypes

p_ratio <- ggplot(long_ratio, aes(x = area, y = value, group = metric,
                                  color = metric, shape = metric, linetype = metric)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 3.0, stroke = 1.0, fill = "white") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(labels = function(x) number(x, accuracy = 0.1, suffix = "%"),
                     limits = c(0, NA), expand = expansion(mult = c(0.04,0.04))) +
  labs(title = sprintf("浜名区：各地区の主要比率（%d年 最新）", latest_year),
       subtitle = "高齢化率 / 後期高齢化率 / 従属人口比率 / 年少人口従属比率 / 老年人口従属比率",
       x = "地区", y = "比率（%）", color = NULL, shape = NULL, linetype = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# 4-2) 総人口（最新年）
# 4-2) 総人口（最新年）
p_total <- ggplot(stats_df, aes(x = area, y = total_pop)) +
  geom_col(width = 0.65) +
  scale_y_continuous(labels = label_comma(accuracy = 1),
                     limits = c(0, 50000)) +   # ← ここで範囲を固定
  labs(title = sprintf("浜名区：各地区の総人口（%d年 最新）", latest_year),
       x = "地区", y = "総人口") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1))


# 4-3) 平均年齢（最新年）
p_mean <- ggplot(stats_df, aes(x = area, y = mean_age)) +
  geom_col(width = 0.65) +
  scale_y_continuous(labels = function(x) number(x, accuracy = 0.1)) +
  labs(title = sprintf("浜名区：各地区の平均年齢（%d年 最新）", latest_year),
       x = "地区", y = "平均年齢（歳）") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# 出力
print(p_ratio);  ggsave(file_overlay, p_ratio,  width = width, height = height, dpi = dpi); message("Saved: ", file_overlay)
print(p_total);  ggsave(file_total,   p_total,  width = width, height = height, dpi = dpi); message("Saved: ", file_total)
print(p_mean);   ggsave(file_meanage, p_mean,   width = width, height = height, dpi = dpi); message("Saved: ", file_meanage)


# ------------------------------------------------------------
# 5) Excel に横並びで書き出し（行列反転・単位付き・総人口割合追加）
# ------------------------------------------------------------
out_excel <- file.path(out_dir, "hamanaku_latest_summary.xlsx")

# 総人口合計
grand_total <- sum(wide$total_pop, na.rm = TRUE)

# データ整形（横持ち）
export_df <- wide |>
  transmute(
    地区        = unname(display_names[area_key]),
    年度        = latest_year,
    総人口      = total_pop,
    平均年齢    = paste0(round(mean_age, 2), "歳"),
    高齢化率    = paste0(round(`高齢化率`, 2), "%"),
    後期高齢化率= paste0(round(`後期高齢化率`, 2), "%"),
    従属人口比率= paste0(round(`従属人口比率`, 2), "%"),
    年少人口従属比率 = paste0(round(`年少人口従属比率`, 2), "%"),
    老年人口従属比率 = paste0(round(`老年人口従属比率`, 2), "%"),
    総人口割合  = paste0(round(total_pop / grand_total * 100, 2), "%")  # ← 追加
  )

# 年度は共通なので削除
export_df <- export_df |> dplyr::select(-年度)

# 総人口はカンマ区切り（人の単位は付けない）
export_df <- export_df |>
  dplyr::mutate(総人口 = scales::comma(総人口)) |>
  dplyr::mutate(dplyr::across(-地区, as.character))

# 行列反転（指標ごとに行、地区を列）
export_long <- export_df |>
  tidyr::pivot_longer(cols = -地区, names_to = "指標", values_to = "値") |>
  tidyr::pivot_wider(names_from = 地区, values_from = 値)

openxlsx::write.xlsx(export_long, out_excel, overwrite = TRUE)
message("Saved Excel: ", out_excel)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# 地区名（連番を除去して降順ソート）
df <- hamanaku_list %>%
  mutate(地区 = sub("^\\d+", "", Area)) %>%
  arrange(desc(Totalp)) %>%
  mutate(人口表示 = paste0(地区, "\n", comma(Totalp)))

# 円グラフ
p_pie <- ggplot(df, aes(x = "", y = Totalp, fill = reorder(地区, -Totalp))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = 人口表示),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "浜名区 各地区の人口（降順）", fill = "地区") +
  theme_void(base_size = 14) +
  theme(legend.position = "none")   # 凡例は不要なら非表示

print(p_pie)







