# =============================================================================
# plot_自治会_style.R
#  - デザイン（theme/余白/グリッド等）
#  - 読み込み（CSV or ダミー）＋ダミー警告
#  - bar/pie プロット関数（district 切替／corporate==FALSE の印付け対応）
#  - SVG保存
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(svglite)
  library(tibble)
})

# ---- ユーティリティ ----------------------------------------------------------

# Windowsでも壊れにくいファイル名へ
safe_filename <- function(x) {
  x <- as.character(x)
  x <- gsub('[\\\\/:*?"<>|]+', "_", x)  # Windows 禁止文字
  x <- gsub("\\s+", "_", x)            # 空白 -> _
  x <- gsub("_+", "_", x)              # _ 連続を潰す
  x
}

# corporate==FALSE のときだけ自治会名に印を付ける

format_plot_year_label <- function(plot_year){
  if (is.null(plot_year)) return(NULL)
  y <- as.character(plot_year)
  
  # すでに「年」「年度」などが入っている場合はそのまま
  if (grepl("年|年度", y)) return(y)
  
  # 4桁の西暦はそのまま返す（2025 → 2025）
  if (grepl("^[0-9]{4}$", y)) return(y)
  
  y
}

decorate_jichikai <- function(jichikai, corporate, non_corp_mark = "×", mark_non_corporate = TRUE) {
  jichikai <- as.character(jichikai)
  corporate <- as.logical(corporate)
  if (!isTRUE(mark_non_corporate)) return(jichikai)
  ifelse(!is.na(corporate) & corporate == FALSE, paste0(non_corp_mark, jichikai), jichikai)
}

# ---- theme ------------------------------------------------------------------
theme_common <- function(size = 11) {
  theme_bw(base_size = size) +
    theme(
      plot.title   = element_text(size = size + 4, face = "bold", hjust = 0),
      axis.text    = element_text(size = size),
      axis.title   = element_text(size = size),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.margin  = margin(15, 25, 15, 25),
      plot.caption = element_text(hjust = 1, size = size * 0.85, color = "grey30")
    )
}

# =============================================================================
# 1) データ読み込み（なければダミー生成） + ダミー警告
# =============================================================================
make_dummy_data <- function() {
  set.seed(1)
  tibble(
    district  = rep(c("三ヶ日町", "細江"), each = 10),
    jichikai  = c(paste0("自治会", sprintf("%02d", 1:10)),
                  paste0("自治会", sprintf("%02d", 1:10))),
    households = c(sample(30:220, 10), sample(20:180, 10)),
    corporate  = sample(c(TRUE, FALSE), 20, replace = TRUE)
  )
}

read_or_dummy <- function(csv_path = NULL, warn_on_dummy = TRUE) {
  used_dummy <- FALSE

  if (!is.null(csv_path) && file.exists(csv_path)) {
    df <- read_csv(csv_path, show_col_types = FALSE)
  } else {
    used_dummy <- TRUE
    df <- make_dummy_data()
    if (isTRUE(warn_on_dummy)) {
      warning(
        "CSVが見つからないためダミーデータで描画しました。出力は実データではありません。",
        call. = FALSE
      )
    }
  }

  # 必須列が無いときの最低限の救済（corporate は無ければ TRUE 扱い）
  if (!("corporate" %in% names(df))) df$corporate <- TRUE

  df <- df |>
    mutate(
      district   = as.character(district),
      jichikai   = as.character(jichikai),
      households = suppressWarnings(as.numeric(households)),
      corporate  = suppressWarnings(as.logical(corporate))
    ) |>
    filter(!is.na(district), !is.na(jichikai), !is.na(households))

  attr(df, "used_dummy") <- used_dummy
  df
}

# =============================================================================
# 2) 横棒（降順、順位=左、数値=右）
# =============================================================================
plot_bar_horizontal_desc <- function(
  df, district_name,
  size = 11,
  bar_color = "#005686",
  non_corp_mark = "×",
  mark_non_corporate = TRUE,
  plot_year = NULL,
  caption = NULL
) {

  dfq2 <- df |>
    filter(district == district_name) |>
    arrange(desc(households)) |>
    mutate(
      prop = households / sum(households),
      rank = row_number(),
      jichikai_show = decorate_jichikai(jichikai, corporate, non_corp_mark, mark_non_corporate),
      jichikai_color = factor(jichikai_show, levels = unique(jichikai_show)),
      jichikai_f = factor(jichikai_show, levels = unique(jichikai_show)),
      jichikai_f = forcats::fct_rev(jichikai_f)
    )

  if (nrow(dfq2) == 0) stop("district が存在しません: ", district_name)

  maxv <- max(dfq2$households, na.rm = TRUE)
  left_x <- -maxv * 0.05

  ggplot(dfq2, aes(x = households, y = jichikai_f)) +
    geom_col(aes(fill = jichikai_color), width = 0.6) +
    guides(fill = "none") +

    # 順位（左）
    geom_text(
      aes(x = left_x, label = paste0(rank, "位")),
      hjust = 1, fontface = "bold", size = 3.8
    ) +

    # 世帯数＋割合（右）
    geom_text(
      aes(label = paste0(
        scales::comma(households), "世帯",
        "（", scales::percent(prop, accuracy = 0.1), "）"
      )),
      hjust = -0.15, fontface = "bold", size = 3.8
    ) +

    labs(
      title = paste0(district_name, if (!is.null(plot_year)) paste0(" ", format_plot_year_label(plot_year)) else "", "：自治会別 世帯数（降順）"),
      x = "世帯数",
      y = NULL,
      caption = caption
    ) +

    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.25)),
      minor_breaks = waiver()
    ) +
    coord_cartesian(clip = "off") +

    theme_common(size = size) +
    theme(
      panel.grid.major.x = element_line(color = "white", linewidth = 0.6),
      panel.grid.major.y = element_line(color = "white", linewidth = 0.9),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "#E6E6E6", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.border     = element_blank()
    )
}

# =============================================================================
# 3) 円グラフ（比率表示、色順反転、名称ラベル）
# =============================================================================
plot_pie_share <- function(
  df, district_name,
  size = 11,
  non_corp_mark = "×",
  mark_non_corporate = TRUE,
  plot_year = NULL,
  caption = NULL
) {

  dfq2 <- df |>
    filter(district == district_name) |>
    arrange(households) |>
    mutate(
      prop = households / sum(households),
      ymax = cumsum(prop),
      ymin = lag(ymax, default = 0),
      mid  = (ymax + ymin) / 2,

      jichikai_show = decorate_jichikai(jichikai, corporate, non_corp_mark, mark_non_corporate),

      # 色順だけ反転（表示名ベースで反転）
      jichikai_color = factor(jichikai_show, levels = rev(unique(jichikai_show))),

      label = paste0(
        jichikai_show, "\n",
        scales::percent(prop, accuracy = 0.1)
      )
    )

  if (nrow(dfq2) == 0) stop("district が存在しません: ", district_name)

  ggplot(dfq2) +
    geom_rect(
      aes(
        ymax = ymax, ymin = ymin,
        xmax = 1, xmin = 0,
        fill = jichikai_color
      ),
      color = "white",
      linewidth = 0.5
    ) +
    geom_text(
      aes(x = 1.15, y = mid, label = label),
      size = 3.6,
      lineheight = 0.95,
      fontface = "bold"
    ) +
    coord_polar(theta = "y", direction = -1) +
    scale_fill_discrete() +
    labs(title = paste0(district_name, if (!is.null(plot_year)) paste0(" ", format_plot_year_label(plot_year)) else "", "：自治会別 世帯数構成比"), caption = caption) +
    theme_void(base_size = size) +
    theme(
      plot.title = element_text(size = size + 4, face = "bold", hjust = 0),
      plot.margin = margin(15, 40, 15, 25),
      legend.position = "none"
    )
}

# =============================================================================
# 4) SVG出力（2枚別ファイル）
# =============================================================================
save_svg_pair <- function(p_bar, p_pie, out_dir = "out_svg", prefix = "plot", w = 10, h = 6) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  bar_path <- file.path(out_dir, paste0(prefix, "_bar.svg"))
  pie_path <- file.path(out_dir, paste0(prefix, "_pie.svg"))

  ggsave(bar_path, plot = p_bar, width = w, height = h, device = svglite::svglite)
  ggsave(pie_path, plot = p_pie, width = w, height = h, device = svglite::svglite)

  message("SVG saved:\n- ", bar_path, "\n- ", pie_path)
  invisible(list(bar = bar_path, pie = pie_path))
}