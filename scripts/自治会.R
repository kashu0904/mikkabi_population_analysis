# =============================================================================
# ggplot2 テンプレ：
#  - 地区内の各自治会の世帯数（降順） 横棒グラフ（順位=左、数値=右）
#  - 円グラフ（比率表示）
#  - プロット表示確認
#  - SVG書き出し
# =============================================================================

# ---- packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(svglite)   # ggsave(device = svglite::svglite) でSVG出力
})

# ---- (任意) CUDっぽい色。必要なら差し替え -----------------------------------
cud_colors <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7",
                "#56B4E9", "#F0E442", "#000000")

# ---- theme ------------------------------------------------------------------
theme_common <- function(size = 11) {
  theme_bw(base_size = size) +
    theme(
      plot.title   = element_text(size = size + 4, face = "bold", hjust = 0),
      axis.text    = element_text(size = size),
      axis.title   = element_text(size = size),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.margin  = margin(15, 25, 15, 25)  # 左右に余白（順位・数値ラベル用）
    )
}

# =============================================================================
# 1) データ読み込み（なければダミー生成）
# =============================================================================

make_dummy_data <- function() {
  set.seed(1)
  tibble::tibble(
    district  = rep(c("三ヶ日町", "細江"), each = 10),
    jichikai  = c(paste0("自治会", sprintf("%02d", 1:10)),
                 paste0("自治会", sprintf("%02d", 1:10))),
    households = c(sample(30:220, 10), sample(20:180, 10))
  )
}

read_or_dummy <- function(csv_path = NULL) {
  if (!is.null(csv_path) && file.exists(csv_path)) {
    # 想定CSV列：district, jichikai, households
    df <- read_csv(csv_path, show_col_types = FALSE) |>
      rename(
        district   = district,
        jichikai   = jichikai,
        households = households
      )
  } else {
    message("CSVが見つからないのでダミーデータで描画します。")
    df <- make_dummy_data()
  }

  df |>
    mutate(
      district = as.character(district),
      jichikai = as.character(jichikai),
      households = as.numeric(households)
    ) |>
    filter(!is.na(households))
}

# =============================================================================
# 2) 横棒（降順、順位=左、数値=右）
# =============================================================================
plot_bar_horizontal_desc <- function(df, district_name, size = 11) {
  
  dfq2 <- df |>
    dplyr::filter(district == district_name) |>
    dplyr::arrange(dplyr::desc(households)) |>
    dplyr::mutate(
      prop = households / sum(households),
      rank = dplyr::row_number(),
      
      # 並び順を数値で固定
      jichikai_f = factor(jichikai, levels = jichikai),
      jichikai_f = forcats::fct_rev(jichikai_f),
      
      # 色順（円と合わせる：反転）
      jichikai_color = factor(jichikai, levels = jichikai)
    )
  
  if (nrow(dfq2) == 0) stop("district が存在しません: ", district_name)
  
  maxv <- max(dfq2$households, na.rm = TRUE)
  left_x <- -maxv * 0.05
  
  ggplot(dfq2, aes(x = households, y = jichikai_f, fill = jichikai_color)) +
    geom_col(width = 0.6) +
    
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
      title = paste0(district_name, "：自治会別 世帯数（降順）"),
      x = "世帯数",
      y = NULL
    ) +
    
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.25)),
      minor_breaks = waiver()   # ★ 細かい補助グリッドのみ増やす
    ) +
    coord_cartesian(clip = "off") +
    
    scale_fill_discrete(guide = "none") +
    
    theme_common(size = size) +
    theme(
      panel.grid.major.x = element_line(color = "white", linewidth = 0.6),
      #panel.grid.minor.x = element_line(color = "white", linewidth = 0.3),
      
      # ★ y軸（カテゴリ境界）を太く
      panel.grid.major.y = element_line(color = "white", linewidth = 0.9),
      panel.grid.minor.y = element_blank(),
      
      panel.background = element_rect(fill = "#E6E6E6", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.border     = element_blank()
    )
  
}


# =============================================================================
# 3) 円グラフ（比率表示、色順固定）
# =============================================================================
plot_pie_share <- function(df, district_name, size = 11) {
    
    dfq2 <- df |>
      dplyr::filter(district == district_name) |>
      dplyr::arrange(households) |>   # 小 → 大（polar -1 で時計回り降順）
      dplyr::mutate(
        prop = households / sum(households),
        ymax = cumsum(prop),
        ymin = dplyr::lag(ymax, default = 0),
        mid  = (ymax + ymin) / 2,
        
        # ★ 色順のみ反転（ここが変更点）
        jichikai_color = factor(jichikai, levels = rev(jichikai)),
        
        # 表示ラベル（名称＋割合）
        label = paste0(
          jichikai, "\n",
          scales::percent(prop, accuracy = 0.1)
        )
      )
    
    if (nrow(dfq2) == 0) stop("district が存在しません: ", district_name)
    
    ggplot(dfq2) +
      geom_rect(
        aes(
          ymax = ymax,
          ymin = ymin,
          xmax = 1,
          xmin = 0,
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
      
      # factor の levels 順（＝反転後）で配色
      scale_fill_discrete() +
      
      labs(title = paste0(district_name, "：自治会別 世帯数構成比")) +
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

# =============================================================================
# 実行例（ここだけ書き換える）
# =============================================================================

# csv_path <- "C:/path/to/jichikai_households.csv"  # 無ければNULLでOK
csv_path <- NULL

district_name <- "三ヶ日町"   # ここを対象地区にする

df <- read_or_dummy(csv_path)

p1 <- plot_bar_horizontal_desc(df, district_name)
p2 <- plot_pie_share(df, district_name)

# ---- 表示確認 ---------------------------------------------------------------
print(p1)
print(p2)

# ---- SVG書き出し ------------------------------------------------------------
save_svg_pair(p1, p2, out_dir = "out_svg", prefix = paste0("jichikai_", district_name), w = 11, h = 6)
