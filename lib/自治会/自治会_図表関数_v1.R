# =============================================================================
# 自治会_図表関数_v1.R
#   - CSV読み込み（無ければダミー）
#   - 同名自治会の表示名ユニーク化（市場（1）, 市場（2）...）
#   - bar/pie プロット関数
#   - 出典（既存システム準拠）生成
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(tibble)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

pick_size <- function(x, fallback) {
  if (is.null(x)) return(fallback)
  if (length(x) == 1 && isTRUE(is.na(x))) return(fallback)
  x
}

safe_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[\\\\/:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  trimws(x)
}

# ---- asof（出典日付）列のピック（最短で実CSV差異に追従） --------------------
# 仕様:
#   - 候補列を順に見て「NA除外後にユニーク値が1つ」の場合だけ採用
#   - ...
pick_asof_value_v1 <- function(
  df,
  candidates = c("asof", "asof_reiwa", "source_date_reiwa", "source_date", "source_date_iso")
) {
  for (col in candidates) {
    if (col %in% names(df)) {
      u <- unique(na.omit(df[[col]]))
      if (length(u) == 1) return(as.character(u))
    }
  }
  NA_character_
}

# ---- 令和日付（既存システム互換） ------------------------------------------
# 入力: "2025-04-01" -> "令和7年4月1日現在"
# ※和暦変換は「令和のみ」対応（運用上は十分）
to_reiwa_date <- function(iso_ymd) {
  if (is.null(iso_ymd) || is.na(iso_ymd) || !nzchar(iso_ymd)) return(NA_character_)
  x <- as.character(iso_ymd)
  # 既に「令和」や「現在」を含む場合はそのまま
  if (grepl("令和", x) || grepl("現在", x)) return(x)

  # YYYY-MM-DD / YYYY/MM/DD
  x2 <- gsub("/", "-", x)
  if (!grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", x2)) return(x)
  parts <- strsplit(x2, "-", fixed = TRUE)[[1]]
  y <- as.integer(parts[1]); m <- as.integer(parts[2]); d <- as.integer(parts[3])
  if (is.na(y) || is.na(m) || is.na(d)) return(x)

  # 令和は2019年開始（2019=令和1）
  reiwa <- y - 2018
  if (reiwa <= 0) {
    # 令和以前はそのまま
    return(sprintf("%d年%d月%d日現在", y, m, d))
  }
  sprintf("令和%d年%d月%d日現在", reiwa, m, d)
}

# ---- データ読み込み ----------------------------------------------------------
make_dummy_data <- function() {
  set.seed(1)
  tibble(
    region_key = "kita",
    district  = rep(c("三ヶ日地区", "細江地区"), each = 10),
    jichikai  = c(paste0("自治会", sprintf("%02d", 1:10)),
                  paste0("自治会", sprintf("%02d", 1:10))),
    households = c(sample(30:220, 10), sample(20:180, 10)),
    corporate  = sample(c(TRUE, FALSE), 20, replace = TRUE),
    asof = "2025-04-01"
  )
}

normalize_df <- function(df) {
  # ---- 実CSVの列名差異を最短で吸収（読み込み部だけ微調整） -----------------
  # 代表的な別名を持っていても動くようにする。
  # 例: area_key / area -> region_key, jichikai_name -> jichikai, hh -> households ...
  rename_if_present <- function(df, from, to) {
    if (from %in% names(df) && !(to %in% names(df))) {
      names(df)[names(df) == from] <- to
    }
    df
  }

  df <- rename_if_present(df, "area_key", "region_key")
  df <- rename_if_present(df, "area", "region_key")
  df <- rename_if_present(df, "region", "region_key")
  df <- rename_if_present(df, "district_name", "district")
  df <- rename_if_present(df, "district_jp", "district")
  df <- rename_if_present(df, "jichikai_name", "jichikai")
  df <- rename_if_present(df, "自治会", "jichikai")
  df <- rename_if_present(df, "世帯数", "households")
  df <- rename_if_present(df, "hh", "households")
  df <- rename_if_present(df, "household", "households")
  df <- rename_if_present(df, "法人格", "corporate")
  df <- rename_if_present(df, "as_of", "asof")
  df <- rename_if_present(df, "source_date", "asof")

  if (!("corporate" %in% names(df))) df$corporate <- TRUE

  df |>
    mutate(
      region_key = as.character(.data$region_key %||% NA_character_),
      district   = as.character(.data$district),
      jichikai   = as.character(.data$jichikai),
      households = suppressWarnings(as.numeric(.data$households)),
      corporate  = suppressWarnings(as.logical(.data$corporate)),
      asof       = as.character(.data$asof %||% NA_character_)
    ) |>
    filter(!is.na(.data$district), !is.na(.data$jichikai), !is.na(.data$households))
}

read_or_dummy <- function(csv_path = NULL, warn_on_dummy = TRUE) {
  used_dummy <- FALSE

  if (!is.null(csv_path) && file.exists(csv_path)) {
    df <- read_csv(csv_path, show_col_types = FALSE)
  } else {
    used_dummy <- TRUE
    df <- make_dummy_data()
    if (isTRUE(warn_on_dummy)) {
      warning("CSVが見つからないためダミーデータで描画しました。出力は実データではありません。", call. = FALSE)
    }
  }

  df <- normalize_df(df)
  attr(df, "used_dummy") <- used_dummy
  df
}

# ---- 同名自治会の表示名ユニーク化 -------------------------------------------
# 仕様: 同名が複数ある場合、全てに (1)(2)... を付ける
# 例: 市場, 市場 -> 市場（1）, 市場（2）
make_unique_labels_paren <- function(x) {
  x <- as.character(x)
  if (length(x) == 0) return(x)

  # 出現順に番号を振る
  idx <- ave(seq_along(x), x, FUN = seq_along)
  n_each <- ave(seq_along(x), x, FUN = length)

  ifelse(n_each > 1, paste0(x, "（", idx, "）"), x)
}

add_unique_jichikai <- function(df) {
  df |>
    group_by(.data$region_key, .data$district) |>
    mutate(jichikai_unique = make_unique_labels_paren(.data$jichikai)) |>
    ungroup()
}

decorate_non_corp <- function(name, corporate, non_corp_mark = "×", mark_non_corporate = TRUE) {
  name <- as.character(name)
  corporate <- as.logical(corporate)
  if (!isTRUE(mark_non_corporate)) return(name)
  ifelse(!is.na(corporate) & corporate == FALSE, paste0(non_corp_mark, name), name)
}

# ---- グリッド補助 ------------------------------------------------------------
make_minor_breaks_from_major <- function(x, div = 5) {
  div <- as.integer(div %||% 5)
  if (is.na(div) || div < 2) return(NULL)

  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(NULL)

  major <- scales::breaks_extended()(rng)
  major <- sort(unique(major))
  if (length(major) < 2) return(NULL)

  minors <- unlist(lapply(seq_len(length(major) - 1), function(i) {
    a <- major[i]; b <- major[i + 1]
    step <- (b - a) / div
    a + step * seq_len(div - 1)
  }))
  minors <- minors[minors > rng[1] & minors < rng[2]]
  sort(unique(minors))
}

# ---- theme -------------------------------------------------------------------
theme_common_v1 <- function(cfg) {
  size <- cfg$font$base_size
  fam  <- cfg$font$family %||% "sans"

  theme_bw(base_size = size, base_family = fam) +
    theme(
      text = element_text(family = fam),
      plot.title   = element_text(size = size + cfg$font$title_add, face = cfg$font$bold_face, hjust = 0),

      legend.position = "none",
      plot.margin  = do.call(margin, as.list((cfg$layout$plot_margin %||% cfg$layout$margin))),
      plot.caption = element_text(
        hjust = cfg$layout$caption_hjust,
        vjust = cfg$layout$caption_vjust %||% 0,
        size  = size * cfg$layout$caption_size_ratio,
        color = cfg$layout$caption_color,
        margin = do.call(margin, as.list(cfg$layout$caption_margin %||% c(0,0,0,0)))
      ),
      plot.caption.position = cfg$layout$caption_position %||% "plot",

      panel.background = element_rect(fill = cfg$bar$panel_bg %||% "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.border     = element_blank(),

      panel.grid.major.x = element_line(color = cfg$bar$grid_x_color %||% "grey80", linewidth = cfg$bar$grid_x_lwd %||% 0.5),
      panel.grid.major.y = element_line(color = cfg$bar$grid_y_color %||% "grey90", linewidth = cfg$bar$grid_y_lwd %||% 0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = if (isTRUE(cfg$bar$grid_x_minor_show %||% TRUE)) {
        element_line(
          color = cfg$bar$grid_x_minor_color %||% (cfg$bar$grid_x_color %||% "grey90"),
          linewidth = cfg$bar$grid_x_minor_lwd %||% ((cfg$bar$grid_x_lwd %||% 0.5) / 2)
        )
      } else {
        element_blank()
      }
    )
}

make_palette <- function(cols, n) {
  cols <- as.character(cols)
  if (length(cols) == 0) cols <- c("#005686")
  cols[(seq_len(n) - 1) %% length(cols) + 1]
}

# =============================================================================
# 出典（既存システム準拠）
# =============================================================================
make_source_caption <- function(region_jp, asof_reiwa) {
  # 既存: 浜松市自治連合会「<地域>地域の地区別単位自治会」（令和...）を基に作成
  # 本v1: region_jp が "北地域" 等なので「地域」を重複させない。
  paste0("出典：浜松市自治連合会「", region_jp, "の地区別単位自治会」（", asof_reiwa, "）を基に作成")
}

# =============================================================================
# bar: 横棒（降順）
# =============================================================================
plot_bar_horizontal_desc_v1 <- function(
  df, district_name,
  n_jichikai,
  cfg,
  non_corp_mark = "×",
  mark_non_corporate = TRUE,
  caption = NULL
) {
  dfq <- df |>
    filter(.data$district == district_name) |>
    arrange(desc(.data$households)) |>
    mutate(
      prop = .data$households / sum(.data$households),
      rank = dplyr::dense_rank(dplyr::desc(.data$households)),
      jichikai_show = decorate_non_corp(.data$jichikai_unique, .data$corporate, non_corp_mark, mark_non_corporate)
    )

  if (nrow(dfq) == 0) stop("district が存在しません: ", district_name)

  # 最高値
  maxv <- max(dfq$households, na.rm = TRUE)


  # 自治会名の左逃がし（0より少し左へ）
  # - name_pad_mult: 最大世帯数に対する比率（例 0.015 → maxv=2000 なら 30 左へ）
  name_pad_mult <- cfg$bar$name_pad_mult %||% 0
  name_pad <- if (is.finite(maxv)) maxv * name_pad_mult else 0
  if (!is.finite(name_pad) || is.na(name_pad)) name_pad <- 0
  name_x <- cfg$bar$name_x %||% if (name_pad > 0) -name_pad else 0
  name_hjust <- cfg$bar$name_hjust %||% if (name_pad > 0) 1.0 else 1.02

  # Y順序: 高いものが上に来るように levels を逆にする
  dfq <- dfq |>
    mutate(jichikai_f = factor(jichikai_show, levels = rev(unique(jichikai_show))))

  # ラベル位置
  rank_side <- cfg$bar$rank_side %||% "right"
  rank_x_right <- maxv * (1 + (cfg$bar$rank_push_ratio %||% 0.12))
  rank_x_left  <- -maxv * (cfg$bar$rank_left_ratio %||% 0.05)
  rank_x <- if (identical(rank_side, "left")) rank_x_left else rank_x_right

  value_x_fixed <- maxv * (cfg$bar$value_x_ratio %||% 1.02)
  dfq <- dfq |>
    mutate(value_x = if (identical(cfg$bar$value_x_mode, "fixed")) value_x_fixed else .data$households)

  # X範囲: ラベルが見切れないように少し広げる
  x_min <- if (identical(rank_side, "left")) min(0, rank_x_left * 1.15) else 0
  x_max <- max(maxv, rank_x_right, value_x_fixed) * 1.02

  # バー枠線
  bar_border_col <- if (is.null(cfg$bar$bar_border_color)) {
    NA
  } else {
    scales::alpha(cfg$bar$bar_border_color, cfg$bar$bar_border_alpha %||% 1)
  }

  # ---- ベース ---------------------------------------------------------------
  p <- ggplot(dfq, aes(x = households, y = jichikai_f))

  if (identical(cfg$bar$fill_mode, "single")) {
    p <- p + geom_col(width = cfg$bar$bar_width, fill = cfg$bar$bar_color, colour = bar_border_col, linewidth = cfg$bar$bar_border_size)
  } else if (identical(cfg$bar$fill_mode, "legacy")) {
    dfq <- dfq |>
      mutate(jichikai_color = factor(jichikai_show, levels = unique(jichikai_show)))
    p <- ggplot(dfq, aes(x = households, y = jichikai_f)) +
      geom_col(aes(fill = jichikai_color), width = cfg$bar$bar_width, colour = bar_border_col, linewidth = cfg$bar$bar_border_size) +
      guides(fill = "none")
  } else {
    dfq <- dfq |>
      mutate(jichikai_color = factor(as.character(jichikai_f), levels = levels(jichikai_f)))
    pal <- make_palette(cfg$bar$bar_palette, nlevels(dfq$jichikai_color))
    names(pal) <- levels(dfq$jichikai_color)
    p <- ggplot(dfq, aes(x = households, y = jichikai_f)) +
      geom_col(aes(fill = jichikai_color), width = cfg$bar$bar_width, colour = bar_border_col, linewidth = cfg$bar$bar_border_size) +
      scale_fill_manual(values = pal, guide = "none")
  }

  # ---- 文字サイズ -----------------------------------------------------------
  rank_size  <- pick_size(cfg$bar$rank_size, 2.5)
  value_size <- pick_size(cfg$bar$value_size, 3.0)
  name_size  <- pick_size(cfg$bar$name_size, cfg$font$base_size)

  p +
    # 順位
    geom_text(
      aes(x = rank_x, label = paste0(rank)),
      hjust = cfg$bar$rank_hjust %||% 0.5,
      fontface = cfg$font$bold_face,
      size = rank_size,
      family = cfg$font$family
    ) +
    # 世帯数 + 割合
    geom_text(
      aes(x = value_x, label = paste0(
        scales::comma(households), "世帯",
        "（", scales::percent(prop, accuracy = 0.01), "）"
      )),
      hjust = cfg$bar$value_hjust,
      fontface = cfg$font$bold_face,
      size = value_size,
      family = cfg$font$family
    ) +

    # 自治会名（左端を揃えるため axis ラベルではなくプロット内に描画）
    geom_text(
      aes(y = jichikai_f, label = jichikai_show),
      x = name_x,
      hjust = name_hjust,
      fontface = cfg$bar$y_axis_label_face,
      color = (cfg$bar$y_axis_text_color %||% cfg$bar$name_color %||% "black"),
      size = name_size,
      family = cfg$font$family
    ) +
    labs(
      title = paste0(district_name, "（計", n_jichikai, "自治会）：自治会別 世帯数（降順）"),
      x = "世帯数",
      y = NULL,
      caption = caption
    ) +
    scale_x_continuous(
      limits = c(x_min, x_max),
      oob = scales::oob_keep,
      expand = expansion(mult = cfg$bar$x_expand),
      minor_breaks = function(x) make_minor_breaks_from_major(x, div = cfg$bar$x_minor_div %||% 2)
    ) +
    scale_y_discrete(
      expand = expansion(mult = (cfg$bar$y_expand %||% c(0.05, 0.04)))
    ) +
    coord_cartesian(clip = "off") +
    theme_common_v1(cfg) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = if (isTRUE(cfg$bar$y_axis_ticks %||% FALSE)) {
        element_line(
          color = cfg$bar$y_axis_tick_color %||% "grey40",
          linewidth = cfg$bar$y_axis_tick_size %||% 0.3
        )
      } else {
        element_blank()
      },
      axis.ticks.x = if (isTRUE(cfg$bar$x_axis_ticks %||% TRUE)) {
        element_line(
          color = cfg$bar$x_axis_tick_color %||% "grey60",
          linewidth = cfg$bar$x_axis_tick_size %||% 0.3
        )
      } else {
        element_blank()
      },
      axis.line.x = if (isTRUE(cfg$bar$x_axis_line %||% FALSE)) {
        element_line(
          color = cfg$bar$x_axis_line_color %||% "grey60",
          linewidth = cfg$bar$x_axis_line_size %||% 0.4
        )
      } else {
        element_blank()
      },
      axis.text.x = element_text(
        color  = cfg$bar$x_axis_text_color %||% "grey40",
        size   = pick_size(cfg$bar$x_axis_text_size, cfg$font$base_size),
        family = cfg$font$family
      ),
      axis.title.x = element_text(
        color  = cfg$bar$x_axis_title_color %||% "grey40",
        size   = pick_size(cfg$bar$x_axis_title_size, cfg$font$base_size),
        family = cfg$font$family
      ),
      axis.ticks.length = grid::unit(cfg$bar$axis_tick_length_pt %||% 3.0, "pt")
    )}

# =============================================================================
# pie: 構成比（ラベルは降順上位Nのみ）
# =============================================================================
plot_pie_share_topN_v1 <- function(
  df, district_name,
  n_jichikai,
  cfg,
  non_corp_mark = "×",
  mark_non_corporate = TRUE,
  caption = NULL
) {
  # 極座標の都合で「小→大」に並べ、direction=-1 で時計回りに大→小表示になる
  dfq <- df |>
    filter(.data$district == district_name) |>
    arrange(.data$households) |>
    mutate(
      prop = .data$households / sum(.data$households),
      ymax = cumsum(.data$prop),
      ymin = lag(.data$ymax, default = 0),
      mid  = (.data$ymax + .data$ymin) / 2,
      jichikai_show = decorate_non_corp(.data$jichikai_unique, .data$corporate, non_corp_mark, mark_non_corporate)
    )

  if (nrow(dfq) == 0) stop("district が存在しません: ", district_name)

  top_n <- (cfg$pie$label_top_n %||% 5)
  top_names <- dfq |>
    dplyr::arrange(dplyr::desc(.data$households), .data$jichikai_show) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(.data$jichikai_show)

  dfq <- dfq |>
    mutate(
      jichikai_color = factor(
        jichikai_show,
        levels = if (isTRUE(cfg$pie$reverse_levels)) rev(unique(jichikai_show)) else unique(jichikai_show)
      ),
      label = ifelse(
        .data$jichikai_show %in% top_names,
        paste0(.data$jichikai_show, "
", scales::percent(.data$prop, accuracy = 0.01)),
        ""
      )
    )

  pal <- make_palette(cfg$pie$palette, nlevels(dfq$jichikai_color))
  names(pal) <- levels(dfq$jichikai_color)

  # 区切り線（自治会数レンジ別に切替）
  lwd <- cfg$pie$slice_border_lwd %||% 0.3
  if (!is.null(n_jichikai)) {
    if (n_jichikai >= 40 && n_jichikai <= 50) {
      lwd <- cfg$pie$slice_border_lwd_40_50 %||% lwd
    }
    if (n_jichikai > 50) {
      lwd <- cfg$pie$slice_border_lwd_50_plus %||% lwd
    }
  }
  border_arg <- if ("linewidth" %in% names(formals(ggplot2::geom_rect))) "linewidth" else "size"

  # ラベル位置（自治会数1〜10は任意で上書き）
  label_x_used <- cfg$pie$label_x %||% 1.15
  if (!is.null(n_jichikai) && n_jichikai <= 10) {
    label_x_used <- cfg$pie$label_x_01_10 %||% label_x_used
  }

  ggplot(dfq) +
    do.call(ggplot2::geom_rect, c(
      list(
        mapping = aes(
          ymax = ymax, ymin = ymin,
          xmax = 1, xmin = 0,
          fill = jichikai_color
        ),
        color = cfg$pie$slice_border_color %||% "white"
      ),
      setNames(list(lwd), border_arg)
    )) +
    geom_text(
      data = dfq |> filter(.data$label != ""),
      aes(x = label_x_used, y = mid, label = label),
      size = cfg$pie$label_size,
      lineheight = cfg$pie$label_lineheight,
      fontface = cfg$font$bold_face,
      family = cfg$font$family
    ) +
    coord_polar(theta = "y", direction = -1) +
    (if (identical(cfg$pie$fill_mode, "legacy")) scale_fill_discrete() else scale_fill_manual(values = pal, guide = "none")) +
    guides(fill = "none") +
    labs(
      title = paste0(district_name, "（計", n_jichikai, "自治会）：自治会別 世帯数構成比"),
      caption = caption
    ) +
    theme_void(base_size = cfg$font$base_size, base_family = cfg$font$family) +
    theme(
      plot.title = element_text(size = cfg$font$base_size + cfg$font$title_add, face = cfg$font$bold_face, hjust = 0),
      plot.margin = do.call(margin, as.list(cfg$pie$margin)),
      legend.position = "none"
    )
}
