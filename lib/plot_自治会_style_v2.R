
# ---- helper: size fallback ----------------------------------------------------
pick_size <- function(x, fallback) {
  if (is.null(x)) return(fallback)
  if (length(x) == 1 && isTRUE(is.na(x))) return(fallback)
  x
}


# ---- grid helper: major breaks -> minor breaks --------------------------------
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

# =============================================================================
# plot_自治会_style_v2.R
#   - データ読み込み（CSV or ダミー）＋ダミー警告
#   - bar/pie プロット関数（district 切替／corporate==FALSE の印付け対応）
#   - "デザイン設定を1か所に集約"（plot_自治会_design_config.R を読む）
#   - SVG/PDF/EPS/PNG など複数形式の書き出し関数（研究用）
#
# 【ポイント】
#   - 旧: bar_color 引数があっても、実際は geom_col(aes(fill=...)) で離散塗りつぶしになっており
#        bar_color が効いていませんでした（見た目が固定される原因）。
#   - 新: cfg$bar$fill_mode で "単色" と "パレット" を切り替え可能。
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(tibble)
})

# ---- 設定の取得 --------------------------------------------------------------
# config を source() していれば JICHAIKAI_PLOT_CFG が使われる。
# 無ければここでデフォルトを作る（最低限動くように）
get_plot_cfg <- function() {
  if (exists("JICHAIKAI_PLOT_CFG", mode = "list")) return(JICHAIKAI_PLOT_CFG)

  list(
    font = list(family = "sans", fallback_family = "sans", base_size = 11, title_add = 4, bold_face = "bold"),
    layout = list(margin = c(15, 25, 15, 25), caption_hjust = 1, caption_size_ratio = 0.85, caption_color = "grey30"),
    bar = list(fill_mode="single", bar_color="#005686", bar_palette=c("#005686"), bar_width=0.6, rank_left_ratio=0.05,
               value_hjust=-0.15, label_size=3.8, x_expand=c(0.05,0.25),
               panel_bg="#E6E6E6", grid_x_color="white", grid_x_lwd=0.6, grid_y_color="white", grid_y_lwd=0.9),
    pie = list(palette=c("#005686"), reverse_levels=TRUE, label_x=1.15, label_size=3.6, label_lineheight=0.95,
               slice_border_color="white", slice_border_lwd=0.5, margin=c(15,40,15,25)),
    export = list(w=11,h=6,png_dpi=300,ragg_scaling=1,try_embed_fonts=FALSE)
  )
}

# ---- 小物 --------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[\\\\/:*?\"<>|]", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  trimws(x)
}

format_plot_year_label <- function(plot_year){
  if (is.null(plot_year)) return(NULL)
  y <- as.character(plot_year)
  if (grepl("年|年度", y)) return(y)
  if (grepl("^[0-9]{4}$", y)) return(y)
  y
}

decorate_jichikai <- function(jichikai, corporate, non_corp_mark = "×", mark_non_corporate = TRUE) {
  jichikai <- as.character(jichikai)
  corporate <- as.logical(corporate)
  if (!isTRUE(mark_non_corporate)) return(jichikai)
  ifelse(!is.na(corporate) & corporate == FALSE, paste0(non_corp_mark, jichikai), jichikai)
}

# ---- theme -------------------------------------------------------------------
# ここは「PDF/SVGのフォント崩れ」に直結するので必ず family を入れる
theme_common <- function(cfg) {
  size <- cfg$font$base_size
  fam  <- cfg$font$family %||% "sans"

  # Grid: prefer the newer knobs (grid_x_color/grid_x_lwd, grid_y_color/grid_y_lwd)
  gx_col <- cfg$bar$grid_x_color %||% cfg$bar$grid_major_color %||% "grey80"
  gx_lwd <- cfg$bar$grid_x_lwd   %||% cfg$bar$grid_major_size  %||% 0.5
  gy_col <- cfg$bar$grid_y_color %||% "grey90"
  gy_lwd <- cfg$bar$grid_y_lwd   %||% 0.5

  theme_bw(base_size = size, base_family = fam) +
    theme(
      text = element_text(family = fam),
      plot.title   = element_text(size = size + cfg$font$title_add, face = cfg$font$bold_face, hjust = 0),

      axis.text.x  = element_text(
        size  = pick_size(cfg$bar$x_axis_text_size, size),
        color = cfg$bar$x_axis_text_color %||% "grey20"
      ),
      axis.title.x = element_text(
        size  = pick_size(cfg$bar$x_axis_title_size, size),
        color = cfg$bar$x_axis_title_color %||% "grey20"
      ),
      axis.text    = element_text(size = size),

      legend.title = element_blank(),
      legend.position = "bottom",
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

      # 背景グリッド
      panel.grid.major.x = element_line(color = gx_col, linewidth = gx_lwd),
      panel.grid.major.y = element_line(color = gy_col, linewidth = gy_lwd),
      panel.grid.minor.y = element_blank()
    )
}

# ---- パレット -----------------------------------------------------------------
make_palette <- function(cols, n) {
  cols <- as.character(cols)
  if (length(cols) == 0) cols <- c("#005686")
  cols[ (seq_len(n) - 1) %% length(cols) + 1 ]
}

# =============================================================================
# 1) データ読み込み（なければダミー生成） + ダミー警告
# =============================================================================
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
  if (!("corporate" %in% names(df))) df$corporate <- TRUE

  df |>
    mutate(
      district   = as.character(district),
      jichikai   = as.character(jichikai),
      households = suppressWarnings(as.numeric(households)),
      corporate  = suppressWarnings(as.logical(corporate))
    ) |>
    filter(!is.na(district), !is.na(jichikai), !is.na(households))
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

# =============================================================================
# 2) 横棒（降順、順位=左、数値=右）
# =============================================================================
plot_bar_horizontal_desc <- function(
  df, district_name,
  size = NULL,
  bar_color = NULL,
  non_corp_mark = "×",
  mark_non_corporate = TRUE,
  plot_year = NULL,
  caption = NULL,
  cfg = get_plot_cfg()
) {
  if (!is.null(size)) cfg$font$base_size <- size
  if (!is.null(bar_color)) cfg$bar$bar_color <- bar_color

  dfq2 <- df |>
    filter(.data$district == district_name) |>
    arrange(desc(.data$households)) |>
    mutate(
      prop = .data$households / sum(.data$households),
      rank = dplyr::dense_rank(dplyr::desc(.data$households)),
      jichikai_show = decorate_jichikai(.data$jichikai, .data$corporate, non_corp_mark, mark_non_corporate),
      jichikai_f = forcats::fct_rev(factor(jichikai_show, levels = unique(jichikai_show)))
    )

  if (nrow(dfq2) == 0) stop("district が存在しません: ", district_name)

  maxv <- max(dfq2$households, na.rm = TRUE)
  rank_x <- maxv * (1 + (cfg$bar$rank_push_ratio %||% 0.08))

  # 値ラベル（世帯・割合）の列位置（固定列 or 棒の終端）
  value_x_fixed <- maxv * (cfg$bar$value_x_ratio %||% 1.02)
  dfq2 <- dfq2 |>
    mutate(value_x = if (identical(cfg$bar$value_x_mode, "bar_end")) households else value_x_fixed)
  left_x  <- -maxv * cfg$bar$rank_left_ratio

  p <- ggplot(dfq2, aes(x = households, y = jichikai_f))


  # ---- バー枠線（輪郭） ----------------------------------------------------
  bar_border_col <- if (is.null(cfg$bar$bar_border_color)) {
    NA
  } else {
    scales::alpha(cfg$bar$bar_border_color, cfg$bar$bar_border_alpha %||% 1)
  }

  # ---- バー色（研究の主戦場） ------------------------------------------------
  if (identical(cfg$bar$fill_mode, "single")) {
    # 単色（バーはすべて同じ色）
    p <- p + geom_col(width = cfg$bar$bar_width, fill = cfg$bar$bar_color, colour = bar_border_col, linewidth = cfg$bar$bar_border_size)

  } else if (identical(cfg$bar$fill_mode, "legacy")) {
    # ★旧方式：ggplot2 既定の離散配色を使う（昔の挙動に戻す）
    # - 色は ggplot2 側が自動決定（環境に依存しにくい）
    # - bar_color / bar_palette は無視される
    dfq2 <- dfq2 |>
      mutate(jichikai_color = factor(jichikai_show, levels = unique(jichikai_show)))

    p <- ggplot(dfq2, aes(x = households, y = jichikai_f)) +
      geom_col(aes(fill = jichikai_color), width = cfg$bar$bar_width, colour = bar_border_col, linewidth = cfg$bar$bar_border_size) +
      guides(fill = "none")

  } else {
    # パレット（自治会ごとに色を振る：研究用）
    dfq2 <- dfq2 |>
      mutate(jichikai_color = factor(as.character(jichikai_f), levels = levels(jichikai_f)))

    pal <- make_palette(cfg$bar$bar_palette, nlevels(dfq2$jichikai_color))
    names(pal) <- levels(dfq2$jichikai_color)

    p <- ggplot(dfq2, aes(x = households, y = jichikai_f)) +
      geom_col(aes(fill = jichikai_color), width = cfg$bar$bar_width, colour = bar_border_col, linewidth = cfg$bar$bar_border_size) +
      scale_fill_manual(values = pal, guide = "none")
  }

p +
    # 順位（左）
    geom_text(
      aes(x = rank_x, label = paste0(rank, "")),
      hjust = (cfg$bar$rank_hjust %||% 1),
      fontface = cfg$font$bold_face, size = pick_size(cfg$bar$value_size, cfg$bar$label_size),
      family = cfg$font$family
    ) +
    # 世帯数＋割合（右）
    geom_text(
      aes(x = value_x, label = paste0(
        scales::comma(households), "世帯",
        "（", scales::percent(prop, accuracy = 0.1), "）"
      )),
      hjust = cfg$bar$value_hjust, fontface = cfg$font$bold_face, size = pick_size(cfg$bar$rank_size, cfg$bar$label_size),
      family = cfg$font$family
    ) +
    labs(
      title = paste0(
        district_name,
        if (!is.null(plot_year)) paste0(" ", format_plot_year_label(plot_year)) else "",
        "：自治会別 世帯数（降順）"
      ),
      x = "世帯数",
      y = NULL,
      caption = caption
    ) +
    scale_x_continuous(
      limits = c(0, maxv),
      oob = scales::oob_keep,
      expand = expansion(mult = cfg$bar$x_expand),
      minor_breaks = function(x) make_minor_breaks_from_major(x, div = cfg$bar$x_minor_div)
    ) +
    coord_cartesian(clip = "off") +
    theme_common(cfg) +
    theme(
      panel.grid.major.x = element_line(color = cfg$bar$grid_x_color, linewidth = cfg$bar$grid_x_lwd),
# ---- X軸（世帯数側）制御 ----
axis.line.x = if (isTRUE(cfg$bar$x_axis_line)) element_line(
  linewidth = cfg$bar$x_axis_line_size,
  color = cfg$bar$x_axis_line_color
) else element_blank(),
axis.ticks.x = if (isTRUE(cfg$bar$x_axis_ticks)) element_line(
  linewidth = cfg$bar$x_axis_tick_size,
  color = cfg$bar$x_axis_tick_color
) else element_blank(),
axis.ticks.length.x = unit(cfg$bar$x_axis_tick_length_pt, "pt"),

# ---- Y軸（自治会名側）制御 ----
axis.text.y = element_text(
  face  = cfg$bar$y_axis_label_face,
  color = (cfg$bar$y_axis_text_color %||% cfg$bar$name_color %||% "black"),
  size  = pick_size(cfg$bar$y_axis_text_size, pick_size(cfg$bar$name_size, cfg$bar$label_size))
),
axis.line.y = if (isTRUE(cfg$bar$y_axis_line)) element_line(
  linewidth = cfg$bar$y_axis_line_size,
  color = cfg$bar$y_axis_line_color
) else element_blank(),
axis.ticks.y = if (isTRUE(cfg$bar$y_axis_ticks)) element_line(
  linewidth = cfg$bar$y_axis_tick_size,
  color = cfg$bar$y_axis_tick_color
) else element_blank(),
axis.ticks.length.y = unit(cfg$bar$y_axis_tick_length_pt, "pt"),

# ---- 補助グリッド（X）----
panel.grid.minor.x = element_line(
  color = (cfg$bar$grid_minor_color %||% (cfg$bar$grid_x_color %||% "grey90")),
  linewidth = (cfg$bar$grid_minor_size %||% 0.25)
),
      panel.grid.major.y = element_line(color = cfg$bar$grid_y_color, linewidth = cfg$bar$grid_y_lwd),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = cfg$bar$panel_bg, color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.border     = element_blank()
    )
}

# =============================================================================
# 3) 円グラフ（比率表示、名称ラベル）
# =============================================================================
plot_pie_share <- function(
  df, district_name,
  size = NULL,
  non_corp_mark = "×",
  mark_non_corporate = TRUE,
  plot_year = NULL,
  caption = NULL,
  cfg = get_plot_cfg()
) {
  if (!is.null(size)) cfg$font$base_size <- size

  dfq2 <- df |>
    filter(.data$district == district_name) |>
    arrange(.data$households) |>
    mutate(
      prop = .data$households / sum(.data$households),
      ymax = cumsum(.data$prop),
      ymin = lag(.data$ymax, default = 0),
      mid  = (.data$ymax + .data$ymin) / 2,
      jichikai_show = decorate_jichikai(.data$jichikai, .data$corporate, non_corp_mark, mark_non_corporate),
      jichikai_color = factor(
        jichikai_show,
        levels = if (isTRUE(cfg$pie$reverse_levels)) rev(unique(jichikai_show)) else unique(jichikai_show)
      ),
      label = paste0(jichikai_show, "\n", scales::percent(.data$prop, accuracy = 0.1))
    )

  if (nrow(dfq2) == 0) stop("district が存在しません: ", district_name)

  pal <- make_palette(cfg$pie$palette, nlevels(dfq2$jichikai_color))
  names(pal) <- levels(dfq2$jichikai_color)

  ggplot(dfq2) +
    geom_rect(
      aes(
        ymax = ymax, ymin = ymin,
        xmax = 1, xmin = 0,
        fill = jichikai_color
      ),
      color = cfg$pie$slice_border_color,
      linewidth = cfg$pie$slice_border_lwd
    ) +
    geom_text(
      aes(x = cfg$pie$label_x, y = mid, label = label),
      size = cfg$pie$label_size,
      lineheight = cfg$pie$label_lineheight,
      fontface = cfg$font$bold_face,
      family = cfg$font$family
    ) +
    coord_polar(theta = "y", direction = -1) +
    (if (identical(cfg$pie$fill_mode, "legacy")) scale_fill_discrete() else scale_fill_manual(values = pal, guide = "none")) +
    (if (identical(cfg$pie$fill_mode, "legacy")) guides(fill = "none") else NULL) +
    labs(
      title = paste0(
        district_name,
        if (!is.null(plot_year)) paste0(" ", format_plot_year_label(plot_year)) else "",
        "：自治会別 世帯数構成比"
      ),
      caption = caption
    ) +
    theme_void(base_size = cfg$font$base_size, base_family = cfg$font$family) +
    theme(
      plot.title = element_text(size = cfg$font$base_size + cfg$font$title_add, face = cfg$font$bold_face, hjust = 0),
      plot.margin = do.call(margin, as.list(cfg$pie$margin)),
      legend.position = "none"
    )
}

# =============================================================================
# 4) 形式別書き出し（研究用）
# =============================================================================
# - svg        : svglite（テキストは基本 "テキスト" のまま）
# - pdf_cairo  : cairo_pdf（フォント埋め込みは embedFonts() で別途）
# - eps_cairo  : cairo_ps（EPSは環境依存が強いので研究対象として）
# - png_ragg   : ragg::agg_png（ラスタの比較用）
#
# showtext を使った "アウトライン化" 出力も追加可能だが、
#   文字が編集できなくなるので「目的に応じて」使うこと。
maybe_embed_fonts <- function(path, outfile = NULL) {
  # embedFonts は Ghostscript を呼ぶ（gswin64c 等が必要）
  # 失敗しても止めない（研究用にログを残す）
  outfile <- outfile %||% path
  ok <- TRUE
  tryCatch({
    grDevices::embedFonts(file = path, outfile = outfile)
  }, error = function(e) {
    ok <<- FALSE
    message("[WARN] embedFonts failed: ", conditionMessage(e))
  })
  ok
}


save_plot_with_showtext <- function(plot, path, w, h, device = c("pdf", "svg"), cfg = get_plot_cfg()) {
  device <- match.arg(device)
  if (!requireNamespace("showtext", quietly = TRUE)) stop("showtext が必要です。install.packages('showtext')")
  # svg はR標準の svg() デバイスでアウトライン化しやすい（テキスト情報は失われる）
  if (device == "pdf") {
    grDevices::cairo_pdf(path, width = w, height = h, family = cfg$font$family)
  } else {
    grDevices::svg(path, width = w, height = h)
  }
  showtext::showtext_begin()
  print(plot)
  showtext::showtext_end()
  grDevices::dev.off()
}

save_pair_multi <- function(
  p_bar, p_pie,
  out_dir,
  prefix,
  w = NULL, h = NULL,
  formats = c("svg", "pdf_cairo", "eps_cairo", "png_ragg"),
  cfg = get_plot_cfg()
) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  w <- w %||% cfg$export$w
  h <- h %||% cfg$export$h

  out <- list()

  # --- SVG -------------------------------------------------------------------
  if ("svg" %in% formats) {
    if (!requireNamespace("svglite", quietly = TRUE)) stop("svglite が必要です。install.packages('svglite')")
    bar_path <- file.path(out_dir, paste0(prefix, "_bar.svg"))
    pie_path <- file.path(out_dir, paste0(prefix, "_pie.svg"))
    ggsave(bar_path, plot = p_bar, width = w, height = h, device = svglite::svglite)
    ggsave(pie_path, plot = p_pie, width = w, height = h, device = svglite::svglite)
    out$svg <- list(bar = bar_path, pie = pie_path)
  }

  # --- SVG（showtextアウトライン） -------------------------------------------
  if ("svg_showtext" %in% formats) {
    bar_path <- file.path(out_dir, paste0(prefix, "_bar_outline.svg"))
    pie_path <- file.path(out_dir, paste0(prefix, "_pie_outline.svg"))
    save_plot_with_showtext(p_bar, bar_path, w, h, device = "svg", cfg = cfg)
    save_plot_with_showtext(p_pie, pie_path, w, h, device = "svg", cfg = cfg)
    out$svg_showtext <- list(bar = bar_path, pie = pie_path)
  }

  # --- PDF（cairo） -----------------------------------------------------------
  if ("pdf_cairo" %in% formats) {
    bar_path <- file.path(out_dir, paste0(prefix, "_bar.pdf"))
    pie_path <- file.path(out_dir, paste0(prefix, "_pie.pdf"))

    ggsave(bar_path, plot = p_bar, width = w, height = h, device = grDevices::cairo_pdf)
    ggsave(pie_path, plot = p_pie, width = w, height = h, device = grDevices::cairo_pdf)

    # 埋め込みテスト
    if (isTRUE(cfg$export$try_embed_fonts)) {
      maybe_embed_fonts(bar_path)
      maybe_embed_fonts(pie_path)
    }
    out$pdf_cairo <- list(bar = bar_path, pie = pie_path)
  }

  # --- PDF（showtextアウトライン） -------------------------------------------
  if ("pdf_showtext" %in% formats) {
    bar_path <- file.path(out_dir, paste0(prefix, "_bar_outline.pdf"))
    pie_path <- file.path(out_dir, paste0(prefix, "_pie_outline.pdf"))
    save_plot_with_showtext(p_bar, bar_path, w, h, device = "pdf", cfg = cfg)
    save_plot_with_showtext(p_pie, pie_path, w, h, device = "pdf", cfg = cfg)
    out$pdf_showtext <- list(bar = bar_path, pie = pie_path)
  }

  # --- EPS（cairo_ps） --------------------------------------------------------
  if ("eps_cairo" %in% formats) {
    bar_path <- file.path(out_dir, paste0(prefix, "_bar.eps"))
    pie_path <- file.path(out_dir, paste0(prefix, "_pie.eps"))

    ggsave(bar_path, plot = p_bar, width = w, height = h, device = grDevices::cairo_ps)
    ggsave(pie_path, plot = p_pie, width = w, height = h, device = grDevices::cairo_ps)

    if (isTRUE(cfg$export$try_embed_fonts)) {
      maybe_embed_fonts(bar_path)
      maybe_embed_fonts(pie_path)
    }
    out$eps_cairo <- list(bar = bar_path, pie = pie_path)
  }

  # --- PNG（ragg） ------------------------------------------------------------
  if ("png_ragg" %in% formats) {
    if (!requireNamespace("ragg", quietly = TRUE)) stop("ragg が必要です。install.packages('ragg')")
    bar_path <- file.path(out_dir, paste0(prefix, "_bar.png"))
    pie_path <- file.path(out_dir, paste0(prefix, "_pie.png"))

    ggsave(bar_path, plot = p_bar, width = w, height = h,
           device = ragg::agg_png, res = cfg$export$png_dpi, scaling = cfg$export$ragg_scaling)
    ggsave(pie_path, plot = p_pie, width = w, height = h,
           device = ragg::agg_png, res = cfg$export$png_dpi, scaling = cfg$export$ragg_scaling)
    out$png_ragg <- list(bar = bar_path, pie = pie_path)
  }

  invisible(out)
}
# NOTE: y_expand hook missing: please add scale_y_* with expand.
