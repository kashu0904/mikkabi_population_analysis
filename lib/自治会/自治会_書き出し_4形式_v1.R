# =============================================================================
# 自治会_書き出し_4形式_v1.R
#   - project_root 検出（getwd依存事故を軽減）
#   - 4形式（PDF/PNG/SVG/EPS）書き出し
#   - bar/pie のサイズ分離
#   - embedFonts(PDF/EPS) を必ず試行（失敗しても続行）
#   - ログ（失敗内容・出力一覧）
# =============================================================================

`%||%` <- function(a,b) if (!is.null(a)) a else b

# ---- project_root 検出 -------------------------------------------------------
find_project_root_v1 <- function(start = getwd(), max_up = 10) {
  start <- normalizePath(start, winslash = "/", mustWork = TRUE)
  cur <- start
  for (i in 0:max_up) {
    if (dir.exists(file.path(cur, "scripts")) && dir.exists(file.path(cur, "lib"))) {
      return(cur)
    }
    parent <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = TRUE)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("project_root を検出できません（scripts/ と lib/ が見つかりません）。setwd(project_root) してから再実行してください。", call. = FALSE)
}

make_run_id_v1 <- function(prefix = "run") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

# ---- Ghostscript 検出（ログ用） ---------------------------------------------
detect_gs_v1 <- function() {
  c(
    R_GSCMD = Sys.getenv("R_GSCMD"),
    gswin64c = Sys.which("gswin64c"),
    gswin32c = Sys.which("gswin32c"),
    gs = Sys.which("gs")
  )
}

# ---- ログ --------------------------------------------------------------------
init_logger_v1 <- function(run_root) {
  log_dir <- file.path(run_root, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    run_root = run_root,
    log_dir = log_dir,
    export_csv = file.path(log_dir, "export_manifest.csv"),
    embed_csv  = file.path(log_dir, "embedFonts_log.csv"),
    unknown_region_txt = file.path(log_dir, "unknown_region_keys.txt"),
    warnings_txt = file.path(log_dir, "warnings.txt"),
    gs_info_txt = file.path(log_dir, "ghostscript_detect.txt")
  )
}

append_csv_row_v1 <- function(path, header, row) {
  # writeLines は append 引数を持たないため cat() で追記する
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    cat(paste(header, collapse = ","), "\n", file = path, sep = "", append = FALSE)
  }

  # CSVとして単純にクォートする（カンマ・改行対応）
  esc <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub('"', '""', x, fixed = TRUE)
    paste0('"', x, '"')
  }

  line <- paste(esc(row), collapse = ",")
  cat(line, "\n", file = path, sep = "", append = TRUE)
}


append_text_v1 <- function(path, line) {
  cat(line, "\n", file = path, append = TRUE, sep = "")
}

# ---- embedFonts（失敗しても止めない＋ログ） ----------------------------------
try_embed_fonts_v1 <- function(path, log, kind = "pdf") {
  # embedFonts は Ghostscript 依存。
  # 失敗しても止めない（継続）/失敗内容はログへ。
  ok <- TRUE
  msg <- ""
  t <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # outfile を同一パスにすると環境によって失敗することがあるため、一旦 tmp に出す
  tmp <- paste0(path, ".embedtmp")

  # 既存 tmp があれば掃除（前回失敗の残骸など）
  if (file.exists(tmp)) {
    try(unlink(tmp), silent = TRUE)
  }

  tryCatch({
    grDevices::embedFonts(file = path, outfile = tmp)
    # 成功したら置換
    if (file.exists(tmp)) {
      ok <- file.rename(tmp, path)
      if (!ok) {
        msg <- "file.rename(tmp -> original) failed"
        append_text_v1(log$warnings_txt, paste0("[WARN] embedFonts succeeded but replace failed (", kind, "): ", path))
      }
    } else {
      ok <- FALSE
      msg <- "embedFonts produced no outfile"
      append_text_v1(log$warnings_txt, paste0("[WARN] embedFonts produced no outfile (", kind, "): ", path))
    }
  }, error = function(e) {
    ok <<- FALSE
    msg <<- conditionMessage(e)
    append_text_v1(log$warnings_txt, paste0("[WARN] embedFonts failed (", kind, "): ", path, " :: ", msg))
    # tmp が残っていたら掃除
    if (file.exists(tmp)) try(unlink(tmp), silent = TRUE)
  })

  append_csv_row_v1(
    log$embed_csv,
    header = c("time", "kind", "path", "ok", "message"),
    row = c(t, kind, path, ok, msg)
  )
  invisible(ok)
}


# embedFonts を「必ず試行」するための薄いラッパ（失敗しても続行）
# - 呼び出し側互換のため (path, kind, log, ...) 形式
apply_embedfonts_v1 <- function(path, kind = "pdf", log, ...) {
  tryCatch({
    try_embed_fonts_v1(path, log, kind = kind)
  }, error = function(e) {
    append_text_v1(log$warnings_txt, paste0("[WARN] embedFonts(", kind, ") failed: ", basename(path), " :: ", conditionMessage(e)))
    FALSE
  })
}


# ---- デバイス別 ggsave -------------------------------------------------------
save_one_plot_v1 <- function(plot, path, w, h, format, cfg) {
  format <- tolower(format)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  bg <- cfg$export$bg %||% "white"

  if (format == "svg") {
    # svglite があれば優先、無ければ grDevices::svg へフォールバック
    if (requireNamespace("svglite", quietly = TRUE)) {
      dev <- svglite::svglite
    } else {
      dev <- grDevices::svg
    }
    ggsave(
      filename = path,
      plot = plot,
      width = w,
      height = h,
      units = "in",
      device = dev,
      bg = bg,
      limitsize = FALSE
    )

  } else if (format == "pdf") {
    ggsave(
      filename = path,
      plot = plot,
      width = w,
      height = h,
      units = "in",
      device = grDevices::cairo_pdf,
      bg = bg,
      limitsize = FALSE
    )

  } else if (format == "png") {
    # ragg があれば優先、無ければ grDevices::png へフォールバック
    dpi <- cfg$export$png_dpi %||% 300
    if (requireNamespace("ragg", quietly = TRUE)) {
      ggsave(
        filename = path,
        plot = plot,
        width = w,
        height = h,
        units = "in",
        device = ragg::agg_png,
        res = dpi,
        bg = bg,
        limitsize = FALSE
      )
    } else {
      # ggsave(dpi=) を使うと内部で px 計算される
      ggsave(
        filename = path,
        plot = plot,
        width = w,
        height = h,
        units = "in",
        device = grDevices::png,
        dpi = dpi,
        bg = bg,
        limitsize = FALSE
      )
    }

  } else if (format == "eps") {
    ggsave(
      filename = path,
      plot = plot,
      width = w,
      height = h,
      units = "in",
      device = grDevices::cairo_ps,
      fallback_resolution = cfg$export$eps_fallback_dpi %||% 300,
      bg = bg,
      limitsize = FALSE
    )

  } else {
    stop("未対応の形式: ", format, call. = FALSE)
  }
}


# ---- barプロフィール決定 ------------------------------------------------------
get_bar_profile_v1 <- function(n_jichikai, cfg) {
  # 優先度：50+ / 11-15 / 6-10 / 1-5 / default
  if (n_jichikai >= 50 && !is.null(cfg$bar_profiles$n50_plus)) return(cfg$bar_profiles$n50_plus)
  if (n_jichikai >= 11 && n_jichikai <= 15 && !is.null(cfg$bar_profiles$n11_15)) return(cfg$bar_profiles$n11_15)
  if (n_jichikai >= 6  && n_jichikai <= 10 && !is.null(cfg$bar_profiles$n06_10)) return(cfg$bar_profiles$n06_10)
  if (n_jichikai >= 1  && n_jichikai <= 5  && !is.null(cfg$bar_profiles$n01_05)) return(cfg$bar_profiles$n01_05)
  cfg$bar_profiles$default
}


apply_bar_profile_to_cfg_v1 <- function(cfg, profile) {
  cfg2 <- cfg
  if (!is.null(profile$bar_width)) cfg2$bar$bar_width <- profile$bar_width
  if (!is.null(profile$x_expand)) cfg2$bar$x_expand <- profile$x_expand
  if (!is.null(profile$y_expand)) cfg2$bar$y_expand <- profile$y_expand
  if (!is.null(profile$name_size))  cfg2$bar$name_size <- profile$name_size
  if (!is.null(profile$value_size)) cfg2$bar$value_size <- profile$value_size
  if (!is.null(profile$rank_size))  cfg2$bar$rank_size <- profile$rank_size

  # ---- 追加: 統計ラベル vjust（profileで上書き可能） ---------------------------
  if (!is.null(profile$cv_label_vjust))     cfg2$bar$cv_label_vjust <- profile$cv_label_vjust
  if (!is.null(profile$mean_label_vjust))   cfg2$bar$mean_label_vjust <- profile$mean_label_vjust
  if (!is.null(profile$median_label_vjust)) cfg2$bar$median_label_vjust <- profile$median_label_vjust

  # ---- 追加: layout（profileで上書き可能） ------------------------------------
  if (!is.null(profile$layout) && is.list(profile$layout)) {
    for (nm in names(profile$layout)) {
      cfg2$layout[[nm]] <- profile$layout[[nm]]
    }

    # x軸タイトル位置（歴史的に cfg$bar 配下に置いてあるので移送）
    if (!is.null(profile$layout$x_axis_title_margin)) cfg2$bar$x_axis_title_margin <- profile$layout$x_axis_title_margin
    if (!is.null(profile$layout$x_axis_title_vjust))  cfg2$bar$x_axis_title_vjust  <- profile$layout$x_axis_title_vjust
    if (!is.null(profile$layout$x_axis_title_hjust))  cfg2$bar$x_axis_title_hjust  <- profile$layout$x_axis_title_hjust
  }

  cfg2
}

# ---- 出力ディレクトリ決定 ----------------------------------------------------
# run_root/
#   pdf/<地域>/bar/<profile>/...
#   pdf/<地域>/pie/fixed__w7h7_top5/...
#   png/... 同様
#   svg/... 同様
#   eps/... 同様
get_pie_folder_v1 <- function(cfg) {
  # 固定にしておく（あとから変えても良い）
  "fixed__w7h7__top5"
}

# ---- ペア書き出し（4形式） ---------------------------------------------------
save_pair_all_formats_v1 <- function(
  p_bar, p_pie,
  run_root,
  region_jp,
  prefix,
  bar_profile,
  cfg,
  log
) {
  formats <- cfg$export$formats

  # bar/pie サイズ
  bar_w <- bar_profile$w %||% cfg$export$bar_w_default
  bar_h <- bar_profile$h %||% cfg$export$bar_h_default
  pie_w <- cfg$export$pie_w
  pie_h <- cfg$export$pie_h

  pie_folder <- get_pie_folder_v1(cfg)

  # manifest
  t <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  for (fmt in formats) {
    base_fmt_dir <- file.path(run_root, fmt, region_jp)

    out_bar_dir <- file.path(base_fmt_dir, "bar", bar_profile$folder)
    out_pie_dir <- file.path(base_fmt_dir, "pie", pie_folder)

    dir.create(out_bar_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(out_pie_dir, recursive = TRUE, showWarnings = FALSE)

    bar_path <- file.path(out_bar_dir, paste0(prefix, "__bar.", fmt))
    pie_path <- file.path(out_pie_dir, paste0(prefix, "__pie.", fmt))

    ok_bar <- TRUE
    ok_pie <- TRUE

    # 1) 保存（失敗しても次形式へ）
    tryCatch(
      save_one_plot_v1(p_bar, bar_path, bar_w, bar_h, fmt, cfg),
      error = function(e) {
        ok_bar <<- FALSE
        append_text_v1(log$warnings_txt, paste0("[WARN] save(bar) failed: fmt=", fmt, " path=", bar_path, " msg=", conditionMessage(e)))
      }
    )
    tryCatch(
      save_one_plot_v1(p_pie, pie_path, pie_w, pie_h, fmt, cfg),
      error = function(e) {
        ok_pie <<- FALSE
        append_text_v1(log$warnings_txt, paste0("[WARN] save(pie) failed: fmt=", fmt, " path=", pie_path, " msg=", conditionMessage(e)))
      }
    )

    # 2) PDF/EPS は embedFonts を必ず試行（失敗しても続行、ログへ記録）
    if (ok_bar && fmt %in% c("pdf", "eps")) {
      apply_embedfonts_v1(bar_path, fmt, log, region_jp, prefix, "bar")
    }
    if (ok_pie && fmt %in% c("pdf", "eps")) {
      apply_embedfonts_v1(pie_path, fmt, log, region_jp, prefix, "pie")
    }

    # 3) export manifest（bar/pie 両方成功した場合のみ）
    if (ok_bar && ok_pie) {
      append_csv_row_v1(
        path = log$export_csv,
        header = c("timestamp", "format", "region", "prefix", "type", "profile_or_pie", "path", "w_in", "h_in"),
        row = c(t, fmt, region_jp, prefix, "bar", bar_profile$folder, bar_path, bar_w, bar_h)
      )
      append_csv_row_v1(
        path = log$export_csv,
        header = c("timestamp", "format", "region", "prefix", "type", "profile_or_pie", "path", "w_in", "h_in"),
        row = c(t, fmt, region_jp, prefix, "pie", pie_folder, pie_path, pie_w, pie_h)
      )
    }
  }
}


# ---- run_root 初期化 ---------------------------------------------------------
init_run_root_v1 <- function(out_root, target_year, run_id) {
  run_root <- file.path(out_root, as.character(target_year), run_id)
  dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
  run_root
}

# ---- ghostscript info をログへ ------------------------------------------------
write_gs_info_v1 <- function(log) {
  info <- detect_gs_v1()
  lines <- paste(names(info), info, sep = "=")
  writeLines(lines, log$gs_info_txt, useBytes = TRUE)
}

# ---- 未知地域ログ -------------------------------------------------------------
log_unknown_region_v1 <- function(log, region_key, chosen_name) {
  t <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  append_text_v1(log$unknown_region_txt, paste0(t, "\t", region_key, "\t", chosen_name))
}

# ---- region_key -> 日本語地域名（未知キー対応） ------------------------------
resolve_region_name_v1 <- function(region_key, cfg, log = NULL) {
  region_key <- as.character(region_key)
  m <- cfg$region$map

  if (!is.null(m) && region_key %in% names(m)) {
    return(unname(m[[region_key]]))
  }

  # 未定義
  fallback <- paste0(cfg$region$unknown_prefix %||% "未定義地域_", region_key)

  if (interactive()) {
    msg <- paste0(
      "未定義の region_key が来ました: ", region_key, "\n",
      "A: 続行（フォルダ名=", fallback, "）\n",
      "B: 停止\n"
    )
    choice <- menu(c("続行", "停止"), title = msg)
    if (choice == 1) {
      if (!is.null(log)) log_unknown_region_v1(log, region_key, fallback)
      return(fallback)
    }
    stop("未定義 region_key のため停止しました: ", region_key, call. = FALSE)
  }

  # 非対話
  if (isTRUE(cfg$region$allow_unknown_noninteractive)) {
    if (!is.null(log)) log_unknown_region_v1(log, region_key, fallback)
    return(fallback)
  }

  stop(
    "未定義 region_key のため停止しました: ", region_key,
    "（非対話実行。続行するには cfg$region$allow_unknown_noninteractive = TRUE にしてください）",
    call. = FALSE
  )
}

            