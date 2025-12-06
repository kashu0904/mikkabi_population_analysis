#----------------------------------------
# 1. ライブラリ
#----------------------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(openxlsx)
library(stringr)
# ======================================================================
# プロット共通モジュール  設定・操作ガイド（最上部に集約）
#   対象: plot_population_by_area_common.R
#   目的: グラフのデザインとスケールをここだけで統一管理／操作
#   注意: 機能は変えず、既存処理の可読性と操作性のみ改善
# ----------------------------------------------------------------------
# 【使い方】
#  1) このブロック内の値だけを必要に応じて調整してください。
#  2) main_plot.R から `area_name` を指定 → 本ファイルを source() → plot_for_area() 実行。
#  3) 目盛や上限などをエリア別に固定したい場合は、main_plot.R 側の custom_scales に
#     例の形式で書きます（下の HOWTO を参照）。
#  4) 値をいじっても描画機能は変わりません。デザイン／スケールの既定値を安全に調整するだけです。
# ----------------------------------------------------------------------
# 【デザイン（共通）】
width_std       <- 0.70   # バーの太さ（全グラフ共通; 推奨: 0.6〜0.8）
linewidth_std   <- 0.35   # バー枠線の太さ（推奨: 0.3〜0.5）
expand_x_std    <- expansion(mult = c(0.04, 0.04))  # X軸左右の余白
expand_y_std    <- expansion(mult = c(0.04, 0.04))  # Y軸上下の余白

# テーマ（背景・罫線などの見た目）
theme_std <- theme(
  panel.background = element_rect(fill = "#CCCCCC"),
  panel.grid.major = element_line(size = 0.35, colour = "white"),
  panel.grid.minor = element_blank(),
  axis.text.x      = element_text(size = 9),
  axis.text.y      = element_text(size = 9),
  axis.title.x     = element_text(size = 11),
  axis.title.y     = element_text(size = 11),
  plot.title       = element_text(size = 13, face = "bold", hjust = 0)
)

# 【最新年の比較図 専用パラメータ】
#  - 「各町字の高齢化率 <end_year>（<base_area>）」の図にのみ適用
width_std_latest        <- 0.50                  # バー太さ（最新年比較）
latest_area_label_size  <- 5                     # X軸ラベル文字サイズ
latest_arate_breaks     <- seq(0, 60, by = 5)    # 目盛（%）
latest_arate_limits     <- c(0, 61.5)            # 表示範囲（%）
expand_x_std_latest     <- expansion(mult = c(0.04, 0.04))  # X余白
expand_y_std_latest     <- expansion(mult = c(0.04, 0.04))  # Y余白

# ======================================================================

# custom_scales が未定義でもコケないようにガード（機能は変わりません）
if (!exists("custom_scales")) custom_scales <- list()

#----------------------------------------
# 2. 定数・パラメータ設定
#----------------------------------------

file_name <- paste0(area_name, "_population_combined.xlsx")
base_dir   <- here::here("data","processed")
file_path <- file.path(base_dir, file_name)
# ① シート名を取得し、「YYYY-04」のものだけを年だけに変換
years <- excel_sheets(file_path) %>%
  grep("^[0-9]{4}-04$", ., value = TRUE) %>%  # YYYY-04 形式のみ抽出
  sub("-04$", "", .) %>%                      #「-04」を外す
  as.integer() %>%                            # 整数化
  sort()                                      # ソート
# ② 読み込むシート名ベクトル
sheets     <- paste0(years, "-04")

#----------------------------------------
# 3. 生データ読み込み＆ヘッダー整合性チェック
#----------------------------------------
# ── 1) Excel ブックを一度だけ開く ─────────────────────
wb <- loadWorkbook(file_path)
# ── 2) “生データ” としてヘッダー行も含めて全シート読み込み ───
rawdata <- lapply(sheets, function(sh) {
  readWorkbook(wb, sheet = sh, colNames = FALSE)
})
# まず先頭に追加：地区名の正規化（空白全除去）
norm_area <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  # 半角/全角スペース、NBSP、タブ等を全部削除
  x <- gsub("[[:space:]\u00A0\u3000]+", "", x, perl = TRUE)
  x
}
# ── 3) 最新のシートから動的に地区数＆地区名を取得 ────────────────
last_data <- rawdata[[length(rawdata)]]

# 45行ごとにある地区ヘッダー（列11）を抽出（外側のカッコ等を落としてから正規化）
header_positions  <- seq(1, nrow(last_data), by = 45)
header_candidates <- as.character(last_data[header_positions, 11])
header_stripped   <- stringr::str_sub(header_candidates, 2, -2)

# 実在する地区名だけに絞り、無駄なスペースを排除
NAMES_area_raw <- header_stripped[!is.na(header_stripped) & header_stripped != ""]
NAMES_area     <- norm_area(NAMES_area_raw)
n_areas        <- length(NAMES_area)

# 全シート共通のヘッダー抽出行位置を再定義
positions <- 1 + 45 * seq(0, n_areas - 1)

# ── 4) 各シートのヘッダー行（列11）を抽出（同じ正規化を適用） ─────────
headers_list_raw <- lapply(rawdata, function(dat) {
  stringr::str_sub(as.character(dat[positions, 11]), 2, -2)
})
headers_list <- lapply(headers_list_raw, norm_area)

# ── 5) 最新シートを基準に、他シートのヘッダーと比較（正規化済みで比較） ─────────
base_header <- headers_list[[length(headers_list)]]
mismatch    <- lapply(headers_list, function(hdr) which(hdr != base_header))
bad_sheets  <- which(sapply(mismatch, length) > 0)

# ── 6) 不一致があれば差分を表示（表示は正規化後の値） ────────────────────
if (length(bad_sheets) > 0) {
  msg <- sapply(bad_sheets, function(i) {
    diffs <- mismatch[[i]]
    paste0(
      sheets[i], " シートでヘッダー不一致：行 ",
      paste(diffs, collapse = ", "),
      " （基準: ", paste(base_header[diffs], collapse = ", "),
      " → 実際: ", paste(headers_list[[i]][diffs], collapse = ", "), "）"
    )
  })
  cat(
    "ヘッダー整合性チェックに失敗しました。\n",
    paste(msg, collapse = "\n"), "\n\n",
    sep = ""
  )
  ans <- utils::askYesNo("ヘッダー不一致を検知しました。処理を続行しますか？")
  if (identical(ans, FALSE)) stop("処理を中断しました。")
  message("ユーザー判断により処理を続行します。")
}


#----------------------------------------
# 4. 通常データ読み込み（ヘッダー行を列名として）
#----------------------------------------
index_vec <- 0:(n_areas - 1)
sdata <- lapply(sheets, function(sh) {
  read_excel(path = file_path, sheet = sh, col_names = TRUE)})
# 以下、ext() を使ってデータ抽出へ進みます
ext <- function(dat, rows, col) {
  as.numeric(dat[rows, col][[1]])}

p0004  <- unlist(lapply(sdata, ext, rows=  2  + 45 * index_vec, col = 2  )) #0 ~ 4歳人口
p0509  <- unlist(lapply(sdata, ext, rows=  8  + 45 * index_vec, col = 2  )) #05~09歳人口
p1014  <- unlist(lapply(sdata, ext, rows=  14 + 45 * index_vec, col = 2  )) #10~14歳人口
p1519  <- unlist(lapply(sdata, ext, rows=  20 + 45 * index_vec, col = 2  )) #15~19歳人口
p2024  <- unlist(lapply(sdata, ext, rows=  26 + 45 * index_vec, col = 2  )) #20~24歳人口
p2529  <- unlist(lapply(sdata, ext, rows=  32 + 45 * index_vec, col = 2  )) #25~29歳人口
p3034  <- unlist(lapply(sdata, ext, rows=  38 + 45 * index_vec, col = 2  )) #30~34歳人口
p3539  <- unlist(lapply(sdata, ext, rows=  2  + 45 * index_vec, col = 6  )) #35~39歳人口
p4044  <- unlist(lapply(sdata, ext, rows=  8  + 45 * index_vec, col = 6  )) #40~44歳人口
p4549  <- unlist(lapply(sdata, ext, rows=  14 + 45 * index_vec, col = 6  )) #45~49歳人口
p5054  <- unlist(lapply(sdata, ext, rows=  20 + 45 * index_vec, col = 6  )) #50~54歳人口
p5559  <- unlist(lapply(sdata, ext, rows=  26 + 45 * index_vec, col = 6  )) #55~59歳人口
p6064  <- unlist(lapply(sdata, ext, rows=  32 + 45 * index_vec, col = 6  )) #60~64歳人口
p6569  <- unlist(lapply(sdata, ext, rows=  38 + 45 * index_vec, col = 6  )) #65~69歳人口
p7074  <- unlist(lapply(sdata, ext, rows=  2  + 45 * index_vec, col = 10 )) #70~74歳人口
p7579  <- unlist(lapply(sdata, ext, rows=  8  + 45 * index_vec, col = 10 )) #75~79歳人口
p8084  <- unlist(lapply(sdata, ext, rows=  14 + 45 * index_vec, col = 10 )) #80~84歳人口
p8589  <- unlist(lapply(sdata, ext, rows=  20 + 45 * index_vec, col = 10 )) #85~89歳人口
p9094  <- unlist(lapply(sdata, ext, rows=  26 + 45 * index_vec, col = 10 )) #90~94歳人口
p9500  <- unlist(lapply(sdata, ext, rows=  32 + 45 * index_vec, col = 10 )) #95~  歳人口

p0014  <- unlist(lapply(sdata, ext, rows = 44 + 45 * index_vec, col = 2  )) #0 ~14歳人口
p1564  <- unlist(lapply(sdata, ext, rows = 44 + 45 * index_vec, col = 6  )) #15~64歳人口
p6500  <- unlist(lapply(sdata, ext, rows = 44 + 45 * index_vec, col = 10 )) #65~  歳人口

p6574  <- p6569+p7074 #65~74歳人口(前期高齢者)
p7500  <- p6500-p6574 #75~  歳人口(後期高齢者)

pTotal <- unlist(lapply(sdata, ext, rows = 43 + 45 * index_vec, col = 10)) #総人口

pTotal_calc <- p0014 + p1564 + p6500 + p6500 - p6569 -p7074 - p7500

# 不一致チェック（許容誤差つき）＆ユーザー選択で続行
# ─────────────────────────────────────────────
# 5歳階級→集計の突き合わせ（全年齢検証フルセット）
# 前提：p0004..p9500, p0014, p1564, p6500, p6574, p7500, pTotal が既に作成済み
# years, NAMES_area, n_areas も定義済み
# ─────────────────────────────────────────────
tol <- 1e-8

# ① 5歳階級の合算から再計算（期待する集計値）
p0014_calc <- p0004 + p0509 + p1014
p1564_calc <- p1519 + p2024 + p2529 + p3034 + p3539 + p4044 + p4549 + p5054 + p5559 + p6064
p6500_calc <- p6569 + p7074 + p7579 + p8084 + p8589 + p9094 + p9500

# ② 65–74 / 75+ の内訳チェック（下位=上位の再現）
p6574_calc <- p6569 + p7074
p7500_calc <- p7579 + p8084 + p8589 + p9094 + p9500

# ③ 全年齢合算＝総人口
pTotal_from_bins <- p0004 + p0509 + p1014 + p1519 + p2024 + p2529 + p3034 +
  p3539 + p4044 + p4549 + p5054 + p5559 + p6064 + p6569 +
  p7074 + p7579 + p8084 + p8589 + p9094 + p9500

# ④ 便宜：比較ユーティリティ（ベクトル×ユーザー続行）
check_vec <- function(label, base, calc, tol=1e-8) {
  if (length(base) != length(calc)) {
    stop(sprintf("%s: ベクトル長が不一致 base=%d calc=%d", label, length(base), length(calc)))
  }
  
  delta <- calc - base
  
  # NA関連と数値不一致を区分
  na_both      <- is.na(base) & is.na(calc)  # NA同士
  na_mismatch  <- xor(is.na(base), is.na(calc))  # 片方だけNA
  num_mismatch <- (!is.na(base) & !is.na(calc) & (abs(delta) > tol))
  
  bad_idx_all  <- which(na_mismatch | num_mismatch)    # ユーザー選択対象
  bad_idx_na   <- which(na_both)                       # NA同士（警告のみ）
  
  if (length(bad_idx_all) == 0 && length(bad_idx_na) == 0) {
    message(sprintf("%s：OK（tol=%.1e）", label, tol))
    return(invisible(TRUE))
  }
  
  L        <- length(base)
  year_idx <- ((seq_len(L) - 1) %/% n_areas) + 1
  area_idx <- ((seq_len(L) - 1) %%  n_areas) + 1
  
  # NA同士の警告
  if (length(bad_idx_na) > 0) {
    lines_na <- vapply(bad_idx_na, function(i) {
      sprintf("%d年 / %s : 基準=NA, 再計算=NA",
              years[year_idx[i]], NAMES_area[area_idx[i]])
    }, character(1))
    cat(
      sprintf("%s：NA同士のデータを検出（tol=%.1e）。処理は続行します。\n", label, tol),
      paste(lines_na, collapse = "\n"), "\n\n", sep = ""
    )
  }
  
  # 数値不一致または片方NAの処理
  if (length(bad_idx_all) > 0) {
    lines_all <- vapply(bad_idx_all, function(i) {
      sprintf("%d年 / %s : 基準=%s, 再計算=%s, 差分=%s",
              years[year_idx[i]], NAMES_area[area_idx[i]],
              format(base[i], big.mark=",", scientific=FALSE, trim=TRUE),
              format(calc[i], big.mark=",", scientific=FALSE, trim=TRUE),
              format(delta[i], big.mark=",", scientific=FALSE, trim=TRUE))
    }, character(1))
    
    cat(
      sprintf("%s で不一致/片方NAを検出（tol=%.1e）。\n", label, tol),
      paste(lines_all, collapse = "\n"), "\n\n", sep = ""
    )
    
    ans <- utils::askYesNo(sprintf("%s の不一致を無視して処理を続行しますか？", label))
    if (identical(ans, FALSE)) stop(sprintf("ユーザー選択により中断：%s", label))
    message(sprintf("ユーザー選択により続行します（%s に不一致あり）。", label))
    return(invisible(FALSE))
  }
  
  invisible(TRUE)
}


# ⑤ 個別チェック（全年齢を網羅）
check_vec("0–14歳（集計=5歳階級合算）", p0014, p0014_calc, tol)
check_vec("15–64歳（集計=5歳階級合算）", p1564, p1564_calc, tol)
check_vec("65歳以上（集計=5歳階級合算）", p6500, p6500_calc, tol)

check_vec("65–74歳（集計=65–69+70–74）", p6574, p6574_calc, tol)
check_vec("75歳以上（集計=75–79+…+95+）", p7500, p7500_calc, tol)

check_vec("総人口（集計=全5歳階級の合算）", pTotal, pTotal_from_bins, tol)

# ⑥ 既存の総人口（=0–14+15–64+65+）検証も（必要なら）
pTotal_calc <- p0014 + p1564 + p6500
check_vec("総人口（集計=0–14+15–64+65+）", pTotal, pTotal_calc, tol)


# 不一致チェック（許容誤差つき）＆ユーザー選択で続行
# 期待マップ（必要なら列番号やラベル文字列をあなたの実シートに合わせて調整）
spec <- data.frame(
  name   = c("p0004","p0509","p1014","p1519","p2024","p2529","p3034",
             "p3539","p4044","p4549","p5054","p5559","p6064","p6569",
             "p7074","p7579","p8084","p8589","p9094","p9500"),
  row0   = c(2,8,14,20,26,32,38,  2,8,14,20,26,32,38,  2,8,14,20,26,32),
  col    = c(rep(1,7),  # 0-4〜30-34
             rep(5,7),  # 35-39〜65-69
             rep(9,6)), # 70-74〜95以上
  expect = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
             "70-74","75-79","80-84","85-89","90-94","95以上") # ←シートの表記に合わせる
)


label_col_name <- NULL  # 可能なら列名で指定（例: "age_label"）
label_col      <- 1     # 数値指定しか無い場合はこれを使う
block_h        <- 45

# 置き換え版：空白・全角空白・NBSPを全部除去、ハイフン類も統一、全角数字→半角
norm_label <- function(x){
  x <- as.character(x)
  
  # 前後の空白トリム
  x <- trimws(x)
  
  # 全角チルダ→半角（使ってないなら無害）
  x <- gsub("〜", "~", x, fixed = TRUE)
  
  # 全ての空白を削除（通常空白・タブ・NBSP・全角空白を含む）
  # [:space:] だけだと全角空白(U+3000)やNBSP(U+00A0)を逃す場合があるので明示指定
  x <- gsub("[[:space:]\u00A0\u3000]+", "", x, perl = TRUE)
  
  # いろんな“横棒”を半角ハイフンに統一（− — – ‐ など）
  x <- gsub("[−—–‐]", "-", x)
  
  # 全角数字→半角数字
  x <- chartr("０１２３４５６７８９", "0123456789", x)
  
  # 最後に「--」のような重複を1本へ（念のため）
  x <- gsub("-{2,}", "-", x)
  
  x
}


get_label_col <- function(dat){
  if (!is.null(label_col_name)) {
    if (!label_col_name %in% names(dat))
      stop(sprintf("ラベル列 '%s' が見つかりません。", label_col_name))
    return(as.character(dat[[label_col_name]]))
  } else {
    if (label_col < 1 || label_col > ncol(dat))
      stop(sprintf("label_col=%d が列数の範囲外です（ncol=%d）。", label_col, ncol(dat)))
    return(as.character(dat[[label_col]]))
  }
}

check_labels <- function(sdata, spec, index_vec, years, NAMES_area) {
  if (length(sdata) != length(years))
    stop("sdata（シート数）と years（年数）の長さが一致しません。")
  if (!all(index_vec %% 1 == 0))
    stop("index_vec は整数ベクトルにしてください（0,1,2,...）。")
  if (min(index_vec) != 0)
    warning("index_vec は 0 始まりを推奨。ズレが疑われます。")
  expect <- norm_label(spec$expect)
  
  bad_lines <- character()
  
  for (yidx in seq_along(sdata)) {
    dat <- as.data.frame(sdata[[yidx]])
    nrow_dat <- nrow(dat); ncol_dat <- ncol(dat)
    
    for (aidx in seq_along(index_vec)) {
      base_row <- block_h * index_vec[aidx]
      
      # ★ここを「kごとに列を切り替える」ループにする
      for (k in seq_len(nrow(spec))) {
        rows <- spec$row0[k] + base_row
        colk <- spec$col[k]
        
        # 範囲外チェック（行・列両方）
        oob <- rows < 1 | rows > nrow_dat | colk < 1 | colk > ncol_dat
        if (oob) {
          msg <- sprintf("[範囲外] %s年 / %s : row=%d col=%d がシート範囲 (nrow=%d,ncol=%d) を外れています。",
                         as.character(years[yidx]), NAMES_area[aidx],
                         rows, colk, nrow_dat, ncol_dat)
          bad_lines <- c(bad_lines, msg)
          next
        }
        
        actual <- norm_label(dat[rows, colk, drop = TRUE])
        
        if (is.na(actual) || actual != expect[k]) {
          msg <- sprintf("[%s] %s年 / %s : 期待='%s', 実='%s' (row=%d,col=%d)",
                         spec$name[k],
                         as.character(years[yidx]),
                         NAMES_area[aidx],
                         expect[k], actual, rows, colk)
          bad_lines <- c(bad_lines, msg)
        }
      }
    }
  }
  
  if (length(bad_lines)) {
    cat("⚠ 年齢ラベル不一致／範囲外を検出：\n", paste(bad_lines, collapse = "\n"), "\n\n", sep = "")
    ans <- utils::askYesNo("ラベル不一致を無視して続行しますか？")
    if (isFALSE(ans)) stop("ユーザー選択により中断（年齢ラベル）。")
    if (is.na(ans))  message("非対話環境のため既定で続行します（不一致あり）。")
    message("ユーザー選択により続行します（年齢ラベル不一致あり）。")
  } else {
    message("年齢ラベル検証：OK")
  }
}

# 実行
check_labels(sdata, spec, index_vec, years, NAMES_area)


#----------------------------------------
# 4. マトリクス化
#----------------------------------------
mat_0004 <- matrix(p0004,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_0014 <- matrix(p0014,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_1564 <- matrix(p1564,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_6500 <- matrix(p6500,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_TOT  <- matrix(pTotal, nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_6574 <- matrix(p6574, nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_7500 <- matrix(p7500, nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_ageing               <- mat_6500 / mat_TOT * 100 #高齢化率
mat_late_ageing          <- mat_7500 / mat_TOT * 100 #後期高齢化率
mat_dependency_ratio         <- (mat_0014 + mat_6500) / mat_1564 * 100 #従属人口比率
mat_youth_dependency_ratio   <- mat_0014 / mat_1564 * 100　　　　　　　#年少人口従属比率
mat_oldage_dependency_ratio  <- mat_6500 / mat_1564 * 100　　　　　　　#老年人口従属比率

#----------------------------------------
# 従属人口比率（mat_）の整合性チェック
# 期待：mat_youth_dependency_ratio + mat_oldage_dependency_ratio == mat_dependency_ratio
# 仕様：NA同士は警告のみで自動続行／片方NA or 数値不一致はユーザー選択
#----------------------------------------
tol <- 1e-8

# 致命：行列次元不一致は続行不可
if (!identical(dim(mat_youth_dependency_ratio), dim(mat_oldage_dependency_ratio)) ||
    !identical(dim(mat_youth_dependency_ratio), dim(mat_dependency_ratio))) {
  stop("従属人口比率行列の次元不一致：mat_* の行列サイズを確認してください。")
}

dep_sum <- mat_youth_dependency_ratio + mat_oldage_dependency_ratio
delta   <- dep_sum - mat_dependency_ratio

# マスク分類
na_both      <- is.na(dep_sum) & is.na(mat_dependency_ratio)                # NA同士 → 警告のみ
na_mismatch  <- xor(is.na(dep_sum), is.na(mat_dependency_ratio))            # 片方NA → 要選択
num_mismatch <- (!is.na(dep_sum) & !is.na(mat_dependency_ratio) &
                   (abs(delta) > tol))                                        # 数値不一致 → 要選択

# 1) NA同士：警告のみで続行
if (any(na_both)) {
  na_rc <- which(na_both, arr.ind = TRUE)
  lines_na <- apply(na_rc, 1, function(rc) {
    r <- rc[1]; c <- rc[2]
    sprintf("%d年 / %s : 年少+老年=NA, 総従属=NA", years[r], NAMES_area[c])
  })
  cat(
    sprintf("従属人口比率：NA同士のデータを検出（tol=%.1e）。処理は続行します。\n", tol),
    paste(lines_na, collapse = "\n"), "\n\n", sep = ""
  )
}

# 2) 片方NA or 数値不一致：ユーザー選択
bad_mask_all <- na_mismatch | num_mismatch
if (any(bad_mask_all)) {
  bad_rc <- which(bad_mask_all, arr.ind = TRUE)
  lines <- apply(bad_rc, 1, function(rc) {
    r <- rc[1]; c <- rc[2]
    sprintf("%d年 / %s : 年少+老年=%s, 総従属=%s, 差分=%s",
            years[r], NAMES_area[c],
            ifelse(is.na(dep_sum[r,c]), "NA", sprintf("%.6f", dep_sum[r,c])),
            ifelse(is.na(mat_dependency_ratio[r,c]), "NA", sprintf("%.6f", mat_dependency_ratio[r,c])),
            ifelse(is.na(delta[r,c]), "NA", sprintf("%.6f", delta[r,c])))
  })
  cat(
    sprintf("従属人口比率の整合性チェックで不一致/片方NAを検出（tol=%.1e）。\n", tol),
    paste(lines, collapse = "\n"), "\n\n", sep = ""
  )
  ans <- utils::askYesNo("不一致を無視して処理を続行しますか？")
  if (identical(ans, FALSE)) stop("ユーザー選択により処理を中断しました。")
  message("ユーザー選択により続行します（従属人口比率チェックに不一致あり）。")
} else {
  message(sprintf("従属人口比率の整合性チェック：OK（tol=%.1e）", tol))
}


#----------------------------------------
# 最新の高齢化率
#----------------------------------------
theme_std <- theme(
  panel.background = element_rect(fill = "#CCCCCC"),
  plot.title       = element_text(size = 20, hjust = 0.5),  # グラフタイトル
  axis.title       = element_blank(),
  axis.text.x      = element_text(
    angle  = 50,      # X軸ラベル角度
    hjust  = 1,       # 位置（1=右寄せ）
    size   = 20,      # 文字サイズ
    margin = margin(t = 0.1, unit = "cm"),
    color  = "#000000"
  ),
  axis.text.y      = element_text(
    size   = 15,
    margin = margin(r = 0.2, unit = "cm"),
    color  = "#000000"
  ),
  plot.margin      = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")
)
#----------------------------------------
# auto scale 用ヘルパー
#----------------------------------------
# auto_scale: データ最大値にパディングをかけ、いい感じの目盛＆上限を返す
# x   : 数値ベクトル（例 df$age0004）
# n   : 目盛の本数目安（大きいほど細かい）
#   pad_brk  - 目盛生成用にかける倍率（例1.05 → データ最大の105%でpretty）
#   pad_lim  - limits用にかける倍率（例0.95 → データ最大の95%を上限に）
auto_scale <- function(x, n = 5, pad_brk = 1.05, pad_lim = 1.05) {
  mx      <- max(x, na.rm = TRUE)
  brk_max <- mx * pad_brk
  brks    <- pretty(c(0, brk_max), n = n)
  lim_max <- mx * pad_lim
  list(breaks = brks,limits = c(0, lim_max))}

#----------------------------------------
# 6. プロット関数定義（サンプルのテーマを共通化）
#----------------------------------------
start_year <- min(years)
end_year   <- max(years)
base_area  <- NAMES_area[1]
plot_for_area <- function(area_name){
  df <- tibble(
    year    = factor(years, levels = years),
    age0004 = mat_0004[, area_name],
    age0014 = mat_0014[, area_name],
    age1564 = mat_1564[, area_name],
    age65up = mat_6500[, area_name]
  ) %>% mutate(
    total = age0004 + age0014 + age1564 + age65up,
    Arate = age65up / total * 100)
  
  ##----------------------------------------
  ## 共通設定
  ##----------------------------------------
  auto_scales <- list(
    age0004 = auto_scale(df$age0004, n = 9),
    age0014 = auto_scale(df$age0014, n = 9),
    age1564 = auto_scale(df$age1564, n = 9),
    age65up = auto_scale(df$age65up, n = 9),
    total   = auto_scale(df$total,   n = 9)
  )
  rate_auto <- auto_rate_scale(df$Arate)
  cs <- custom_scales[[area_name]]
  if (is.null(cs)) cs <- list()
  y_scales <- modifyList(auto_scales, cs)
  if (!is.null(cs$Arate)) {y_scales$Arate <- cs$Arate} else {y_scales$Arate <- rate_auto}
  
  # 1) 0〜4歳
  pop_00_04 <- ggplot(df, aes(x = year, y = age0004)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#90569C", width = width_std) +
    labs(title = paste0(area_name, "：0〜4歳の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age0004$breaks, limits = y_scales$age0004$limits,labels = comma,expand = expand_y_std) +
    theme_std
  
  # 2) 0〜14歳
  pop_00_14 <- ggplot(df, aes(x = year, y = age0014)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#6BC7F1", width = width_std) +
    labs(title = paste0(area_name, "：0〜14歳の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age0014$breaks, limits = y_scales$age0014$limits,labels = comma,expand = expand_y_std) +
    theme_std
  
  # 3) 15〜64歳
  pop_15_64 <- ggplot(df, aes(x = year, y = age1564)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#00AD7A", width = width_std) +
    labs(title = paste0(area_name, "：15〜64歳の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age1564$breaks, limits = y_scales$age1564$limits,labels = comma,expand = expand_y_std) +
    theme_std
  
  # 4) 65歳以上
  pop_65_plus <- ggplot(df, aes(x = year, y = age65up)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#006EBB", width = width_std) +
    labs(title = paste0(area_name, "：65歳以上の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age65up$breaks, limits = y_scales$age65up$limits,labels = comma,expand = expand_y_std) +
    theme_std
  
  # 5) 総人口（積み上げ：3区分）
  df_long <- df %>%
    select(year, age0014, age1564, age65up) %>%
    pivot_longer(-year, names_to = "group", values_to = "value") %>%
    mutate(group = factor(group,
                          levels = c("age0014","age1564","age65up"),
                          labels = c("0〜14歳","15〜64歳","65歳以上")))
  
  pop_age3_count <- ggplot(df_long, aes(x = year, y = value, fill = group)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, width = width_std) +
    labs(title = paste0(area_name, "：総人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$total$breaks, limits = y_scales$total$limits,labels = comma,expand = expand_y_std) +
    scale_fill_manual(values = c("0〜14歳"="#6BC7F1", "15〜64歳"="#00AD7A", "65歳以上"="#006EBB")) +
    theme(legend.position = "none") + theme_std
  
  # 3区分：構成比（100%）
  pop_age3_share <- ggplot(df_long, aes(x = year, y = value, fill = group)) +
    geom_bar(stat = "identity", position = "fill",
             colour = 1, linewidth = linewidth_std, width = width_std) +
    labs(title = paste0(area_name, "：年齢3区分の構成比推移 ", start_year, "〜", end_year, "（", base_area, "）"),
         y = "構成比（%）", x = NULL) +
    scale_x_discrete(expand = expand_x_std) +
    scale_y_continuous(labels = scales::percent, expand = expand_y_std) +
    scale_fill_manual(
      values = c("0〜14歳"="#6BC7F1", "15〜64歳"="#00AD7A", "65歳以上"="#006EBB"),
      breaks = c("0〜14歳","15〜64歳","65歳以上"),
      labels = c("0〜14歳","15〜64歳","65歳以上"),
      guide  = guide_legend(title = "年齢階級")
    ) +
    theme_std
  
  # ─────────────────────────────────────
  # 年齢20区分（人数/構成比）
  # ─────────────────────────────────────
  pal20 <- c(
    "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2",
    "#7f7f7f","#bcbd22","#17becf","#aec7e8","#ffbb78","#98df8a","#ff9896",
    "#c5b0d5","#c49c94","#f7b6d2","#c7c7c7","#dbdb8d","#9edae5"
  )
  
  bins <- list(
    p0004,p0509,p1014,p1519,p2024,p2529,p3034,
    p3539,p4044,p4549,p5054,p5559,p6064,p6569,
    p7074,p7579,p8084,p8589,p9094,p9500
  )
  
  aidx <- match(area_name, NAMES_area)
  if (is.na(aidx)) stop(sprintf("area_name '%s' が NAMES_area にありません。", area_name))
  grab_area <- function(v){
    mat <- matrix(v, nrow = length(years), byrow = TRUE)
    as.numeric(mat[, aidx])
  }
  
  df20 <- data.frame(
    year   = factor(years, levels = years),
    `0–4`  = grab_area(bins[[1]]),
    `5–9`  = grab_area(bins[[2]]),
    `10–14`= grab_area(bins[[3]]),
    `15–19`= grab_area(bins[[4]]),
    `20–24`= grab_area(bins[[5]]),
    `25–29`= grab_area(bins[[6]]),
    `30–34`= grab_area(bins[[7]]),
    `35–39`= grab_area(bins[[8]]),
    `40–44`= grab_area(bins[[9]]),
    `45–49`= grab_area(bins[[10]]),
    `50–54`= grab_area(bins[[11]]),
    `55–59`= grab_area(bins[[12]]),
    `60–64`= grab_area(bins[[13]]),
    `65–69`= grab_area(bins[[14]]),
    `70–74`= grab_area(bins[[15]]),
    `75–79`= grab_area(bins[[16]]),
    `80–84`= grab_area(bins[[17]]),
    `85–89`= grab_area(bins[[18]]),
    `90–94`= grab_area(bins[[19]]),
    `95+`  = grab_area(bins[[20]])
  )
  
  age_levels  <- names(df20)[-1]
  disp_labels <- c("0〜4","5〜9","10〜14","15〜19","20〜24","25〜29","30〜34",
                   "35〜39","40〜44","45〜49","50〜54","55〜59","60〜64","65〜69",
                   "70〜74","75〜79","80〜84","85〜89","90〜94","95+")
  pal20_named <- setNames(pal20, age_levels)
  
  df20_long <- tidyr::pivot_longer(df20, -year, names_to = "age_bin", values_to = "value")
  df20_long$age_bin <- factor(df20_long$age_bin, levels = age_levels)
  
  total_by_year <- df20_long |>
    dplyr::group_by(year) |>
    dplyr::summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
  
  pop_age20_count <- ggplot(df20_long, aes(x = year, y = value, fill = age_bin)) +
    geom_bar(stat = "identity", colour = 1, linewidth = linewidth_std, width = width_std) +
    labs(title = paste0(area_name, "：年齢20区分の総人口推移 ", start_year, "〜", end_year),
         y = "人口（人）", x = NULL) +
    scale_x_discrete(expand = expand_x_std) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, max(total_by_year$total, na.rm = TRUE) * 1.05),
                       expand = expand_y_std) +
    scale_fill_manual(values = pal20_named, breaks = age_levels, labels = disp_labels,
                      guide = guide_legend(ncol = 2, title = "年齢階級")) +
    theme_std
  
  pop_age20_share <- ggplot(df20_long, aes(x = year, y = value, fill = age_bin)) +
    geom_bar(stat = "identity", position = "fill", colour = 1, linewidth = linewidth_std, width = width_std) +
    labs(title = paste0(area_name, "：年齢20区分の構成比推移 ", start_year, "〜", end_year),
         y = "構成比（%）", x = NULL) +
    scale_x_discrete(expand = expand_x_std) +
    scale_y_continuous(labels = scales::percent, expand = expand_y_std) +
    scale_fill_manual(values = pal20_named, breaks = age_levels, labels = disp_labels,
                      guide = guide_legend(ncol = 2, title = "年齢階級")) +
    theme_std
  
  if (any(is.na(df20_long$age_bin))) {
    warning("age_bin に NA。列名と levels の不一致の可能性。levels=",
            paste(age_levels, collapse = ", "))
  }
  
  # ─────────────────────────────────────────────
  # 6) 高齢化率 & 関連比率（個別コントロール対応）
  # ─────────────────────────────────────────────
  style_default <- list(
    point_color   = "#000000",
    point_fill    = "#000000",
    point_shape   = 21,
    point_size    = 3.0,
    point_stroke  = 1.0,
    line_color    = "#BB0000",
    line_width    = 1.5,
    alpha_points  = 1.0,
    highlight_latest  = TRUE,
    latest_size       = 5.0,
    latest_shape      = 21,
    latest_color      = "#BB0000",
    latest_fill       = "#BB0000",
    latest_stroke     = 0,
    highlight_extrema = FALSE,
    extrema_shape     = 24,
    extrema_size      = 4.2,
    max_color         = "#E86A10",
    min_color         = "#006EBB",
    label_mode        = "latest",
    label_n_digits    = 1
  )
  style_ratio_ageing    <- style_default
  style_ratio_late      <- style_default
  style_ratio_dep       <- style_default
  style_ratio_dep_youth <- style_default
  style_ratio_dep_old   <- style_default
  
  yscale_opts_default <- list(step=5, pad=1.10, min_limit=0, max_limit=120)
  yscale_opts <- list(
    ageing    = yscale_opts_default,
    late      = yscale_opts_default,
    dep       = yscale_opts_default,
    dep_youth = yscale_opts_default,
    dep_old   = yscale_opts_default
  )
  
  df <- df |>
    dplyr::mutate(
      LateArate = as.numeric(mat_late_ageing[, area_name]),
      DepRatio  = as.numeric(mat_dependency_ratio[, area_name]),
      YouthDep  = as.numeric(mat_youth_dependency_ratio[, area_name]),
      OldDep    = as.numeric(mat_oldage_dependency_ratio[, area_name])
    )
  
  build_rate_plot <- function(df, yvec, title, ylab, style, yscale_args) {
    d <- df |> dplyr::mutate(y = {{ yvec }})
    latest_year <- tail(years, 1)
    d <- d |> dplyr::mutate(
      is_latest = (as.character(year) == as.character(latest_year)),
      is_max    = (y == max(y, na.rm = TRUE)),
      is_min    = (y == min(y, na.rm = TRUE))
    )
    fmt_pct <- function(x) scales::number(x, accuracy = 1/10^style$label_n_digits, suffix = "%")
    label_mask <- dplyr::case_when(
      style$label_mode == "none"   ~ FALSE,
      style$label_mode == "latest" ~ d$is_latest,
      TRUE                         ~ TRUE
    )
    d_lab <- d[label_mask, , drop = FALSE]
    d_lab$lab <- fmt_pct(d_lab$y)
    
    top <- auto_rate_scale(d$y,
                           step = yscale_args$step,
                           pad  = yscale_args$pad,
                           min_limit = yscale_args$min_limit,
                           max_limit = yscale_args$max_limit)
    
    ggplot(d, aes(x = year, y = y, group = 1)) +
      geom_line(linewidth = style$line_width, color = style$line_color) +
      geom_point(size=style$point_size, shape=style$point_shape, stroke=style$point_stroke,
                 color=style$point_color, fill=style$point_fill, alpha=style$alpha_points) +
      { if (style$highlight_latest) geom_point(
        data  = subset(d, is_latest),
        size  = style$latest_size,
        shape = style$latest_shape,
        stroke= style$latest_stroke,
        color = style$latest_color,
        fill  = style$latest_fill
      ) else NULL } +
      { if (style$highlight_extrema) list(
        geom_point(data=subset(d, is_max), size=style$extrema_size, shape=24, stroke=.8,
                   color=style$max_color, fill=style$max_color),
        geom_point(data=subset(d, is_min), size=style$extrema_size, shape=25, stroke=.8,
                   color=style$min_color, fill=style$min_color)
      ) else NULL } +
      { if (style$label_mode!="none") geom_text(data=d_lab, aes(label=lab), vjust=-0.6, size=3.5) else NULL } +
      labs(title = title, y = ylab, x = NULL) +
      scale_x_discrete(expand = expand_x_std) +
      scale_y_continuous(breaks = top$breaks, limits = top$limits, expand = expand_y_std) +
      theme_std
  }
  
  ratio_ageing <- build_rate_plot(
    df, Arate,
    paste0(area_name, "：高齢化率の推移 ", start_year, "〜", end_year, "（", base_area, "）"),
    "高齢化率（%）", style_ratio_ageing, yscale_opts$ageing
  )
  ratio_late <- build_rate_plot(
    df, LateArate,
    paste0(area_name, "：後期高齢化率（75歳以上/総人口）", start_year, "〜", end_year, "（", base_area, "）"),
    "後期高齢化率（%）", style_ratio_late, yscale_opts$late
  )
  ratio_dep <- build_rate_plot(
    df, DepRatio,
    paste0(area_name, "：従属人口比率（年少+老年）/生産年齢 ", start_year, "〜", end_year, "（", base_area, "）"),
    "従属人口比率（%）", style_ratio_dep, yscale_opts$dep
  )
  ratio_dep_youth <- build_rate_plot(
    df, YouthDep,
    paste0(area_name, "：年少人口従属比率（0〜14歳/15〜64歳）", start_year, "〜", end_year, "（", base_area, "）"),
    "年少人口従属比率（%）", style_ratio_dep_youth, yscale_opts$dep_youth
  )
  ratio_dep_old <- build_rate_plot(
    df, OldDep,
    paste0(area_name, "：老年人口従属比率（65歳以上/15〜64歳）", start_year, "〜", end_year, "（", base_area, "）"),
    "老年人口従属比率（%）", style_ratio_dep_old, yscale_opts$dep_old
  )
  
  col_ageing <- "#BB0000"; col_late <- "#7E3AF2"; col_dep <- "#333333"; col_ydep <- "#2CA02C"; col_odep <- "#006EBB"
  
  df_rates_long <- df |>
    dplyr::select(
      year,
      `高齢化率` = Arate,
      `後期高齢化率` = LateArate,
      `従属人口比率` = DepRatio,
      `年少人口従属比率` = YouthDep,
      `老年人口従属比率` = OldDep
    ) |>
    tidyr::pivot_longer(-year, names_to = "metric", values_to = "value")
  
  all_rate_scale <- auto_rate_scale(df_rates_long$value, step = 5, pad = 1.10, min_limit = 0, max_limit = 100)
  
  ratio_all <- ggplot(df_rates_long, aes(x = year, y = value, color = metric, group = metric)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2.6) +
    scale_color_manual(
      values = c(
        "高齢化率" = col_ageing,
        "後期高齢化率" = col_late,
        "従属人口比率" = col_dep,
        "年少人口従属比率" = col_ydep,
        "老年人口従属比率" = col_odep
      )
    ) +
    labs(
      title = paste0(area_name, "：主要人口比率の推移（まとめ） ", start_year, "〜", end_year, "（", base_area, "）"),
      y = "比率（%）", x = NULL, color = "指標"
    ) +
    scale_x_discrete(expand = expand_x_std) +
    scale_y_continuous(breaks = all_rate_scale$breaks, limits = all_rate_scale$limits, expand = expand_y_std) +
    theme_std
  
  # 返り値（意味のあるキー名）
  list(
    pop_00_04       = pop_00_04,        # 0〜4歳人口推移
    pop_00_14       = pop_00_14,        # 0〜14歳人口推移
    pop_15_64       = pop_15_64,        # 15〜64歳人口推移
    pop_65_plus     = pop_65_plus,      # 65歳以上人口推移
    pop_age3_count  = pop_age3_count,   # 年齢3区分総人口（人数）
    pop_age3_share  = pop_age3_share,   # 年齢3区分構成比（%）
    pop_age20_count = pop_age20_count,  # 年齢20区分総人口（人数）
    pop_age20_share = pop_age20_share,  # 年齢20区分構成比（%）
    ratio_ageing    = ratio_ageing,     # 高齢化率
    ratio_late      = ratio_late,       # 後期高齢化率
    ratio_dep       = ratio_dep,        # 従属人口比率
    ratio_dep_youth = ratio_dep_youth,  # 年少人口従属比率
    ratio_dep_old   = ratio_dep_old,    # 老年人口従属比率
    ratio_all       = ratio_all         # 比率まとめ（オーバーレイ）
  )
}



#以下は最新年で各地区のデータを比べる。同じ図表に落とす。
# ③ 年の数
analysis_period <- length(years)
latest_Arate = mat_ageing[analysis_period, ]

df <- data.frame(
  area = names(latest_Arate),
  Arate = as.numeric(latest_Arate)
)
df$fill_color <- ifelse(df$Arate > 50, "#AD000E", "#0B318F")
# 先頭の地区名を取得（=「三ヶ日地区」の代わり）
special_area <- names(latest_Arate)[1]

# その地区以外をメインにして降順ソート
df_main <- df[df$area != special_area, ]
df_main <- df_main[order(-df_main$Arate), ]

# 特別扱いする先頭の地区を取り出す
df_special <- df[df$area == special_area, ]

# 並び順を再構成
df_ordered <- rbind(df_main, df_special)
area_levels <- df_ordered$area

# 並び順を factor に適用
df$area <- factor(df$area, levels = area_levels)


latest_arate_plot <- function() {
  # （ここに現在 p7 を作っている前処理〜ggplot本体までをそのまま移す）
  # 例：analysis_period, latest_Arate, df 作成、並び順決定、ggplot(...) など
  p7 <- ggplot(df, aes(x = area, y = Arate, fill = fill_color)) +
    geom_bar(stat = "identity", colour = 1, linewidth = linewidth_std, width = width_std_latest) +
    labs(title = paste0("各町字の高齢化率 ", end_year, "（", base_area, "）"),
         y = "高齢化率（%）") +
    scale_x_discrete(expand = expand_x_std_latest) +
    scale_fill_identity() +
    scale_y_continuous(
      breaks = latest_arate_breaks,
      limits = latest_arate_limits,
      labels = scales::comma,
      expand = expand_y_std_latest
    ) +
    theme_std +
    theme(axis.text.x = element_text(size = latest_area_label_size))
  
  return(p7)
}

latest_total <- mat_TOT[analysis_period, ]

df2 <- data.frame(
  area  = names(latest_total),
  total = as.numeric(latest_total)
)

# 先頭の地区を完全に削除（再投入しない）
if (nrow(df2) >= 1) df2 <- df2[-1, ]

# 不要データの除去（安全策）
df2 <- df2[!is.na(df2$total), ]

# 並び順：総人口降順
df2 <- df2[order(-df2$total), ]
df2$area <- factor(df2$area, levels = df2$area)

# パラメータ（調整可）
width_std_latest_pop       <- 0.5
latest_area_label_size_pop <- 5
expand_x_std_latest_pop    <- expansion(mult = c(0.04, 0.04))
expand_y_std_latest_pop    <- expansion(mult = c(0.04, 0.04))

latest_total_pop_breaks <- pretty(c(0, max(df2$total, na.rm = TRUE)), n = 6)
latest_total_pop_limits <- c(0, max(latest_total_pop_breaks))

latest_total_pop_plot <- function() {
  p8 <- ggplot(df2, aes(x = area, y = total)) +
    geom_bar(
      stat = "identity",
      fill = "#0B318F",
      colour = 1,
      linewidth = linewidth_std,
      width = width_std_latest_pop
    ) +
    labs(
      title = paste0("各町字の人口 ", end_year, "（", base_area, "）"),
      y = "人口（人）"
    ) +
    scale_x_discrete(expand = expand_x_std_latest_pop) +
    scale_y_continuous(
      breaks = latest_total_pop_breaks,
      limits = latest_total_pop_limits,
      labels = scales::comma,
      expand = expand_y_std_latest_pop
    ) +
    theme_std +
    theme(axis.text.x = element_text(size = latest_area_label_size_pop))
  
  return(p8)
}


latest_total_pie_plot <- function() {
  df_pie <- within(df2, {
    share <- total / sum(total, na.rm = TRUE)
    label <- paste0(area, "\n", scales::percent(share, accuracy = 0.1))
  })
  
  p_pie <- ggplot(df_pie, aes(x = "", y = share, fill = area)) +
    geom_col(width = 1, colour = 1, linewidth = linewidth_std) +
    coord_polar(theta = "y") +
    labs(
      title = paste0("各町字の人口構成比 ", end_year, "（", base_area, "）"),
      fill  = "地区"
    ) +
    theme_void(base_size = latest_area_label_size_pop) +
    theme_std +
    theme(
      legend.title = element_text(size = latest_area_label_size_pop),
      legend.text  = element_text(size = latest_area_label_size_pop),
      plot.title   = element_text(hjust = 0.5)
    )
  
  return(p_pie)
}
