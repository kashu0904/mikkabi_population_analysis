# 「浜名区」シートの2025年分だけ抽出（全月 or 最新月のみ）
library(readxl)
library(openxlsx)

# ==== 設定 ====
input_dir    <- "C:/Users/pirat/Documents/Mikkabi_population_analysis/data/raw/Population_By_Town_and_Age/kitaku and hamanaku"
pattern      <- "^jinkousu_areaage_.*\\.(xls|xlsx)$"
target_sheet <- "浜名区"
target_year  <- 2025
latest_only  <- FALSE   # ← 最新月だけ欲しければ TRUE に

# 出力ファイル（latest_onlyで名前を変える）
output_path <- if (latest_only) {
  "C:/Users/pirat/Documents/Mikkabi_population_analysis/data/processed/hamanaku_population_2025_latest.xlsx"
} else {
  "C:/Users/pirat/Documents/Mikkabi_population_analysis/data/processed/hamanaku_population_2025.xlsx"
}

# ==== ファイル列挙 ====
files <- list.files(path = input_dir, pattern = pattern, full.names = TRUE)
if (length(files) == 0) stop("対象ファイルが見つからない。pattern/パスを再確認。")

# ==== 収集 ====
buf <- list()   # シート名(YYYY-MM) → data.frame
meta <- data.frame(sheet="", year=NA_integer_, month=NA_integer_, stringsAsFactors = FALSE)

for (file in files) {
  # ファイル名から元号・年月抽出（例: h31-04- / r06-10-）
  m <- regexec("([hr])([0-9]+)-([0-9]+)-", basename(file))
  g <- regmatches(basename(file), m)[[1]]
  if (length(g) < 4) {
    cat("⚠️ スキップ（日付抽出不可）：", basename(file), "\n")
    next
  }
  era   <- g[2]                # h or r
  y_jpn <- as.integer(g[3])    # 和暦年
  mon   <- as.integer(g[4])    # 月
  
  # 和暦→西暦
  y_ad <- if (era == "h") y_jpn + 1988 else y_jpn + 2018
  if (y_ad != target_year) next
  
  # 該当シート確認
  shs <- excel_sheets(file)
  if (!(target_sheet %in% shs)) {
    cat("⚠️ シートなし：", basename(file), " → ", target_sheet, "\n")
    next
  }
  
  # 取込
  dat <- read_excel(file, sheet = target_sheet)
  sname <- sprintf("%d-%02d", y_ad, mon)
  
  # 同じ月が複数あった場合は“後勝ち”で上書き（運用上問題なければこのまま）
  buf[[sname]] <- dat
  meta <- rbind(meta, data.frame(sheet = sname, year = y_ad, month = mon))
  cat("✅ 読み込み：", basename(file), " → ", target_sheet, " → ", sname, "\n")
}

# 収集ゼロなら終了
if (length(buf) == 0) stop("2025年の「浜名区」シートは見つからなかった。")

# ==== 最新月のみオプション ====
if (latest_only) {
  meta <- meta[complete.cases(meta$month), ]
  last_mon <- max(meta$month, na.rm = TRUE)
  keep_name <- sprintf("%d-%02d", target_year, last_mon)
  buf <- buf[keep_name]
  if (is.null(buf[[1]])) stop("最新月シートの抽出に失敗。内部メタデータを確認。")
}

# ==== シート名を年月順に並べて書き出し ====
ordered_names <- sort(names(buf))
buf <- buf[ordered_names]

write.xlsx(buf, file = output_path)
cat("📤 書き出し完了：", output_path, "（シート：", paste(ordered_names, collapse = ", "), "）\n")
