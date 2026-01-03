# =============================================================================
# 区×地域のフラット表を作る（区情報を同じ行に複製）
#
# 前提:
#   - scripts/自治会/aggregate_jichikai_metrics_2025.R がプロジェクト内にある
#   - 集計対象CSV: data/自治会/2025/自治会世帯数__2025.csv
#
# 出力:
#   - ward_region_flat（tibble）
#   - 必要ならCSV保存（下の write_csv をON）
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# ---- プロジェクトルート検出（aggregate スクリプトと同等） --------------------
find_project_root <- function(start_dir = NULL, max_up = 6) {
  is_root <- function(p) {
    has_data <- dir.exists(file.path(p, "data"))
    has_scripts <- dir.exists(file.path(p, "scripts"))
    has_rproj <- length(list.files(p, pattern = "\\.Rproj$", ignore.case = TRUE)) > 0
    (has_data && has_scripts) || has_rproj
  }

  env_root <- Sys.getenv("PROJECT_ROOT", unset = NA_character_)
  if (!is.na(env_root) && nzchar(env_root) && dir.exists(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  if (is.null(start_dir) || !nzchar(start_dir)) start_dir <- getwd()
  p <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  for (i in 0:max_up) {
    if (dir.exists(p) && is_root(p)) return(p)
    parent <- normalizePath(file.path(p, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, p)) break
    p <- parent
  }

  stop("プロジェクトルートを自動検出できませんでした。PROJECT_ROOT を環境変数で指定してください。", call. = FALSE)
}

guess_start_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) NA_character_)
    if (!is.na(p) && nzchar(p)) return(dirname(p))
  }
  getwd()
}

PROJECT_ROOT <- find_project_root(guess_start_dir())
message("[INFO] PROJECT_ROOT = ", PROJECT_ROOT)

# ---- 集計スクリプトをsource --------------------------------------------------
agg_path <- file.path(PROJECT_ROOT, "scripts", "自治会", "aggregate_jichikai_metrics_2025.R")
if (!file.exists(agg_path)) stop(paste0("集計スクリプトが見つかりません: ", agg_path), call. = FALSE)

res <- source(agg_path, local = TRUE)$value

# ---- 入力（日本語列） --------------------------------------------------------
ward_tbl   <- res$区_集計
region_tbl <- res$区_地域_集計

# ---- フラット表（区情報を同一行に複製） --------------------------------------
# 目的:
#   - 区×地域 の各行に、区側の「地域数」「地域一覧」などを付与する
#   - 地域側の「地区数」「地区名一覧」「自治会数」「自治会名一覧」も併載する
#
# 出力列（日本語）:
#   区, 地域, 区内_地域数, 区内_地域一覧, 地域内_地区数, 地域内_地区名一覧, 地域内_自治会数, 地域内_自治会名一覧

ward_region_flat <- region_tbl %>%
  left_join(
    ward_tbl %>%
      transmute(
        区,
        区内_地域数   = 地域数,
        区内_地域一覧 = 地域一覧
      ),
    by = "区"
  ) %>%
  transmute(
    区,
    地域,
    区内_地域数,
    区内_地域一覧,
    地域内_地区数      = 地区数,
    地域内_地区名一覧  = 地区名一覧,
    地域内_自治会数    = 自治会数,
    地域内_自治会名一覧 = 自治会名一覧
  ) %>%
  arrange(区, 地域)

ward_region_flat

# ---- 保存（必要ならON） ------------------------------------------------------
# out_dir <- file.path(PROJECT_ROOT, "data", "自治会", "2025", "derived")
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# write_csv(ward_region_flat, file.path(out_dir, "区_地域_フラット表.csv"))
