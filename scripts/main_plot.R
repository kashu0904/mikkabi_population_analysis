# ======================================================================
# main_plot.R  — 実行用スクリプト（機能は変更せず、操作を明確化）
# ----------------------------------------------------------------------
# 1) 下の「基本操作」だけ触ればOK。必要なら「詳細設定（任意）」で微調整。
# 2) カスタム目盛は custom_scales で area_name ごとに指定（例あり）。
# ======================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
})

# ----------------------------------------------------------------------
# 基本操作
# ----------------------------------------------------------------------
# ① データ対象（Excelの接頭）を指定： 'mikkabi' / 'hosoe' / 'inasa' など
area_name <- "mikkabi"

# ② 共通モジュールを読み込み（相対パス）
source(here::here("lib", "plot_population_by_area_common.R"), encoding = "UTF-8")

# ③ 描画したい地区（町字）を選ぶ
#    ※ 一覧は plot_population_by_area_common.R 内で読み込まれる NAMES_area を参照
target_area <- "三ヶ日地区"

# ----------------------------------------------------------------------
# 詳細設定（任意）— 最新年の比較図の見た目（ユーザー要望のまとめ）
# ----------------------------------------------------------------------
width_std_latest       <- 0.5
latest_area_label_size <- 5
latest_arate_breaks    <- seq(0, 60, by = 5)
latest_arate_limits    <- c(0, 75.0)
expand_x_std_latest    <- expansion(mult = c(0.04, 0.04))  # X左右余白
expand_y_std_latest    <- expansion(mult = c(0.04, 0.04))  # Y上下余白

# ----------------------------------------------------------------------
# 【custom_scales HOWTO（main_plot.R 側で設定）】
#  - 面ごと（area_name ごと）に Y軸の breaks/limits を固定したいときに使います。
#  - 指定しない指標は自動スケール（auto_scale/auto_rate_scale）を採用します。
#  - 指標名は: age0004 / age0014 / age1564 / age65up / total / Arate
#    ※ Arate は高齢化率（%）。数値類は「人口（人）」です。
#
#  例: main_plot.R で次のように定義してください。
#  --------------------------------------------------------------------
#  custom_scales <- list(
#    "引佐町四方浄" = list(
#      age0004 = list(breaks = seq(0, 80, by = 10), limits = c(0, 80)),
#      total   = list(breaks = seq(0, 1200, by = 200), limits = c(0, 1200)),
#      Arate   = list(breaks = seq(0, 60, by = 5),   limits = c(0, 61.5))
#    ),
#    "三ヶ日町三ヶ日" = list(
#      age65up = list(breaks = seq(0, 900, by = 100), limits = c(0, 900))
#    )
#  )
#  --------------------------------------------------------------------
#  ※ main_plot.R 側で `custom_scales` を定義しなかった場合は、自動スケールのみで描画します。
# ----------------------------------------------------------------------
custom_scales <- list(
  # 例1: 引佐町四方浄の一部指標を固定
  "引佐町四方浄" = list(
    age0004 = list(breaks = seq(0, 80, by = 10),  limits = c(0, 80)),
    total   = list(breaks = seq(0, 1200, by = 200), limits = c(0, 1200)),
    Arate   = list(breaks = seq(0, 60, by = 5),   limits = c(0, 61.5))
  ),
  # 例2: 別エリアで 65歳以上だけ固定
  "三ヶ日町三ヶ日" = list(
    age65up = list(breaks = seq(0, 900, by = 100), limits = c(0, 900))
  )
)

#----------------------------------------
# Arate（％）専用 auto scale
#----------------------------------------
# auto_scale: データ最大値にパディングをかけ、いい感じの目盛＆上限を返す
# x   : 数値ベクトル（例 df$age0004）
# n   : 目盛の本数目安（大きいほど細かい）
#   pad_brk  - 目盛生成用にかける倍率（例1.05 → データ最大の105%でpretty）
#   pad_lim  - limits用にかける倍率（例0.95 → データ最大の95%を上限に）
auto_rate_scale <- function(x, step = 5, pad = 1.10, min_limit = 0, max_limit = 50) {
  # step: ％刻み幅
  # pad : 自動スケーリング時の倍率
  # min_limit: Y軸の下限（デフォルト0）
  # max_limit: 手動で指定したい上限（NULLなら自動）
  mx <- max(x, na.rm = TRUE)
  # 自動上限候補（手動指定がない場合）
  top_auto <- ceiling(mx / step) * step * pad
  top <- if (!is.null(max_limit)) max_limit else top_auto
  brks <- seq(min_limit, top, by = step)
  list(breaks = brks, limits = c(min_limit, top))
}

# ----------------------------------------------------------------------
# 実行
# ----------------------------------------------------------------------
plots <- plot_for_area("三ヶ日地区")

# 必要な図だけ print() してください

print(plots$pop_00_04)       # 0〜4歳人口推移
print(plots$pop_00_14)       # 0〜14歳人口推移
print(plots$pop_15_64)       # 15〜64歳人口推移
print(plots$pop_65_plus)     # 65歳以上人口推移
print(plots$pop_age3_count)  # 年齢3区分総人口（人数）
print(plots$pop_age3_share)  # 年齢3区分構成比（%）
print(plots$pop_age20_count) # 年齢20区分総人口（人数）
print(plots$pop_age20_share) # 年齢20区分構成比（%）
print(plots$ratio_ageing)    # 高齢化率
print(plots$ratio_late)      # 後期高齢化率
print(plots$ratio_dep)       # 従属人口比率
print(plots$ratio_dep_youth) # 年少人口従属比率
print(plots$ratio_dep_old)   # 老年人口従属比率
print(plots$ratio_all)       # 比率まとめ（オーバーレイ）

