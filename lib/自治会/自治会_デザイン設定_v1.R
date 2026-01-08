# =============================================================================
# 自治会_デザイン設定_v1.R
#   デザイン・出力・地域名対応・未知地域の扱いを一括設定
#
# 目的
#   - ggplot → Illustrator 編集の前段として、R側で決めるべき見た目/出力条件を1箇所に集約
#   - Plot pane での試行錯誤（サイズ/余白/フォント/色/ラベル）を素早く回す
#   - SVG/PDF/EPS/PNG を同条件で比較できるようにする
#
# 重要仕様（合意済み）
#   - bar と pie で書き出しサイズを別指定（barは自治会数レンジ別、pieは常に7×7）
#   - PDF/EPS は embedFonts() を必ず試行（失敗しても続行。失敗内容はログへ）
#   - 未定義 region_key は「対話実行: 選択UI」「非対話: configで許可が無ければ停止」
#   - 地域フォルダ名は日本語（例：北地域）
#   - 円グラフのラベルは「降順 上位5」のみ
# =============================================================================

JICHAIKAI_PLOT_CFG_V1 <- list(

  # ---------------------------
  # 0) 地域名対応表（region_key -> 日本語地域名）
  # ---------------------------
  region = list(
    # 例: region_key が "kita" の場合、"北地域" というフォルダ名・出典表示に使う
    map = c(
      kita   = "北地域",
      higashi= "東地域",
      nishi  = "西地域",
      minami = "南地域",
      naka   = "中地域",
      hamakita = "浜北地域",
      tenryu   = "天竜地域"
    ),

    # 未定義 region_key の扱い
    # - interactive() == TRUE  : 選択UIを出す（停止/続行）
    # - interactive() == FALSE : ここが TRUE なら続行、FALSE なら stop
    allow_unknown_noninteractive = FALSE,

    # 未定義のときに続行した場合の日本語地域名フォルダ
    # 例: 未定義地域_kita2
    unknown_prefix = "未定義地域_"
  ),

  # ---------------------------
  # 1) フォント（最重要）
  # ---------------------------
  font = list(
    family = "Noto Sans JP",
    fallback_family = "Yu Gothic",
    base_size = 11,
    title_add = 4,
    bold_face = "bold"
  ),

  # ---------------------------
  # 2) 共通レイアウト
  # ---------------------------
  layout = list(
    margin = c(15, 25, 15, 50),
    plot_margin = c(15, 20, 15, 50),

    # キャプション（出典）
    caption_hjust = 1,
    caption_size_ratio = 0.65,
    caption_color = "grey30",
    caption_vjust = 0,
    caption_position = "plot",
    caption_margin = c(5, -10, -5, 0)
  ),

  # ---------------------------
  # 3) 横棒グラフ設定
  # ---------------------------
  bar = list(
    # fill_mode: "legacy"(ggplot既定) / "single"(単色) / "palette"(手動パレット)
    fill_mode = "legacy",
    bar_color = "#005686",
    bar_palette = c(
      "#005686", "#0072B2", "#009E73", "#E69F00", "#D55E00",
      "#CC79A7", "#56B4E9", "#F0E442"
    ),

    # 棒の太さ（デフォルト）
    bar_width = 0.65,

    # 順位の出す側: "right" or "left"
    rank_side = "right",
    rank_push_ratio = 0.30,
    rank_left_ratio = 0.05,
    rank_hjust = 0.5,

    # 値ラベル
    value_hjust = -0.06,
    value_x_mode = "bar_end",     # "bar_end" / "fixed"
    value_x_ratio = 0.00,

    # X軸余白（左,右）
    x_expand = c(0.02, -0.04),
    # Y軸の余白（下,上）…棒の上下の空き（離散軸の expand）
    # - 下を増やす: 一番下の棒が窮屈ならここを上げる
    # - 上を増やす: 一番上の棒が窮屈ならここを上げる
    y_expand = c(0.025, 0.025),

    # ===== X軸（世帯数側）制御 =====
    # ※棒の長さ（世帯数）の軸。数字ラベルと目盛りの見た目をここで制御。
    x_axis_line  = FALSE,
    x_axis_ticks = TRUE,
    x_axis_line_color = "grey60",
    x_axis_line_size  = 0.2,
    x_axis_tick_color = "grey60",
    x_axis_tick_size  = 0.5,
    # 目盛りの長さ（pt）…X/Y共通（ggplot2 の axis.ticks.length）
    axis_tick_length_pt = 3.0,
    x_axis_text_color = "grey40",
    x_axis_text_size  = NA,   # NAなら font$base_size を使う
    x_axis_title_color = "grey40",
    x_axis_title_size  = NA,  # NAなら font$base_size を使う


    # 補助グリッド分割
    x_minor_div = 2,

    # バー枠線
    bar_border_color = "black",
    bar_border_size  = 0.1,
    bar_border_alpha = 1.0,
    # 棒の透明度（1.0=不透明）。NULL だと ggplot がエラーになるため必ず数値で持つ
    bar_alpha = 1.0,

    # 文字サイズ（デフォルト）
    #  - name_size : Y軸（自治会名）
    #  - value_size: 値ラベル（世帯・割合）
    #  - rank_size : 順位ラベル
    name_size  = 3.1,
    value_size = 2.8,
    rank_size  = 2.8,

    # Y軸（自治会名）
    y_axis_label_face = "bold",
    name_color = "#000000",
    # 自治会名を 0 から「ほんの少し左」に逃がす（最大世帯数×比率）
    # 例：0.015 → 最大 2000 世帯なら 30 だけ左へ
    # ※ 文字が外にはみ出す場合は layout$margin の left（4番目）を増やすか、この値を下げる
    name_pad_mult = 0.040,

    # ===== Y軸（自治会名側）制御 =====
    # ※軸ラベル（axis.text.y）は使っていないので、ここは主に「目盛り/軸線の見た目」確認用。
    y_axis_line  = FALSE,
    y_axis_ticks = TRUE,
    y_axis_line_color = "grey20",
    y_axis_line_size  = 0.30,
    y_axis_tick_color = "grey20",
    y_axis_tick_size  = 0.30,

    y_axis_text_color = NULL,
    y_axis_text_size  = NA,

    # 背景
    panel_bg = "#E6E6E6",
    grid_x_color = "white",
    grid_x_lwd = 0.20,
    # X方向の補助グリッド（minor）
    #  - TRUE で表示 / FALSE で非表示
    #  - *_color / *_lwd が NULL の場合は既定値にフォールバック
    grid_x_minor_show  = TRUE,
    grid_x_minor_color = NULL,
    grid_x_minor_lwd   = NULL,

    grid_y_color = "white",
    grid_y_lwd = 0.30,
    grid_y_color = "white",
    grid_y_lwd = 0.30,
    
    # ===== バー領域（panel）の外枠 =====
    
    panel_frame_color = "grey50",
    panel_frame_size  = 0.2,
    
    # ---- 平均/中央値 参照線（縦線） ------------------------------------------
    stat_label_value_accuracy = 1,   # {value} の丸め（1=整数表示。小数出したければ 0.1 など）
    stat_label_digits = 2,          # {value} を必ず小数点以下2桁で表示（例: 120.00）

    # 変動係数（CV）
    cv_show = TRUE,
    cv_label_text = "変動係数（CV） {value}",
    cv_label_digits = 3,
    cv_label_color = "grey30",
    cv_label_size  = NA,
    cv_label_hjust = 1,
    cv_label_vjust = 1.2,
    cv_label_x_nudge_ratio = 0.02,
    
    mean_line_show     = TRUE,
    mean_line_color    = "grey30",
    mean_line_size     = 0.6,
    mean_line_linetype = "solid",
    
    mean_label_show         = TRUE,
    mean_label_text         = "平均 {value}",   # {value} を数値に置換
    mean_label_color        = NULL,             # NULLなら line_color を使う
    mean_label_size         = 3.0,
    mean_label_hjust        = 0,
    mean_label_vjust        = -0.8,
    mean_label_x_nudge_ratio = 0.01,            # x方向に少し右へ（x幅に対する比率）
    
    median_line_show     = TRUE,
    median_line_color    = "grey30",
    median_line_size     = 0.6,
    median_line_linetype = "dashed",
    
    median_label_show          = TRUE,
    median_label_text          = "中央値 {value}",
    median_label_color         = NULL,
    median_label_size          = 3.0,
    median_label_hjust         = 0,
    median_label_vjust         = -1.8,          # 平均ラベルと被るならここを変える
    median_label_x_nudge_ratio = 0.01
    
  ),

  # ---------------------------
  # 4) 円グラフ設定
  # ---------------------------
  pie = list(
    fill_mode = "legacy",
    palette = c(
      "#005686", "#0072B2", "#009E73", "#E69F00", "#D55E00",
      "#CC79A7", "#56B4E9", "#F0E442"
    ),
    reverse_levels = TRUE,

    # ラベル
    label_x = 0.77,
    label_x_01_05 = 0.60,    # 1〜5自治会のとき
    label_x_06_10 = 0.65,
    
    label_size = 4.85,
    label_lineheight = 0.98,
    label_top_n = 5,      # ★統一：上位5のみ表示

    # 区切り線
    slice_border_color = "white",
    slice_border_lwd = 0.4,


    # 自治会数レンジ別（任意）
    slice_border_lwd_40_50 = NULL,
    slice_border_lwd_50_plus = NULL,

    # 余白
    margin = c(15, 30, 15, 25)
  ),

  # ---------------------------
  # 5) 書き出し
  # ---------------------------
  export = list(
    # 出力形式
    formats = c("pdf", "png", "svg", "eps"),

    # bar のデフォルト書き出しサイズ（インチ）
    bar_w_default = 7,
    bar_h_default = 6,

    # pie は常に 7x7
    pie_w = 7,
    pie_h = 7,

    # PNG
    png_dpi = 300,
    ragg_scaling = 1,

    # PDF/EPS: embedFonts を必ず試行する（失敗しても止めない）
    embed_fonts_always = TRUE
  ),

  # ---------------------------
  # 6) bar のプロフィール（自治会数レンジ別）
  # ---------------------------
  bar_profiles = list(
    # デフォルト
    default = list(
      id = "default",
      folder = "default__w7h7",
      bar_width = NULL,
      w = 7.5,
      h = NULL,
      name_size = NULL,
      value_size = NULL,
      rank_size = NULL,
      x_expand = NULL,
      y_expand = NULL
    ),
    
    # 1〜5
    n01_05 = list(
      id = "n01_05",
      folder = "n01_05__w7h4__bw0.5",
      bar_width = 0.28,
      w = 7.5,
      h = 3,
      name_size = NULL,
      value_size = NULL,
      rank_size = NULL,
      x_expand = NULL,
      y_expand = c(0.200, 0.200)
    ),

    # 6〜10
    n06_10 = list(
      id = "n06-10",
      folder = "n06-10__w7h3__bw0.5",
      bar_width = 0.38,
      w = 7.5,
      h = 4,
      name_size = NULL,
      value_size = NULL,
      rank_size = NULL,
      x_expand = NULL,
      y_expand = c(0.100, 0.100)
    ),

    # 11〜15
    n11_15 = list(
      id = "n11-15",
      folder = "n11-15__w7h5__bw0.5",
      bar_width = 0.45,
      w = 7.5,
      h = 5,
      name_size = NULL,
      value_size = NULL,
      rank_size = NULL,
      x_expand = NULL,
      y_expand = c(0.050, 0.050)
    ),

    # 50以上
    n50_plus = list(
      id = "n50+",
      folder = "n50plus__w7h7__bw0.5__smalltext",
      bar_width = 0.70,
      w = 7.5,
      h = 8,
      name_size  = 1.8,
      value_size = 1.8,
      rank_size  = 1.8,
      x_expand = NULL,
      y_expand = c(0.0125, 0.015)
    )
  ),

  # ---------------------------
  # 7) 表示・記号
  # ---------------------------
  label = list(
    non_corp_mark = "×",
    mark_non_corporate = TRUE
  )
)