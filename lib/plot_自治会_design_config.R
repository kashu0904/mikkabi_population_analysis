# =============================================================================
# plot_自治会_design_config.R
#   ここだけ触ればデザイン全体が変わる「設定ファイル」
#
# 【目的】
#   - ggplot → Illustrator 編集の前段として、"R側で決めるべき見た目" を一箇所に集約
#   - Plot pane での試行錯誤（サイズ/余白/フォント/色/ラベル）を素早く回す
#   - SVG/PDF/EPS/PNG の出力テストを同条件で比較できるようにする
#
# 【使い方】
#   1) scripts/01_test_plotpane_mikkabi.R を走らせる（Plot pane に三ヶ日地区だけ出る）
#   2) このファイルの値をいじる → もう一度 scripts を走らせる
#   3) scripts/02_export_test_formats_mikkabi.R で各形式を書き出して Illustrator で比較
#
# 【重要】フォント名は "Windows と Illustrator が同じ名前で認識しているもの" を指定すること。
#   例: "Noto Sans JP" / "Noto Sans CJK JP" / "Yu Gothic" / "Meiryo" など
# =============================================================================

JICHAIKAI_PLOT_CFG <- list(

  # ---------------------------
  # 1) フォント（最重要）
  # ---------------------------
  font = list(
    # ggplot の base_family（theme_* のベース）
    family = "Noto Sans JP",

    # SVGは「フォントを埋め込まない」ことが多く、PCにフォントが無いと置換で崩れます。
    # PDFは埋め込みに失敗すると同様に崩れます。
    # ここで「確実に入っているフォント」に切り替えて比較するのが研究の第一歩です。
    fallback_family = "Yu Gothic",

    # 基本文字サイズ（plot.title は +title_add で上げる）
    base_size = 11,
    title_add = 4,

    # 太字
    bold_face = "bold"
  ),

  # ---------------------------
  # 2) 共通レイアウト
  # ---------------------------
  layout = list(
    # プロット余白（上,右,下,左）: 単位は pt（ggplot の margin）
    # Illustrator でのズレ検証にも効くので頻繁に触る
    margin = c(15, 25, 15, 0),

    # 外側余白（plot.margin）を別名で触りたい場合（NULLなら margin を使う）
    plot_margin = NULL,

    # キャプション（出典）
    caption_hjust = 1,
    # キャプションの縦位置（vjust）。0=下寄せ / 1=上寄せ（>1で上へ押し上げ）
    caption_vjust = 1.5,
    caption_size_ratio = 0.85,
    caption_color = "grey30",
    # "plot"（プロット全体基準） / "panel"（パネル基準）
    caption_position = "plot",
    # キャプション周りの余白（上,右,下,左）
    caption_margin = c(0, 0, 0, 0)
  ),

  # ---------------------------
  # 3) 横棒グラフ設定
  # ---------------------------
  bar = list(
    # fill_mode:
    #   "single"  : 全バーを単色（bar_color）にする
    #   "palette" : 自治会ごとに色を振る（bar_palette）
    #   "legacy"  : ★旧方式（ggplot2既定の離散配色）を使う
    fill_mode = "legacy",

    # 単色バー色（fill_mode="single" のときだけ効く）
    bar_color = "#005686",

    # palette モードの色（足りないと recycle する。色が重複するのが嫌なら色数を増やす）
    bar_palette = c(
      "#005686", "#0072B2", "#009E73", "#E69F00", "#D55E00",
      "#CC79A7", "#56B4E9", "#F0E442"
    ),

    # 棒の太さ
    bar_width = 0.5,

    # 順位ラベルを出す側: "right" or "left"
    rank_side = "right",

    # 右側に順位を出すときの位置（最大値に対する比率）
    rank_right_ratio = 0.03,

    # 順位ラベルの寄せ（ggplotの hjust）: 1=右寄せ, 0=左寄せ
    rank_hjust = 0.5,

    # 順位ラベルを右余白側へ押し出す量（最大値に対する比率）
    # 例: 0.08 なら x = max*1.08 に置く（ただしスケールは limits で固定）
    rank_push_ratio = 0.19,

    # 旧: rank_nudge_ratio（data単位でスケールが伸びるため非推奨・未使用）
    # rank_nudge_ratio = 0.06,


    # 左の「順位」位置（最大値に対するマイナス比率）
    # 例: 0.05 なら maxv*0.05 だけ左へ出す
    rank_left_ratio = 0.05,

    # 右のラベル位置（hjust を -0.15 などにすると棒の右へ出る）
    value_hjust = -0.06,


    # 値ラベル（世帯・割合）のX位置モード: "bar_end"(棒の終端) / "fixed"(固定列)
    value_x_mode = "bar_end",

    # 固定列のときのX位置（最大値に対する比率）
    value_x_ratio = 0.00,
    # ===== グリッド（縦線）調整 =====
    # 主グリッドの位置は自動（pretty breaks）に任せる。
    # 補助グリッドだけを「主グリッドの分割数」で制御する。
    x_minor_div = 2,

    # グリッド線の見た目（太さ・色）
    grid_major_size = 0.20,
    grid_minor_size = 0.10,
    grid_major_color = "white",
    grid_minor_color = "white",

    # ===== バー枠線 =====
    # 枠線を消したい場合は bar_border_color = NULL
    bar_border_color = "black",
    bar_border_size  = 0.1,
    bar_border_alpha = 1.0,


    # ラベルサイズ（ggplot の size は pt ではなく相対値）
    label_size = 3.8,
    # 文字サイズ（個別調整）
    # NA の場合は label_size を使う
    name_size  = 9,
    value_size = 3.0,
    rank_size  = 2.5,

    # X軸の余白（左,右）…右にラベルが出るので右は大きめに
    x_expand = c(0.02, 0.16),

    # Y軸の余白（下,上）…棒の上下の空き
    y_expand = c(0.05, 0.04),
    # ===== X軸（世帯数側）制御 =====
    x_axis_line  = FALSE,
    x_axis_ticks = TRUE,
    x_axis_line_color = "grey40",
    x_axis_line_size  = 0.4,
    x_axis_tick_color = "grey40",
    x_axis_tick_size  = 0.3,
    x_axis_tick_length_pt = 3.0,
    x_axis_text_color = "grey20",
    x_axis_text_size  = NA,   # NAなら font$base_size を使う
    x_axis_title_color = "grey20",
    x_axis_title_size  = NA,  # NAなら font$base_size を使う

    # Y軸（自治会名）の太さ: "plain" / "bold"
    y_axis_label_face = "bold",
    # 自治会名（Y軸ラベル）の色
    name_color = "#000000",
    # ===== Y軸（自治会名側）制御 =====
    y_axis_line  = FALSE,
    y_axis_ticks = TRUE,
    y_axis_line_color = "grey40",
    y_axis_line_size  = 0.30,
    y_axis_tick_color = "grey40",
    y_axis_tick_size  = 0.30,
    y_axis_tick_length_pt = 3.0,
    y_axis_text_color = NULL,   # NULLなら name_color を使う
    y_axis_text_size  = NA,     # NAなら name_size/label_size を使う


    # グリッド／背景
    panel_bg = "#E6E6E6",
    grid_x_color = "white",
    grid_x_lwd = 0.20,
    grid_y_color = "white",
    grid_y_lwd = 0.20
  ),

  # ---------------------------
  # 4) 円グラフ設定
  # ---------------------------
  pie = list(
    # fill_mode:
    #   "palette" : palette を使って手動配色（既定）
    #   "legacy"  : ★旧方式（ggplot2既定の離散配色）を使う
    fill_mode = "legacy",

    # 円の配色（足りないと recycle。色数不足はすぐ破綻するので増やすのが正解）
    palette = c(
      "#005686", "#0072B2", "#009E73", "#E69F00", "#D55E00",
      "#CC79A7", "#56B4E9", "#F0E442"
    ),

    # 「色順反転」をどう扱うか（現行は factor levels を rev している）
    reverse_levels = TRUE,

    # ラベル位置（x=1.15 など）
    label_x = 1.15,
    label_size = 3.6,
    label_lineheight = 0.95,

    # 区切り線
    slice_border_color = "white",
    slice_border_lwd = 0.5,

    # 余白（右にラベルが出るので右を大きく）
    margin = c(15, 30, 15, 25)
  ),

  # ---------------------------
  # 5) 書き出し（研究用）
  # ---------------------------
  export = list(
    # 書き出しサイズ（インチ）
    w = 7,
    h = 3,

    # PNG 解像度（dpi）
    png_dpi = 300,

    # ragg の scaling（線幅/文字サイズを拡大する。高解像度で小さく見える時に上げる）
    ragg_scaling = 1,

    # PDF/PS で embedFonts を試すか（Ghostscript が必要）
    # embedFonts() は Ghostscript を呼びます。Windows だと PATH に gswin64c が必要になりがちです。
    try_embed_fonts = TRUE
  )
)
