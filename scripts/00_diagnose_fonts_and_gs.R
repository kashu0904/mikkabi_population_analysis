# =============================================================================
# 00_diagnose_fonts_and_gs.R
#   フォント・Ghostscript（embedFonts用）の最低限チェック
# =============================================================================

cat("R:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n\n")

# ---- フォント候補 ------------------------------------------------------------
if (requireNamespace("systemfonts", quietly = TRUE)) {
  cat("[systemfonts] available\n")
  # 既に知ってる候補をチェック（必要に応じて追加）
  candidates <- c("Noto Sans JP", "Noto Sans CJK JP", "Yu Gothic", "Meiryo", "MS Gothic")
  for (f in candidates) {
    m <- systemfonts::match_font(f)
    ok <- !is.null(m$path) && nzchar(m$path)
    cat(sprintf("  %-20s -> %s\n", f, if (ok) m$path else "NOT FOUND"))
  }
} else {
  cat("[systemfonts] NOT installed (install.packages('systemfonts'))\n")
}

cat("\n")

# ---- Ghostscript -------------------------------------------------------------
# embedFonts() は Ghostscript が必要
gs <- Sys.which(c("gswin64c", "gswin32c", "gs"))
cat("[Ghostscript] which:\n")
print(gs)

cat("\nIf embedFonts fails, try:\n")
cat('  - Install Ghostscript and put "gswin64c.exe" in PATH\n')
cat('  - Or set Sys.setenv(R_GSCMD="C:/path/to/gswin64c.exe")\n')
