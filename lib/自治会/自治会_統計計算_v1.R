# =============================================================================
# 自治会_統計計算_v1.R
#   - 自治会データの統計計算（母集団/標本）
#   - 図表ロジックから切り離して再利用しやすくする
#
# 重要:
#   - ここでいう「母集団SD」は分母 n を用いる（1/n）
#   - 「標本SD」は stats::sd()（分母 n-1）
# =============================================================================

sd_population_v1 <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n <= 0) return(NA_real_)
  m <- mean(x)
  sqrt(mean((x - m)^2))
}

cv_v1 <- function(x, mode = c("population", "sample"), na.rm = TRUE) {
  mode <- match.arg(mode)
  if (na.rm) x <- x[!is.na(x)]
  m <- mean(x)
  if (!is.finite(m) || is.na(m) || m == 0) return(NA_real_)
  s <- if (identical(mode, "population")) {
    sd_population_v1(x, na.rm = FALSE)
  } else {
    stats::sd(x)
  }
  s / m
}
skewness_v1 <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd_population_v1(x, na.rm = FALSE)
  if (!is.finite(s) || is.na(s) || s == 0) return(NA_real_)
  mean((x - m)^3) / (s^3)
}
