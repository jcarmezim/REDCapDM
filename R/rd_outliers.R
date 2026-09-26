#' Detect Outliers Using the IQR (Tukey) Method
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Flags values of a numeric vector that fall outside the Tukey interquartile-range (IQR) fences (`[Q1 - k*IQR, Q3 + k*IQR]`). Designed to be used directly inside the `expression` argument of [rd_query()] and [rd_event()] so outlier queries can be generated without writing manual thresholds.
#'
#' @param x A numeric vector.
#' @param k Non-negative number controlling the fence width. Default `1.5` (the classic Tukey "outlier" fence); use `3` for the more permissive "far out" fence.
#'
#' @details
#' `NA` values in `x` are always returned as `NA` (consistent with how [rd_query()] handles missing values through other expressions such as `is.na(x)`), so `NA` rows are dropped by the internal `dplyr::filter()` call rather than reported as outliers.
#'
#' If `x` has fewer than 2 non-missing values, the IQR cannot be estimated: the function warns and returns `NA` for every element.
#'
#' @return A logical vector the same length as `x`: `TRUE` where the value lies outside the fences, `FALSE` where it doesn't, and `NA` where `x` is missing.
#'
#' @seealso [outlier_zscore()]
#'
#' @examples
#' outlier_iqr(c(1, 2, 3, 4, 100))
#'
#' \dontrun{
#' # Use directly inside rd_query()
#' result <- rd_query(covican,
#'   variables = "potassium",
#'   expression = "outlier_iqr(x)",
#'   event = "baseline_visit_arm_1",
#'   query_name = "The value is an outlier according to the IQR method"
#' )
#' }
#'
#' @export
outlier_iqr <- function(x, k = 1.5) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 0) {
    stop("`k` must be a single non-negative number.", call. = FALSE)
  }

  if (sum(!is.na(x)) < 2) {
    warning("Not enough non-missing values in `x` to compute IQR fences. Returning NA for every element.", call. = FALSE)
    return(rep(NA, length(x)))
  }

  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7)
  iqr <- q[2] - q[1]
  lower <- q[1] - k * iqr
  upper <- q[2] + k * iqr

  x < lower | x > upper
}

#' Detect Outliers Using the Z-Score Method
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Flags values of a numeric vector whose absolute z-score (`(x - mean(x)) / sd(x)`) exceeds a given threshold. Designed to be used directly inside the `expression` argument of [rd_query()] and [rd_event()] so outlier queries can be generated without writing manual thresholds.
#'
#' @param x A numeric vector.
#' @param threshold Positive number. Values with an absolute z-score greater than `threshold` are flagged. Default `3`.
#'
#' @details
#' `NA` values in `x` are always returned as `NA` (consistent with how [rd_query()] handles missing values through other expressions such as `is.na(x)`), so `NA` rows are dropped by the internal `dplyr::filter()` call rather than reported as outliers.
#'
#' If `x` has fewer than 2 non-missing values, or its standard deviation is `0` (no variability), no z-score can be computed: the function warns and returns `FALSE` (or `NA` where `x` is missing) for every element.
#'
#' @return A logical vector the same length as `x`: `TRUE` where the absolute z-score exceeds `threshold`, `FALSE` where it doesn't, and `NA` where `x` is missing.
#'
#' @seealso [outlier_iqr()]
#'
#' @examples
#' outlier_zscore(c(1, 2, 3, 4, 100))
#'
#' \dontrun{
#' # Use directly inside rd_query()
#' result <- rd_query(covican,
#'   variables = "potassium",
#'   expression = "outlier_zscore(x)",
#'   event = "baseline_visit_arm_1",
#'   query_name = "The value is an outlier according to the z-score method"
#' )
#' }
#'
#' @export
outlier_zscore <- function(x, threshold = 3) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) || threshold <= 0) {
    stop("`threshold` must be a single positive number.", call. = FALSE)
  }

  if (sum(!is.na(x)) < 2) {
    warning("Not enough non-missing values in `x` to compute a z-score. Returning NA for every element.", call. = FALSE)
    return(rep(NA, length(x)))
  }

  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)

  if (is.na(s) || s == 0) {
    warning("The standard deviation of `x` is zero, so no z-score outliers can be computed. Returning FALSE for every non-missing element.", call. = FALSE)
    return(ifelse(is.na(x), NA, FALSE))
  }

  z <- (x - m) / s
  abs(z) > threshold
}
