
#' Get p value of linear model
#'
#' @param model A linear model fitted via \code{lm}.
#'
#' @return The p-value of the model, i.e., the overall p-value of the F-statistic (not the p-value of the individual coefficients).
#' @export
#'
#' @examples
#'
#' x = rnorm(42)
#' y = x + rnorm(42)
#' m = lm(y ~ x)
#' p_lm(m)
#' # [1] 2.783375e-09

p_lm = function(model) {
  # code is a copy of: https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
  f_stats = summary(model)$fstatistic
  p = pf(f_stats[1], f_stats[2], f_stats[3], lower.tail = FALSE)
  names(p) = NULL
  p
}

