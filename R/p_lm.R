
#' Get p values of linear model
#'
#' @param model A linear model fitted via \code{lm}.
#'
#' @return Named numeric vector containing p-values of the model, i.e., the overall p-value of the F-statistic,
#'         and the p-values of the intercept and coefficients.
#'         For naming convention of output see example.
#' @export
#'
#' @examples
#'
#' x = rnorm(42)
#' y = x + rnorm(42)
#' m = lm(y ~ x)
#' p_lm(m)
#' #      p_model  p_intercept      p_slope
#' # 9.010565e-11 3.330814e-01 9.010565e-11

p_lm = function(model) {
  sm = summary(model)
  # first code block is an adapted a copy from
  # https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
  f_stats = sm$fstatistic
  p_model = pf(f_stats[1], f_stats[2], f_stats[3], lower.tail = FALSE)
  names(p_model) = "p_model"

  p_coeff = sm$coefficients[, "Pr(>|t|)"]
  names(p_coeff) = c("p_intercept", "p_slope") #paste0("p_", gsub("^\\(Intercept\\)$", "intercept", names(p_coeff)))

  c(p_model, p_coeff)
}

