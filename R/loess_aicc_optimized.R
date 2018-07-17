
#' Fitting of AICC optimized loess (groupwise)
#'
#' Given a data.table, three columns may be specified as dependent variable, independent variable, and group respectively
#' to fit an AICC-optimized-span loess model. The predicted values and upper/lower values of confidence band are added as new columns per group.
#' Alternatively the function may simply return the full optimized model objects per group.
#'
#' For the default confidence interval of \code{0.95}, upper / lower values of confidence band
#' are calculated as \code{model$fit +/- qt(0.25,model$df)*model$se}.
#' This is only an estimation of the true confidence interval based on the standard error, see:
#' https://stackoverflow.com/questions/22717930/how-to-get-the-confidence-intervals-for-lowess-fit-using-r
#'
#' @param x A \code{data.table} with data in long format (use \code{data.table::melt} if your data is in wide format).
#' @param independent_var Name of column containing the independent variable.
#' @param independent_var_range By default \code{loess} is fitted for the full range of the independent variable. A limited range may be specified here, usually an \code{integer} vector.
#' @param dependent_var The data column of the values to be fitted by \code{loess}.
#' @param groups Name of the column containing the names of the groups.
#' @param group_subset By default \code{loess} is fitted for all groups. A subset may be specified as \code{character} vector.
#' @param span_interval The span interval to be considered for AICC optimization of the loess span. By default \code{c(.1,.9)}.
#' @param degree Degree parameter passed to \code{loess}. By default \code{2}.
#' @param family Family parameter passed to \code{loess}. By default \code{"gaussian"}.
#' @param normalize Normalize parameter passed to \code{loess}. By default \code{"TRUE"}.
#' @param confidence_interval The confidence interval for calculating upper/lower values for plotting confidence band. By default \code{0.95}.
#'                            See detail section for details on calculation.
#' @param output "fitted_values" results in new columns added to \code{x} containing fitted values for plotting, etc.
#'               "model" results in return of the full optimized model objects per group.
#'
#' @return The original \code{x} with three additional columns named \code{"aicc_loess_fit"} and \code{"aicc_loess_lwr"}, \code{"aicc_loess_upr"}.
#'         The two latter columns inlcude the upper and lower values of the specified confidence interval.
#'         The entries in these additional columns that correspond to excluded groups or independent variables appear as \code{NA_real_}.
#'
#'         If \code{output = "model"} a two column \code{data.table} is returned containing the group names and associated fitted model object per row.
#' @export
#'
#' @examples
#'
#'
#' # the following example shows how aicc loess
#' # produces a fit that is more sensible to the data
#' # in comparison to standard loess using a span of 0.75
#' # if such less smooth appearance makes sense is, of course, case specific
#'
#'
#' library(data.table)
#' years <- 1990:2010
#' set.seed(42)
#' topics <- c(rep(1, length(years)), rep(2, length(years)))
#' values1 <- jitter(rnorm(length(years), mean = 0.5, sd = .25), amount = pi)
#' values2 <- jitter(rnorm(length(years), mean = 0.25, sd = 0), amount = exp(1))
#' data <- data.table(year = years, topic = topics, value = c(values1, values2))
#'
#' # first option: output = "fitted_values"
#' data <- loess_aicc_optimized(x = data
#'                              , independent_var = "year"
#'                              , independent_var_range = 1991:2009
#'                              , dependent_var = "value"
#'                              , groups = "topic"
#'                              , group_subset = NULL
#'                              , span_interval = NULL
#'                              , degree = 2
#'                              , family = "gaussian"
#'                              , normalize = TRUE
#'                              , output = "fitted_values")
#' # check the new content
#' print(data)
#'
#' plot(data[topic == 1,year], data[topic == 1,value], col = 1, ylim = c(min(data$value), max(data$value)))
#' #default span is set to 0.75 in loess; this is explicitly included in below call for the sake of clarity
#' lines(data[topic == 1,year], predict(loess(value ~ year, span = 0.75, data = data[topic == 1,])), col = 1)
#' lines(data[topic == 1,year], data[topic == 1,aicc_loess_fit], col = 2)
#' # add aicc loess confidence intervals
#' lines(data[topic == 1,year], data[topic == 1,aicc_loess_lwr], col = 2, lty = "dashed")
#' lines(data[topic == 1,year], data[topic == 1,aicc_loess_upr], col = 2, lty = "dashed")
#' legend(x = 1990, y = max(data[topic == 1,value]), c("loess_aicc (dashed: conf. intv.)", "loess_standard"), fill = c(2,1))
#'
#' plot(data[topic == 2,year], data[topic == 2,value], col = 1)
#' lines(data[topic == 2,year], predict(loess(value ~ year, data = data[topic == 2,])), col = 1)
#' lines(data[topic == 2,year], data[topic == 2,aicc_loess_fit], col = 2)
#' legend(x = 1990, y = max(data[topic == 2,value]), c("loess_aicc", "loess_standard"), fill = c(2,1))
#'
#' # second option: output = "model"
#' models = loess_aicc_optimized(x = data
#'                               , independent_var = "year"
#'                               , dependent_var = "value"
#'                               , groups = "topic"
#'                               , output = "model")
#' models
#' # group model_aicc_loess
#' # 1:     1          <loess>
#' # 2:     2          <loess>
#' class(models[1,2])
#' # [1] "data.table" "data.frame"
#' class(models[1,2][[1]])
#' # [1] "list"
#' class(models[1,2][[1]][[1]])
#' # [1] "loess"

loess_aicc_optimized <- function(x
                                 , independent_var = NULL, independent_var_range = NULL
                                 , dependent_var = NULL
                                 , groups = NULL, group_subset = NULL
                                 , span_interval = NULL
                                 , degree = 2
                                 , family = "gaussian"
                                 , normalize = TRUE
                                 , confidence_interval = 0.95
                                 , output = c("model")) {

  #INPUT CHECKS
  stopifnot(!is.null(independent_var))
  stopifnot(!is.null(dependent_var))
  stopifnot(!is.null(groups))

  if (is.null(span_interval)) {
    span_interval <- c(.1,.9)
    message("No span interval specified. Interval set to c(.1, .9).")
  }

  if (is.null(group_subset)) {
    group_subset <- unique(x[, eval(parse(text = groups))])
  }

  if (is.null(independent_var_range)) {
    independent_var_range <- unique(x[, eval(parse(text = independent_var))])
  }

  #OPTIMIZATION FUNCTIONS
  # copied and adapted from: https://gist.github.com/kylebgorman/6444612 by Kyle Gorman <gormanky@ohsu.edu>
  # also implemented in fANCOVA package with more features: https://cran.r-project.org/web/packages/fANCOVA/index.html

  # theoretical backgorund proposed by Hurvich / Simonoff / Tsai, e.g., in
  # Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
  # parameter selection in nonparametric regression using an improved
  # Akaike Information Criterion. Journal of the Royal Statistical
  # Society B 60: 271 to 293.

  # apart from academic literature, see also
  #https://en.wikipedia.org/wiki/Akaike_information_criterion

  aicc_loess <- function(fit) {
    # @param fit        loess fit
    # @return           'aicc' value
    stopifnot(inherits(fit, "loess"))
    # parameters
    #sample size
    n <- fit$n
    # effective number of parameters used in the smoothing fit  (see, for example,  Hastie and Tibshirani  (1990), section 3.5)
    trace <- fit$trace.hat
    #mean residual sum of squares, i.e., variance
    sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
    #AICC for  least squares model fitting
    return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
  }

  if (output == "fitted_values") {
  # initialize new columns
  x <- cbind(x
             , aicc_loess_fit = rep(NA_real_, nrow(x))
             , aicc_loess_lwr = rep(NA_real_, nrow(x))
             , aicc_loess_upr = rep(NA_real_, nrow(x))
             )
  } else {
  #  https://stackoverflow.com/questions/51332754/how-to-initialize-a-data-table-column-with-empty-lists-and-loop-over-it
  y <- data.table(group = group_subset, model_aicc_loess = vector(length(group_subset), mode="list"))
  }

  # optimiize
  for (g in group_subset) {
    loess_data <- x[eval(parse(text = groups)) == g & eval(parse(text = independent_var)) %in% independent_var_range, .SD, .SDcols = c(independent_var, groups, dependent_var)]
    function_to_optimize <- function(span) eval(parse(text = paste0("aicc_loess(loess("
                                                 , dependent_var, " ~ ", independent_var
                                                 ,", data = loess_data, span = ", span
                                                 , ",degree = ", degree
                                                 , ", family = \"", family, "\", normalize = ", normalize,"))"
                                                 )))
    span_optimum <- optimize(function_to_optimize, interval = span_interval)$minimum
    loess_model_optimum <- eval(parse(text = paste0("loess(",dependent_var, " ~ ", independent_var,", data = loess_data
                                            , span = ", span_optimum, # <<< optimized span
                                            ",degree = ", degree, ", family = \"", family, "\", normalize = ", normalize, ")")))
    if (output == "fitted_values") {
    #update data with loess data
    fit = predict(loess_model_optimum, se=T)
    # https://stackoverflow.com/questions/22717930/how-to-get-the-confidence-intervals-for-lowess-fit-using-r
    confidence_quantile = 1-((1-confidence_interval)/2)
    x[eval(parse(text = groups)) == g & eval(parse(text = independent_var)) %in% independent_var_range, aicc_loess_fit:= predict(loess_model_optimum)]
    x[eval(parse(text = groups)) == g & eval(parse(text = independent_var)) %in% independent_var_range, aicc_loess_lwr:= fit$fit - qt(confidence_quantile,fit$df)*fit$se]
    x[eval(parse(text = groups)) == g & eval(parse(text = independent_var)) %in% independent_var_range, aicc_loess_upr:= fit$fit + qt(confidence_quantile,fit$df)*fit$se]
    } else {
    y[group == g, model_aicc_loess:= list(list(loess_model_optimum))]
    }
  }
  if (output == "fitted_values") {
  return(x)
  } else {
  return(y)
  }
}
