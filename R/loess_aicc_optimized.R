
#' Fitting of AICC optimized loess (groupwise)
#'
#' Given a data.table with a year and topic column and additional columns with values for these variables (e.g. mean topic probability per year),
#' the function fits a loess model and optimizes the span via Akaikes information criterion for each topic.
#' Modelling may be limited to a specified time range.
#' The results are joined with the input data.table. NOTE: currently the order of columns is not preserved.
#'
#' @param x A \code{data.table} with data in long format (use \code{data.table::melt} if your data is in wide format).
#' @param group_subset By default \code{loess} is fitted for all groups. A subset may be specified as \code{character} vector.
#' @param independent_var_range By default \code{loess} is fitted for the full range of the independent variable. A limited range may be specified here, usually an \code{integer} vector.
#' @param dependent_var The data column of the values to be fitted by \code{loess}.
#' @param span_interval The span interval to be considered for AICC optimization of the loess span. By default \code{c(.1,.9)}.
#'
#' @return The original \code{x} with tow additional columns named \code{"loess_value"} and \code{"span"}
#'         Entries in span are identical for an individual group.
#'         The entries in the additional columns that correspond to excluded groups or independent variables appear as \code{NA}.
#' @export
#'
#' @examples

# the following example show how aicc loess
# produces a fit that is more sensible to the data
# in comparison to standard loess using a span of 0.75
# if such less smooth appearance makes sense is, of course, case specific

years <- 1990:2010
set.seed(42)
topics <- c(rep(1, length(years)), rep(2, length(years)))
values1 <- jitter(rnorm(length(years), mean = 0.5, sd = .25), amount = pi)
values2 <- jitter(rnorm(length(years), mean = 0.25, sd = 0), amount = exp(1))

data <- data.table(year = years, topic = topics, value = c(values1, values2))
data <- loess_aicc_optimized(x = data
                             , independent_var = "year"
                             , independent_var_range = 1991:2009
                             , dependent_var = "value"
                             , groups = "topic"
                             , group_subset = NULL
                             , span_interval = NULL
                             , degree = 2
                             , family = "gaussian"
                             , normalize = TRUE)
# check the new content
data

plot(data[topic == 1,year], data[topic == 1,value], col = 1, ylim = c(min(data$value), max(data$value)))
#default span is set to 0.75 in loess; this is explicitly included in below call for the sake of clarity
lines(data[topic == 1,year], predict(loess(value ~ year, span = 0.75, data = data[topic == 1,])), col = 1, lty = "dashed")
lines(data[topic == 1,year], data[topic == 1,loess_value], col = 1, lty = "solid")
legend(x = 1990, y = -1, c("solid: loess_aicc\ndashed: loess_standard"))

plot(data[topic == 2,year], data[topic == 2,value], col = 1)
lines(data[topic == 2,year], predict(loess(value ~ year, data = data[topic == 2,])), col = 1, lty = "dashed")
lines(data[topic == 2,year], data[topic == 2,loess_value], col = 1, lty = "solid")
legend(x = 1990, y = 2, c("solid: loess_aicc\ndashed: loess_standard"))


loess_aicc_optimized <- function(x
                                 , independent_var = NULL, independent_var_range = NULL
                                 , dependent_var = NULL
                                 , groups = NULL, group_subset = NULL
                                 , span_interval = NULL
                                 , degree = 2
                                 , family = "gaussian"
                                 , normalize = TRUE) {

  #INPUT CHECKS
  stopifnot(!is.null(independent_var))
  stopifnot(!is.null(dependent_var))
  stopifnot(!is.null(groups))

  if (is.null(span_interval)) {
    span_interval <- c(.1,.9)
    message("No span interval sepcified. Interval set to c(.1, .9).")
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

  # initialize new columns
  x <- cbind(x, loess_value = rep(NA_real_, nrow(x)), span = rep(NA_real_, nrow(x)))
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
                                            ",degree = ", degree, ", family = \"", family, "\", normalize = ", normalize,")")))
    #update data with loess data
    x[eval(parse(text = groups)) == g & eval(parse(text = independent_var)) %in% independent_var_range, loess_value:= predict(loess_model_optimum)]
    x[eval(parse(text = groups)) == g & eval(parse(text = independent_var)) %in% independent_var_range, span:= rep(span_optimum, nrow(loess_data))]
  }
  return(x)
}
