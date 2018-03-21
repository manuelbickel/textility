



#' Fitting of AICC optimized loess per topic over specified time range
#'
#' Given a data.table with a year and topic column and additional columns with values for these variables (e.g. mean topic probability per year),
#' the function fits a loess model and optimizes the span via Akaikes information criterion for each topic.
#' Modelling may be limited to a specified time range.
#' The results are joined with the input data.table. NOTE: currently the order of columns is not preserved.
#'
#' @param DT A \code{data.table}. Current implementation requires data.table to have two columns named year and topic.
#' @param topic_selection By default \code{loess} is fitted for all topics. A selection of topic ids may be specified here.
#' @param year_selection By default \code{loess} is fitted for all years. A selection of years may be specified here.
#' @param fit_columns The columns to be fitted by \code{loess}.
#' @param span_interval The span intervall to be considered for optimization of loess span.
#'
#' @return The original \code{DT} with additional columns as specified via \code{fit_columns} with the suffix \code{"_loess"}.
#'          The entires in the new columns for combinations of year and topic that have not specified in
#'          the \code{topic_selection} or \code{year_selection} are set to \code{NA} (standard DT join).
#' @export
#'
#' @examples



#TODO EXAMPLE
# years <- 1990:2010
# set.seed(1990)
# topics <- c(rep(1, length(years)), rep(2, length(years)))
# values1 <- (jitter(seq_along(years)/10, amount = pi)^3)
# values2 <- (mean(values1)+-jitter(seq_along(years), amount = pi)^2)*(jitter(seq_along(years)/10, amount = pi)^3)
# data <- data.table(year = years, topic = topics, value = c(values1, values2))
# data <- loess_aicc_optimized(DT = data, topic_selection = NULL
#                              , year_selection = 1991:2009
#                              , fit_columns = "value"
#                              , span_interval = c(.1, .9))
#
# plot(data[topic == 1,year], data[topic == 1,value], col = 1, ylim = c(min(data$value), max(data$value)))
# lines(data[topic == 1,year], predict(loess(value ~ year, data = data[topic == 1,])), col = 1, lty = "solid")
# lines(data[topic == 1,year], data[topic == 1,value_loess], col = 1, lty = "dashed")
#
# points(data[topic == 2,year], data[topic == 2,value], col = 2)
# plot(data[topic == 2,year], data[topic == 2,value], col = 1)
# lines(data[topic == 2,year], predict(loess(value ~ year, data = data[topic == 2,])), col = 2, lty = "dashed")
# lines(data[topic == 2,year], data[topic == 2,value_loess], col = 2, lty = "solid")



loess_aicc_optimized <- function(DT, topic_selection = NULL, year_selection = NULL, fit_columns = NULL, span_interval = NULL) {
  #TODO
  #make function more flexible regarding column names, currently names have to be topic and year
  #restore original order of columns as in DT

  #INPUT CHECKS
  if (is.null(fit_columns)) {
    stop("Please specify colnames of columns to be via fit_columns argument.")
  }

  if (is.null(span_interval)) {
    span_interval <- c(.1,.9)
  }

  if (is.null(topic_selection)) {
    topic_selection <- unique(DT$topic)
  }

  if (is.null(year_selection)) {
    year_selection <- unique(DT$year)
  }

  #OPTIMIZATION FUNCTIONS
  # taken (and adapted) from: https://gist.github.com/kylebgorman/6444612 by Kyle Gorman <gormanky@ohsu.edu>
  # or also implemented in fANCOVA package with more features: https://cran.r-project.org/web/packages/fANCOVA/index.html

  #based on
  # Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
  # parameter selection in nonparametric regression using an improved
  # Akaike Information Criterion. Journal of the Royal Statistical
  # Society B 60: 271–293.

  # apart from academic literature, see also
  #https://en.wikipedia.org/wiki/Akaike_information_criterion

  aicc.loess <- function(fit) {
    #
    # @param fit        loess fit
    # @return           'aicc' value
    stopifnot(inherits(fit, 'loess'))
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

  #original code of autoloess not directly usable in funtional programming (see comments below) but same general approach is used below
  # autoloess <- function(fit, span=c(.1, .9)) {
  #   # compute loess fit which has span minimizes AIC_C
  #   #
  #   # @param fit        loess fit; span parameter value doesn't matter
  #   # @param span       a two-value vector representing the minimum and
  #   #                   maximum span values
  #   # @return           loess fit with span minimizing the AIC_C function
  #   #stopifnot(inherits(fit, 'loess'), length(span) == 2)
  #   # loess function in form to be used by optimize
  #
  #   # f <- function(span) aicc.loess(update(fit, span=span))
  #   f <- function(span) eval(parse(text = paste0("aicc.loess(loess(",column, " ~ year, data = loess_data, span = ",span,"))")))
  #   span_opt <- optimize(f, span)$minimum
  #   # find best loess according to loss function
  #   return(update(fit, span=span_opt))
  # }

  #FITTING OF MULTIPLE COLUMNS PER GROUP
  #subsetting in i for loess is required to exclude certain years from modelling process
  #direct use of predict(autoloess(loess())) applied on columns while subsetting in i was not sucessful with my approaches
  #since functions do not access the subsetted but the full data
  #this might be solved in the future with a more elegant pure data.table approach, for the time being the following works
  loess_smoothed <- rbindlist(lapply(topic_selection, function(tpc) {
    #unfortunately loess does not seem to work with data.table object
    loess_data <- data.frame(DT[topic == tpc & year %in% year_selection, .SD, .SDcols = c("year", "topic", fit_columns)])
    for (column in fit_columns) {
      #TODO
      #packaging the loess calls as eval(parse()) seemed to be necessary to allow evaluation of variables in the call
      #using simple update and optimize caused trouble with environment handling, since these function only search in parent.env by default
      #check e.g. base::update.default method, which uses getCall
      #but we also need higher env to get loess_data, for now below approach works,
      #it is not slower than when using update, etc. only not very readable
      #environment scoping required for future versions, check Advanced R by Hadley for this...something like pryr::where will be required or so...
      #in below functions that use
      #if call includes a variable name this name is not evaluated automatically
      #with information from higher environments but intepreted as a variable of the calling environment, here loess_data

      #first two lines were taken and adapted from autoloess implementation see above, commented out
      f <- function(span) eval(parse(text = paste0("aicc.loess(loess(",column, " ~ year, data = loess_data, span = ",span,"))")))
      span_opt <- optimize(f, interval = span_interval)$minimum
      loess_model <- eval(parse(text = paste0("loess(",column, " ~ year, data = loess_data, span = ",span_opt,")")))
      #loess_model <- autoloess(loess_model)
      #update columns with loess data
      loess_data[, column] <- predict(loess_model)
      #colnames(loess_data)[which(colnames(loess_data) == column)] <- paste0(column, "_loess")
    }
    colnames(loess_data)[which(colnames(loess_data) %in% fit_columns)] <- paste0(fit_columns, "_loess")
    return(loess_data)
  }))
  #join loess data into data while returning NA in loess columns for years that shall not be used for smoothing
  loess_smoothed[DT, on = c("year", "topic")]
}
