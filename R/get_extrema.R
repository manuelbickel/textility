

#' Get idxs and values of extreme points in discrete sequence of numbers
#'
#' The function uses a sliding window of size 3 to identify extreme points via zoo::rollapply .
#' It is designed for finding extreme points in smoothing functions, e.g., loess.
#' In this kind of sequences it may be assumed that in most cases two consectutive entries do not have the same value.
#' In the other rare cases a portion of -1e-12 of the difference between two points is substracted from the first point,
#' so that for longer sequences of same values always the second one will be marked as extreme point.
#'
#'
#' @param x A numeric vector to be analyzed for extreme points.
#' @param format_output May be set to "boolean" or "idxs_values".
#'                     "boolean" returns boolean vectors with TRUE for local maxima and minima positions in \code{x}.
#'                     "idxs_values" returns the number, indices and asociated values of maxima and minima.
#'
#' @return A \code{list} storing the boolean positions of extrema or the number, indices and asociated values of maxima and minima.
#'         List style was currently chosen to allow direct use in data.table, e.g. DT[, c("minima", "maxima"):= get_extrema(column, "boolean")].
#'         If ouptut is required as \code{data.table} simply do \code{as.data.table(get_extrema(...))}.
#'
#' @export
#'
#' @examples
#'
#' get_extrema(c(0,1,0,2,0,3), "boolean")
#' # $minima_boolean
#' # [1] FALSE FALSE  TRUE FALSE  TRUE FALSE
#' # $maxima_boolean
#' # [1] FALSE  TRUE FALSE  TRUE FALSE FALSE
#'
#' get_extrema(c(0,1,0,2,0,3), "idxs_values")
#' # $minima_n
#' # [1] 2
#' # $minima_idxs
#' # $minima_idxs[[1]]
#' # [1] 3 5
#' # $minima_values
#' # $minima_values[[1]]
#' # [1] 0 0
#' # $maxima_n
#' # [1] 2
#' # $maxima_idxs
#' # $maxima_idxs[[1]]
#' # [1] 2 4
#' # $maxima_values
#' #' # $maxima_values[[1]]
#' #' # [1] 1 2
#' #output as data.table
#' as.data.table(get_extrema(c(0,1,1,1,0), "idxs_values")) #<- second "1" is marked as extreme point
#'   minima_n minima_idxs minima_values maxima_n maxima_idxs maxima_values
#' 1:        0          NA            NA        1           3             1


get_extrema <- function(x, format_output = c("boolean", "idxs_values")) {
  #introduce slight numeric change to numbers to exclude saddle points with repeating numbers from maxima/minima
  #this also shifts plateau maxima to the right of the plateau
  #hence, make saddle points of x strictly monotonically de/increasing
  #mathematically this is not fully correct but an acceptable numerical approach for the desired purpose
  x_monotonic_saddle <- function(x) {
    idxs_adapt <- which(x[-1]-x[1:(length(x)-1)] == 0)
    x[idxs_adapt] <- x[idxs_adapt]-1e-12*(x[idxs_adapt]-x[idxs_adapt-1])
    return(x)
  }

  #https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
  minima_boolean <- c(FALSE, zoo::rollapply(x_monotonic_saddle(x), width = 3, function(y) which.min(y)==2), FALSE)
  maxima_boolean <- c(FALSE, zoo::rollapply(x_monotonic_saddle(x), width = 3, function(y) which.max(y)==2), FALSE)
  #rollapply consumes the first/last value, hence, +1 has to be added to indices
  #minima <- which(zoo::rollapply(x, width = 3, function(y) which.min(y)==2) == TRUE)+1
  #maxima <- which(zoo::rollapply(x, width = 3, function(y) which.max(y)==2) == TRUE)+1

  if (format_output == "boolean") {
    list(minima_boolean =   minima_boolean
               ,maxima_boolean = maxima_boolean
    )
  } else if (format_output == "idxs_values") {
    minima_n = sum(minima_boolean)
    maxima_n = sum(maxima_boolean)
    list(
               minima_n = minima_n
              ,minima_idxs =    ifelse(minima_n == 0, NA, list(which(minima_boolean == TRUE)))
              ,minima_values = ifelse(minima_n == 0, NA, list(x[minima_boolean]))
              ,maxima_n = maxima_n
              ,maxima_idxs =   ifelse(maxima_n == 0, NA, list(which(maxima_boolean == TRUE)))
              ,maxima_values = ifelse(maxima_n == 0, NA, list(x[maxima_boolean]))
              )
  }
}
