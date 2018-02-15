



#' normalize
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' normalize(c(0,50,100))
#' #[1] 0.0 0.5 1.0
#' normalize(c(1,6,11))
#' #[1] 0.0 0.5 1.0
normalize <- function(x){(x-min(x))/(max(x)-min(x))}



