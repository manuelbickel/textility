



#' Scale values normal with boundaries zero and one
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' scale_normal(c(0,50,100))
#' #[1] 0.0 0.5 1.0
#' scale_normal(c(1,6,11))
#' #[1] 0.0 0.5 1.0
scale_normal <- function(x){(x-min(x))/(max(x)-min(x))}



