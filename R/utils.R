



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




#' Get numeric gradient between points
#'
#' Simple caluclation of gradient between points.
#'
#' @param x steps / independent variable
#' @param y observed data points / response variable
#'
#' @return Matrix showing the gradient values between points.
#'         The x positions of gradient values are returned as the middle point between the steps.
#'         Note that \code{nrow(gradient) = length(x)-1}.
#' @export
#'
#' @examples

gradient_numeric <- function(x, y) {
  dx <- diff(x)
  gradient <- diff(y)/dx
  position <- x[1:(length(x)-1)] + dx/2
  cbind(position, gradient)
}
