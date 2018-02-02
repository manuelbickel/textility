

#' Get the co-occurrence of column elements per row entity (e.g. document co-occurrence of terms)
#'
#' @param x A matrix (with a document term matrix containing integer counts in mind) of type base::matrix, Matrix::sparseMatrix or slamm::simple_triplet_matrix.
#' @param binarize By default \code{TRUE}. Values larger the set threshold are turned to \code{1}, lower values are turned to \code{0}.
#' @param threshold By default \code{1}. See \code{binarize} parameter for effect of parameter.
#'                  If input is not an integer matrix of counts but, e.g., a probability matrix, a threshold such as 0.5 might be reasonable.
#' @param format_output Function to be applied on the output, e.g., turning into the desired class.
#'                      By default \code{NULL}, which maintains the class produced by the native transpose crossproduct function
#'                      associated with the input class (e.g. slam::tcrossprod_simple_triplet_matrix).
#'                      An exemplary function for formatting output from the \code{as.y} family would be \code{base::as.matrix}.
#'                      Please remember that regarding type conversion some input types might require more complex conversion functions from the \code{x_to_y} family.
#'
#' @return A matrix of the specified ouput type with the summed (document) cooccurrence per row of the specified column elements (words) of \code{m}.
#' @export
#'
#' @examples
#' mat <- cbind(A = c(2,1,1,0), B = c(2,0,1,0), C = c(0,1,1,0))
#' #      A B C D
#' # [1,] 2 2 0 0
#' # [2,] 1 0 1 0
#' # [3,] 1 1 1 0
#' # [4,] 0 0 0 0
#' get_cooccurrence(mat)
#' #   A B C
#' # A 3 2 2
#' # B 2 2 1
#' # C 2 1 2
#' get_cooccurrence(mat, binarize = FALSE)
#' #   A B C
#' # A 6 5 2  <- note the difference reagrding A and B
#' # B 5 5 1
#' # C 2 1 2
#' get_cooccurrence(mat, format_output = slam::as.simple_triplet_matrix)
#' #A 3x3 simple triplet matrix.

get_cooccurrence <- function(x, binarize = TRUE, threshold = 1, format_output = NULL) {
  if(binarize == TRUE) {
    x[x>threshold] <- threshold #possibility for simple_triplet_matrix x$v <- ifelse(x$v>1, 1, x$v)
    x[x<threshold] <- 0
  }

  #cross product (document co-occurrence) depending on input class
  if ("simple_triplet_matrix" %in% class(x)) {
    tcm <- slam::tcrossprod_simple_triplet_matrix(t(x))
  } else if ("Matrix" %in% unlist(attributes(class(x)))) {
    tcm <- Matrix::t(x) %*% x
  } else {
    tcm <- t(x) %*% x
  }

  if (is.null(format_output)) {
    return(tcm)
  } else {
    return(format_output(tcm))
  }
}
