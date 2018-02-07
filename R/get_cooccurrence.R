

#' Get the co-occurrence of column elements per row entity (e.g. document co-occurrence of terms)
#'
#' @param x A matrix object (with a document term matrix containing integer counts in mind). Currently accepts base::matrix, Matrix::sparseMatrix or slam::simple_triplet_matrix.
#' @param binarize By default \code{TRUE}. Values larger than the set threshold are turned to \code{1}, lower values are turned to \code{0}.
#' @param threshold Threshold for binarization. By default \code{1}.
#'                  If input is not an integer matrix of counts but, e.g., a probability matrix, a threshold such as 0.5 might be reasonable.
#'
#' @return A \code{sparseMatrix} with the summed (document) co-occurrence per row of the specified column elements (words) of \code{x}.
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
#' # A 6 5 2  <- note the difference regarding A and B
#' # B 5 5 1
#' # C 2 1 2

get_cooccurrence <- function(m, binarize = TRUE, threshold = 1) {

  if ("simple_triplet_matrix" %in% class(m)) {
   m <- tripl_to_sparse(m)
  } else if ("matrix" %in% class(m)) {
   m <- Matrix(m, sparse = TRUE)
  }

  #speed /memory efficiency might be increased on basis of using Tsparsematrix, see
  #https://stackoverflow.com/questions/33775291/r-matrix-set-particular-elements-of-sparse-matrix-to-zero
  if(binarize == TRUE) {
    m@x <- ifelse(m@x >= threshold, 1, 0)
    # x[x>threshold] <- threshold
    # x[x<threshold] <- 0
  }

  Matrix::crossprod(m)
}


