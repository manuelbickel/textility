

#' Get the co-occurrence of column elements per row
#'
#'
#' @param m A matrix object (with a document term matrix in mind) of type base::matrix, Matrix::sparseMatrix or slamm::simple_triplet_matrix.
#' @param cols The columns to be used to get cooccurrence. By default all columns of \code{m}.
#' @param rows The rows to be used to get cooccurrence. By default all rows of \code{m}.
#' @param format_output Function to convert the output, by default \code{base::as.matrix()}.
#'
#' @return A matrix of the specified ouput type with the (document) summed cooccurrence per row of the specified column elements of \code{m}.
#' @export
#'
#' @examples
#' mat <- cbind(A = c(1,1,1,0), B = c(1,0,1,0), C = c(0,1,1,0))
#' #      A B C D
#' # [1,] 1 1 0 0
#' # [2,] 1 0 1 0
#' # [3,] 1 1 1 0
#' # [4,] 0 0 0 0
#' get_cooccurrence(mat)
#' #   A B C
#' # A 3 2 2
#' # B 2 2 1
#' # C 2 1 2
#' get_cooccurrence(mat, format_output = as.simple_triplet_matrix)
#' #A 3x3 simple triplet matrix.
#' get_cooccurrence(mat, cols = 1:2, rows = 1:2)
#'     A B
#' # A 2 1
#' # B 1 1

get_cooccurrence <- function(m, cols = NULL, rows = NULL, format_output = as.matrix) {
  if (is.null(cols) & is.null(rows)) {
    m_subs <- m
  } else if (!is.null(cols) & is.null(rows)) {
    m_subs <- m[,cols]
  } else {
    m_subs <- m[rows,cols]
  }
  #binarization and cross product (document co-occurrence) depending on input class
  if ("simple_triplet_matrix" %in% class(m)) {
    m_subs$v <- ifelse(m_subs$v>1, 1, m_subs$v) #binarize
    tcm <- slam::tcrossprod_simple_triplet_matrix(t(m_subs))
  } else if ("Matrix" %in% unlist(attributes(class(m)))) {
    m_subs[m_subs>0] <- 1
    tcm <- Matrix::t(m_subs) %*% m_subs
  } else {
    m_subs[m_subs>0] <- 1
    tcm <- t(m_subs) %*% m_subs
  }
  return(format_output(tcm))
}
