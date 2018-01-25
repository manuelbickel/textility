

#' Get the co-occurrence of column elements per row
#'
#' Documentation to be done....
#'
#' @param m
#' @param cols
#' @param format_output
#'
#' @return
#' @export
#'
#' @examples
#' mat <- matrix(sample(1:2, 18, replace = T), nrow = 3)
#' get_cooccurrence(m = mat, c(1,3,6))
#' #     [,1] [,2] [,3]
#' # [1,]    3    3    3
#' # [2,]    3    3    3
#' # [3,]    3    3    3
#' get_cooccurrence(m = mat, c(1,3,6), format_output = as.simple_triplet_matrix)
#' #A 3x3 simple triplet matrix.


get_cooccurrence <- function(m, cols, format_output = as.matrix) {
  m_subs <- m[,cols]
  #binarization and cross product (document co-occurrence) depending on class of matrix
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
