#' Simple triple matrix to sparse matrix
#'
#' @param simple_triplet_matrix A simple_triplet_matrix as used in package slam.
#' @param giveCsparse By default \code {TRUE}. Set to \code{FALSE} if a \code{dgTMatrix} shall be returned instead of a \code{dgCMatrix}.
#'
#' @return \code{sparseMatrix} from package Matrix.
#' @export
#'
#' @examples
#'
#'

tripl_to_sparse <- function(simple_triplet_matrix, giveCsparse = TRUE) {
  #see https://stackoverflow.com/questions/20004493/convert-simple-triplet-matrixslam-to-sparse-matrixmatrix-in-r
  Matrix::sparseMatrix(i=simple_triplet_matrix$i
                       ,j=simple_triplet_matrix$j
                       ,x=simple_triplet_matrix$v
                       ,giveCsparse= giveCsparse
                       ,dims=c(simple_triplet_matrix$nrow
                              ,simple_triplet_matrix$ncol)
                       ,dimnames = dimnames(simple_triplet_matrix)
  )
}
