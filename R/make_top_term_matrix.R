#' Create a top terms per topic (or document) matrix from a numeric term per topic (or document) distribution
#'
#' The function achives the same as, e.g., topicmodels::terms or text2vec or text2vec::lda$get_top_words
#' but takes a raw matrix as input instead of a native model object of a specific package. Hence, it may not only be used to
#' produce top n words per topic given the "raw" word probabilities per topic object of an LDA model ( words as columns,
#' topics as rows, entries are word probabilities) but might also help to find top n words per document given a
#' document term matrix. Basically the function orders the elements of a matrix per row and returns
#' the actual top n column items per row.
#'
#' @param beta A numeric matrix object (incl. base::matrix, Matrix::sparse, slam::simple_triplet_matrix).
#' @param n Number of top n terms per row to take into account. By default 10.
#' @return A character matrix with the top n terms per row.
#'         The output is "transposed" so that the row elements appear as columns.
#'
#' @export
#'
#' @examples
#'
#' beta <- rbind(T1 = c(0.5,0.3,0.19, 0.01), T2 = c(0.19,0.3,0.5, 0.01),  T3 = c(0.3,0.5,0.19, 0.01))
#' make_top_term_matrix(beta, 2, terms = c("A", "B", "C", "D"))
#' #      T1  T2  T3
#' # [1,] "A" "C" "B"
#' # [2,] "B" "B" "A"

make_top_term_matrix <- function(beta, n = 10, terms = NULL) {
  if (is.null(colnames(beta)) & is.null(terms)) {
    stop("Input matrix has no colnames. Please use the terms parameter for providing colnames or assign colnames to beta directly.
         Colnames should be the same as in the document term matrix used as model input.")
  }
  if(!is.null(terms)) {
    if (!is.null(colnames(beta))) {
      if ((colnames(beta) !=  terms)) {
        warning("Beta has colnames that have been overwritten by the names provided by the terms parameter.")
      }
    }
    colnames(beta) <- terms
  }
  if (n > ncol(beta)) {
    n <- ncol(beta)
    warning(paste0("Specified n exceeds the number of columns in beta. Maximum posasible n was used, i.e., ncol(beta) = ", ncol(beta)))
  }
  idxs_top <- apply(beta, 1, order, decreasing=TRUE)[1:n,]
  top_terms <- apply(idxs_top, 2, function(x) colnames(beta)[x])
  colnames(top_terms) <- paste0("T", 1:ncol(top_terms))
  top_terms
}


