
#' Calculate the cosine similarity of a set of word vectors to inspect resulting terms
#'
#' A wrapper for faster customized inspection of glove models fitted with text2vec package.
#'
#' @param v_add Names of word vectors to be added during calculation.
#' @param v_substract Names of word vectors to be substracted during calculation.
#' @param n Number of resulting vector entries to show, which provide information on the resulting terms, i.e, names of the items.
#' @param word_vector_set The full set of word vectors to be used for calculation.
#'
#' @return The resulting cosine similarity vector whose names are terms.
#' @export
#'
#' @examples
#' #in the following just a syntax example without data (hence, cannot be run)
#' #for details on fitting glove models please refer to documentation of text2vec
#' # wv_main = glove$fit_transform(tcm,...)
#' # wv_context = glove$components
#' # word_vectors = wv_main + t(wv_context)
#' # cos_sim2_vectors(c("renewable", "energy"),
#' #                  v_substract = "large",
#' #                  n = 20, word_vector_set = word_vectors)

cos_sim2_vectors <- function(v_add, v_substract = NULL, n = 10, word_vector_set) {
  v_name <- v_add[1]
  v <- t(colSums(word_vector_set[v_add, , drop = FALSE]))
  rownames(v) <- v_name
  if (!is.null(v_substract)) {
    v <- v -  t(colSums(word_vector_set[v_substract, , drop = FALSE]))
  }
  cos_sim <- sim2(x = word_vector_set
                  ,y = v
                  ,method = "cosine", norm = "l2")
  if (!is.null(n)) {
    sort(cos_sim[,1], decreasing = TRUE)[1:n]
  } else {
    sort(cos_sim[,1], decreasing = TRUE)
  }
}
