
#' Calculate topic co-occurrence from text2vec model
#'
#' @param x A named list containing a fitted text2vec LDA model and the document topic distribution.
#'          Names need to be: \code{list(model = ..., doc_topic_distr = ...)}.
#'          The first element should be the lda model object and the second the topic distribution
#'          to be extraced from the former via \code{$doc_topic_distr}.
#' @param n_top_words Number of top words to of each topic to be used as topic label. Passed to \code{get_top_words}.
#' @param term_order_lambda Lambda for re-ordering the extracted top terms. Passed to \code{get_top_words}.
#' @param diag_to_zero Shall the diagonal of the output be set to zero, i.e.,
#'                     should the self-co-occurrence (which is the occurrence of a topic of all docs) be neglected?
#'                     By default \code{TRUE}.#'
#' @return An symmetric matrix showing the co-occurrence of topics in documents.
#' @export
#'
#' @examples

topic_cooccurrence = function(x, n_top_words = 5, term_order_lambda = 1, diag_to_zero = TRUE, n_top_topics = 10) {
  doc_topic_distr = x$doc_topic_distr
  n_topics = ncol(doc_topic_distr)
  top_terms = x$model$get_top_words(n = 5, topic_number = 1:n_topics, lambda = term_order_lambda)
  doc_topic_distr = t(apply(doc_topic_distr, 1, function(x) {
    # https://stackoverflow.com/questions/4915704/how-to-get-ranks-with-no-gaps-when-there-are-ties-among-values
    to_zero = which(match(x, sort(unique(x), decreasing = T)) > n_top_topics)
    if (length(to_zero) > 0) {
      x[to_zero] = 0
    }
    x
  }))
  topic_cooccurrence = crossprod(doc_topic_distr)
  colnames(topic_cooccurrence) =  apply(top_terms, 2, function(x) {paste(x, collapse = "\n")})
  rownames(topic_cooccurrence) = colnames(topic_cooccurrence)
  # exclude duplicate values (e.g., igraph will exclude edges with zero co-occurrence)
  # topic_cooccurrence[lower.tri(topic_cooccurrence)] = 0
  if (diag_to_zero == TRUE) {
    # exclude connection of node with itself (its occurrence)
    diag(topic_cooccurrence) = 0
  }
  topic_cooccurrence
}


