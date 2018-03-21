#' Create a top features per entity matrix from a numeric feature per entity distribution
#'
#' The function orders the elements of a matrix per row and returns the actual corresponding features within the top n rank range, i.e., column items, per row.
#' Considering a document-entity term-feature matrix, it achieves the same as, e.g., topicmodels::terms or text2vec or text2vec::get_top_words
#' but takes a raw matrix as input instead of a native model object of a specific package.
#' Hence, apart from getting top n term-features per topic from a "raw" word probabilities per topic object of an LDA model (words as columns,
#' topics as rows, entries are word probabilities) it might also help to find top n word-features per document given a document term matrix
#' or top topic-features per document given a document topic matrix.
#'
#' @param entity_feature_matrix A numeric matrix object.
#'                              Each row represents an entity, e.g., document, each column a feature, e.g., term.
#' @param n Number of highest rank number to consider for getting features per row. By default 10.
#' @param include_all_ties By default \code{TRUE}. Output includes all ties for each rank, hence, output number of features may be higher than \code{n}.
#'                         If set to \code{FALSE}, the number of rows of the output is limited to \code{n}. The highest output rank number may then be different
#'                         for each entity.
#'
#'
#' @return A character matrix with the top n features per row - hence, a top-feature-entity-matrix.
#'         For better readability, the output is "transposed" so that the entities appear as columns and features as rows.
#'         If \code{include_all_ties = TRUE}, the trailing elements of a column are set to \code{NA} if another entity has more top features (due to ties)
#'         than that entity. The number of top features per entity depends on their rank.
#'
#' @export
#'
#' @examples
#'
#' example for word topic distribution as output from LDA model
#' beta <- rbind(T1 = c(0.3,0.3,0.3, 0.1), T2 = c(0.19,0.3,0.5, 0.01),  T3 = c(0.3,0.5,0.19, 0.01))
#' top_feature_matrix(entity_feature_matrix = beta, n = 2, terms = c("A", "B", "C", "D"), include_all_ties = FALSE)
#' #      T1  T2  T3
#' # [1,] "A" "C" "B"
#' # [2,] "B" "B" "A"
#' case if no terms are specified and all ties shall be considered
#' top_feature_matrix(entity_feature_matrix = beta, n = 2, include_all_ties = TRUE)
#' # T1  T2  T3
#' # [1,] "1" "3" "2"
#' # [2,] "2" "2" "1"
#' # [3,] "3" NA  NA
#' # [4,] "4" NA  NA
#' # Warning message:
#' #   In top_feature_matrix(entity_feature_matrix = beta, n = 2, include_all_ties = TRUE) :
#' #   Input entity_feature_matrix has no colnames and no colnames to be used have been specified. Column indices were used as feature names.

top_feature_matrix <- function(entity_feature_matrix, n = 10, terms = NULL, include_all_ties = TRUE) {
  #check for availability of colnames and set them accordingly
  if (is.null(colnames(entity_feature_matrix)) & is.null(terms)) {
    warning("Input entity_feature_matrix has no colnames and no colnames to be used have been specified. Column indices were used as feature names.")
    colnames(entity_feature_matrix) <- as.character(1:ncol(entity_feature_matrix))
  }
  if(!is.null(terms)) {
    if (!is.null(colnames(entity_feature_matrix))) {
      if (!identical(colnames(entity_feature_matrix), terms)) {
        warning("entity_feature_matrix had colnames that have been overwritten by the names provided via the terms parameter.")
      }
    }
    colnames(entity_feature_matrix) <- terms
  }
  if (n > ncol(entity_feature_matrix)) {
    n <- ncol(entity_feature_matrix)
    warning(paste0("Specified n exceeds the number of columns in entity_feature_matrix. Maximum possible n was used, i.e., ncol(entity_feature_matrix) =", ncol(entity_feature_matrix)))
  }

  #get top n features by rank - with or without ties
  if (include_all_ties == TRUE) {
    idxs_top <- apply(entity_feature_matrix, 1, order, decreasing=TRUE)
    top_ranks_with_ties <- apply(entity_feature_matrix, 1, function(x) {
      ranks <- textility::get_rank(x, decreasing = TRUE, na.last = TRUE, ties.method = "dense")
      ranks <- sort(ranks, decreasing = FALSE)
      ranks[which(ranks > n)] <- NA
      ranks
    })
    #note the sign to introduce NA values by multiplication
    idxs_top <- idxs_top*sign(top_ranks_with_ties)
  } else {
    idxs_top <- apply(entity_feature_matrix, 1, order, decreasing=TRUE)[1:n,]
  }

  top_terms <- apply(idxs_top, 2, function(x) colnames(entity_feature_matrix)[x])

  if (!is.null(rownames(entity_feature_matrix))) {
    colnames(top_terms) <- rownames(entity_feature_matrix)
  } else {
    colnames(top_terms) <- paste0("E", 1:ncol(top_terms))
  }
  #class(top_terms) <- paste0("top n feature entity matrix; n = ",n, "; all ties = ", include_all_ties)
  top_terms
}
