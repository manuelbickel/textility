

#' Convert sparse Matrix to format required by stm for modelling
#'
#' stm has a readCorpus function that does the same, however, it may choke on large matrices. Hence, this function is simply a more memory efficent version
#' for sparseMatrix input using \code{text2vec::as.lda_c} for conversion with slight adaptions to make output fit to stm requirements in terms of document indices.
#'
#' @param x A \code{sparseMatrix}.
#' @param keep_rownames By default TRUE, documents are named according to the rownames of \code{x}. When set to FALSE, document names are \code{NULL}.
#'
#' @return A list y of 2 items, y$documents are documents represented similar to lda_c format, but vocabulary indices start with 1 instead of 0)
#'         and y$vocab containing the vocabulary (i.e. orignal colnames of \code{x}).
#' @export
#'
#' @examples
#'
#' library(text2vec)
#' library(stm)
#' data("movie_review")
#' it = itoken(substr(movie_review$review[1:3], 1, 50), preprocess_function = tolower,
#'            tokenizer = word_tokenizer)
#' v = create_vocabulary(it)
#' vectorizer = vocab_vectorizer(v)
#' it = itoken(movie_review$review[1:3], preprocess_function = tolower,
#'            tokenizer = word_tokenizer)
#' dtm = create_dtm(it, vectorizer)
#' all.equal(textility::sparse_to_stm(dtm), stm::readCorpus(dtm))
#' #[1] TRUE


sparse_to_stm <- function(x, keep_rownames = TRUE) {
  dtm_stm <- list(documents = NULL, vocab = colnames(x))
  if (keep_rownames == TRUE) {
     doc_names <- rownames(x)
  } else {
    doc_names <- NULL
  }
  dtm_stm$documents <- lapply(text2vec::as.lda_c(x), function(y){
    y[1,] <- y[1,]+1L
    return(y)
  })
  names(dtm_stm$documents) <- doc_names
  dtm_stm
}
