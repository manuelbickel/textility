

#' Replace blanks by replacement pattern in known ngrams in a string
#'
#' Usually ngrams are identified and modified by probabilistic collocation extraction,
#' but in certain situations one might want to fix specific word combinations before further processing of the text
#' independent of collocation statistics such as PMI.
#'
#' @param string A character vector in which blanks of ngrams shall be replaced.
#' @param ngrams Character vector of known ngrams. Please note that ngrams in the \code{return} will have the case formatting of these \code{ngrams}.
#' @replacement A fixed pattern that shall replace blanks in ngrams. By default a dash "_".
#' @param case_insensitive By default TRUE. Note that case is only used for matching (see ngram parameter)
#'
#' @return The \code{string} with modified ngrams.
#' @export
#'
#' @examples
#'
#' bind_ngrams(c("The United Nations are an important organization.",
#'               "They are concerned, e.g., with sustainable development and climate change.")
#'             , ngrams = c("United Nations", "CLIMATE CHANGE"))
#' # [1] "The United_Nations are an important organization."
#' # [2] "They are concerned, e.g., with sustainable development and CLIMATE_CHANGE."


bind_ngrams <- function(string, ngrams, replacement = "_", case_insensitive = TRUE) {
  ngrams <- unique(ngrams)
  ngrams <- ngrams[order(stringi::stri_count_fixed(ngrams, " "), decreasing = TRUE)]
  ngrams_new <- stringi::stri_replace_all_fixed(ngrams, " ", replacement)
  stringi::stri_replace_all_fixed(str = string
                                  ,pattern = ngrams
                                  ,replacement = ngrams_new
                                  ,case_insensitive = case_insensitive
                                  ,vectorize_all = FALSE)
}
