#' Replace acronyms by words
#'
#' Searches for acronyms that are followed by a bracketed explanation and replaces the acronym by the content in the brackets.
#'
#' Currently requires stringi and magrittr loaded (the latter dependency will be removed in the future).
#'
#' NOTE: The implementation is currently quite slow if there are many acronyms that are not matched. Might be improved in future...
#'
#' Documentation still to be updated, please check the code in the meantime.
#'
#' @param docs The documents to be processed in form of a \code{character} vector. Each document is processed separately, hence no shared acronyms are assumed.
#' @param remove_replaced_acr By default TRUE. If an acronym was identified and replaced, the acronym itself is deleted.
#' @param remove_all_bracketed_acr By default TRUE. All acronyms in brackets are removed. The identification pattern is \code{([A-Z][A-Za-z]*[A-Z]s?\\)}.
#'
#' @return The documents wit acronyms replaced.
#' @export
#'
#' @examples
#' replace_acronyms_by_words("The United Nations Framework Convention on Climate Change (UNFCCC) is a milestone. The UNFCCC came into force in 1994.")
#  #[1] "The United Nations Framework Convention on Climate Change  is a milestone. The  United Nations Framework Convention on Climate Change came into force in 1994."
#'

replace_acronyms_by_words <- function(docs, remove_replaced_acr = TRUE, remove_all_bracketed_acr = TRUE) {

  docs <- sapply(docs, function(d) {
    acronyms <- unlist(stringi::stri_extract_all_regex(d, "(?<=\\()([^\\)]+)(?=\\))")) %>%
      .[grep("^[A-Z][^\\s]*[A-Z][^\\s]*$", ., perl = T)] %>%
      .[ !grepl("\\W", .)] %>%
      gsub("s$", "",.) %>%
      unique

    if (length(acronyms) == 0) {
      return(d)
    } else {
      acronym_word_pattern <- lapply(strsplit(acronyms, ""), function(a) {
        words_before <- paste0("(^| )", paste0("("%s+%a[1:length(a)-1]%s+%"\\w+-?\\w+ )((of the ){0,1}|(from the ){0,1}|(and ){0,1}|(of ){0,1}|(by ){0,1}|(to ){0,1}|(on ){0,1}|(from ){0,1})", collapse = ""),
                               "("%s+%a[length(a)]%s+%"\\w+-?\\w+ )", collapse = "")
        paste0(words_before, "\\(", paste0(a, collapse = ""), "s?\\)")
      })

      acronym_word_pattern <- lapply(acronym_word_pattern, function(p) {
        p <- unlist(stringi::stri_extract_all_regex(d, p, case_insensitive = TRUE))
        res <- gsub("\\)", "", stringi::stri_split_fixed(p, " (", simplify = T))

        if ( any(is.na(res)) ) {
          return(NULL)
        } else {
          res <- as.data.frame(res, stringsAsFactors = FALSE)
          colnames(res) <- c("words", "acronym")
          res$acronym <- gsub("s$", "", res$acronym)
          return(res)
        }
      })

      acronym_word_pattern <- unique(data.table::rbindlist(acronym_word_pattern, fill = TRUE, use.names = TRUE))

      if (nrow(acronym_word_pattern) == 0) {
        return(d)
      } else {
        d <- stringi::stri_replace_all_regex(d
                                    , paste0("(?<=\\s)(", acronym_word_pattern$acronym , "s?)(?=\\s|\\.|,)")
                                    , acronym_word_pattern$word
                                    , vectorize_all = FALSE)

        d <- stringi::stri_replace_all_regex(d
                                    , paste0("\\(", acronym_word_pattern$acronym , "s?\\)")
                                    , rep("", length(acronym_word_pattern$acronym))
                                    , vectorize_all = FALSE)
        return(d)
      }
    }
  }, USE.NAMES = FALSE)

  if (remove_all_bracketed_acr == TRUE) {
    docs <- stringi::stri_replace_all_regex(docs, "\\([A-Z][A-Za-z]*[A-Z]s?\\)", " ", vectorize_all = FALSE)
    return(docs)
  } else {
    return(docs)
  }
}
