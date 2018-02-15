#' A wrapper function for various preprocessing options for strings
#'
#' @param x A \code{character} vector.
#' @param force_encoding The encdoding to be forced on the string.
#' @param alltolower Turn all letters to lower case.
#' @param erase_patterns Fixed non-regex patterns to be erased from text as is.
#'                       The \code{force_encoding} and \code{tolower} settings are applied on these patterns before matching for removal.
#' @param token_exclude_length Remove tokens that have specified number of characters or less, enclosed by word boundaries.
#' @param rm_diacritics Turn diacritics into their ASCII pendnant.
#' @param replace_dashes_hyphens_by Various forms of dashes and hyphens, e.g., long dash, dash, hyphen, etc., defined in Unicode table are replaced by the sepcified fixed pattern.
#' @param rm_roman_numeral_listing Erase all brackets and their content if bracket includes a combination of i,v, and x.
#'                                 There are also higher number that require M and C, however, functions aims at listing of lower numbers
#'                                 usually used in reports. More sophisticated regex replacements possible with below parameter.
#'
#' @param replace_by_blank_regex A regex pattern to be replaced by a blank. Use "|" to replace more than one pattern.
#' @param erase_regex A regex pattern to be replaced by nothing, i.e., "". Use "|" to replace more than one pattern.
#' @param harmonize_blanks Remove blanks at the begining and end of a string and collapses sequences of multiple blanks into one.
#'
#' @return The processed string.
#' @export
#'
#' @examples
#'

stri_process <- function(x
                                   , force_encoding = "UTF-8"
                                   , alltolower = FALSE
                                   , erase_patterns = NULL
                                   , token_exclude_length = NULL
                                   , rm_diacritics = FALSE
                                   , replace_dashes_hyphens_by = NULL
                                   , rm_roman_numeral_listing = FALSE
                                   , replace_by_blank_regex =  NULL #c("\\b\\d+\\b", "\\b\\W+\\b")
                                   , erase_regex = NULL #c("[^A-Za-z_ ]")
                                   , harmonize_blanks = FALSE
) {

  if (!is.null(force_encoding)) {
    Encoding(x) <- force_encoding
  }

  if (alltolower == TRUE) {
    x <-  stri_trans_general(x, "lower") #tolower(x)
  }

  if (!is.null(erase_patterns)) {

    if (!is.null(force_encoding)) {
      Encoding(erase_patterns) <- force_encoding
    } else {
      Encoding(erase_patterns) <- Encoding(x)
    }

    if (alltolower == TRUE) {
      erase_patterns <-  stri_trans_general(erase_patterns, "lower")
    }

    erase_patterns <- unique(erase_patterns)

    length_erase_patterns_uncleaned <- length(erase_patterns)

    erase_patterns <- erase_patterns[ !is.na(erase_patterns) & erase_patterns != ""]

    erase_patterns <- erase_patterns[order(stringi::stri_count_fixed(erase_patterns, " "), decreasing = TRUE)]

    if (length_erase_patterns_uncleaned != length(erase_patterns)) {
      warning("Erase patterns inlcuded NA values or empty entries. These were ignored during replacement.")
    }

    #https://stackoverflow.com/questions/26676045/replace-a-set-of-pattern-matches-with-corresponding-replacement-strings-in-r
    x <- stringi::stri_replace_all_fixed(x
                                         ,pattern = erase_patterns
                                         ,replacement = rep(" ", length(erase_patterns))
                                         ,vectorize_all = F
                                         ,case_insensitive = T)

  }

  #remove listing in Roman letters
  if (rm_roman_numeral_listing == TRUE) {
    x <- stringi::stri_replace_all_regex(x
                                         ,pattern = "\\([ivx]+\\)"
                                         ,replacement = ""
                                         ,vectorize_all = F
                                         ,case_insensitive = T)
  }

  if (rm_diacritics == TRUE) {
    x <- iconv(x, from = force_encoding, to ="ASCII//TRANSLIT")
  }

  if (!is.null(replace_dashes_hyphens_by)) {
    #https://stackoverflow.com/questions/1011708/regex-to-replace-hyphen-in-the-middle-of-a-word
    #http://www.unicode.org/versions/Unicode10.0.0/ch06.pdf
    dashes <- stringi::stri_trans_general(paste0("U+", c(2010:2015, 2043, "002D", "FE63", "FF0D")), "Hex-Any/Name")
    if (!is.null(force_encoding)) {
      Encoding(dashes) <- force_encoding
    } else {
      Encoding(dashes) <- Encoding(x)
    }
    x <- gsub(paste0("(?<=\\w)(", paste(dashes, collapse = "|") , ")(?=\\w)"), replace_dashes_hyphens_by, x, perl=T)
  }

  if (!is.null(replace_by_blank_regex)) {
    x <- gsub(paste(replace_by_blank_regex, collapse = "|"), " ", x, perl=T)
  }

  if (!is.null(erase_regex)) {
    x <- gsub(paste(erase_regex, collapse = "|"), "", x, perl=T)
  }

  if (!is.null(token_exclude_length)) {
    #https://stackoverflow.com/questions/33226616/how-to-remove-words-of-specific-length-in-a-string-in-r
    x <- gsub(paste0("(?<=\\b)(\\w{1,", token_exclude_length, "})(?=\\b)"), "", x, perl=T)
  }

  if (harmonize_blanks == TRUE) {
    x <- gsub("\\s+", " ", x , perl=T)
    x <- gsub("^\\s|\\s$", "", x, perl=T)
  }

  if( force_encoding == TRUE & !all(stri_enc_isutf8(x)) ) {
    warning("A check via stri_enc_isutf8(x) tells that a part of the ouput is not in UTF-8 after pre-processing. This may or may not have an effect on your downstream tasks.")
  }

  return(x)
}
