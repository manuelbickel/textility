#' Split columns on basis of a string and duplicate entries
#'
#' Based on \code{stringi::stri_split_fixed()} column names are split on basis of a split pattern into new columns.
#' The content of the original column is duplicated in all split columns.
#' @param x A \code{data.frame} or \code{data.table}.
#' @param pattern The pattern to split the columns (will not be included in the new column names).
#' @return Dependding on the input class, a \code{data.frame} or \code{data.table} with the additional columns.
#' @export
#'
#' @examples
#' df <- data.frame(split_here = 11:13, and_here = letters[11:13],  and_again_here = 14:16)
#' split_columns(df, "_")
#' #   split here and here and again here
#' # 1    11   11   k    k  14    14   14
#' # 2    12   12   l    l  15    15   15
#' # 3    13   13   m    m  16    16   16

split_columns <- function(x, pattern) {
  duplications_names <- stringi::stri_split_fixed(colnames(x), pattern, simplify = FALSE)
  duplications_numbers <- sapply(duplications_names, length)
  duplications_columns <- mapply(rep, 1:length(duplications_numbers), duplications_numbers)
  if (class(x)[1] == "data.table") {
    x <- x[, unlist(duplications_columns), with=FALSE]
  } else {
    x <- x[, unlist(duplications_columns)]
  }
  colnames(x) <- unlist(duplications_names)
  return(x)
}


