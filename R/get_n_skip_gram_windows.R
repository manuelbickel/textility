

#' Get the number of skip gram windows
#'
#' Given a list of tokens, the functions calculates the number of skip gram windows
#' that is used, e.g., during creating a \code{tcm} with \code{text2vec}, for a given window size. This function
#' calcualtes something like the total number of virtual documents used to create a \code{tcm}.
#'
#' @param tokens A list of tokens, as created, e.g., by \code{text2vec::wordtokenizer()}.
#' @param window_size The window size to be considered.
#'
#' @return The total number of windows or virtual documents.
#' @export
#'
#' @examples


get_n_skip_gram_windows = function(tokens, window_size) {
  sum(sapply(tokens, function(x) {
    #first window
    n_windows = 1
    #additional windows
    add = length(x) - window_size
    if (add > 0) {
      n_windows =  n_windows + add
    }
    return(n_windows)
  }))
}
