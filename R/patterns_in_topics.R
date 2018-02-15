

#' Check occurrence and rank of patterns per topic
#'
#' @param term_patterns The term patterns to be checked for each topic.
#'                      Passed to \code{grep}, hence, regex such as "|" for checking multiple terms at a time allowed.
#' @param topics A character \code{matrix} containing term lists per topic.
#'               The input might e.g. be \code{topicmodels::terms(model, model@Dim[2])}
#' @param check_n_top_terms The number of top terms to be considered for the check.
#'
#' @return A list of character matrices named after and having elements equal to number of \code{term_patterns}.
#'         Matrices contain the input \code{topics} reduced to the columns that contain the pattern to be checked
#'         and reduced to number of rows as specified via \code{check_n_top_terms}.
#'         Furthermore, the minimum rank (1 is best) of the pattern in each topic is pasted into the respective colname.
#' @export
#'
#' @examples
#'
#'
#'
#'patterns_in_topics(term_patterns = c("environ*", "terms")
#'                   , topics = cbind(c("irrelevant", "topic", "terms"), c("global", "environmental", "protection"))
#'                   , check_n_top_terms = 2)
#' $`environ*`
#' Topic 2 - min_rank_2
#' [1,] "global"
#' [2,] "environmental"
#'
#' $terms
#' NULL


patterns_in_topics <- function(term_patterns, topics, check_n_top_terms = 50) {
  if (is.null(colnames(topics))) {
    colnames(topics) <- paste0("Topic ", 1:ncol(topics))
  }
  sapply(term_patterns, function(x) {
    #highest rank of term pattern in topics that contain the pattern at all
    rank <- apply(topics, 2, function(topic) {
      grep(x, topic)[1]
    })
    rank <- rank[!is.na(rank)]
    #limit to ranks below threshold
    rank <- rank[which(rank <= check_n_top_terms)]
    if (length(rank) == 0) {
      return(NULL)
    }
    topic_term_subset <- topics[1:max(rank), names(rank)]
    if (length(rank) == 1) {
      topic_term_subset <- as.matrix(topic_term_subset, ncol = 1)
      colnames(topic_term_subset) <- names(rank)
    }
    colnames(topic_term_subset) <-  paste0(colnames(topic_term_subset), paste0(" - min_rank_", rank))
    return(topic_term_subset)
  }, USE.NAMES = TRUE, simplify = F)
}

