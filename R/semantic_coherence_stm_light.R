

#' Semantic Coherence (as used in stm package, but a light version)
#'
#' This function is simply an adapted copy of to core function in stm package to calcuate semantic coherence.
#' Basically it is the UMass coherence measure with log smooth constant of .01 instead of 1.
#' Core function was extracted to make it versatile by enabling input that does not come from stm package
#' (especially for testing other coherence calculation implementaions).
#' Its a light version, because no checks are made regarding correctness of input and for true versatility
#' acceptance of more input types would have to be imnplemented than only simple triplet matrices.
#' Note that some names were changed in comparison to original implementation.
#'
#' @param simple_triplet_dtm A document term matrix in simple triplet format from slam package.
#' @param n_top_words Number of top n words in \code{beta} to be considererd per topic.
#' @param beta NUmeric matrix with Word distributions over topics (column = word, each row = 1 topic, entries = probabilities)
#'
#' @return Coherence score per topic.
#' @export
#'
#' @examples

semantic_coherence_stm_light <-  function(simple_triplet_dtm, n_top_words, beta){
  #Get the Top N Words
  top.words <- apply(beta, 1, order, decreasing=TRUE)[1:n_top_words,]
  wordlist <-  unique(as.vector(top.words))
  simple_triplet_dtm <- simple_triplet_dtm[,wordlist]
  simple_triplet_dtm$v <- ifelse(simple_triplet_dtm$v>1, 1,simple_triplet_dtm$v) #binarize

  #do the cross product to get co-occurences
  cross <- slam::tcrossprod_simple_triplet_matrix(t(simple_triplet_dtm))

  #create a list object with the renumbered words (so now it corresponds to the rows in the table)
  temp <- match(as.vector(top.words),wordlist)
  labels <- split(temp, rep(1:nrow(beta), each=n_top_words))

  #Note this could be done with recursion in an elegant way, but let's just be simpler about it.
  sem <- function(ml,cross) {
    m <- ml[1]; l <- ml[2]
    log(.01 + cross[m,l]) - log(cross[l,l] + .01)
  }
  result <- vector(length=nrow(beta))
  for(k in 1:nrow(beta)) {
    grid <- expand.grid(labels[[k]],labels[[k]])
    colnames(grid) <- c("m", "l") #corresponds to original paper
    grid <- grid[grid$m > grid$l,]
    calc <- apply(grid,1,sem,cross)
    result[k] <- sum(calc)
  }
  return(result)
}
