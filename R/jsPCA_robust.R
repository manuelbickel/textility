#' (numerically robust) Dimension reduction via Jensen-Shannon Divergence & Principal Components
#'
#' This function is a largely a copy of the repsective function in
#' https://github.com/cpsievert/LDAvis/blob/master/R/createJSON.R, however,
#' with a fix to avoid log(0) proposed by Maren-Eckhoff in
#' https://github.com/cpsievert/LDAvis/issues/56
#'
#' (function currently a pending issue in text2vec package)
#'
#' @param phi matrix, with each row containing the distribution over terms
#' for a topic, with as many rows as there are topics in the model, and as
#' many columns as there are terms in the vocabulary.
#'
#' @export
jsPCA_robust <- function(phi) {
  # first, we compute a pairwise distance between topic distributions
  # using a symmetric version of KL-divergence
  # http://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
  jensenShannon <- function(x, y) {
    m <- 0.5*(x + y)
    # fixed calculation
    0.5*(sum(ifelse(x==0,0,x*log(x/m)))+sum(ifelse(y==0,0,y*log(y/m))))
  }
  dist.mat <- proxy::dist(x = phi, method = jensenShannon)
  # then, we reduce the K by K proximity matrix down to K by 2 using PCA
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[,1], y = pca.fit[,2])
}
