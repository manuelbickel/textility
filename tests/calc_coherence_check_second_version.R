#' dtm = cbind(A = c(1,2,3,0, 0), B = c(4,0,5,0, 0), C = c(0,6,7,8, 0), D = c(0,0,0,1, 1))
#' topic_word_distribution = rbind(T1 = c(0.5,0.3,0.19, 0.01), T2 = c(0.19,0.3,0.5, 0.01),  T3 = c(0.3,0.5,0.19, 0.01))
#' colnames(topic_word_distribution) = c("A", "B", "C", "D")
#' n_topterms = 2
#' top_term_mat = apply(topic_word_distribution, 1 , function(x) order(x, decreasing = T))[1:n_topterms, ]
#' top_term_mat = apply(top_term_mat, 2 , function(x) colnames(topic_word_distribution)[x])
#' dtm_top_terms = dtm[,(unique(as.vector(top_term_mat)))]
#' dtm_top_terms[dtm_top_terms > 1] = 1
#' tcm_top_terms = crossprod(dtm_top_terms)
#'
#' coherence( tcm = tcm_top_terms
#'                , top_term_matrix = top_term_mat
#'                , n_tcm_windows = nrow(dtm))
#' # Topic logratio_UMass logratio prob_logratio    pmi    npmi prob_dif
#' # 1:    T1        -0.4055  -0.4055       -0.4055  0.737  0.5575      0.6
#' # 2:    T2        -0.6931  -1.0986       -1.0986 -0.263 -0.1133      0.1
#' # 3:    T3        -0.4055  -0.4055       -0.4055  0.737  0.5575      0.6




#Demonstation of basic functionality with some (unrealistic) numbers -------------------------------------------------
dtm <- cbind(A = c(1,2,3,0, 0), B = c(4,0,5,0, 0), C = c(0,6,7,8, 0), D = c(0,0,0,1, 1))
ndocs <- nrow(dtm)
topic_word_distribution <- rbind(T1 = c(0.5,0.3,0.19, 0.01), T2 = c(0.19,0.3,0.5, 0.01),  T3 = c(0.3,0.5,0.19, 0.01))
top_term_mat <- top_feature_matrix(topic_word_distribution, 2, terms = c("A", "B", "C", "D"))
# top_term_mat <- structure(c("A", "B", "C", "B", "B", "A")
#                           , .Dim = 2:3, .Dimnames = list(NULL, c("T1", "T2", "T3")))
dtm_top_terms <- dtm[,na.omit(unique(as.vector(top_term_mat)))]
# dtm_top_terms <- structure(c(1, 2, 3, 0, 0, 4, 0, 5, 0, 0, 0, 6, 7, 8, 0)
#                             , .Dim = c(5L, 3L), .Dimnames = list(NULL, c("A", "B", "C")))
tcm_top_terms <- get_cooccurrence(dtm_top_terms)
# tcm_top_terms <- structure(c(3, 2, 2, 2, 2, 1, 2, 1, 3)
                             # , .Dim = c(3L, 3L), .Dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
twcm <- tcm_top_terms
top_term_matrix <- top_term_mat

calc_coherence( tcm = tcm
                , top_term_matrix = top_term_matrix
                , average_over_topics = FALSE
                , log_smooth_constant = .01 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
                , ndocs = nrow(dtm))
# Topic logratio_UMass logratio prob_logratio     pmi    npmi prob_dif
# 1:    T1        -0.4038  -0.4038       -0.3808  0.7726  0.6006      0.6
# 2:    T2        -0.6882  -1.0920       -1.0498 -0.1926 -0.0856      0.1
# 3:    T3        -0.4038  -0.4038       -0.3808  0.7726  0.6006      0.6


#Test 1: check if different input types work
#a:  check if different input formats from from topicmodels result in same ouput:
#    raw beta, i.e., numeric word distribution over topics, VS top words beta, i.e., actual words as characters per topic
#b:  input from text2vec: top words beta, i.e., actual words as characters per topic
#c:  difference of output for input from text2vec vs topicmodels

#Test 2: for validaation, check if coherence results for adapted UMAss measure are in line with results from stm
library(data.table)
library(topicmodels)
library(text2vec)
library(Matrix)
library(slam)

seedpar <- 42 #as input to topicmodels LDA
set.seed(seedpar) #seed for text2vec LDA
n_topics <- 5
n_topwords <- 10
alpha_prior <- 0.1

#1a- test if different input formats really results in same ouput---------------------------------------
data("AssociatedPress", package = "topicmodels")
dtm <- AssociatedPress[1:100,]
fitted <- LDA(dtm, method = "Gibbs" , control = list(alpha = alpha_prior, seed = seedpar), k = n_topics)

tt_mat <- make_top_term_matrix(fitted@beta, terms = fitted@terms, n = n_topwords)
dtm_tt <- dtm[,(unique(as.vector(tt_mat)))]
tcm_tt <- get_cooccurrence(dtm_tt)

coherence <- calc_coherence( tcm = tcm_tt
                             , top_term_matrix = tt_mat
                             , average_over_topics = FALSE
                             , log_smooth_constant =  .01#.1e-12 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
                             , ndocs = nrow(dtm))
# Topic logratio_UMass  logratio prob_logratio    pmi   npmi prob_dif
# 1:    T1       -66.5718  -78.8201       -1.4696 0.6826 0.1789   0.2574
# 2:    T2       -65.5420  -71.1473       -1.3861 1.0470 0.2741   0.3849
# 3:    T3       -64.4604  -82.1350       -1.4825 1.2059 0.2805   0.3168
# 4:    T4      -153.6410 -173.3197       -1.8565 2.1617 0.3801   0.2525
# 5:    T5      -105.8565 -114.5438       -1.7396 1.0560 0.2242   0.2340


#1b - test if input from text2vec works ---------------------------------------------------------------
#following functions needed to use same data as above but as sparseMatrix as required by text2vec
tripl_to_sparse <- function(simple_triplet_matrix) {
  Matrix::sparseMatrix(i=simple_triplet_matrix$i
                       ,j=simple_triplet_matrix$j
                       ,x=simple_triplet_matrix$v,
                       dims=c(simple_triplet_matrix$nrow
                              , simple_triplet_matrix$ncol)
                       ,dimnames = dimnames(simple_triplet_matrix)
  )
}
dtm_t2v = tripl_to_sparse(dtm)

set.seed(seedpar)
lda_model = LDA$new(n_topics = n_topics, topic_word_prior = alpha_prior)
doc_topic_distr = lda_model$fit_transform(dtm_t2v, n_iter = 2000)

tt_mat_t2v <- lda_model$get_top_words(n = n_topwords, topic_number = 1L:n_topics, lambda = 1)
tcm_tt_t2v <- get_cooccurrence(dtm_t2v[,(unique(as.vector(tt_mat_t2v)))])

coherence_t2v <- calc_coherence( tcm = tcm_tt_t2v
                , top_term_matrix = tt_mat_t2v
                , average_over_topics = FALSE
                , log_smooth_constant =  .01#.1e-12 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
                , ndocs = nrow(dtm_t2v))

#1c - difference between text2vec vs topicmodels ---------------------------------------------------------------
#compare input dtm
all(dim(dtm_t2v) == dim(dtm))
#TRUE
all(colnames(dtm_t2v) == colnames(dtm))
#TRUE
all(as.matrix(dtm_t2v) == as.matrix(dtm))
#TRUE
#compare input beta
all(dim(tt_mat) == dim(tt_mat_t2v))
#TRUE

#A difference becomes clear when looking at unique topterms (resulting fromm different LDA algorithm)
length(setdiff(unique(as.vector(tt_mat)), unique(as.vector(tt_mat_t2v))))
#[1] 16
length(setdiff(unique(as.vector(tt_mat_t2v)), unique(as.vector(tt_mat))))
#[1] 19

#Hence, direct comparison of results between topicmodels and text2vec by taking the same input not perfect due to different LDA algorithm
#still, coherence output for both packages shown below
coherence
#    Topic logratio_UMass  logratio prob_logratio    pmi   npmi prob_dif
# 1:    T1       -66.5718  -78.8201       -1.4696 0.6826 0.1789   0.2574
# 2:    T2       -65.5420  -71.1473       -1.3861 1.0470 0.2741   0.3849
# 3:    T3       -64.4604  -82.1350       -1.4825 1.2059 0.2805   0.3168
# 4:    T4      -153.6410 -173.3197       -1.8565 2.1617 0.3801   0.2525
# 5:    T5      -105.8565 -114.5438       -1.7396 1.0560 0.2242   0.2340

coherence_t2v
#    Topic logratio_UMass logratio prob_logratio    pmi   npmi prob_dif
# 1:    T1       -82.6419 -86.4202       -1.5647 0.7272 0.1755   0.2044
# 2:    T2       -77.4350 -82.6703       -1.5140 0.8324 0.2189   0.3231
# 3:    T3       -78.6591 -83.1103       -1.5783 0.4833 0.1161   0.1271
# 4:    T4       -59.8379 -69.6215       -1.3571 1.0350 0.2644   0.3276
# 5:    T5       -83.0569 -85.3334       -1.5523 0.7201 0.1742   0.2077

#quite some differences in numbers and coherence order of topics
coherence[,2:ncol(coherence)] - coherence_t2v[,2:ncol(coherence_t2v)]
#   logratio_UMass  logratio prob_logratio     pmi   npmi prob_dif
# 1:        16.0701    7.6001        0.0951 -0.0446 0.0034   0.0530
# 2:        11.8930   11.5230        0.1279  0.2146 0.0552   0.0618
# 3:        14.1987    0.9753        0.0958  0.7226 0.1644   0.1897
# 4:       -93.8031 -103.6982       -0.4994  1.1267 0.1157  -0.0751
# 5:       -22.7996  -29.2104       -0.1873  0.3359 0.0500   0.0263

coherence[, lapply(.SD, order), .SDcols = 2:ncol(coherence)] - coherence_t2v[, lapply(.SD, order), .SDcols = 2:ncol(coherence_t2v)]
# logratio_UMass logratio prob_logratio pmi npmi prob_dif
# 1:             -1        3             1  -2   -2        2
# 2:              4        0             4  -3    0        3
# 3:             -2        0            -2   4    1       -4
# 4:              0       -1            -1   1    1        1
# 5:             -1       -2            -2   0    0       -2


#2 - validation, check results of the logratio_UMass (with smoothing of .01) against results of stm package-------------------------------
#the following is a copy of the internal core function from stm package to calculate coherence within the function semanticCoherence
#(similar to UMass but with smoothing factor epsilon = .1)
semCoh1_stmoriginal <- function(mat, M, beta){
  #Get the Top N Words
  top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
  wordlist <- unique(as.vector(top.words))
  mat <- mat[,wordlist]
  mat$v <- ifelse(mat$v>1, 1,mat$v) #binarize

  #do the cross product to get co-occurences
  cross <- slam::tcrossprod_simple_triplet_matrix(t(mat))

  #create a list object with the renumbered words (so now it corresponds to the rows in the table)
  temp <- match(as.vector(top.words),wordlist)
  labels <- split(temp, rep(1:nrow(beta), each=M))

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

#directly compare results
coherence <- calc_coherence( tcm = tcm_tt
                             , top_term_matrix = tt_mat
                             , average_over_topics = FALSE
                             , log_smooth_constant =  .01#.1e-12 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
                             , ndocs = nrow(dtm))
coherence[,  stm_semCo:=  round(semCoh1_stmoriginal(mat = dtm, M = n_topwords, beta = fitted@beta), d = 4)]
coherence[,compare:= (logratio_UMass - stm_semCo)]
coherence
#    Topic logratio_UMass  logratio prob_logratio    pmi   npmi prob_dif stm_semCo compare
# 1:    T1       -66.5718  -78.8201       -1.4696 0.6826 0.1789   0.2574  -66.5718       0
# 2:    T2       -65.5420  -71.1473       -1.3861 1.0470 0.2741   0.3849  -65.5420       0
# 3:    T3       -64.4604  -82.1350       -1.4825 1.2059 0.2805   0.3168  -64.4604       0
# 4:    T4      -153.6410 -173.3197       -1.8565 2.1617 0.3801   0.2525 -153.6410       0
# 5:    T5      -105.8565 -114.5438       -1.7396 1.0560 0.2242   0.2340 -105.8565



#--------------------------------third version

#Demonstation of basic functionality with some (unrealistic) numbers -------------------------------------------------
dtm <- cbind(A = c(1,2,3,0, 0), B = c(4,0,5,0, 0), C = c(0,6,7,8, 0), D = c(0,0,0,1, 1))
ndocs <- nrow(dtm)
topic_word_distribution <- rbind(T1 = c(0.5,0.3,0.19, 0.01), T2 = c(0.19,0.3,0.5, 0.01),  T3 = c(0.3,0.5,0.19, 0.01))
top_term_mat <- make_top_term_matrix(topic_word_distribution, 2, terms = c("A", "B", "C", "D"))
# top_term_mat <- structure(c("A", "B", "C", "B", "B", "A")
#                           , .Dim = 2:3, .Dimnames = list(NULL, c("T1", "T2", "T3")))
dtm_top_terms <- dtm[,(unique(as.vector(top_term_mat)))]
# dtm_top_terms <- structure(c(1, 2, 3, 0, 0, 4, 0, 5, 0, 0, 0, 6, 7, 8, 0)
#                             , .Dim = c(5L, 3L), .Dimnames = list(NULL, c("A", "B", "C")))
tcm_top_terms <- get_cooccurrence(dtm_top_terms)
# tcm_top_terms <- structure(c(3, 2, 2, 2, 2, 1, 2, 1, 3)
# , .Dim = c(3L, 3L), .Dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
tcm <- tcm_top_terms
top_term_matrix <- top_term_mat

calc_coherence( tcm = tcm
                , top_term_matrix = top_term_matrix
                , average_over_topics = FALSE
                , log_smooth_constant = .01 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
                , ndocs = nrow(dtm))
# Topic logratio_UMass logratio prob_logratio     pmi    npmi prob_dif
# 1:    T1        -0.4038  -0.4038       -0.3808  0.7726  0.6006      0.6
# 2:    T2        -0.6882  -1.0920       -1.0498 -0.1926 -0.0856      0.1
# 3:    T3        -0.4038  -0.4038       -0.3808  0.7726  0.6006      0.6


#Test 1: check if different input types work
#a:  check if different input formats from from topicmodels result in same ouput:
#    raw beta, i.e., numeric word distribution over topics, VS top words beta, i.e., actual words as characters per topic
#b:  input from text2vec: top words beta, i.e., actual words as characters per topic
#c:  difference of output for input from text2vec vs topicmodels

#Test 2: for validaation, check if coherence results for adapted UMAss measure are in line with results from stm
library(data.table)
library(topicmodels)
library(text2vec)
library(Matrix)
library(slam)
library(textility)

seedpar <- 42 #as input to topicmodels LDA
set.seed(seedpar) #seed for text2vec LDA
n_topics <- 5
n_topwords <- 10
alpha_prior <- 0.1

#1a- test if different input formats really results in same ouput---------------------------------------
data("AssociatedPress", package = "topicmodels")
dtm <- AssociatedPress[1:100,]
fitted <- LDA(dtm, method = "Gibbs" , control = list(alpha = alpha_prior, seed = seedpar), k = n_topics)

tt_mat <- top_feature_matrix(fitted@beta, terms = fitted@terms, n = n_topwords, include_all_ties = FALSE)
dtm_tt <- dtm[,(unique(as.vector(tt_mat)))]
tcm_tt <- get_cooccurrence(dtm_tt)
#
# coherence <- coherence2( tcm = tcm_tt
#                              , top_term_matrix = tt_mat
#                              #, average_over_topics = FALSE
#                              #, log_smooth_constant =  .01#.1e-12 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
#                              , n_tcm_windows = nrow(dtm))


#1b - test if input from text2vec works ---------------------------------------------------------------
#following functions needed to use same data as above but as sparseMatrix as required by text2vec
tripl_to_sparse <- function(simple_triplet_matrix) {
  Matrix::sparseMatrix(i=simple_triplet_matrix$i
                       ,j=simple_triplet_matrix$j
                       ,x=simple_triplet_matrix$v,
                       dims=c(simple_triplet_matrix$nrow
                              , simple_triplet_matrix$ncol)
                       ,dimnames = dimnames(simple_triplet_matrix)
  )
}



#2 - validation, check results of the logratio_UMass (with smoothing of .01) against results of stm package-------------------------------
#the following is a copy of the internal core function from stm package to calculate coherence within the function semanticCoherence
#(similar to UMass but with smoothing factor epsilon = .1)
semCoh1_stmoriginal <- function(mat, M, beta){
  #Get the Top N Words
  top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
  wordlist <- unique(as.vector(top.words))
  mat <- mat[,wordlist]
  mat$v <- ifelse(mat$v>1, 1,mat$v) #binarize

  #do the cross product to get co-occurences
  cross <- slam::tcrossprod_simple_triplet_matrix(t(mat))

  #create a list object with the renumbered words (so now it corresponds to the rows in the table)
  temp <- match(as.vector(top.words),wordlist)
  labels <- split(temp, rep(1:nrow(beta), each=M))

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

#directly compare results
coherence <- coherence2( twcm = tcm_tt
                             , top_term_matrix = tt_mat

                           ,measure = c("all",  "prob_diff_nonvectorized")

                             , n_twcm_windows = nrow(dtm))
coherence[,  stm_semCo:=  round(semCoh1_stmoriginal(mat = dtm, M = n_topwords, beta = fitted@beta), d = 4)]
coherence[,compare:= (sum_logratio_stm_pckg - stm_semCo)]
coherence
#    Topic logratio_UMass  logratio prob_logratio    pmi   npmi prob_dif stm_semCo compare
# 1:    T1       -66.5718  -78.8201       -1.4696 0.6826 0.1789   0.2574  -66.5718       0
# 2:    T2       -65.5420  -71.1473       -1.3861 1.0470 0.2741   0.3849  -65.5420       0
# 3:    T3       -64.4604  -82.1350       -1.4825 1.2059 0.2805   0.3168  -64.4604       0
# 4:    T4      -153.6410 -173.3197       -1.8565 2.1617 0.3801   0.2525 -153.6410       0
# 5:    T5      -105.8565 -114.5438       -1.7396 1.0560 0.2242   0.2340 -105.8565

beta <-  fitted@beta
colnames(beta) <- colnames(dtm)
textmineR::CalcProbCoherence(phi = beta
                                  ,dtm = tripl_to_sparse(dtm)
                                  ,M =   n_topwords)





