#test and compare results of calc_coherence
library(data.table)
library(topicmodels)
library(text2vec)
library(Matrix)
library(slam)

#Testing
n_topics = 5
ntopwords <- 6
#1a- test if different input formats really results in same ouput---------------------------------------
library(topicmodels)
data("AssociatedPress", package = "topicmodels")
dtm <- AssociatedPress[1:100,]
fitted <- LDA(dtm, method = "Gibbs" , control = list(alpha = 0.1), k = n_topics)
# str(fitted@beta)
# num [1:5, 1:10473] -10.6 -10.8 -11.2 -10.5 -10.8 ...
#str(terms(fitted, ntopwords))
# str(beta_terms)
# chr [1:6, 1:5] "police" "man" "school" "friday" "rating" "agents" "soviet" "official" ...
# - attr(*, "dimnames")=List of 2
# ..$ : NULL
# ..$ : chr [1:5] "Topic 1" "Topic 2" "Topic 3" "Topic 4" ...

coherence_beta <- calc_coherence(dtm, beta = fitted@beta, n = ntopwords)
coherence_beta_terms <-  calc_coherence(dtm, terms(fitted, ntopwords) ,n = ntopwords)
all.equal(coherence_beta, coherence_beta_terms)
#TRUE
coherence_beta
# Topic    dif_wiwj    dif_wjwi prob_lgrat lgrat_UMassep.01 prob_lgrat_ep1em12       PMI        NPMI lgrat_UMass lgrat_UMass_orig lgrat_UMassep.01_orig
# 1:    T1 -0.02788439   0.3528533   1.895745        -1.337157          -0.713224 0.6605914   0.1978244   -1.215062        -18.22593             -20.05736
# 2:    T2 -0.09933333   0.1626667   3.092282        -4.296978          -14.78266 -15.92822  -0.3346493   -1.790594        -26.85891             -64.45467
# 3:    T3 -0.04062434    0.171418   2.521489        -2.619468          -4.641662 -3.734807 0.008233418   -1.746743        -26.20115             -39.29202
# 4:    T4   0.0649426  0.04593094   1.533867        -1.308287          -1.394425 0.2211088  0.06927048   -1.161306        -17.41958              -19.6243
# 5:    T5   0.2473757 -0.01842513   1.249004       -0.8050402          -1.272997 0.5090321   0.1509717  -0.7438661        -11.15799              -12.0756

#1b - test if input from text2vec works ---------------------------------------------------------------
#direct comparison of results to topicmodels by taking the same input docs not perfect
#since text2vec uses different LDA algorithm (WarpLDA)
#still, turn dtm used by topicmodels into sparseMatrix and do LDA with text2vec
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
lda_model = LDA$new(n_topics = n_topics)
doc_topic_distr = lda_model$fit_transform(dtm_t2v, n_iter = 20)
coherence_beta_terms_textx2vec <- calc_coherence(dtm = dtm_t2v, beta = lda_model$get_top_words(n = ntopwords, topic_number = 1L:n_topics, lambda = 1)
                                                 , n = ntopwords)
coherence_beta_terms_textx2vec
# Topic    dif_wiwj    dif_wjwi prob_lgrat lgrat_UMassep.01 prob_lgrat_ep1em12       PMI        NPMI lgrat_UMass lgrat_UMass_orig lgrat_UMassep.01_orig
# 1:    T1  0.03465879   0.2238039   2.013988          -1.4735            -1.0754 0.6953608   0.1601691   -1.310128        -19.65192              -22.1025
# 2:    T2 -0.02480204   0.2098212   1.843998        -1.445024         -0.9473924 0.4589237      0.1241   -1.317684        -19.76525             -21.67536
# 3:    T3  0.03810141   0.3805788   2.242857        -1.366152         -0.8070379  1.181035    0.266452   -1.230361        -18.45541             -20.49228
# 4:    T4 -0.02941679   0.1035897   1.816026        -1.693123          -1.383667 0.1020394  0.04124577   -1.496205        -22.44308             -25.39685
# 5:    T5   0.1072522 -0.08431808   1.429139        -1.709229          -3.491649 -2.251081 -0.03540125   -1.246713         -18.7007             -25.63843

#2 - check results of the lgrat_UMass_ep.01 against results of stm package-------------------------------
#copy of internal function from stm package to calculate coherence
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

coherence_beta[,  stm_semCo:=  semCoh1_stmoriginal(mat = dtm, M = ntopwords, beta = fitted@beta)]
coherence_beta[, c("stm_semCo", "lgrat_UMassep.01_orig")]
#there are slight difference which result from different combinations of wiwj used for calucaltion
#I am not sure which calcualtion is "more correct", see differences below
# stm_semCo lgrat_UMassep.01_orig
# 1: -20.05736             -20.05736
# 2: -64.45467             -64.45467
# 3: -39.29202             -39.29202
# 4: -23.12671              -19.6243
# 5: -12.07560              -12.0756

#investigate differences in topic 4
beta_test <- fitted@beta
test_idxs <- data.table(Topic = paste0("T", 1:nrow(beta_test)))
idxs_topwords_topic <- apply(beta_test, 1, order, decreasing=TRUE)[1:ntopwords,]
idxs_topwords_unique <- unique(as.vector(idxs_topwords_topic))
test_idxs[,tcm_idxs_topwords := split(match(as.vector(idxs_topwords_topic), idxs_topwords_unique)
                                      , rep(1:nrow(beta_test), each=ntopwords))]
#up to here code is the "same" in stm
idxs_T4 <- unlist(test_idxs[4, tcm_idxs_topwords])
#[1] 17 18 19 20  2 21

create_wiwj_asym <- function(idxs) {
  do.call(rbind,
          sapply(2:length(idxs), function(x) {
            cbind(wi = rep(idxs[x], length(idxs[1:length(idxs[1:(x-1)])]))
                  ,wj = idxs[1:length(idxs[1:(x-1)])])
          }, USE.NAMES = FALSE))}

create_wiwj_asym_stm <- function(idxs) {
  grid <- expand.grid(idxs,idxs)
  colnames(grid) <- c("m", "l") #corresponds to original paper
  grid <- grid[grid$m > grid$l,] #I think this line causes the difference
  grid
}

wiwj <- create_wiwj_asym(idxs_T4)
wiwj_stm <- create_wiwj_asym_stm(idxs_T4)
#significant differences
wiwj
wiwj_stm

#assuming that the order of indices from high to low probability is
create_wiwj_asym(c(5,3,1))
#       wi wj
# [1,]  3  5
# [2,]  1  5
# [3,]  1  3
create_wiwj_asym_stm(c(5,3,1))
#   m l
# 4 5 3
# 7 5 1
# 8 3 1

#not sure what is correct, now.
#Original UMass says SUM(from m=2 to M)SUM(from l=1 to m-1)
#Does above function create_wiwj_asym do this, or is there something wrong in the function.
#If the function is correct stm package implementation is wrong?


#this step is needed to be able to use CalcProbCoh from textminer, which requires colnames and sparseMatrix
colnames(beta) <- fitted@terms
