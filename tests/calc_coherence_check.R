#test and compare results of calc_coherence

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
n_topics <- 20
n_topwords <- 25
alpha_prior <- 0.1


#1a- test if different input formats really results in same ouput---------------------------------------
data("AssociatedPress", package = "topicmodels")
dtm <- AssociatedPress[1:100,]
fitted <- LDA(dtm, method = "Gibbs" , control = list(alpha = alpha_prior, seed = seedpar), k = n_topics)
# str(fitted@beta)
# num [1:20, 1:10473] -9.93 -9.57 -9.56 -9.72 -9.53 ...
# str(terms(fitted, n_topwords))
# chr [1:25, 1:20] "bush" "dukakis" "campaign" "i" "thats" "administration" "farmer" "believe" "farm" "kind" "put" "trade" ...
# - attr(*, "dimnames")=List of 2
# ..$ : NULL
# ..$ : chr [1:20] "Topic 1" "Topic 2" "Topic 3" "Topic 4" ...

#input beta vs terms(fitted, n_topwords)
coherence_beta <- calc_coherence(dtm = dtm, beta = fitted@beta, n = n_topwords)
coherence_beta_terms <-  calc_coherence(dtm = dtm, beta = terms(fitted, n_topwords) ,n = n_topwords)
all.equal(coherence_beta, coherence_beta_terms)
#TRUE

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
coherence_text2vec <- calc_coherence(dtm = dtm_t2v, beta = lda_model$get_top_words(n = n_topwords, topic_number = 1L:n_topics, lambda = 1)
                                     , n = n_topwords)
#works, at least no error...

#1c - difference between text2vec vs topicmodels ---------------------------------------------------------------
#FIXME when using text2vec beta as input tcm must be set to as.matrix to make code work, not sure why
beta_t2v <- lda_model$get_top_words(n = n_topwords, topic_number = 1L:n_topics)
beta <- terms(fitted, n_topwords)

#compare input dtm
all(dim(dtm_t2v) == dim(dtm))
#TRUE
all(colnames(dtm_t2v) == colnames(dtm))
#TRUE
all(as.matrix(dtm_t2v) == as.matrix(dtm))
#TRUE
#compare input beta
all(dim(terms(fitted, n_topwords)) == dim(beta_t2v))
#TRUE
all(class(terms(fitted, n_topwords)) == class(beta_t2v))
#TRUE

#A difference becomes clear when looking at unique topterms (resulting fromm different LDA algorithm)
length(setdiff(unique(as.vector(beta)), unique(as.vector(beta_t2v))))
#[1] 195
length(setdiff(unique(as.vector(beta_t2v)), unique(as.vector(beta))))
#[1] 160

#Hence, direct comparison of results between topicmodels and text2vec by taking the same input not perfect due to different LDA algorithm
#still, coherence output for both packages shown below
print(coherence_beta, digits = 2)
# Topic prob_lgrat lgrat_UMassep.01 prob_lgrat_ep1em12    pmi   npmi dif_wjwi lgrat_UMass lgrat_UMass_orig lgrat_UMassep.01_orig
# 1:    T1        2.6             -2.1               -5.7  -2.64  0.236   0.0930       -1.12             -335                  -621
# 2:    T2        3.3             -2.4               -9.9  -7.14  0.157   0.1688       -0.83             -248                  -732
# 3:    T3        3.2             -2.5              -11.0  -8.74  0.064   0.1205       -0.76             -227                  -746
# 4:    T4        2.7             -2.9               -9.5  -7.25  0.056   0.0515       -1.27             -382                  -859
# 5:    T5        3.1             -3.6              -15.8 -15.28 -0.205   0.0535       -1.08             -324                 -1082
# 6:    T6        3.2             -3.2              -14.1 -12.82 -0.089   0.1030       -0.94             -282                  -961
# 7:    T7        2.4             -3.1              -11.1  -9.40 -0.037  -0.0095       -1.32             -395                  -920
# 8:    T8        2.5             -2.1               -6.0  -3.32  0.215   0.1078       -1.09             -327                  -624
# 9:    T9        3.5             -3.0              -14.6 -13.04 -0.054   0.1536       -0.72             -217                  -896
# 10:   T10        2.6             -3.1              -11.4  -9.70 -0.036   0.0256       -1.24             -373                  -919
# 11:   T11        3.1             -3.6              -15.0 -14.08 -0.150   0.0664       -1.14             -341                 -1068
# 12:   T12        3.2             -3.0              -12.3 -10.20  0.020   0.1310       -1.04             -311                  -913
# 13:   T13        3.1             -3.3              -14.5 -13.14 -0.115   0.0764       -1.01             -302                  -988
# 14:   T14        2.8             -2.5               -8.7  -6.39  0.101   0.1058       -1.08             -324                  -759
# 15:   T15        3.3             -3.5              -15.6 -14.93 -0.169   0.0881       -0.97             -291                 -1043
# 16:   T16        1.2             -1.3               -1.3   0.24  0.078  -0.0421       -1.17             -352                  -379
# 17:   T17        2.0             -1.3               -1.8   1.42  0.370   0.0974       -1.10             -330                  -402
# 18:   T18        3.1             -3.5              -14.7 -13.65 -0.141   0.0627       -1.12             -336                 -1052
# 19:   T19        3.2             -2.9              -13.0 -11.27 -0.029   0.1114       -0.85             -255                  -870
# 20:   T20        2.7             -2.6               -9.9  -7.70  0.041   0.0444       -1.08             -323                  -795

print(coherence_text2vec, digits = 2)
# Topic prob_lgrat lgrat_UMassep.01 prob_lgrat_ep1em12   pmi     npmi dif_wjwi lgrat_UMass lgrat_UMass_orig lgrat_UMassep.01_orig
# 1:    T1        2.1             -2.5               -6.0  -4.3  0.02794   0.0034        -1.5             -454                  -756
# 2:    T2        2.1             -3.2              -11.8 -10.9 -0.17388  -0.0732        -1.4             -414                  -961
# 3:    T3        2.2             -2.5               -6.8  -4.4  0.11189   0.0156        -1.4             -422                  -760
# 4:    T4        2.3             -2.7               -7.8  -5.5  0.06770   0.0100        -1.4             -421                  -806
# 5:    T5        2.2             -3.0              -10.2  -9.1 -0.09555  -0.0222        -1.4             -420                  -905
# 6:    T6        2.0             -2.3               -5.3  -3.2  0.11374   0.0028        -1.4             -431                  -687
# 7:    T7        1.8             -2.5               -6.6  -4.9  0.01221  -0.0688        -1.5             -450                  -757
# 8:    T8        2.2             -2.9               -8.2  -6.3  0.01561  -0.0221        -1.5             -461                  -864
# 9:    T9        1.9             -2.2               -4.9  -2.2  0.15432  -0.0336        -1.5             -439                  -669
# 10:   T10        2.2             -3.1               -9.5  -7.8 -0.01175  -0.0036        -1.5             -461                  -922
# 11:   T11        2.0             -3.1               -8.7  -7.1 -0.05304  -0.0702        -1.7             -502                  -921
# 12:   T12        2.3             -2.5               -7.6  -5.6  0.04282   0.0177        -1.3             -390                  -761
# 13:   T13        1.9             -2.7               -7.0  -5.2 -0.00029  -0.0539        -1.6             -474                  -801
# 14:   T14        2.1             -3.6              -11.5 -10.7 -0.15042  -0.0498        -1.7             -524                 -1082
# 15:   T15        2.0             -2.2               -5.3  -3.1  0.08619   0.0057        -1.4             -421                  -673
# 16:   T16        2.0             -2.4               -6.8  -4.7  0.06850   0.0028        -1.4             -409                  -719
# 17:   T17        2.3             -1.3               -1.8   1.7  0.41665   0.1727        -1.0             -303                  -391
# 18:   T18        2.4             -3.0               -9.2  -7.5  0.02653   0.0363        -1.5             -438                  -889
# 19:   T19        2.2             -3.0               -8.7  -6.6  0.00535  -0.0316        -1.6             -476                  -894
# 20:   T20        2.2             -2.6               -7.2  -5.0  0.09897   0.0172        -1.4             -423                  -772



#2 - validation, check results of the lgrat_UMass_ep.01 against results of stm package-------------------------------
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
coherence_beta <- calc_coherence(dtm = dtm, beta = fitted@beta, n = n_topwords)
coherence_beta[,  stm_semCo:=  semCoh1_stmoriginal(mat = dtm, M = n_topwords, beta = fitted@beta)]
all(coherence_beta[, stm_semCo - lgrat_UMassep.01_orig] == 0)
#TRUE
coherence_beta[,compare:= (lgrat_UMassep.01_orig - stm_semCo)]
coherence_beta[, c("lgrat_UMassep.01_orig",  "stm_semCo", "compare")]
# lgrat_UMassep.01_orig  stm_semCo compare
# 1:             -621.4987  -621.4987       0
# 2:             -731.6476  -731.6476       0
# 3:             -745.9893  -745.9893       0
# 4:             -858.5350  -858.5350       0
# 5:            -1081.9264 -1081.9264       0
# 6:             -961.2239  -961.2239       0
# 7:             -919.7757  -919.7757       0
# 8:             -624.0398  -624.0398       0
# 9:             -895.7719  -895.7719       0
# 10:             -919.3926  -919.3926       0
# 11:            -1068.0184 -1068.0184       0
# 12:             -913.1634  -913.1634       0
# 13:             -988.1994  -988.1994       0
# 14:             -759.0453  -759.0453       0
# 15:            -1042.6307 -1042.6307       0
# 16:             -379.4287  -379.4287       0
# 17:             -402.3760  -402.3760       0
# 18:            -1051.8613 -1051.8613       0
# 19:             -870.3910  -870.3910       0
# 20:             -794.7066  -794.7066       0
