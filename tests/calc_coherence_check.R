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

#1c - difference between text2vec vs topicmodels ---------------------------------------------------------------
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
coherence_beta
# Topic     dif_wiwj  dif_wjwi prob_lgrat lgrat_UMassep.01 prob_lgrat_ep1em12         pmi        npmi lgrat_UMass lgrat_UMass_orig lgrat_UMassep.01_orig
# 1:    T1 0.2144558676 0.3173870   3.209479        -2.071662          -5.096897  -2.6368812  0.23636313  -1.1174641        -335.2392             -621.4987
# 2:    T2 0.2975269841 0.3187714   3.758637        -2.438825          -9.426048  -7.1434628  0.15747227  -0.8257632        -247.7290             -731.6476
# 3:    T3 0.3143253968 0.1905853   3.438388        -2.486631         -10.741806  -8.7358656  0.06430550  -0.7569753        -227.0926             -745.9893
# 4:    T4 0.1208370993 0.2692602   3.495524        -2.861783          -8.757312  -7.2536501  0.05564233  -1.2724346        -381.7304             -858.5350
# 5:    T5 0.1395555556 0.1342167   3.395805        -3.606421         -15.432944 -15.2812604 -0.20525609  -1.0802746        -324.0824            -1081.9264
# 6:    T6 0.2203624339 0.1762011   3.597944        -3.204080         -13.689879 -12.8162914 -0.08922309  -0.9416061        -282.4818             -961.2239
# 7:    T7 0.1172619799 0.2556525   3.319249        -3.065919         -10.236365  -9.4006165 -0.03692816  -1.3164939        -394.9482             -919.7757
# 8:    T8 0.2250038429 0.2800458   2.993077        -2.080133          -5.519443  -3.3172128  0.21495364  -1.0896355        -326.8906             -624.0398
# 9:    T9 0.3131042328 0.2122640   3.777196        -2.985906         -14.322896 -13.0353430 -0.05427199  -0.7247756        -217.4327             -895.7719
# 10:   T10 0.1331590579 0.2361175   3.352746        -3.064642         -10.596386  -9.7014775 -0.03580339  -1.2449894        -373.4968             -919.3926
# 11:   T11 0.1279940356 0.2207640   3.688577        -3.560061         -14.434810 -14.0849786 -0.15030210  -1.1352860        -340.5858            -1068.0184
# 12:   T12 0.2146987013 0.3391714   3.962407        -3.043878         -11.589564 -10.2006386  0.02046043  -1.0372778        -311.1833             -913.1634
# 13:   T13 0.2060904762 0.2011032   3.649932        -3.293998         -13.922930 -13.1353220 -0.11486226  -1.0065719        -301.9716             -988.1994
# 14:   T14 0.1774317460 0.2357056   3.272323        -2.530151          -8.181589  -6.3908580  0.10119259  -1.0795578        -323.8673             -759.0453
# 15:   T15 0.1746079365 0.1483095   3.571189        -3.475436         -15.363741 -14.9265554 -0.16855346  -0.9691645        -290.7494            -1042.6307
# 16:   T16 0.0005750203 0.1167268   1.515876        -1.264762          -1.074719   0.2370158  0.07799922  -1.1742963        -352.2889             -379.4287
# 17:   T17 0.1659580806 0.4121020   2.613030        -1.341253          -1.149140   1.4212355  0.36963321  -1.1008203        -330.2461             -402.3760
# 18:   T18 0.1447227106 0.1939026   3.689069        -3.506204         -14.120315 -13.6523482 -0.14089812  -1.1184176        -335.5253            -1051.8613
# 19:   T19 0.2313388278 0.2108138   3.572287        -2.901303         -12.568800 -11.2663983 -0.02885031  -0.8501291        -255.0387             -870.3910
# 20:   T20 0.2098001023 0.2123354   3.219697        -2.649022          -9.321714  -7.6975832  0.04115544  -1.0774677        -323.2403             -794.7066

coherence_text2vec
# Topic     dif_wiwj   dif_wjwi prob_lgrat lgrat_UMassep.01 prob_lgrat_ep1em12        pmi          npmi lgrat_UMass lgrat_UMass_orig lgrat_UMassep.01_orig
# 1:    T1  0.053718860 0.12747347   2.531631        -2.519031          -5.572414  -4.268090  0.0279361980   -1.512896        -453.8688             -755.7094
# 2:    T2  0.081765173 0.06108055   2.671503        -3.202675         -11.219024 -10.887360 -0.1738816834   -1.378820        -413.6459             -960.8026
# 3:    T3  0.088213919 0.27963329   3.048709        -2.531710          -6.029650  -4.421642  0.1118864836   -1.406834        -422.0503             -759.5129
# 4:    T4  0.102189927 0.24864719   3.139004        -2.687482          -6.989736  -5.486353  0.0677015842   -1.403389        -421.0166             -806.2447
# 5:    T5  0.084481144 0.09972983   2.683998        -3.018285          -9.712162  -9.099192 -0.0955457348   -1.398907        -419.6720             -905.4855
# 6:    T6  0.075351648 0.22529128   2.598613        -2.289718          -4.670725  -3.231664  0.1137370336   -1.436966        -431.0897             -686.9155
# 7:    T7  0.022295556 0.18449987   2.540020        -2.524970          -5.872722  -4.899118  0.0122075716   -1.500631        -450.1893             -757.4909
# 8:    T8  0.051072080 0.22473223   3.013178        -2.879642          -7.370345  -6.301684  0.0156095136   -1.535803        -460.7408             -863.8927
# 9:    T9  0.054115164 0.31699665   2.864998        -2.229108          -3.994122  -2.226395  0.1543242267   -1.464532        -439.3597             -668.7323
# 10:   T10  0.039626363 0.26797783   3.206822        -3.073795          -8.514374  -7.809292 -0.0117508479   -1.537688        -461.3064             -922.1384
# 11:   T11 -0.009598343 0.19807378   2.956855        -3.071333          -7.729234  -7.108606 -0.0530367348   -1.673622        -502.0865             -921.3998
# 12:   T12  0.124068546 0.15633834   2.810275        -2.535726          -7.122300  -5.563663  0.0428225690   -1.299557        -389.8670             -760.7179
# 13:   T13  0.008908254 0.20017474   2.654346        -2.670390          -6.159907  -5.201845 -0.0002895437   -1.580908        -474.2724             -801.1169
# 14:   T14 -0.012636347 0.18039365   3.047493        -3.605856         -10.557189 -10.663248 -0.1504207056   -1.748039        -524.4116            -1081.7567
# 15:   T15  0.086471035 0.17947775   2.554168        -2.243866          -4.714565  -3.135283  0.0861901957   -1.404431        -421.3293             -673.1598
# 16:   T16  0.087641343 0.22721726   2.697966        -2.395392          -6.084022  -4.741348  0.0684960806   -1.362287        -408.6861             -718.6175
# 17:   T17  0.231292691 0.40212648   2.792254        -1.302522          -1.271736   1.658252  0.4166459918   -1.009653        -302.8959             -390.7567
# 18:   T18  0.093367043 0.25708949   3.138085        -2.963665          -8.386212  -7.480042  0.0265272629   -1.459579        -437.8736             -889.0995
# 19:   T19  0.052128273 0.29120714   3.292453        -2.981371          -7.635856  -6.575361  0.0053540828   -1.586311        -475.8933             -894.4112
# 20:   T20  0.112213515 0.27004522   3.023677        -2.573518          -6.432295  -4.960795  0.0989688538   -1.410395        -423.1184             -772.0553

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
coherence_beta[,  stm_semCo:=  semCoh1_stmoriginal(mat = dtm, M = n_topwords, beta = fitted@beta)]
all(coherence_beta[, stm_semCo - lgrat_UMassep.01_orig] == 0)
#TRUE
