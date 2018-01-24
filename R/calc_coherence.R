
#####STILL A WORKING VERSION - Results may be flawed

#' Calculation of various coherence measures for topic models based on LDA algorithm
#'
#' @param dtm The term document matrix, may be a \code{sparseMatrix} or \code{simple_triplet_matrix}.
#' @param beta Either the raw distribution of word probabilities (columnnames = words, entries = probabiliities) over topics (each row = Topic), e.g.,
#'             as the beta@@fitted_model from package topicmodels.
#'            Or the actual top words (entries = words) per topic (columnnames = topics), e.g.,
#'            as the output \code{terms(fitted_model, ...)} from topicmodesl pacakge or fitted_model$get_top_words(...) from text2vec package.
#' @param n Number of top words per topic to be used in the calculation. (if terms() or get_top_words() input is used,
#'          the number needs to be the same as in these calls) - a better routine to check and adjust numbers will be implemented
#'
#' @return A \code{data.table} showing the coherence scores for different measures for each topic.
#'         For comparison of various models results need to be averaged over topics.
#'
#' @examples
#'
#' @export


calc_coherence <-  function(dtm, beta, n = 10) {
  #Credits:
  #the first part of the code within the first if else statement to get the
  #indices of top words per topic is largely a copy from the stm package
  #adaptions were applied to make the code accept addtional types of input matrices
  #Authors: Molly Roberts, Brandon Stewart and Dustin Tingley
  #https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R
  #Furthermore, the Java implementation palmetto to calculate topic coherence served as inspiration
  #Main Author / Maintainer: Michael Röder
  #https://github.com/dice-group/Palmetto
  #http://aksw.org/Projects/Palmetto.html
  #Apart from software Authors of Palmetto have written the following paper that served as the basis for this code
  #https://dl.acm.org/citation.cfm?id=2685324
  #Röder, Michael; Both, Andreas; Hinneburg, Alexander (2015):
  #Exploring the Space of Topic Coherence Measures.
  #In: Xueqi Cheng, Hang Li, Evgeniy Gabrilovich und Jie Tang (Hg.):
  #Proceedings of the Eighth ACM International Conference on Web Search and Data Mining - WSDM '15.
  #the Eighth ACM International Conference. Shanghai, China, 02.02.2015 - 06.02.2015.
  #New York, New York, USA: ACM Press, S. 399–408.

  #TBD
  #1turn on radix order/sort
  #2use of external corpus or at least context vectors
  #3larger subsets thatn S_one_one, such as, S_one_any, etc.

  #regarding TBD - #2 something like the following might be used to contsruct the corpus tcm
  # library(text2vec)
  # #just an example for demonstration
  # corpus <- c(paste(letters, collapse = " "), paste(c("a", letters, "b", letters), collapse = " "))
  # it = itoken(word_tokenizer(corpus))
  # v = create_vocabulary(it)
  # topwords <- c("a", "b", "z")
  # v <- v[v$term %in% topwords,]
  # window_size <- 5
  # tcm = create_tcm(it, vocab_vectorizer(v), skip_grams_window = window_size,  weights = rep(1, window_size))
  # #entries of diagonal need to be adapted to create result that resembles something like cross-product
  # diag(tcm) <- v$term_count
  # #the final tcm might be used as reference tcm to get probabilities
  # #formulas need to be adapted to cover cases of division by zero
  # tcm
  #regarding TBD - #3
  #creating, e.g., one any subsets requires to store one index against a list of indices, hence, formulas need
  #adaption, e.g., something like tcm[unlist(wi), unlist(wj)] might work


  #cover the case of beta coming in form of ordered words per topic (as in text2vec)
  if (mode(beta) == "character") {

    topic_coherence <- data.table(Topic = paste0("T", 1:ncol(beta)))
    topwords_unique <- unique(as.vector(beta))

    dtm_topwords <- dtm[, topwords_unique]
    #binarization and cross product (co-occurrences) depending on class of matrix
    if ("simple_triplet_matrix" %in% class(dtm)) {
      dtm_topwords$v <- ifelse(dtm_topwords$v>1, 1, dtm_topwords$v) #binarize
      tcm <- slam::tcrossprod_simple_triplet_matrix(t(dtm_topwords))
    } else if ("Matrix" %in% unlist(attributes(class(dtm)))) {
      dtm_topwords[dtm_topwords>0] <- 1
      tcm <- Matrix::t(dtm_topwords) %*% dtm_topwords
    } else {
      dtm_topwords[dtm_topwords>0] <- 1
      tcm <- t(dtm_topwords) %*% dtm_topwords
    }

    #create list with entries that each contains the idxs of the top words
    #that correspond to the rows in the cooccurrence matrix
    #to be selected for coherence calculation for individual topics
    #first get idx numbers in tcm by match and then split using number of rows and n
    #NOTE difference to the other else branch: ncol instead of nrow
    topic_coherence[,tcm_idxs_topwords := split(match(as.vector(beta), topwords_unique)
                                                , rep(1:ncol(beta), each=n))]

  } else { #case if beta comes as the raw LDA result, i.e., word distribution (columns) per topic (rows)

    topic_coherence <- data.table(Topic = paste0("T", 1:nrow(beta)))
    #apply puts each result in a column, hence subset [1:n,]
    #instead of [,1:n] which would work with the input data
    idxs_topwords_topic <- apply(beta, 1, order, decreasing=TRUE)[1:n,]
    idxs_topwords_unique <- unique(as.vector(idxs_topwords_topic))

    dtm_topwords <- dtm[,idxs_topwords_unique]
    #binarization and cross product (co-occurrences) depending on class of matrix
    if ("simple_triplet_matrix" %in% class(dtm)) {
      dtm_topwords$v <- ifelse(dtm_topwords$v>1, 1, dtm_topwords$v) #binarize
      tcm <- slam::tcrossprod_simple_triplet_matrix(t(dtm_topwords))
    } else if ("Matrix" %in% unlist(attributes(class(dtm)))) {
      dtm_topwords[dtm_topwords>0] <- 1
      tcm <- Matrix::t(dtm_topwords) %*% dtm_topwords
    } else {
      stop("Please provide dtm as simple triplet matrix (slam package) or sparse matrix (Matrix package).")
    }
    #create list with entries that each contains the idxs of the top words
    #that correspond to the rows (and columns) in the cooccurrence matrix
    #to be selected for coherence calculation for individual topics
    #first get idx numbers in tcm by match and then split using number of rows and n
    topic_coherence[,tcm_idxs_topwords := split(match(as.vector(idxs_topwords_topic), idxs_topwords_unique)
                                                , rep(1:nrow(beta), each=n))]
  }

  #FUNCTIONS TO CREATE SETS OF wi/wj
  #following approach was taken from textmineR package and turned into generalized function
  #to create indices of token combinations to extract their probabilities from tcm to calcualte, e.g, P(wi|wj))
  #resembles utils::combn(idxs, 2) but is slightly faster for higher number of idxs
  #another base R implementation via grid() is used in stm package
  #S_one_one - all word pair combinations
  create_wiwj_sym <- function(idxs) {
    do.call(rbind,
            sapply(1:(length(idxs)-1), function(x) {
              cbind(wi = rep(idxs[x], length(idxs[(x + 1):length(idxs)]))
                    ,wj = idxs[(x + 1):length(idxs)])
            } , USE.NAMES = F))}
  #S_one_pre - required for asymmetric UMass measure
  create_wiwj_asym <- function(idxs) {
    do.call(rbind,
            sapply(2:length(idxs), function(x) {
              cbind(wi = rep(idxs[x], length(idxs[1:length(idxs[1:(x-1)])]))
                    ,wj = idxs[1:length(idxs[1:(x-1)])])
            }, USE.NAMES = FALSE))}

  #DEFINITION OF COHERENCE MEASURES
  coh_funs <- list(
    #DIFFERENCE
    #assuming we use ordered topwords, it follows p(wi)>p(wj)
    #to allow a halfway intuitive output with bounds from low to high score, p(wj) is used as reference
    dif_wiwj = function(wi, wj, ndocs, tcm) {tcm[wi,wj]/tcm[wi,wi] - (tcm[wi,wi]/ndocs)}
    ,dif_wjwi = function(wi, wj, ndocs, tcm) {tcm[wj,wi]/tcm[wj,wj] - (tcm[wj,wj]/ndocs)}
    #LOG-RATIO
    #with smoothing parameter = 1, resembles UMAss
    #note the lgrat_UMass is the original formula used by Mimno
    ,lgrat_UMass = function(wi, wj, ndocs, tcm) {log(1 + tcm[wi,wj]) - log(1 + tcm[wj,wj])}
    ,prob_lgrat = function(wi, wj, ndocs, tcm)  {log(1 + (tcm[wi,wj]/ndocs)) - log(tcm[wj,wj]/ndocs)}
    #LOG-RATIO SMOOTHED
    #with adapted smoothing parameter (used in stm package)
    ,lgrat_UMassep.01 =  function(wi, wj, ndocs, tcm)  {log(.01 + tcm[wi,wj]) - log(.01 + tcm[wj,wj])}
    #PROBABILISTIC LOG-RATIO with seven mall smoothing constant as in above mentioned paper by Röder
    ,prob_lgrat_ep1em12 =  function(wi, wj, ndocs, tcm)  {log(.1e-12 + (tcm[wi,wj]/ndocs)) - log(tcm[wj,wj]/ndocs)}
    #PMI
    ##note: UCI is also based on PMI, however, UCI uses external context vector of words constructed from Wikipedia
    #in the following only intrinsic PMI without sliding window (in other words window = whole document)
    #format of PMI formula proposed by @andland - https://github.com/dselivanov/text2vec/issues/236
    ,PMI = function(wi, wj, ndocs, tcm)  {log2((tcm[wi,wj]/ndocs) + 1e-12) - log2(tcm[wi,wi]/ndocs) - log2(tcm[wj,wj]/ndocs)}
    #NORMALIZED PMI
    #again, in contrast, to other implementations, only intrinsic NPMI as in PMIM (for implementation with sliding window see, e.g., Bouma, 2009)
    ,NPMI = function(wi, wj, ndocs, tcm) {(log2((tcm[wi,wj]/ndocs) + 1e-12) - log2(tcm[wi,wi]/ndocs) - log2(tcm[wj,wj]/ndocs)) /  -log2((tcm[wi,wj]/ndocs) + 1e-12)}
  ) #end list coherence function definitions


  #wrapper function to calculate coherence measures taking calculation function and parameters as input
  calc_coh <- function(ndocs = nrow(dtm), tcm, idxs, wiwj_comb_fun, coh_funs = coh_funs, coh_measure, aggr_fun = function(x) {mean(x, na.rm = T)}) {
    wiwj_idxs <- wiwj_comb_fun(idxs)
    res <- mapply(function(x,y) coh_funs[[coh_measure]](x,y, tcm = tcm, ndocs = ndocs), wiwj_idxs[,"wi"], wiwj_idxs[,"wj"])
    aggr_fun(res)
  }

  #calculate coherence for selected combinations of coherence functions and calculation options
  #https://stackoverflow.com/questions/11680579/assign-multiple-columns-using-in-data-table-by-group
  use_sym_mean <- setdiff(names(coh_funs), c("lgrat_UMass", "lgrat_ep.01"))
  topic_coherence[,   (use_sym_mean):= lapply(use_sym_mean, function(f) {
    lapply(tcm_idxs_topwords, function(x) {
      calc_coh(tcm = tcm
               , idxs = x
               , coh_measure = f
               , coh_funs = coh_funs
               , wiwj_comb_fun = create_wiwj_sym)
    })
  }), by = Topic]

  use_asym_mean <- c("lgrat_UMass", "lgrat_UMassep.01")
  topic_coherence[,   (use_asym_mean):= lapply(use_asym_mean, function(f) {
    lapply(tcm_idxs_topwords, function(x) {
      calc_coh(tcm = tcm
               ,idxs = x
               ,coh_measure = f
               ,coh_funs = coh_funs
               , wiwj_comb_fun = create_wiwj_asym
               ,aggr_fun = function(x) mean(x, na.rm = T))
    })
  }), by = Topic]

  #for comparison to original implementation of approaches by Mimno or stm package
  use_asym_sum <- c("lgrat_UMass", "lgrat_UMassep.01")
  topic_coherence[,   paste0(use_asym_sum, "_orig"):= lapply(use_asym_sum, function(f) {
    lapply(tcm_idxs_topwords, function(x) {
      calc_coh(tcm = tcm
               ,idxs = x
               ,coh_measure = f
               ,coh_funs = coh_funs
               ,wiwj_comb_fun = create_wiwj_asym
               ,aggr_fun = function(x) sum(x, na.rm = T))
    })
  }), by = Topic]

  topic_coherence[,c("tcm_idxs_topwords"):= NULL]
  return(topic_coherence)
}
