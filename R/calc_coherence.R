
#' Calculation of various coherence measures for topic models based on Latent Dirichlet Allocation (LDA)
#'
#' @param dtm The term document matrix, may be a \code{sparseMatrix} or \code{simple_triplet_matrix}.
#' @param beta Either the raw distribution of word probabilities (columnnames = words, entries = probabiliities) over topics (each row = Topic), e.g.,
#'             as the beta@@fitted_model from package topicmodels.
#'            Or the actual top words (entries = words) per topic (columnnames = topics), e.g.,
#'            as the output \code{terms(fitted_model, ...)} from topicmodesl pacakge or fitted_model$get_top_words(...) from text2vec package.
#' @param n Number of top words per topic to be used in the calculation. (if terms() or get_top_words() input is used,
#'          the number needs to be the same as in these calls) - a better routine to check and adjust numbers will be implemented
#' @param mean_over_topics By default FALSE to ouptut the coherence score for each topic.
#'                         Setting to TRUE outputs mean scores over all topics. The latter is useful to compare multiple models.
#'
#' @return A \code{data.table} showing the coherence scores for different measures for each topic or mean over all topics.
#'
#' @examples
#'
#' @export


calc_coherence <-  function(dtm, beta, n = 10, mean_over_topics = FALSE
                            #TODO, wiwj_cooccurrence_reference = NULL for creating tcm let input be dtm OR alternatively documents with a sliding window
                            #TODO,window_size = NULL #allow user to specify skip_grams_window (input check needed concering dtm or documents)
                            ) {


  #####STILL A WORKING VERSION - Results may be erroneous

  #GENERAL LOGIC
  #1 get top N words per topic
  #2 reduce dtm to top N word space
  #3 create tcm with document co-occurence of top N words (binarize dtm, do cross product)
  #(3b - divide tcm by ndocs gives probability, since some Coherence measures, e.g., UMass originally use counts,
  #      the division is done at later step)
  #4 calculate coherence for each topic (following steps 4.x are done individually for each topic)
  #4.1 reduce tcm to top n words
  #4.2 create pairs of indices of wi / wj to be used for coherence calculation
  #    Umass uses ASYMmetric combination: SUM(from m=2 to N)SUM(from l=1 to m-1) ...P(wm,wl)...
  #    other measures use SYMmetric combination: SUM(from i=1 to N-1)SUM(from j=i+1 to N) ...P(wi,wj)...
  #4.3 get values for all wiwj pairs from tcm and calculate coherence
  #     e.g. pmi = function(wi, wj, ndocs, tcm)  {log2((tcm[wi,wj]/ndocs) + 1e-12) - log2(tcm[wi,wi]/ndocs) - log2(tcm[wj,wj]/ndocs)}
  #4.4 aggregate the results via mean over number of wiwj pairs (original UMass uses counts and sum)

  #TODO
  #(i) using a sliding window over a corpus (usually external, e.g. Wikipedia) for document co-occurrence of top N words
  #    initial approach in below code (still as comment)
  #(ii) use word vectors for wi / wj instead of single words, hence, subsets such as S_one_any, etc.
  #creating, e.g., one any subsets requires to store one index against a list of indices, hence, formulas need
  #adaption, e.g., something like tcm[unlist(wi), unlist(wj)] might work

  #CREDITS / REFERENCES:
  #the first part of the code within the first if else statement to get the
  #indices of top words per topic is largely a copy from the stm package
  #adaptions were applied to make the code accept addtional types of input matrices
  #Authors: Molly Roberts, Brandon Stewart and Dustin Tingley
  #https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R
  #Furthermore, the Java implementation palmetto to calculate topic coherence served as inspiration
  #Main Author / Maintainer: Michael R?der
  #https://github.com/dice-group/Palmetto
  #http://aksw.org/Projects/Palmetto.html
  #Apart from software Authors of Palmetto have written the following paper that served as the basis for this code
  #https://dl.acm.org/citation.cfm?id=2685324
  #R?der, Michael; Both, Andreas; Hinneburg, Alexander (2015):
  #Exploring the Space of Topic Coherence Measures.
  #In: Xueqi Cheng, Hang Li, Evgeniy Gabrilovich und Jie Tang (Hg.):
  #Proceedings of the Eighth ACM International Conference on Web Search and Data Mining - WSDM '15.
  #the Eighth ACM International Conference. Shanghai, China, 02.02.2015 - 06.02.2015.
  #New York, New York, USA: ACM Press, S. 399-408.







#GET DOCUMENT CO-OCCURRENCE OF TOP N WORDS
  #case of beta coming in form of ordered words per topic (e.g. as from text2vec)
  if (mode(beta) == "numeric") {

    topic_coherence <- data.table(Topic = paste0("T", 1:nrow(beta)))
    #apply puts each result in a column, hence subset [1:n,]
    #instead of [,1:n] which would work with the input data
    idxs_topwords_topic <- apply(beta, 1, order, decreasing=TRUE, method = "radix")[1:n,]
    idxs_topwords_unique <- unique(as.vector(idxs_topwords_topic))

    dtm_topwords <- dtm[,idxs_topwords_unique]
    #binarization and cross product (document co-occurrence) depending on class of matrix
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

    #TODO
    #FIXME
    #current output of the complete function is in line with stm results (asymmetric subset of indices)
    #for symmetric sets it would be better to order tcm by word occurrence/probability as shown below
    #so that it can always be assumed that p(wi) > p(wj) when going from left to right through tcm
    #but then the output differs from stm (also for different sorting options of the indices in below function create_wiwj_asym)
    #have not figured out, yet, how to best program the two options (the brute force way would be to create two different tcms, but thats a large copy)

    #order columns of tcm from high to low entries for [wi,wi]
    # reorder_decr <- order(diag(tcm), decreasing = TRUE, method = "radix")
    # tcm <- tcm[reorder_decr, reorder_decr]
    # idxs_topwords_unique <- idxs_topwords_unique[reorder_decr]

    #create list with entries that each contains the idxs of the top words
    #that correspond to the rows (and columns) in the cooccurrence matrix
    #to be selected for coherence calculation for individual topics
    #first get idx numbers in tcm by match and then split using number of rows and n
    topic_coherence[,tcm_idxs_topwords := split(match(as.vector(idxs_topwords_topic), idxs_topwords_unique)
                                                , rep(1:nrow(beta), each=n))]

  #case of beta coming as the raw LDA result, i.e., word distribution (columns) per topic (rows)
  } else if (mode(beta) == "character") {

    topic_coherence <- data.table(Topic = paste0("T", 1:ncol(beta)))
    topwords_unique <- unique(as.vector(beta))

    dtm_topwords <- dtm[, topwords_unique]
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

    #order columns of tcm from high to low entries for [wi,wi]
    # reorder_decr <- order(diag(tcm), decreasing = FALSE, method = "radix")
    # tcm <- tcm[reorder_decr, reorder_decr]
    # topwords_unique <- topwords_unique[reorder_decr]

    #create list with entries that each contains the idxs of the top words
    #that correspond to the rows in the cooccurrence matrix
    #to be selected for coherence calculation for individual topics
    #first get idx numbers in tcm by match and then split using number of rows and n
    #NOTE difference to the other else branch: ncol instead of nrow
    topic_coherence[,tcm_idxs_topwords := split(match(as.vector(beta), topwords_unique)
                                                , rep(1:ncol(beta), each=n))]
  }

  #FIXME when using beta from text2vec as input the code only works if setting tcm to as.matrix, not sure why, yet...., see some checks in Test 1c
  tcm <- as.matrix(tcm)

  #TODO allow corpus as input to be used for wi wj document cooccurrence
  #a silly example below
  # corpus <- c(paste(letters, collapse = " "), paste(c("a", letters, "b", letters), collapse = " "))
  # it = itoken(word_tokenizer(corpus))
  # v = create_vocabulary(it)
  # topwords_unique <- c("a", "b", "z")
  # v <- v[v$term %in% topwords,]
  # window_size <- 5  #only for testing, provided as function argument
  # tcm = create_tcm(it, vocab_vectorizer(v), skip_grams_window = window_size,  weights = rep(1, window_size))
  # #entries of diagonal need to be adapted to create result that resembles something like cross-product
  # diag(tcm) <- v$term_count
  #TODO formulas need to be adapted regarding division by zero

#CREATE SETS OF wi/wj (functions)
  #following approach was taken from textmineR package and turned into generalized function
  #to create indices of token combinations to extract their probabilities from tcm to calcualte, e.g, P(wi|wj))
  #resembles utils::combn(idxs, 2) but is slightly faster for higher number of idxs
  #another base R implementation via grid() is used in stm package
  #S_one_one - all word pair combinations
  create_wiwj_sym <- function(idxs) {
    #from high to low prob, so that p(wi) > p(wj) starting from the left in the tcm
    #idxs <- sort(idxs, decreasing = FALSE)
      do.call(rbind,
            sapply(1:(length(idxs)-1), function(x) {
              cbind(wi = rep(idxs[x], length(idxs[(x + 1):length(idxs)]))
                    ,wj = idxs[(x + 1):length(idxs)])
            } , USE.NAMES = F))
     }
  #S_one_pre - required for asymmetric UMass measure
  create_wiwj_asym <- function(idxs) {
    #to comply with stm results idxs have to be reorderd
    idxs <- idxs[order(idxs, decreasing = F, method = "radix")]
    do.call(rbind,
            sapply(2:length(idxs), function(x) {
              cbind(wi = rep(idxs[x], length(idxs[1:length(idxs[1:(x-1)])]))
                    ,wj = idxs[1:length(idxs[1:(x-1)])])
            }, USE.NAMES = FALSE))
    }

#DEFINITION OF COHERENCE MEASURES
  coh_funs <- list(
  #LOG-RATIO
    #with smoothing parameter = 1, resembles UMAss
    #note the lgrat_UMass is the original formula used by Mimno
    lgrat_UMass = function(wi, wj, ndocs, tcm) {log(1 + tcm[wi,wj]) - log(1 + tcm[wj,wj])}
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
    ,pmi = function(wi, wj, ndocs, tcm)  {log2((tcm[wi,wj]/ndocs) + 1e-12) - log2(tcm[wi,wi]/ndocs) - log2(tcm[wj,wj]/ndocs)}
    #NORMALIZED PMI
    #again, in contrast, to other implementations, only intrinsic NPMI as in PMIM (for implementation with sliding window see, e.g., Bouma, 2009)
    ,npmi = function(wi, wj, ndocs, tcm) {(log2((tcm[wi,wj]/ndocs) + 1e-12) - log2(tcm[wi,wi]/ndocs) - log2(tcm[wj,wj]/ndocs)) /  -log2((tcm[wi,wj]/ndocs) + 1e-12)}
  #DIFFERENCE
    #assuming we use ordered tcm it follows that p(wi)>p(wj) for symmetric measure
    #to set bounds of the measures [-1,1] (1 is good)  wi/wj are switched in formula
    #this is similar to the measure of textmineR packakge, see
    #https://github.com/TommyJones/textmineR/issues/35
    ,dif_wjwi = function(wi, wj, ndocs, tcm) {tcm[wj,wi]/tcm[wj,wj] - (tcm[wj,wj]/ndocs)}
  )

#CALCULATE COHERENCE
  #wrapper function taking coherence function and parameters as input
  #TODO coh_funs needs to be passed to the function as argument, later steps would be less verbose when fetching it from the functioni environment -> scoping
  calc_coh <- function(ndocs = nrow(dtm), tcm, idxs, wiwj_comb_fun, coh_funs = coh_funs, coh_measure, aggr_fun = function(x) {mean(x, na.rm = T)}) {
    wiwj_idxs <- wiwj_comb_fun(idxs)
    res <- mapply(function(x,y) coh_funs[[coh_measure]](x,y, tcm = tcm, ndocs = ndocs), wiwj_idxs[,"wi"], wiwj_idxs[,"wj"])
    aggr_fun(res)
  }

  #calculate coherence for selected combinations of coherence functions and calculation options
  #https://stackoverflow.com/questions/11680579/assign-multiple-columns-using-in-data-table-by-group
  use_sym_mean <- setdiff(names(coh_funs), c("lgrat_UMass", "lgrat_ep.01"))
  topic_coherence[,   (use_sym_mean):= sapply(use_sym_mean, function(f) {
    lapply(tcm_idxs_topwords, function(x) {
      calc_coh(tcm = tcm
               , idxs = x
               , coh_measure = f
               , coh_funs = coh_funs
               , wiwj_comb_fun = create_wiwj_sym)
    })
  }, USE.NAMES = F), by = Topic]

  use_asym_mean <- c("lgrat_UMass", "lgrat_UMassep.01")
  topic_coherence[,   (use_asym_mean):= sapply(use_asym_mean, function(f) {
    lapply(tcm_idxs_topwords, function(x) {
      calc_coh(tcm = tcm
               ,idxs = x
               ,coh_measure = f
               ,coh_funs = coh_funs
               , wiwj_comb_fun = create_wiwj_asym
               )
    })
  }, USE.NAMES = F), by = Topic]

  #for comparison to original implementation of approaches by Mimno or stm package
  use_asym_sum <- c("lgrat_UMass", "lgrat_UMassep.01")
  topic_coherence[,   paste0(use_asym_sum, "_orig"):= sapply(use_asym_sum, function(f) {
    lapply(tcm_idxs_topwords, function(x) {
      calc_coh(tcm = tcm
               ,idxs = x
               ,coh_measure = f
               ,coh_funs = coh_funs
               ,wiwj_comb_fun = create_wiwj_asym
               ,aggr_fun = function(x) sum(x))
    })
  }, USE.NAMES = F), by = Topic]

  topic_coherence[,c("tcm_idxs_topwords"):= NULL]

  if (mean_over_topics == TRUE) {
     topic_coherence[, lapply(.SD, function(x) mean(x, na.rm = T)), .SDcols = setdiff(names(topic_coherence), "Topic")]
  }

  return(topic_coherence[])
}
