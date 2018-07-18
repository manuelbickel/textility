#' Scale values normal with boundaries zero and one
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' scale_normal(c(0,50,100))
#' #[1] 0.0 0.5 1.0
#' scale_normal(c(1,6,11))
#' #[1] 0.0 0.5 1.0
scale_normal <- function(x){(x-min(x))/(max(x)-min(x))}

#' Get numeric gradient between points
#'
#' Simple caluclation of gradient between points.
#'
#' @param x steps / independent variable
#' @param y observed data points / response variable
#'
#' @return Matrix showing the gradient values between points.
#'         The x positions of gradient values are returned as the middle point between the steps.
#'         Note that \code{nrow(gradient) = length(x)-1}.
#' @export
#'
#' @examples

gradient_numeric <- function(x, y) {
  dx <- diff(x)
  gradient <- diff(y)/dx
  position <- x[1:(length(x)-1)] + dx/2
  cbind(position, gradient)
}

#' Initializes the standard specifications for tcms to be used as reference for coherence calculation
#'
#' @return data.table with standard specifications, see example...
#' @export
#'
#' @examples
#' tcm_specs_standard()
#' #            tcm_id type window_size
#' # 1: standard_ref_1  ext           5
#' # 2: standard_ref_2  ext          10
#' # 3: standard_ref_3  ext         110
#' # 4: standard_ref_4  int         Inf
tcm_specs_standard = function() {
  data.table(tcm_id = c("standard_ref_1","standard_ref_2","standard_ref_3","standard_ref_4")
             ,type = c("ext", "ext", "ext", "int")
             ,window_size = c(5,10,110,Inf)
  )
}

# DEPRECATED FUNCTIONS KEPT FOR THE PURPOSE OF DOCUMENTATION
calc_coherence = function(X) {
  .Deprecated("text2vec::coherence")
  warning(paste0("The code of this function is only kept in the package for the purpose of documentation.", "\n ",
                 "It includes an earlier version of the respective function that was finally established in text2vec.", "\n",
                 "This ealier version is non-vectorized and might be helpful to understand the vectorized code in text2vec."))
  # #' Calculation of various coherence measures for topic models
  # #'
  # #' DEPRECATED - PLEASE USE text2vec::coherence
  # #'
  # #' @param top_term_matrix A character matrix with the top words (entries = words) ranked starting with row 1 per topic (columnnames = topics). e.g.,
  # #'                        For example, the output of \code{topicmodels::terms} or text2vec::get_top_words(...).
  # #' @param tcm A term co-occurrence matrix that serves as reference to calculate coherence scores for the terms in \code{top_term_matrix}.
  # #' @param ndocs_tcm The number of documents that served to create the \code{tcm} to calculate probabilities from counts in the\code{tcm} used as input to some coherence measures.
  # #' @param log_smooth_constant Smoothing constant to avoid logarithm of zero in calcualtions, for example, \code{log(log_smooth_constant + 0)}.
  # #'                            By default \code{.1e-12} as used by Röder et al.(palmetto).
  # #'                            Use \code{1} to for UMass logratio as used by Mimno.
  # #'                            Use \code{.01} to calculate logratio as used in stm package.
  # #' @param average_over_topics By default \code{FALSE} to return coherence score for each indivual topic.
  # #'                            When set to \code{TRUE} the mean score over all topics is returned.
  # #'
  # #' @return A \code{data.table} showing the various coherence scores per topic (or average over all topics).
  # #' @export
  # #'
  # #' @examples
  #
  # calc_coherence <-  function( top_term_matrix
  #                             ,tcm
  #                             ,ndocs_tcm = NULL
  #                             ,log_smooth_constant = .1e-12
  #                           #  ,average_over_topics = FALSE
  #                             ) {
  #
  # #GENERAL LOGIC--------------------------------------------------------
  #   #STEPS PPREP_1 to PREP_3 have to be performed separately from this function
  #   #PREP_1 get top N words per topic
  #   #PREP_2 reduce dtm to top N word space
  #   #PREP_3 create tcm with document co-occurence of top N words (binarize dtm, do cross product)
  #   #(PREP_3b - divide tcm by ndocs_tcm gives probability, since some Coherence measures, e.g., UMass originally use counts,
  #   #      the division is done at later step)
  #
  #   #calculate coherence for each topic (following steps 4.x are done individually for each topic)
  #   #1 reduce tcm to top n word space
  #   #2 create pairs of tcm reference indices of wi / wj to be used for coherence calculation
  #   #3 get values for all wiwj pairs from tcm and calculate score for each pair given a calculation rule specfied by particular coherence measure
  #   #     e.g. pmi = function(wi, wj, ndocs_tcm, tcm)  {log2((tcm[wi,wj]/ndocs_tcm) + 1e-12) - log2(tcm[wi,wi]/ndocs_tcm) - log2(tcm[wj,wj]/ndocs_tcm)}
  #   #4 aggregate the results via mean over number of wiwj pairs (and if desired aggreagte over all topics)
  #
  #   #TODO
  #   #(ii) use word vectors for wi / wj instead of single words, hence, subsets such as S_one_any, etc.
  #   #     creating, e.g., one any subsets requires to store one index against a list of indices, hence, formulas need
  #   #     adaption, e.g., something like tcm[unlist(wi), unlist(wj)] might work
  #   #(iii)Currently indices of subsets are stored in memory, this might be turned to dynamic creation of indices to spare memory usage
  #   #     the lines topic_coherence[,tcm_idxs := split(match... would have to be incorporated into create_wiwj_set
  #
  # #CREDITS / REFERENCES -------------------------------------------------
  #   #The following paper is the main theoretical basis for this code
  #   #https://dl.acm.org/citation.cfm?id=2685324
  #   #Authors: Roeder, Michael; Both, Andreas; Hinneburg, Alexander (2015)
  #   #Title: Exploring the Space of Topic Coherence Measures.
  #   #In: Xueqi Cheng, Hang Li, Evgeniy Gabrilovich und Jie Tang (Hg.):
  #   #Proceedings of the Eighth ACM International Conference on Web Search and Data Mining - WSDM '15.
  #   #the Eighth ACM International Conference. Shanghai, China, 02.02.2015 - 06.02.2015.
  #   #New York, New York, USA: ACM Press, S. 399-408.
  #   #The paper has already been implemented as a Java implementation "palmetto" that exceeds the current functionality of this R implementation
  #   #Main Author / Maintainer: Michael Roeder
  #   #https://github.com/dice-group/Palmetto
  #   #http://aksw.org/Projects/Palmetto.html
  #
  #   #several ideas for getting / ordering /accessing indices of tcm have been inspired by packages stm and textmineR
  #   #(although, these packages use different functions to achieve the same result)
  #   #stm: Molly Roberts, Brandon Stewart and Dustin Tingley
  #   #https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R
  #   #textmineR: Tommy Jones
  #   #https://github.com/TommyJones/textmineR/blob/master/R/CalcProbCoherence.R
  #
  # #INITIAL----------------------------------------------------------------------------
  #   top <- as.vector(top_term_matrix)
  #   top_unq <- unique(top)
  #   ntopics <- ncol(top_term_matrix)
  #   nterms <- nrow(top_term_matrix)
  #   topic_coherence <- data.table(Topic = paste0("T", 1:ntopics))
  #
  # #PREPARE TCM------------------------------------------------------------------------
  #   #check if input tcm really contains the same elements in rows and columns
  #   if ( !all.equal(rownames(tcm)[order(rownames(tcm))], colnames(tcm)[order(colnames(tcm))]) ) {
  #     warning("There is a difference between the terms in rows and columns of the tcm.
  #          Please check tcm, the sets of the terms have to be the same (they do not necessarily have to be in the same order).")
  #   }
  #   #check if tcm includes all top terms
  #   if (!(all(intersect(top_unq, colnames(tcm)) == top_unq) == TRUE)) {
  #     warning("Not all terms of top_term_matrix are included in tcm.
  #           For individual topics coherence scores would be based on incomplete word sets.
  #           Please adapt top term matrix or tcm to ensure full intersection of terms.")
  #   }
  #   #reduce tcm to top word space (makes tcm symmetric)
  #   #TODO
  #   top_unq_subset <- which(colnames(tcm) %in% top_unq)
  #   tcm <- tcm[top_unq_subset, top_unq_subset]
  #
  #   #order tcm by term probability (entries in diagonal)
  #   #from left to right the probability of terms follows the rule p(tcm[i,i]) > p(tcm[i+1,i+1])
  #   #ordering tcm is relevant for asymmetric measures that require a certain order
  #   #some asymmetric measures require the original order in the topic (e.g. UMass),
  #   #which is therefore also stored for re-mapping indices of tcm to this order
  #   if ("Matrix" %in% unlist(attributes(class(tcm)))) { #TODO? require sparseMatrix as input
  #     prob_order <- order(Matrix::diag(tcm),  decreasing = TRUE)
  #   } else {
  #     prob_order <- order(diag(tcm),  decreasing = TRUE)
  #   }
  #   tcm <- tcm[prob_order, prob_order]
  #   restore_topic_order <- match(top_unq, colnames(tcm))
  #   #TODO ?
  #   #when (input) tcm is a (larger) sparse matrix the code is quite slow
  #   #speed of frequent subsetting of sparse tcm is the bottleneck, example:
  #   # m <- cbind(A = c(1,0,0,0), B = c(1,0,0,0), C = c(0,0,0,1))
  #   # m.sp <- Matrix(m, sparse = T)
  #   # microbenchmark::microbenchmark(
  #   #   m[3,3],
  #   #   m.sp[3,3]
  #   # )
  #   # Unit: nanoseconds
  #   # expr    min       lq      mean median     uq    max neval
  #   # m[3, 3]    790    791.0   1492.89   1579   1580  13817   100
  #   # m.sp[3, 3] 144480 147637.5 152647.01 148822 150401 367514   100
  #
  #   #since tcm is limited on top n word space its size is usually not incredibly large at this point
  #   #example: 1000 topics with top 20 words would be about 1 Mb
  #   # format(object.size(
  #   #     lapply(1:1000, function(x) {
  #   #       sapply(1:20, function(y) {
  #   #        paste(sample(letters, 7), collapse = "")
  #   #       })
  #   #     })
  #   #   ), units = "Mb")
  #   #hence, workaround using base::matrix for faster subsetting instead of sparseMatrix seems acceptable
  #   tcm <- as.matrix(tcm)
  #   # #TODO #depending on input, matrix might have to be turned to upper triangle matrix
  #   # if (any((sign(tcm[upper.tri(tcm, diag = F)]) + sign(t(tcm)[upper.tri(tcm, diag = F)])) > 1)) {
  #   #   warning("Input TCM is not symmetric or not coercible to upper triangle matrix.
  #   #           Entries of upper and lower triangle overlap (both have entries >1) where one of the entries should be zero.")
  #   # }
  #   # #make upper triangle matrix
  #   # tcm[upper.tri(tcm, diag = F)] <- tcm[upper.tri(tcm, diag = F)] + t(tcm)[upper.tri(tcm, diag = F)]
  #   # #setting lower triangle to zero is not necessary since index combinations are only taken from upper triangle
  #   # #however, to make potential mistakes in index subsetting visible it is still set to zero
  #   # tcm[lower.tri(tcm, diag = F)] <-  tcm[upper.tri(tcm, diag = F)]
  #
  #
  # #GET REFERENCE INDICES OF TOP TERMS IN TCM FOR EACH TOPIC---------------------------
  #   #credits for this approach of getting indices go to authors of stm package
  #   topic_coherence[,tcm_idxs := split(match(top, colnames(tcm))
  #                                      , rep(1:ntopics, each=nterms))]
  # #FUNCTION TO CREATE SETS OF wi/wj------------------------------------------------------------
  #   #order of indices in S_one_pre and S_one_suc matters for asymmetric measures
  #   #S_one_pre follows the logic SUM(from i=2 to N)SUM(from j=1 to i-1)
  #   #S_one_suc follows the logic: SUM(from i=1 to N-1)SUM(from j=i+1 to N)
  #   #Note that there are two options for sets:
  #   #(i) sets that build on the original order of terms per topic
  #   #(ii) sets that build on the terms per topic, however, ordered by probability
  #
  #   create_wiwj_set <- function(idxs, set_type = "one_suc",  alternative_order) {
  #     if (set_type == "one_pre") {
  #     wiwj <- t(combn(idxs,2, FUN = function(x) sort(x, decreasing = TRUE)))
  #     } else if (set_type == "one_suc") {
  #     wiwj <- t(combn(idxs,2, FUN = function(x) sort(x, decreasing = FALSE)))
  #     } else if (set_type == "one_pre_topic_order") {
  #       #for asymmetric sets the original order of words (hence, indexes of tcm) has to be restored
  #       reorder <- order(match(idxs, alternative_order), decreasing = TRUE)
  #       idxs <- idxs[reorder]
  #       #in contrast to the other subsets, no additional reordering of indices at this point
  #       #to maintain original topic order
  #       wiwj <- t(combn(idxs,2))
  #     }
  #     colnames(wiwj) <- c("wi", "wj")
  #     return(wiwj)
  #   }
  #
  # #DEFINITION OF COHERENCE MEASURES------------------------------------------------------------
  #   #TODO
  #   #more convenient interface for defintion of coherence functions might be established
  #   #e.g. by defining them as reference/S6 class, this would also allow more flexible definition of additional measures by user
  #   coh_funs <- list(
  #     #LOG-RATIO
  #     logratio_UMass = structure(function(wi, wj, ndocs_tcm, tcm, log_smooth_constant) {log(log_smooth_constant + tcm[wi,wj]) - log(log_smooth_constant + tcm[wj,wj])}
  #                          ,set_type = "one_pre_topic_order"
  #                          ,aggr_fun = "function(x) sum(x, na.rm = T)")
  #     #smoothing parameter = 1 resembles UMAss, .01 resembles stm package, default as in paper by Röder, i.e. .1e-12
  #     ,logratio = structure(function(wi, wj, ndocs_tcm, tcm, log_smooth_constant) {log(log_smooth_constant + tcm[wi,wj]) - log(log_smooth_constant + tcm[wj,wj])}
  #                          ,set_type = "one_pre"
  #                          ,aggr_fun = "function(x) sum(x, na.rm = T)")
  #     ,prob_logratio = structure(function(wi, wj, ndocs_tcm, tcm, log_smooth_constant)  {log(log_smooth_constant + (tcm[wi,wj]/ndocs_tcm)) - log(tcm[wj,wj]/ndocs_tcm)}
  #                                ,set_type = "one_pre"
  #                                ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #     #PMI
  #     #format of PMI formula proposed by @andland - https://github.com/dselivanov/text2vec/issues/236
  #     ,pmi = structure(function(wi, wj, ndocs_tcm, tcm, log_smooth_constant)  {log2((tcm[wi,wj]/ndocs_tcm) + log_smooth_constant) - log2(tcm[wi,wi]/ndocs_tcm) - log2(tcm[wj,wj]/ndocs_tcm)}
  #                      ,set_type = "one_pre"
  #                      ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #     #NORMALIZED PMI
  #     #again, in contrast, to other implementations, only intrinsic NPMI as in PMIM (for implementation with sliding window see, e.g., Bouma, 2009)
  #     ,npmi = structure(function(wi, wj, ndocs_tcm, tcm, log_smooth_constant) {(log2((tcm[wi,wj]/ndocs_tcm) + log_smooth_constant) - log2(tcm[wi,wi]/ndocs_tcm) - log2(tcm[wj,wj]/ndocs_tcm)) /  -log2((tcm[wi,wj]/ndocs_tcm) + log_smooth_constant)}
  #                       ,set_type = "one_pre"
  #                       ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #     #DIFFERENCE
  #     #assuming we use ordered tcm it follows that p(wi)>p(wj)
  #     #to set bounds of the measures [-1,1] (1 is good)  wi/wj are switched in formula
  #     #this is similar (not exactly the same) to the measure of textmineR package https://github.com/TommyJones/textmineR/issues/35
  #     ,prob_dif = structure(function(wi, wj, ndocs_tcm, tcm, log_smooth_constant) {tcm[wj,wi]/tcm[wj,wj] - (tcm[wj,wj]/ndocs_tcm)}
  #                           ,set_type = "one_suc"
  #                           ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #   )
  #
  # #WRAPPER FUNCTION TO CALCULATE COHERENCE-------------------------------------------------------
  #   #TODO coh_funs and other args need to be passed to the function as argument,
  #   #later steps would be less verbose when fetching several of the arguments,e.g., tcm, from the function environment -> scoping
  #   calc_coh <- function( idxs
  #                        , tcm
  #                        , coh_funs = coh_funs
  #                        , ndocs_tcm
  #                        , coh_measure
  #                        , log_smooth_constant = log_smooth_constant
  #                        , alternative_order
  #                          ) {
  #     #select coherence function from the ones availble
  #     coh_fun <- coh_funs[[coh_measure]]
  #     #define the wi wj set
  #     #TODO
  #     #remove NA values
  #     idxs <- idxs[!is.na(idxs)]
  #     if (length(idxs) < 2) { #minimum of two terms required for coherence calculation
  #       return(NA)
  #     }
  #     coh <- as.data.table(create_wiwj_set(idxs, set_type = attr(coh_fun, "set_type"), alternative_order = alternative_order))
  #     #calculate score for each pair of wi wj
  #     coh[, coh_res:= mapply(function(x,y) coh_fun(x,y, tcm = tcm, ndocs_tcm = ndocs_tcm, log_smooth_constant = log_smooth_constant),wi, wj)]
  #     #aggregate
  #     aggr_fun <- eval(parse(text = attr(coh_fun, "aggr_fun")))
  #     coh[, round(aggr_fun(coh_res), d = 4)]
  #   }
  #
  # #CALCULATE COHERENCE----------------------------------------------------------------
  #   for (i in names(coh_funs)) {
  #     topic_coherence[,(i):= lapply(tcm_idxs, function(x) {
  #                                      calc_coh( idxs = x, coh_measure = i
  #                                     , tcm = tcm
  #                                     , coh_funs = coh_funs
  #                                     , ndocs_tcm = ndocs_tcm
  #                                     , log_smooth_constant = log_smooth_constant
  #                                     , alternative_order = restore_topic_order)})
  #                     , by = Topic]
  #   }
  #
  #   topic_coherence[, tcm_idxs := NULL]
  # #
  # #   if (average_over_topics == TRUE) {
  # #     topic_coherence <- topic_coherence[, lapply(.SD, function(x) round(mean(x, na.rm = T), d = 4)), .SDcols = setdiff(names(topic_coherence), "Topic")]
  # #   }
  #   return(topic_coherence[])
  # }
}
