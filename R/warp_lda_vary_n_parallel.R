
#' Fit Warp LDA models for varying n in parallel
#'
#' LDA models with text2vec are fitted in parallel via \code{foreach} package for a set of candidate number of topics.
#' Function is not made failsafe, yet, it is just straightforward parallel fitting without safety nets, e.g., regarding overwriting of files.
#'
#' @param dtm The document term matrix to be used in LDA.
#' @param candidate_n \code{Integer} vector containing the candidate number of topics, e.g., \code{seq(5, 500, 5)}.
#' @param ncores Number of cores to be used.
#'               If computer should be usable during model fitting leave on processor spare via \code{parallel::detectCores()-1}.
#' @param model_dir The directory to save the fitted models.
#'                  The directory needs to end with "/", e.g., "~/mydir/" (currently no check is implemented).
#' @param deltaprior The delta prior parameter passed to \code{topic_word_prior} in \code{text2vec::LDA}. By default \code{0.1}.
#' @param convtol The convergence tolerance parameter passed to \code{text2vec::LDA}. By default \code{1e-3}.
#' @param n_iter The number of iterations parameter passed to \code{text2vec::LDA}. By default \code{2000}.
#' @param seedpar The seed parameter to ensure reproducibility. By default \code{42}.
#'
#' @return For each \code{candidate_n} a model is fitted and saved via \code{saveRDS} in a file in \code{model_dir}.
#'        The filenames include information on \code{n} and the elapsed time for fitting the individual model.
#'        They appear, e.g., as: \code{"n5_Warp_LDA_model_0h_1min.rds"}.
#'        Especially the initial part \code{"nX_Warp_LDA_model"} may be used for programmatically accessing model files.
#' @export
#'
#' @examples
#'
#' # data part of the example is copied from text2vec::LatendDirichletAllocation
#' library(text2vec)
#' data("movie_review")
#' N = 500
#' tokens = word_tokenizer(tolower(movie_review$review[1:N]))
#' it = itoken(tokens, ids = movie_review$id[1:N])
#' v = create_vocabulary(it)
#' v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
#' dtm = create_dtm(it, vocab_vectorizer(v))
#' lda_model = LDA$new(n_topics = 10)
#' doc_topic_distr = lda_model$fit_transform(dtm, n_iter = 20)
#'
#' modeldir = paste0(getwd(), "/modeldir/")
#' if (dir.exists(modeldir)) {
#'   stop("Standard directory used for this example already exists. Please change.")
#' } else {
#'   dir.create(modeldir)
#' }
#' library(doParallel)
#' # you might want to check the work load of your processors with your favorite monitor...
#' warp_lda_vary_n_parallel( dtm = dtm
#'                           , candidate_n = seq(5,40, 5)
#'                           , ncores = detectCores()-1
#'                           , model_dir = modeldir)
#' list.files(modeldir)
#' # [1] "n10_Warp_LDA_model_0h_0min.rds" "n15_Warp_LDA_model_0h_0min.rds"
#' # [3] "n20_Warp_LDA_model_0h_0min.rds" "n25_Warp_LDA_model_0h_0min.rds"
#' # [5] "n30_Warp_LDA_model_0h_0min.rds" "n35_Warp_LDA_model_0h_0min.rds"
#' # [7] "n40_Warp_LDA_model_0h_0min.rds" "n5_Warp_LDA_model_0h_0min.rds"
#'
#' # delete directory
#' unlink(paste0(getwd(), "/modeldir"), recursive=TRUE)


warp_lda_vary_n_parallel =       function( dtm
                                               , candidate_n
                                               , ncores
                                               , model_dir
                                               , deltaprior = 0.1
                                               , convtol = 1e-3
                                               , n_iter = 2000
                                               , seedpar = 42
) {
  method =  "Warp_LDA"

  cluster = makeCluster(ncores)
  registerDoParallel(cluster)

  # load required packages on all parallel sessions
  clusterEvalQ(cluster, {
    library(text2vec)
  })

  # export objects to parallel sessions
  clusterExport(cluster, list( ls(environment()) ),  envir = environment())

  result_data = foreach(j = 1:length(candidate_n)
                        #, .export= c("functon_names")
  ) %dopar% {

    n = candidate_n[j]
    start_time = as.numeric(proc.time()[3])

    filename = paste0(model_dir, "n", n, "_", method, "_model.rda")

    # if (file.exists(filename)) {
    #   next(j)
    # } else {

    start = proc.time()
    set.seed(seedpar)

    lda_model = text2vec::LDA$new(n_topics = n, doc_topic_prior = 50/n, topic_word_prior = deltaprior)
    doc_topic_distr = lda_model$fit_transform(dtm, n_iter = n_iter, convergence_tol = convtol, n_check_convergence = 10, progressbar = FALSE)
    res = list(model = lda_model, doc_topic_distr = doc_topic_distr)

    end_time = as.numeric(proc.time()[3])
    elapsed_time = paste0("_", (end_time-start_time)%/%(60*60), "h_", round(((end_time-start_time)%%(60*60))/60, d = 0), "min")

    #add time information to filename
    filename = gsub("_model.rda$"
                    ,paste0("_model"
                            , elapsed_time
                            , ".rds" )
                    ,filename
    )

    saveRDS(res, file = filename)
    #}
  }
  stopCluster(cluster)
}
