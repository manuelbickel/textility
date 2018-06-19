

#' Get loglikelihood from multiple locally saved models
#' 
#' The function loads each of the specified LDA models created with text2vec
#' in parallel and extracts the loglikelihodd from the last iteration step.
#' This function is useful if not all models can be loaded into RAM at once 
#' due to memory restrictions.
#'
#' @param modelfiles The full filenames as \code{character} of the models saved as \code{.rds} to extract loglik.
#' @param ncores The number of cores to be used.
#'              Advise: leave one processor spare, if you want to
#'              use your computer during operation of the function.
#' @return A \code{data.table} listing the number of topics of each model with associated loglik.
#' @export
#'
#' @examples

get_loglik_parallel <- function(modelfiles, ncores) {
  cluster <- makeCluster(ncores)
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(text2vec)
    library(data.table)
  })
  #parallel::clusterExport(cluster,list( ls(environment()) ),  envir = environment())
  result_data <- foreach(j = 1:length(modelfiles)
                         ,.combine = function(...) rbindlist(list(...))
                         ,.multicombine= TRUE
  ) %dopar% {
    fitted <- readRDS(modelfiles[j])
    model_loglik <- data.table(modeltype = class(fitted$model)[1]
                               , ntopics = ncol(fitted$doc_topic_distr)
                               , loglik =  tail(attr(fitted$doc_topic_distr, "likelihood")$loglikelihood,1)
    )
    return(model_loglik)
  }
  stopCluster(cluster)
  return(result_data)
}
