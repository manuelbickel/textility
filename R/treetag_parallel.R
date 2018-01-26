

#' Wrapper around treetag function from koRpus for parallel part of speech tagging with Treetagger
#'
#' NOTE: To increase speed, the input documents are pasted together by a marker phrase mimicking a sentence.
#' This may or may not have an effect on the usability of results for your downstream analysis.
#' NOTE: The package \code{doParallel} and \code{foreach} have to be loaded. The package \code{koRpus} has to be installed.
#'
#' @param docs The strings to tag.
#' @param ids Ids used to mark each document. By default set to NULL which results in \code{ids = seq_along(docs)}.
#' @param ncores Number of cores to use. By default 2.
#' @param chunk_size Maximum size of document subset to pass to single thread for tagging.
#'                   By default set to length(docs)/ncores.
#'                   Depending on document size and number of documents, different chunk sizes might be reasonable.
#' @param language Langauge to be assumed for the documents. By default "en". Parameter is passed to \code{treetag} from korPus package.
#' @param treetagger_path Path to the Treetagger program that has to be installed separately. By default "C:/TreeTagger".
#'
#' @return A \code{data.table} inlcuding tagged tokens. Documents appear consecutively in the data.table marked with the provided ids.
#'         Furthermore, the beginning of each document is marked with the token "STARTOFDOCMARKER".
#'         This marker is introduced before tagging by collapsing documents with \code{paste(..., collapse = ". STARTOFDOCMARKER ."}.
#'         Hence, also additional dots appear.
#' @export
#'
#' @examples
#'
#' docs = c("This is the first sentence.", "This is the second sentence to tag.")
#'
#' system.time(pos_tag_parallel(docs = docs, ids = seq_along(docs)))
#' # User      System elapsed
#' # 0.03        0.00    1.81
#' system.time(treetag(docs, treetagger = "manual", format = "obj",
#'                   TT.tknz = FALSE, lang =  "en",
#'                     TT.options=list(path = "C:/TreeTagger", preset ="en")))
#' # User      System     elapsed
#' # 0.03        0.00        0.91
#'
#' #here only a small number of documents to make code run "quick"
#' #for larger number of documents the timewise advantage will be higher
#' many_longer_docs <- rep(paste(rep(docs, 30), collapse = " "), 200)
#' ncores <- 4
#' chunk_size <- length(many_longer_docs)/ncores #this is the default when chunk_size = NULL
#' system.time(res_parallel <- pos_tag_parallel(docs = many_longer_docs, ids = seq_along(many_longer_docs), chunk_size = chunk_size, ncores = ncores))
#' # User      System     elapsed
#' # 0.13        0.03       11.01
#'
#' system.time(res_standard <- treetag(many_longer_docs, treetagger = "manual", format = "obj",
#'                                     TT.tknz = FALSE, lang =  "en",
#'                                     TT.options=list(path = "C:/TreeTagger", preset ="en")))
#' # User      System   elapsed
#' # 13.61       2.01     16.97
#'
#' #make results comparable
#' res_standard <- as.data.table(res_standard@TT.res)
#' res_standard <- res_standard[!(token %in% "."), ]
#'
#' res_parallel <- res_parallel[!(token %in% c("STARTOFDOCMARKER", ".")), ]
#' res_parallel <- res_parallel[, .SD, .SDcols = setdiff(colnames(res_parallel), c("doc_id"))]
#'
#' all.equal(res_parallel,res_standard)
#' #[1] TRUE
#'


treetag_parallel <-  function(docs, ids = NULL, ncores = 2, chunk_size = NULL, language = "en", treetagger_path = "C:/TreeTagger") {

  if (is.null(ids)) {
    ids <- seq_along(docs)
  }

  if (is.null(chunk_size)) {
    chunk_size <- length(docs)/ncores
  }

  if (length(docs) != length(ids)) {
    stop("Number of docs and numnber of ids is not equal.")
  }
  #non parallel version of the basic function for pos tagging with Treetagger via koRpus
  pos_tag <- function(rowidxs, ids, docs) {
    ids <- ids[rowidxs]
    docs <- docs[rowidxs]
    doc_ids_internal = data.table(doc_id_internal = seq_along(ids), doc_id = ids)
    #mark start of each document
    x <- paste0("STARTOFDOCMARKER . ", paste(docs, collapse = " . STARTOFDOCMARKER . "))
    res <- suppressMessages(
                  treetag(x, treetagger = "manual", format = "obj",
                          TT.tknz = FALSE, lang =  language,
                          TT.options=list(path =  treetagger_path, preset = language))
                  )
    res <- as.data.table(res@TT.res)
    setDT(res)
    res[, doc_id_internal:= (findInterval(1:nrow(res), grep("STARTOFDOCMARKER", res$token, fixed = TRUE)))]
    res <- res[doc_ids_internal, on = "doc_id_internal"]
    res[, doc_id_internal:= NULL]
    return(res)
  }

  chunks <- seq_along(docs)
  chunks <- split(chunks, ceiling(seq_along(chunks)/chunk_size))

  cluster <- makeCluster(ncores)
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(data.table)
    library(koRpus)
  })
  clusterExport(cluster, list( ls(environment()) ),  envir = environment())

  res_par <- foreach(j = 1:length(chunks)
                     ,.combine = function(...) rbindlist(list(...))
                     ,.multicombine= TRUE) %dopar% {
                                              pos_tag(rowidxs = chunks[[j]], ids = ids, docs = docs)
                                            }
  stopCluster(cluster)
  return(res_par)
}
