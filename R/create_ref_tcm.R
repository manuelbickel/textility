#' Create several reference tcms with different settings for coherence calculation
#'
#' Wrapper around  text2vec::create_tcm with suitable settings to generate tcms as required by
#' text2vec::coherence as reference tcms.
#' Please note that the documentation requires improvement,
#' meanwhile you might check the example to understand what is happening.
#'
#' @param dtm The dtm of the corpus under investigation.
#' @param tokens_ext The token lists of the external reference corpus.
#' @param ngram_order The maximum upper limit of ngram order to be considered.
#'                    Passed as c(1L, ngram_order) to text2vec::create_vocabulary.
#'                    If not specified maximum ngram order found in dtm is used (this may or may not be reasonable).
#' @param tcm_specs The specifications to create tcms. A data.table with one row per tcm to be created.
#'                  The default creates 4 tcms. Specififications can be viewed via calling:
#'                  \code{tcm_specs_standard()}.
#'                  If own specifications shall be used, the data.table returned by above call may be amended or changed.
#' @param dir_save The directory to save the tcms as \code{.rds} files.
#'                 Function handles directory names with or without the final "\" or "/".
#' @return Tcms are stored as separate files.
#' @export
#'
#' @examples
#' doc = c("A B x x x x x x x C")
#' doc_ext = c("A x", "A x x x x x x x B")
#'
#' tokens = word_tokenizer(doc)
#' it = itoken(tokens)
#' v = create_vocabulary(it,  ngram = c( 1L, ngram_max = 2L))
#' vectorizer = vocab_vectorizer(v)
#' dtm = create_dtm(it, vectorizer)
#'
#' # specify test dir for saving that is removed at the end of this example
#' dir_test = paste0(getwd(), "/create_reference_tcm_test")
#' dir.create(dir_test)
#'
#' tokens_ext = word_tokenizer(doc_ext)
#'
#' create_reference_tcm(dtm
#'                      , tokens_ext
#'                      , dir_save = dir_test)
#' list.files(dir_test)
#' # [1] "tcm__standard_ref_1__ext__ws_5.rds"   "tcm__standard_ref_2__ext__ws_10.rds"
#' # [3] "tcm__standard_ref_3__ext__ws_110.rds" "tcm__standard_ref_4__int__ws_Inf.rds"
#'
#' # check two of the created tcms
#' tcm5 = readRDS(paste0(dir_test, "/tcm__standard_ref_1__ext__ws_5.rds"))
#'
#' attr(tcm5, "term_coverage_rate_tcm_dtm")
#' # [1] 0.75
#' # compare
#' tokens_doc = unique(unlist(strsplit(doc, " ")))
#' tokens_doc_ext = unique(unlist(strsplit(doc_ext, " ")))
#' sum(tokens_doc_ext %in% tokens_doc) / length(tokens_doc)
#'
#' tcm5
#' # 3 x 3 sparse Matrix of class "dgTMatrix"
#' #   B A x
#' # B 1 . 5
#' # A . 2 2
#' # x . . 8
#'
#' tcm110 = readRDS(paste0(dir_test, "/tcm__standard_ref_3__ext__ws_110.rds"))
#' attr(tcm110, "term_coverage_rate_tcm_dtm")
#' # [1] 0.75
#' tcm110
#' # 3 x 3 sparse Matrix of class "dgTMatrix"
#' #   B A x
#' # B 1 1 7
#' # A . 2 2
#' # x . . 8
#' tcm110-tcm5
#' # 3 x 3 sparse Matrix of class "dgCMatrix"
#' #   B A x
#' # B 0 1 2
#' # A . 0 0
#' # x . . 0
#' # compare diagonal to input doc statistics
#' table(unlist(strsplit(doc_ext, " ")))
#' # A B x
#' # 2 1 8
#'
#' # for logic of counting co-occurrence check:
#' # https://github.com/dselivanov/text2vec/issues/253
#'
#' # delete test directory
#' unlink(dir_test, recursive = TRUE)

create_reference_tcm = function(dtm, tokens_ext, ngram_order = NULL,  tcm_specs = tcm_specs_standard(), dir_save = getwd()) {

  if (!(stringi::stri_sub(dir_save,-1,-1) %in% c("\\", "/"))) {
    dir_save = paste0(dir_save, "/")
  }

  # get number of skip gram windows (virtual documents) used for tcms
  # for some coherence metrics this number is needed to convert term co-occurrence counts to probability values by division
  n_skip_gram_windows = sum(sapply(tokens_ext, function(x) {length(x)}))

  it = itoken(iterable = tokens_ext, progressbar = FALSE, n_chunks = 10)
  #if sufficient RAM is available go parallel
  #N_WORKERS = 2
  #if(require(doParallel)) registerDoParallel(N_WORKERS)
  # it = itoken_parallel(iterable = tokens_ext
  #                         ,progressbar = FALSE
  #                         ,n_chunks = 10
  # )

  #check higher order collocations to set ngram order in external vocabulary
  if (is.null(ngram_order)) {
    ngram_order = 1+max(stringi::stri_count_fixed(colnames(dtm), "_"))
    message("Ngram order for external vocab/tcm was not specified. It was set to maximum ngram order found in dtm.")
  }
  vocab = create_vocabulary(it , c(1L, ngram_order))
  # reduce vocabulary to term space of the corpus under investigation to spare memory
  vocab = vocab[vocab$term %in% colnames(dtm),]
  vectorizer = vocab_vectorizer(vocab)
for (i in seq_len(nrow(tcm_specs))) {
  if (tcm_specs[i, type] == "ext") {
  tcm =  create_tcm(it
                    , vectorizer
                    , skip_grams_window =  tcm_specs[i, window_size]
                    , skip_grams_window_context = "symmetric"
                    , weights = rep(1,  tcm_specs[i, window_size])
                    , binary_cooccurence = TRUE
                    )
  # specify marginal probabilities (i.e. co-occurrence of term with itself)
  diag(tcm) = attributes(tcm)$word_count
  } else if (tcm_specs[i, type] == "int") {
  tcm = Matrix::crossprod(sign(dtm))
  attr(tcm, "n_windows") = nrow(dtm)
  }
  # store number of windows
  attr(tcm, "n_windows") = ifelse(tcm_specs[i,type] == "ext", n_skip_gram_windows, nrow(dtm))
  # store info on term coverage of tcm with respect to dtm
  attr(tcm, "term_coverage_rate_tcm_dtm") = round(ncol(tcm)/ncol(dtm), d = 2)
  filename = paste0(dir_save, "tcm__", tcm_specs[i,tcm_id],"__", tcm_specs[i,type] ,"__ws_",  tcm_specs[i, window_size], ".rds")
  saveRDS(tcm, file = filename)
}
}
