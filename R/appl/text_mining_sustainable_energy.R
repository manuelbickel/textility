
# settings ----------------------------------------------------------------

dirproj <- "C:/Science/Publication/3_sustainable_energy/"

dirdat <-  paste0(dirproj, "data/csv_export/")

dirres <- paste0(dirproj, "results/")

dirmod <- paste0(dirres, "models/")

#set locale for reproducibility or provide sessioninfo after loading libraries etc.
#sessionInfo()
#Sys.setlocale("LC_COLLATE", "C")

# load libraries ----------------------------------------------------------

libraries <- c(
                "stringi",
                # "data.table",
                "Matrix",
                "text2vec"
                ,"data.table"
                #,"tm"
                , "SnowballC"
                # ,"cluster"
                # , "factoextra"
                # ,"FactoMineR"
                # ,"wordnet"
                ,"magrittr"
                #,"ldatuning"
                , "doParallel"
                , "LDAvis"
                , "topicmodels"
                , "textmineR"
                , "ggplot2"
                , "slam"
                ,"stm"
                , "koRpus"
                ,"textility"
                #,"scales"
)


for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)

# set number of cores to be used in parallel processing functions ---------
#leave one core spare
ncores <- detectCores()-1


# functions ---------------------------------------------------------------
stm_adapt_semCoh1beta <-  function(mat, M, beta){
  #Get the Top N Words
  top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
  wordlist <-  unique(as.vector(top.words))
  mat <- mat[,wordlist]
  mat$v <- ifelse(mat$v>1, 1,mat$v) #binarize

  #cross product to get co-occurences
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


stm_adapt_exclusivity <- function(phi, M, frexw=.7){
  w <- frexw
  #if(length(model$beta$logbeta)!=1) stop("Exclusivity calculation only designed for models without content covariates")
  tbeta <- t(exp(phi))
  s <- rowSums(tbeta)
  mat <- tbeta/s #normed by columns of beta now.

  ex <- apply(mat,2,rank)/nrow(mat)
  fr <- apply(tbeta,2,rank)/nrow(mat)
  frex<- 1/(w/ex + (1-w)/fr)
  index <- apply(tbeta, 2, order, decreasing=TRUE)[1:M,]
  out <- vector(length=ncol(tbeta))
  for(i in 1:ncol(frex)) {
    out[i] <- sum(frex[index[,i],i])
  }
  out
}


#in textility
tripl_to_sparse <- function(simple_triplet_matrix) {

  Matrix::sparseMatrix(i=simple_triplet_matrix$i
                       ,j=simple_triplet_matrix$j
                       ,x=simple_triplet_matrix$v,
                       dims=c(simple_triplet_matrix$nrow
                              , simple_triplet_matrix$ncol)
                       ,dimnames = dimnames(simple_triplet_matrix)
  )
}


sparse_to_stm <- function(x) {
  out <- list(documents = NULL, vocab = colnames(x))
  out$documents <- lapply(text2vec::as.lda_c(x), function(y){
    y[1,] <- y[1,]+1
    return(y)
  })
  names(out$documents) <- NULL
  out
}


#in textility
subset_rows_DT <- function(DT, idxs, delete_idxs = FALSE) {
  if (delete_idxs == TRUE) {
    idxs <- (1:nrow(DT))[!(1:nrow(DT) %in% idxs)]
  }
  cols = names(DT)
  DT.subset = data.table(DT[[1]][idxs])
  setnames(DT.subset, cols[1])
  for (col in cols[2:length(cols)]){
    DT.subset[, (col) := DT[[col]][idxs]]
    DT[, (col) := NULL] #delete
  }
  return(DT.subset)
}


#in textility
replace_acronyms_by_words <- function(docs, remove_replaced_acr = TRUE
                                      , remove_all_bracketed_acr = TRUE
                                      ) {

  docs <- sapply(docs, function(d) {

    acronyms <- unlist(stri_extract_all_regex(d, "(?<=\\()([^\\)]+)(?=\\))")) %>%
      .[grep("^[A-Z][^\\s]*[A-Z][^\\s]*$", ., perl = T)] %>%
      .[ !grepl("\\W", .)] %>%
      gsub("s$", "",.) %>%
      unique

    if (length(acronyms) == 0) {
      return(d)
    } else {
      acronym_word_pattern <- lapply(strsplit(acronyms, ""), function(a) {
        words_before <- paste0("(^| )", paste0("("%s+%a[1:length(a)-1]%s+%"\\w+-?\\w+ )((of the ){0,1}|(from the ){0,1}|(and ){0,1}|(of ){0,1}|(by ){0,1}|(to ){0,1}|(on ){0,1}|(from ){0,1})", collapse = ""),
                              "("%s+%a[length(a)]%s+%"\\w+-?\\w+ )", collapse = "")
        paste0(words_before, "\\(", paste0(a, collapse = ""), "s?\\)")
      })

      acronym_word_pattern <- lapply(acronym_word_pattern, function(p) {
        p <- unlist(stri_extract_all_regex(d, p, case_insensitive = TRUE))
        res <- gsub("\\)", "", stri_split_fixed(p, " (", simplify = T))

        if ( any(is.na(res)) ) {
          return(NULL)
        } else {
          res <- as.data.frame(res, stringsAsFactors = FALSE)
          colnames(res) <- c("words", "acronym")
          res$acronym <- gsub("s$", "", res$acronym)
          return(res)
        }
      })

      acronym_word_pattern <- unique(rbindlist(acronym_word_pattern, fill = TRUE, use.names = TRUE))

      if (nrow(acronym_word_pattern) == 0) {
        return(d)
      } else {
        d <- stri_replace_all_regex(d
                                    , paste0("(?<=\\s)(", acronym_word_pattern$acronym , "s?)(?=\\s|\\.|,)")
                                    , acronym_word_pattern$word
                                    , vectorize_all = FALSE)

        d <- stri_replace_all_regex(d
                                    , paste0("\\(", acronym_word_pattern$acronym , "s?\\)")
                                    , rep("", length(acronym_word_pattern$acronym))
                                    , vectorize_all = FALSE)
        return(d)
      }
    }
  }, USE.NAMES = FALSE)

  if (remove_all_bracketed_acr == TRUE) {

    docs <- stri_replace_all_regex(docs
                           , "\\([A-Z][A-Za-z]*[A-Z]s?\\)"
                           , " "
                           , vectorize_all = FALSE)

    return(docs)

  } else {

    return(docs)

  }

}


#for testing
# d <- "The extreme importance of the Combined Heat and Power (CHP) technique in th ?aj lsdkjf CHP in sdflso erto rt Chair of the Nations (CN) WEKLJLKJF CN."
#
# acronym_word_pattern <- lapply(strsplit(acronyms, ""), function(a) {
#   words_before <- paste0("(^| )", paste0("("%s+%a[1:length(a)-1]%s+%"\\w+-?\\w+ )((of the ){0,1}|(from the ){0,1}|(and ){0,1}|(of ){0,1}|(by ){0,1}|(from ){0,1})", collapse = ""),
#                          "("%s+%a[length(a)]%s+%"\\w+-?\\w+ )", collapse = "")
#   paste0(words_before, "\\(", paste0(a, collapse = ""), "\\)")
# })
#
#
# a <- unlist(strsplit(acronyms, ""))
# acronym_word_pattern <- lapply(strsplit(acronyms, ""), function(a) {
#   words_before <- paste0(paste0("("%s+%a[1:length(a)-1]%s+%"\\w+ )((and ){0,1})", collapse = ""),
#                          "("%s+%a[length(a)]%s+%"\\w+ )", collapse = "")
#   paste0(words_before, "\\(", paste0(a, collapse = ""), "\\)")
# })
#
# p <- unlist(acronym_word_pattern)
# unlist(stri_extract_all_regex(d, p, case_insensitive = TRUE))

# test <- c("slkj United Nations (UN) lfslkdfkj dsjkf UN. "
#           , "aslkfkdj sustain indicator (SI) sdaldkfkj SI sdfdlkk "
#           , "sdfklj black mambda (BM) sdlkfj BM.",
#           "salkdfj co-production knowledge (CK) slkdfj CK CK "
#           , "saldfjh Bla BLa (BB) sdlfd sdsldkakfjh BB ?laksd"
#           , "slfdkj slkjk sdldakfj ")
#
# ids_test <- 1:length(test)
#
# replace_acronyms_by_words_parallel(docs = test, ids = ids_test, ncores = 3)
#rm(test)

# replace_acronyms_by_words_parallel <-  function(docs_ids, ncores = 2) {
#
#   #non parallel version of the basic function
#   replace_acronyms_by_words <- function(docs, remove_replaced_acr = TRUE
#                                         , remove_all_bracketed_acr = TRUE
#   ) {
#
#     docs <- sapply(docs, function(d) {
#
#       acronyms <- unlist(stri_extract_all_regex(d, "(?<=\\()([^\\)]+)(?=\\))")) %>%
#         .[grep("^[A-Z][^\\s]*[A-Z][^\\s]*$", ., perl = T)] %>%
#         .[ !grepl("\\W", .)] %>%
#         gsub("s$", "",.) %>%
#         unique
#
#       if (length(acronyms) == 0) {
#         return(d)
#       } else {
#         acronym_word_pattern <- lapply(strsplit(acronyms, ""), function(a) {
#           words_before <- paste0("(^| )", paste0("("%s+%a[1:length(a)-1]%s+%"\\w+-?\\w+ )((of the ){0,1}|(from the ){0,1}|(and ){0,1}|(of ){0,1}|(by ){0,1}|(to ){0,1}|(on ){0,1}|(from ){0,1})", collapse = ""),
#                                  "("%s+%a[length(a)]%s+%"\\w+-?\\w+ )", collapse = "")
#           paste0(words_before, "\\(", paste0(a, collapse = ""), "s?\\)")
#         })
#
#         acronym_word_pattern <- lapply(acronym_word_pattern, function(p) {
#           p <- unlist(stri_extract_all_regex(d, p, case_insensitive = TRUE))
#           res <- gsub("\\)", "", stri_split_fixed(p, " (", simplify = T))
#
#           if ( any(is.na(res)) ) {
#             return(NULL)
#           } else {
#             res <- as.data.frame(res, stringsAsFactors = FALSE)
#             colnames(res) <- c("words", "acronym")
#             res$acronym <- gsub("s$", "", res$acronym)
#             return(res)
#           }
#         })
#
#         acronym_word_pattern <- unique(rbindlist(acronym_word_pattern, fill = TRUE, use.names = TRUE))
#
#         if (nrow(acronym_word_pattern) == 0) {
#           return(d)
#         } else {
#           d <- stri_replace_all_regex(d
#                                       , paste0("(?<=\\s)(", acronym_word_pattern$acronym , "s?)(?=\\s|\\.|,)")
#                                       , acronym_word_pattern$word
#                                       , vectorize_all = FALSE)
#
#           d <- stri_replace_all_regex(d
#                                       , paste0("\\(", acronym_word_pattern$acronym , "s?\\)")
#                                       , rep("", length(acronym_word_pattern$acronym))
#                                       , vectorize_all = FALSE)
#           return(d)
#         }
#       }
#     }, USE.NAMES = FALSE)
#
#     if (remove_all_bracketed_acr == TRUE) {
#
#       docs <- stri_replace_all_regex(docs
#                                      , "\\([A-Z][A-Za-z]*[A-Z]s?\\)"
#                                      , " "
#                                      , vectorize_all = FALSE)
#
#       return(docs)
#
#     } else {
#
#       return(docs)
#
#     }
#
#   }
#
#   cluster <- makeCluster(ncores)
#   registerDoParallel(cluster)
#
#   # load up the needed R package on all the parallel sessions
#   clusterEvalQ(cluster, {
#     library(stringi)
#     library(data.table)
#     library(magrittr)
#   })
#
#
#   #does not make sense, simply parallelise over 1:nrow
#   #to pass more or less equal amount of text to each core
#   #docs are ordered by number of characers and then split
#   #split into chunks
#   # docs_ids <- docs_ids[order(stri_count_charclass(docs_ids$doc, "\\p{Letter}")),]
#   # docs_ids <- split(docs_ids, rank(1:nrow(docs_ids)) %% ncores)
#
#
#   # export all the needed R objects to the parallel sessions
#   clusterExport(cluster, list( ls(environment()) ),  envir = environment())
#   # we parallelize by the different number of topics.  A processor is allocated a value
#   # of k, and does the cross-validation serially.  This is because it is assumed there
#   # are more candidate values of k than there are cross-validation folds, hence it
#   # will be more efficient to parallelise
#
#
#
#   #.export= c("tripl_to_sparse", "stm_adapt_exclusivity","stm_adapt_semCoh1beta")
#
#   result_data <- foreach(j = 1:nrow(docs_ids)
#                          #,.combine = rbind
#                          ,.combine = function(...) rbindlist(list(...))
#                          ,.multicombine= TRUE
#                          ) %dopar% {
#
#                           docs_ids[j ,doc := replace_acronyms_by_words(doc)]
#
#                           return(docs_ids)
#
#                          }
#
#   stopCluster(cluster)
#
#   return(result_data)
# }

#in textility
uncontract_negations <- function(x) {

  contracted_negations <- c("isn.t",     "aren.t"  ,  "wasn.t"  ,  "weren.t" ,  "hasn.t"   , "haven.t"
                            ,"hadn.t"   , "doesn.t" ,  "don.t" ,    "didn.t"  ,  "won.t"  ,   "wouldn.t"
                            ,"shan.t" ,   "shouldn.t" ,"can.t"  ,   "couldn.t" ,"mustn.t"  , "cannot")
  uncontracted_negations <- c("is not"  ,   "are not"  ,  "was not"  ,  "were not"  , "has not" ,   "have not"
                              ,"had not"   , "does not"  , "do not"  ,   "did not"  ,  "will not"   ,  "would not"
                              ,"shall not"  , "should not", "can not"  ,   "could not" , "must not", "can not")
  stri_replace_all_regex(x
                         , paste0("\\b", contracted_negations , "\\b")
                         , uncontracted_negations
                         , vectorize_all = FALSE
  )
}



#in textility as stri_process
#token_exclude_length <- 2 #exlcude unigrams of x characters or less
preprocessing_function <- function(x
                                   , force_encoding = "UTF-8"
                                   , alltolower = FALSE
                                   , erase_patterns = NULL
                                   , token_exclude_length = NULL
                                   , rm_diacritics = FALSE
                                   , replace_dashes_hyphens_by = NULL
                                   , rm_roman_numeral_listing = FALSE
                                   , replace_by_blank_regex =  NULL #c("\\b\\d+\\b", "\\b\\W+\\b")
                                   , erase_regex = NULL #c("[^A-Za-z_ ]")
                                   , harmonize_blanks = FALSE
                                   ) {

  if (!is.null(force_encoding)) {
  Encoding(x) <- force_encoding
  }

  if (alltolower == TRUE) {
  x <-  stri_trans_general(x, "lower") #tolower(x)
  }

  if (!is.null(erase_patterns)) {

    if (!is.null(force_encoding)) {
    Encoding(erase_patterns) <- force_encoding
    } else {
      Encoding(erase_patterns) <- Encoding(x)
    }

    if (alltolower == TRUE) {
    erase_patterns <-  stri_trans_general(erase_patterns, "lower")
    }

    erase_patterns <- unique(erase_patterns)

    length_erase_patterns_uncleaned <- length(erase_patterns)

    erase_patterns <- erase_patterns[ !is.na(erase_patterns) & erase_patterns != ""]

    erase_patterns <- erase_patterns[order(stringi::stri_count_fixed(erase_patterns, " "), decreasing = TRUE)]

    if (length_erase_patterns_uncleaned != length(erase_patterns)) {
      warning("Erase patterns inlcuded NA values or empty entries. These were ignored during replacement.")
    }

    #https://stackoverflow.com/questions/26676045/replace-a-set-of-pattern-matches-with-corresponding-replacement-strings-in-r
    x <- stringi::stri_replace_all_fixed(x
                                         ,pattern = erase_patterns
                                         ,replacement = rep(" ", length(erase_patterns))
                                         ,vectorize_all = F
                                         ,case_insensitive = T)

  }

  #remove listing in Roman letters
  if (rm_roman_numeral_listing == TRUE) {
  x <- stringi::stri_replace_all_regex(x
                                       ,pattern = "\\([ivx]+\\)"
                                       ,replacement = ""
                                       ,vectorize_all = F
                                       ,case_insensitive = T)
  }

  if (rm_diacritics == TRUE) {
  x <- iconv(x, from = force_encoding, to ="ASCII//TRANSLIT")
  }

  if (!is.null(replace_dashes_hyphens_by)) {
  #keeping compounds is now decided in collocation model
  #see comment on ouput for vocabulary[..fossil..]
  #formerly dashes were replaced by "", now by blank
  #ensure that compounds are kept, e.g. long-term should show up as longterm
  #https://stackoverflow.com/questions/1011708/regex-to-replace-hyphen-in-the-middle-of-a-word
  #http://www.unicode.org/versions/Unicode10.0.0/ch06.pdf
  dashes <- stringi::stri_trans_general(paste0("U+", c(2010:2015, 2043, "002D", "FE63", "FF0D")), "Hex-Any/Name")
  if (!is.null(force_encoding)) {
    Encoding(dashes) <- force_encoding
  } else {
    Encoding(dashes) <- Encoding(x)
  }
  x <- gsub(paste0("(?<=\\w)(", paste(dashes, collapse = "|") , ")(?=\\w)"), replace_dashes_hyphens_by, x, perl=T)
  }

   if (!is.null(replace_by_blank_regex)) {
     x <- gsub(paste(replace_by_blank_regex, collapse = "|"), " ", x, perl=T)
   }

  if (!is.null(erase_regex)) {
    x <- gsub(paste(erase_regex, collapse = "|"), "", x, perl=T)
  }

  if (!is.null(token_exclude_length)) {
  #https://stackoverflow.com/questions/33226616/how-to-remove-words-of-specific-length-in-a-string-in-r
  x <- gsub(paste0("(?<=\\b)(\\w{1,", token_exclude_length, "})(?=\\b)"), "", x, perl=T)
  }

  if( !all(stri_enc_isutf8(x)) ) {
    warning("Some strings are not in UTF-8 after pre-processing. This may or may not have an effect on results.")
  }

  if (harmonize_blanks == TRUE) {
    x <- gsub("\\s+", " ", x , perl=T)
    x <- gsub("^\\s|\\s$", "", x, perl=T)
  }

  return(x)
}



# pos_lemma_tokenizer_udpipe = function(x, udp_model, pos_keep = c("NOUN", "PROPN", "ADJ", "ADV", "VERB"), words_keep = c("no","not", "none","ignored","poorely")) {
#   res = as.data.table(udpipe_annotate(udp_model, x = x, doc_id = seq_along(x), parser = "none"))
#   setDT(res)
#   temp = data.table(doc_id = seq_along(x))
#   res[ , lemma:= tolower(lemma)]
#   if(!is.null(words_keep)) {
#     res[ , token:= tolower(token)]
#     words_keep <- tolower(words_keep) #
#     res[(res$lemma %in% words_keep | res$token %in% words_keep), upos := "KEEP"]  #new
#     pos_keep <- c(pos_keep, "KEEP")
#   }
#   if(!is.null(pos_keep))
#     res = res[upos %in% pos_keep, ]
#   #res = res[, .(doc_id = as.integer(doc_id), lemma = tolower(lemma))] #new
#   res = res[, .(doc_id = as.integer(doc_id), lemma = lemma)] #new
#   res = res[, .(tokens = list(lemma)), keyby = doc_id]
#   res = res[temp, on = .(doc_id = doc_id)]
#   res[is.na(tokens), tokens := NULL]
#   res$tokens
# }

#udpipe_english <- udpipe_download_model(language = "english")
#udp_model_english <- udpipe_load_model(file = udpipe_english$file_model)
#udp_model_english <- udpipe_load_model(file = paste0("C:/Science/Publication/3_sustainable_energy/data/", "english-ud-2.0-170801.udpipe"))
#POS TAG codes

# pos_lemma_tokenizer = function(x, udp_model, pos_keep = c("NOUN", "PROPN", "ADJ", "ADV", "VERB"), negations_keep = c("no","not", "none","ignored","poorely")) {
#   res = as.data.table(udpipe_annotate(udp_model, x = x, doc_id = seq_along(x), parser = "none"))
#   setDT(res)
#   temp = data.table(doc_id = seq_along(x))
#   if(!is.null(pos_keep))
#     res = res[upos %in% pos_keep, ]
#   res = res[, .(doc_id = as.integer(doc_id), lemma = tolower(lemma))]
#   res = res[, .(tokens = list(lemma)), keyby = doc_id]
#   res = res[temp, on = .(doc_id = doc_id)]
#   res[is.na(tokens), tokens := NULL]
#   res$tokens
# }

#http://universaldependencies.org/docs/u/pos/index.html
# Open class words
# ADJ
# ADV
# INTJ
# NOUN
# PROPN
# VERB
#
# Closed class words
# ADP
# AUX
# CONJ
# DET
# NUM
# PART
# PRON
# SCONJ
#
# Other
# PUNCT
# SYM
# X

# Alphabetical listing
# ADJ: adjective
# ADP: adposition
# ADV: adverb
# AUX: auxiliary verb
# CONJ: coordinating conjunction
# DET: determiner
# INTJ: interjection
# NOUN: noun
# NUM: numeral
# PART: particle
# PRON: pronoun
# PROPN: proper noun
# PUNCT: punctuation
# SCONJ: subordinating conjunction
# SYM: symbol
# VERB: verb
# X: other

pos_lemma_tokenizer_treetagger = function(x
                                          , corpus_language = "en"
                                          , wclass_keep = c("noun", "verb", "adjective", "adverb", "name")
                                          , words_keep = c("no","not", "none")
                                          , tretagger_path = "C:/TreeTagger"
                                          , preset = "en"
) {
  #using an lapply loop on the indiviual docs is very slow
  #therefore docs are first bind together and split later again
  #introduce marker to split documens
  doc_ids_internal = data.table(doc_id = seq_along(x))
  #mark start of documents
  x <- paste(x, collapse = " . SPLITDOCSHERE . ")
  res <- suppressMessages(
    treetag(x, treetagger = "manual", format = "obj",
            TT.tknz = FALSE, lang = corpus_language,
            TT.options=list(path =  tretagger_path, preset = preset))
  )
  res <- as.data.table(res@TT.res)
  setDT(res)
  #add internal document id to each row
  res[, doc_id:= (findInterval(1:nrow(res), grep("SPLITDOCSHERE", res$token, fixed = TRUE))+1)]
  #remove marker
  res <- res[!(token %in% "SPLITDOCSHERE"), ]
  res[ , lemma:= tolower(lemma)]
  #mark words / lemmas to keep
  if(!is.null(words_keep)) {
    res[ , token:= tolower(token)]
    words_keep <- tolower(words_keep)
    res[(res$lemma %in% words_keep | res$token %in% words_keep), tag := "KEEP"]
    wclass_keep <- c(wclass_keep, "KEEP")
  }
  #remove undesired POS
  if(!is.null(wclass_keep))
    res = res[wclass %in% wclass_keep, ]
  #use original token  as lemma if lemma is not identified
  #correct plural s that has not been removed from tokens with unknown lemma
  #this can be the case, e.g., for "biofuels" if in comma separated list
  res[lemma == "<unknown>" & tag == "NNS", lemma:= gsub("(?<=.)s$", "", token, perl = T)]
  res[lemma == "<unknown>", lemma:= token]
  res <- res[, .(doc_id, lemma)]
  res <- res[, .(tokens = list(lemma)), keyby = doc_id]
  res <- res[doc_ids_internal, on = .(doc_id = doc_id)]
  res[is.na(tokens), tokens := NULL]
  res$tokens
}

#in textility
pos_tag_parallel <-  function(docs_ids, ncores = 2, chunk_size = 1000) {

  #non parallel version of the basic function
  pos_tag <- function(rowidxs) {
    x <- docs_ids[rowidxs]
    doc_ids_internal = data.table(doc_id_internal = seq(1,nrow(x)), doc_id = x$id)
    #mark start of each document
    x <- c("SPLITDOCSHERE . ", paste(x$doc, collapse = " . SPLITDOCSHERE . "))
    res <- suppressMessages(
      treetag(x, treetagger = "manual", format = "obj",
              TT.tknz = FALSE, lang =  "en",
              TT.options=list(path =  "C:/TreeTagger", preset = "en"))
    )
    res <- as.data.table(res@TT.res)
    setDT(res)
    res[, doc_id_internal:= (findInterval(1:nrow(res), grep("SPLITDOCSHERE", res$token, fixed = TRUE)))]
    res <- res[doc_ids_internal, on = "doc_id_internal"]
    return(res)
  }

  chunks <- 1:nrow(docs_ids)
  chunks <- split(chunks, ceiling(seq_along(chunks)/chunk_size))

  cluster <- makeCluster(ncores)
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(data.table)
    library(koRpus)
  })
  clusterExport(cluster, list( ls(environment()) ),  envir = environment())

  res_par <- foreach(j = 1:length(chunks)
                     #,.combine = rbind
                     ,.combine = function(...) rbindlist(list(...))
                     ,.multicombine= TRUE
                     #,.export("pos_tag")
  ) %dopar% {
    pos_tag(chunks[[j]])
  }

  stopCluster(cluster)
  return(res_par)
}


fit_LDA_Gibbs_vary_k_parallel <-       function( dtm_tripl
                                                 , candidate_k
                                                 , ctrl_LDA_Gibbs
                                                 , ncores
                                                 , model_dir
) {
  method <-  "LDA_Gibbs"
  ctrl_meth_list <- ctrl_LDA_Gibbs

  cluster <- makeCluster(ncores)
  registerDoParallel(cluster)

  # load up the needed R package on all the parallel sessions
  clusterEvalQ(cluster, {
    library(topicmodels)
    library(Matrix)
  })

  # export all the needed R objects to the parallel sessions
  clusterExport(cluster, list( ls(environment()) ),  envir = environment())
  # we parallelize by the different number of topics.  A processor is allocated a value
  # of k, and does the cross-validation serially.  This is because it is assumed there
  # are more candidate values of k than there are cross-validation folds, hence it
  # will be more efficient to parallelise

  result_data <- foreach(j = 1:length(candidate_k), .export= c("tripl_to_sparse")) %dopar% {

    k <- candidate_k[j]
    start_time <- as.numeric(proc.time()[3])

    filename <- paste0(model_dir, "k", k, "_", method, "_model.rda")

    # if (file.exists(filename)) {
    #   next(j)
    # } else {

    fitted       <- LDA(x = dtm_tripl
                        ,k = k
                        ,method = method
                        ,control = ctrl_LDA_Gibbs
    )
    #add time information to filename
    filename <- gsub("_model.rda$"
                     ,paste0("_hours", as.character(round((as.numeric(proc.time()[3])-start_time)/(60*60), d = 0))
                             , "_model.rda" )
                     ,filename
                     )
    save(fitted, file = filename)
    #}
  }
  stopCluster(cluster)
}


#test
# data("AssociatedPress", package = "topicmodels")
# fit_LDA_Gibbs_vary_k_parallel(dtm_tripl = AssociatedPress[1:20,]
#                               ,candidate_k = candidate_k
#                               ,ctrl_LDA_Gibbs = control_LDA_Gibbs_topicmodels
#                               ,ncores = ncores
#                               ,model_dir = dirmod)


# import stopwords --------------------------------------------------------
stopwords <- c(
  #from packages
  tm::stopwords("SMART")
  ,tm::stopwords("en")
  ,tokenizers::stopwords("en")
 # ,setdiff(lexicon::sw_buckley_salton, c("ill", "edu", "ignored", "immediate", "novel"))


  #from files
 # ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_english_xpo6com.txt"))
#  ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_english_ranknl.txt"))

 # ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_academic.txt"))

 # ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_msu_jdowell_and_smart_wordsorg.txt"))

  #manual definition
  ,"approach", "methodolology", "paper", "publish", "publication", "abstract", "conclusion", "method"
  ,"original", "article", "issue", "study", "studied", "published"
  ,"study"
  #, "relation", "relate"
  ,"issue", "article", "noabstract"
   , "contribution", "contribute"
  , "become"
  #, "estimate"
  ,"br", "br_br", "inf", "inf_inf"

)

stopwords <- setdiff(stopwords, c("least", "mine", "change", "changes", "whole", "old", "new") )

#remove comment lines and make unique
stopwords <- stopwords[!grepl("^#", stopwords)] %>% #remove comments
 { unlist(stringi::stri_split_regex(., "\\s|\\W|\\t|\\n")) } %>% #get individual lines and terms
  unique() %>%
  .[. != ""] %>% #exclude empty lines
  setdiff(c(
    "no","not", "none","ignored","poorely", "shortcomings", "questionable", "lacking", "reject", "overlooks", "restricted",
    "contested", "controversial", "inadequacies", "inaccurate", "adequate", "abilities", "attempted", "break",
    "government", "governing", "agencies", "agency", "laboratory", "experiment",
    "governments", "strategy", "strategies", "innovative", "innovation",
    "association", "standard", "standardised", "discourse", "discourses",
    "biological", "ground", "biosynthetic", "framework", "criticisms", "embodies", "error", "exposition",
    "measurement", "constitutes", "critics", "emerged", "synthesis" ,"adopted", "assess", "criteron", "critiqued", "nature"
  ) #make sure that some important terms are still included
  )

stopphrases <- readLines("C:/Science/R/data/stopwords_en/stopphrases_msu_jdowell_and_smart_wordsorg.txt")
#check which stopphrases are removed anyway by pre-processing and stopwords
stopphrases_check <- preprocessing_function(stopphrases, token_exclude_length = 2, alltolower = TRUE) %>%
  .[ !grepl( paste0("^",stopwords, "$", collapse = "|"), ., perl = T)] %>%
  stri_replace_all_regex(., paste0("^" ,stopwords,"\\s"), "", vectorize_all = FALSE) %>%
  stri_replace_all_regex(., paste0("\\s" ,stopwords,"$"), "", vectorize_all = FALSE) %>%
  gsub("\\s+", "", .)

stopphrases <- stopphrases[which(stopphrases_check != "")] %>%
               .[ !grepl("^\\w\\s|\\s\\w$", ., perl = T) ] #keep only phrases that do not start with single letter

#add some manually identified patterns
stopphrases <- c(stopphrases,
                 "graphical abstract","figure not available", "see fulltext")

# import data --------------------------------------------------------------
data <- list.files(dirdat, pattern = ".csv",  full.names = TRUE)
#data <- data[c(57, 58)] #for testing<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TEST

data <- lapply(data, function(f) {

  read.csv(f, sep = ",", header = T, stringsAsFactors = F)

})

data <- rbindlist(data)
data <- data[ !duplicated(data[, "EID"]), ]

#check number of entries...fits to number of docs in Scopus (1 element is missing)
nrow_data <- nrow(data)
nrow_data
#[1] 32165

data[, id := 1:nrow(data)]

data[, ncharAbstract := nchar(Abstract)]


# quantiative statistics on number of pubications ------------------------------------------
histpublications <- ggplot(data = data[ !is.na(Year) , c("Year", "id")], aes(Year)) +
  geom_histogram(binwidth = 0.5) +
  xlab("Year") +
  ylab("Number of abstracts") +
  theme_bw() +
  #theme(
  #axis.text.x = element_text(size=10
  #  , angle=45)) +
  scale_x_continuous(breaks=seq(min(data$Year, na.rm = TRUE)
                                , max(data$Year, na.rm = TRUE), 5))



# select data range for analysis ----------------------------------------------------
#exclude documents with missing abstract or less than 100 characters
missing_abstract <- data[Abstract %like% "(?i)no abstract|^$" & ncharAbstract < 200, ]
missing_abstract_n <- nrow(missing_abstract)

data <- data[with(data, !(id %in% missing_abstract$id)), ]

#remove conference proceedings and technical reports
#as.data.frame(data[ Source.title %like% "report|Report|conference|Conference"  , c("Source.title", "Document.Type", "Publisher")])
source_titles_maybe_remove <- unique(data[ Source.title %like% "report|Report|conference|Conference"  , Source.title])
# [1] "USDA Forest Service - General Technical Report PNW-GTR"  #https://www.fs.fed.us/pnw/publications/gtrs.shtml
# [2] "Czech Polar Reports" #keep    #http://www.sci.muni.cz/CPR/
# [3] "USDA Forest Service - General Technical Report PNW"  #same as above
# [4] "Biotechnology Reports"   #keep #https://www.journals.elsevier.com/biotechnology-reports/
# [5] "Annual Reports on the Progress of Chemistry - Section B"  #exclude, a review journal https://en.wikipedia.org/wiki/Annual_Reports_on_the_Progress_of_Chemistry
#http://pubs.rsc.org/en/journals/journalissues/ar#!issueid=ar1966_63_0&type=archive&issnprint=0365-6217
# [6] "Minerals and Energy - Raw Materials Report" #keep  #http://www.tandfonline.com/action/journalInformation?journalCode=smin20
# [7] "Moravian Geographical Reports"    #keep
# [8] "Oceans Conference Record (IEEE)"   #http://www.scimagojr.com/journalsearch.php?q=13654&tip=sid&clean=0
# [9] "Water Environment '96. Supply and demand - a fragile balance. Proc. conference, London, 1996" #http://searchfirst.library.unsw.edu.au/primo_library/libweb/action/dlDisplay.do?vid=UNSWS&docId=UNSW_ALMA51183019620001731&fromSitemap=1&afterPDS=true
# [10] "General Technical Report - US Department of Agriculture, Forest Service" #same as above USDA
# [11] "Executive Report, International Institute for Applied Systems Analysis"
# [12] "Geneva Reports on the World Economy"   #http://cepr.org/content/geneva-reports-world-economy-0
# [13] "Atomic Energy of Canada Limited, AECL (Report)"    #http://www.scimagojr.com/journalsearch.php?q=29349&tip=sid&clean=0
# [14] "Energy Reports" #https://www.journals.elsevier.com/energy-reports/
# [15] "Quarterly Report of RTRI (Railway Technical Research Institute) (Japan)"  #http://www.rtri.or.jp/eng/publish/qr/
# [16] "Quarterly Report of RTRI (Railway Technical Research Institute)"    #see above
# [17] "Canadian Geotechnical Conference"   #https://www.cgs.ca/
# [18] "JFE Technical Report"  #http://www.jfe-steel.co.jp/en/research/report.html
# [19] "Conference Record of the IEEE Photovoltaic Specialists Conference"  #http://www.scimagojr.com/journalsearch.php?q=17298&tip=sid
# [20] "DWI Reports"   #http://www.scimagojr.com/journalsearch.php?q=15365&tip=sid&clean=0
# [21] "IERE Conference Proceedings"  #https://trove.nla.gov.au/work/9201616?selectedversion=NBD27673
# [22] "R and D: Research and Development Kobe Steel Engineering Reports" #exclude, company
# [23] "Kawasaki Steel Technical Report"  #exclude, company
# [24] "Nippon Steel Technical Report"  #exclude, company
# [25] "Scientific Reports"  #keep #https://www.nature.com/srep/about
# [26] "Journal of Physics: Conference Series" #http://iopscience.iop.org/journal/1742-6596  #https://en.wikipedia.org/wiki/Journal_of_Physics:_Conference_Series
# [27] "Romanian Reports in Physics" #http://www.rrp.infim.ro/aims.html
# [28] "AHURI Final Report" #https://www.ahuri.edu.au/research
# [29] "Field Actions Science Report"  #exclude, company #https://www.institut.veolia.org/fr
# [30] "China Report"  #unclear, check abstracts -> #keep #https://uk.sagepub.com/en-gb/eur/china-report/journal200761#description
# [31] "Environmental Change and Security Project report" #unclear check manually #-> exclude #https://www.ncbi.nlm.nih.gov/nlmcatalog/101085292

#data[ Source.title ==  "China Report" , Abstract] #-> keep

#data[ Source.title ==  "Environmental Change and Security Project report" , Abstract] #exclude

keep_source_titles <- c("Czech Polar Reports"
                        , "Biotechnology Reports"
                        , "Minerals and Energy - Raw Materials Report"
                        , "Moravian Geographical Reports"
                        , "Scientific Reports"
                        , "Romanian Reports in Physics")

remove_source_titles <- setdiff(source_titles_maybe_remove, keep_source_titles)
rm(source_titles_maybe_remove)
rm(keep_source_titles)

delete_idxs <- which(data[, Source.title] %in% remove_source_titles)
data <- subset_rows_DT(data, idxs = delete_idxs, delete_idxs = TRUE)

#delete editorial articles and special issue introductions
delete_ids <- sapply(c("this editorial"
                        ,"the editorial article"
                        ,"This introductory article of the special issue"
                        ,"this special issue"
                        ,"the introduction to the special issue"
                        ,"invited me to edit a special issue"
                        ,"The special issue of International Journal"
                        ,"introduction article to the special issue"
                        ,"This paper summarises the contributions to a special issue"
                        ,"introduction to the SPECIAL ISSUE"
                        ,"answer the questions posed by the SPECIAL ISSUE"
                        ,"This double SPECIAL ISSUE"
                        ,"This paper provides an overview of the SPECIAL ISSUE"), function(x) {

                           data[grep(x, Abstract, ignore.case = T), id]
                        }, USE.NAMES = F)

delete_ids <- unique(unlist(delete_ids))
delete_idxs <- which(data[, id] %in% delete_ids)
data <- subset_rows_DT(data, idxs = delete_idxs, delete_idxs = TRUE)

# elsevier_request_full <- data[sample(1:nrow(data), 10), c("Abstract", "Year", "id")]
# Encoding(elsevier_request_full$Abstract) <- "UTF-8"
# write.csv(elsevier_request_full, paste0(dirproj, "abstract_year_uncleaned.csv"))

#subset for testing
# subset <- grep("Elsevier", data[,  Abstract] )
# subset <- c(subset[1:10], grep("IWA Publishing", data[,  Abstract] ))
# docs <- data[subset,  Abstract]
# ids <-  data[subset, id]
# Encoding(docs) <- "UTF-8"

#save / load image -------------------------------------
#setwd("C:/Science/Publication/3_sustainable_energy/data/")
#save.image(file = "before_pubaff_replacement.RData")

# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# load(file = "before_pubaff_replacement.RData")
# for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)

docs_ids <- data[, c("Abstract", "id")]
colnames(docs_ids) <- c("doc", "id")
Encoding(docs_ids$doc) <- "UTF-8"

# data set specific pre-processing -----------------------------------------
#check articles of ncharAbstract up to 5000
histncharabstracts_max5k <- ggplot(data = data[ ncharAbstract <= 5000  , c("ncharAbstract", "id")], aes(data[ ncharAbstract <= 5000  , ncharAbstract])) +
  geom_histogram(bins = 50) +
  # xlab("Year") +
  ylab("Number of characters in abstracts") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=10
                               , angle=90)) +
  scale_x_continuous(breaks=seq(min(data$ncharAbstract, na.rm = TRUE)
                                , max(data$ncharAbstract, na.rm = TRUE), 100))
histncharabstracts_max5k

#before going further, erase copyright information from data
#erase trailing copyright information patterns
#since there is no clear pattern to erase copyright information safely from all abstacts
#this taks is done iteratively / manually
#the individual steps are wrapped inside a function that only takes docs as input
#function is used to be able to run the whole script at once and delete interim variables after cleaning (free memory)
#of course the steps can be executed indivdually if interim results shall be checked

#docs ids is a data.table containing two columnns named docs and ids containing the corresponding content
remove_pusblisher_affiliation_names <- function(docs_ids, publisher_names, affiliation_names) {

copyrightsymbol <- stringi::stri_trans_general("\u00A9", "Hex-Any/Name")

#how many docs include copyright symbol
docs_w_copyrightsymbol_total <- length(grep(copyrightsymbol, docs_ids$doc))
#[1] 25586

#-------------------------------------------------------------
#1st: remove the well formatted copyright information, i.e., marked by copyright symbol, e.g., "text. ? 2011 Elsevier Ltd."
#estimate length of trailing pattern to 1000 chars
pattern <- paste0("(.*)(", copyrightsymbol, "\\s\\d{4}|", copyrightsymbol, "\\d{4})(.{1,1000}$)")
docs_tail <- gsub(pattern, "\\2\\3", docs_ids$doc, ignore.case = T, perl = T)
#summary(nchar(docs_tail))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 7.0    20.0    41.0   393.3   655.0  9993.0

#check result for trailing pattern with 401 chars, only the docs that include the pattern
pattern <- paste0("(.*)(", copyrightsymbol, "\\s\\d{4}|", copyrightsymbol, "\\d{4})(.{1,401}$)")
docs_tail <- gsub(pattern, "\\2\\3", docs_ids$doc[grep(pattern, docs_ids$doc, perl = T)], ignore.case = T, perl = T)
#summary(nchar(docs_tail))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 7.00   20.00   33.00   34.67   41.00  268.00

#one raqndom example of a longer tail
#docs_tail
nchar_long <-  nchar("? 2014 by the American Society of Agronomy, 5585 Guilford Road, Madison, WI 53711. All rights reserved.")

#the following looks fine...
#docs_tail[which(nchar(docs_tail) > nchar_long  )]
#docs_tail[which(nchar(docs_tail) < 40 )]

#hence, the pattern can be erased from the docs...
docs_ids[, doc:= gsub(paste0("(", copyrightsymbol, "\\s\\d{4})(.{1,401}$)"), "", doc, ignore.case = T, perl = T)]
docs_ids[, doc:= gsub(paste0("(", copyrightsymbol, "\\d{4})(.{1,401}$)"), "", doc, ignore.case = T, perl = T)]

#any(grepl(pattern, docs_ids$doc, ignore.case = T, perl = T))
#1] FALSE

#check how many docs with copyright symbol are left
docs_w_copyrightsymbol_remaining  <- length(grep(copyrightsymbol, docs_ids$doc))
#[1] 2665


#remove remaining copyright symbol patterns-----------------------------------------
pattern <- paste0("(.*)( ", copyrightsymbol, ".{1,1000}$)")
docs_tail <- gsub(pattern, "\\2", docs_ids$doc[grep(pattern, docs_ids$doc, perl = T)], ignore.case = T, perl = T)
#summary(nchar(docs_tail))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 3.00   22.00   34.00   35.31   39.00  899.00
#docs_tail[1:1000] #looks fine
#docs_tail[1001:1999] # NOT fine
#[982] " ? 2008 Springer-Verlag Zusammenfassung: Das kontinuierliche Verkehrswachstum und die daraus resultierenden Umwelt- und Ressourcenprobleme des Verkehrsbereichs k?nnen zuk?nftig durch den konventionellen Einsatz fossiler Treibstoffe nicht bew?ltigt werden. Die Herausforderungen des Verkehrs liegen dabei zunehmend im Energiebereich. Energieeffizienz und erneuerbare Energien r?cken in den Mittelpunkt. Um fundamentale Verbesserungen zu erzielen, werden nachhaltige Strategien erforderlich sein, konsequent den Einsatz von CO2-emittierenden Treibstoffen einzuschr?nken und zu Null-Emissions-Fahrzeugen ?berzugehen. Der elektrische Antrieb im Fahrzeug sowie die nachhaltige elektrische Energieerzeugung der Antriebsenergie werden zuk?nftig an Bedeutung gewinnen. Im vorliegenden Beitrag werden deshalb die wesentlichen Aspekte der Energiebereitstellung f?r elektrische Mobilit?t dargelegt.
#[297] " ? 2009 Inderscience Enterprises Ltd. costs. Baranzini is the author of several publications on topics such as environmental policy instruments and green national accounting, the economics of sustainable development, water management, and the economic valuation of external costs and benefits. He is a member of several associations, a referee for various specialised journals and a member of the editorial board of the International Journal of Sustainable Development. Copyright "
#docs_tail[2000:length(docs_tail)] #NOT fine
#[227] " ? 2015 SETAC. Key Points. The study describes how life cycle assessment's (LCA) robustness can be improved by respecting prospective fluctuations, like the transition of the German electricity mix, in the modeling of the life cycle inventory. It presents a feasible and rather simple process to add time-resolved data to LCA. The study selects 2 different future scenarios from important German studies and processes their data systematically to make them compatible with the requirements of a life cycle inventory. The use of external scenarios as basis for future-oriented LCA is reflected critically. A case study on electric mobility is presented and used to compare historic, prospective static, and prospective time-resolved electricity mix modeling approaches. The case study emphasizes the benefits of time-resolved LCA in direct comparison with the currently used approaches. "

#check some critical examples from above
#interim <- docs_ids$doc[grep(pattern, docs_ids$doc, perl = T)] #looks fine, long patters emerge from ammended abstracts in additonal languae or ammended authors information
#interim[which(nchar(docs_tail) > 400)] #can all be deleted

#delete pattern
docs_ids[,  doc:= gsub(paste0("( ", copyrightsymbol, ".{1,1000}$)"), "", doc, ignore.case = T, perl = T)]

#any(grepl(pattern, docs_ids$doc, ignore.case = T, perl = T))
#[1] FALSE


#check remaining docs with copyright symbol----------------------------------------------------
docs_w_copyrightsymbol_remaining2  <- length(grep(copyrightsymbol, docs_ids$doc))
# [1] 28

#check manually
#docs_ids$doc[grep(copyrightsymbol, docs_ids$doc)]
#everything after copyright symbol can be deleted in
patterns_long <- c("? 2011 WILEY-VCH Verlag GmbH & Co. KGaA, Weinheim) Life cycle analysis is proposed as a tool for investigating the sustainability of nanostructured material"
,"? 2008 Springer-Verlag Zusammenfassung: Seit Anbeginn der Zivilisation nimmt der Bedarf an Mobilit?t stetig zu."
,"? 2001 Elsevier Science B.V.Charcoal is a major source for cooking energy in most African countries, for which demand from a burgeoning human population has sometimes outstripped the supply of wood from forests"
)
patterns_short_tail <- c("Copyright © Materials Research Society 2015"
                         ,"? De Boeck Sup?rieur. Tous droits r?serv?s pour tous pays"
                         ,"? Dc Boeck Universit?"
                         ,".? IJTech 2011"
                         ,"? IWA Publishing 2012"
                         ,"? IWA Publishing 2010"
                         ,"? Emerald Group Publishing Limited."
                         ,"? Emerald Group Publishing Limited."
                         ,"? INRA and Springer-Verlag, France 2012"
                         ,"?Springer Science+Business Media B.V. 2009")
#delete manually identified short patterns
docs_ids[, doc:= stri_replace_all_fixed(doc, patterns_short_tail, "", vectorize_all = FALSE)]
#any(grepl(paste(patterns_short_tail, collapse = "|"), docs_ids$doc))
#[1] FALSE

#delete long patterns
pattern <- paste0("(.*)(", copyrightsymbol, " \\d{4} )(WILEY|Springer|Elsevier)(.*$)")
docs_tail <- gsub(pattern, "\\2\\3\\4", docs_ids$doc[grep(pattern, docs_ids$doc, perl = T)], ignore.case = T, perl = T)
#docs_tail #looks fine delete
docs_ids[, doc:= gsub(paste0("(", copyrightsymbol, " \\d{4} )(WILEY|Springer|Elsevier)(.*$)"), "", doc, ignore.case = T, perl = T)]

docs_w_copyrightsymbol_remaining3  <- length(grep(copyrightsymbol, docs_ids$doc))
#is as it shold be
#(docs_w_copyrightsymbol_remaining3  == (docs_w_copyrightsymbol_remaining2 - length(patterns_long) - length(patterns_short_tail)) )
#[1] TRUE


#2nd  copyright pattern (C) -------------------------------------

#note the\\w, manual screening showed that there is only one abstract with ap pattern
#"(C) 1980 -" that should not be deleted, therefore, restriction to word symbol
pattern <- paste0("(.*)(\\(C\\)\\s\\d{4} \\w|\\(C\\)\\d{4} \\w)(.*)($)")
docs_with_copyrightpattern2 <- length(grep(pattern, docs_ids$doc))
#[1] 63

docs_tail <- gsub(pattern, "\\2\\3", docs_ids$doc[grep(pattern, docs_ids$doc)], ignore.case = T, perl = T)
#summary(nchar(docs_tail))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 24.0    30.0    62.0   708.1  1475.5  2335.0

#one of the longer but still relatively short patterns
#nchar_long_pattern <- nchar("(C) 2000 United Nations. Published by Elsevier Science Ltd. All rights reserved.")
#interim <- docs_ids$doc[grep(pattern, docs_ids$doc)]
#interim[ which(nchar(docs_tail) > nchar_long_pattern) ] #looks fine
#the text following the copyright symbol (C) is usually an extended abstract
#we only use the licenced abstract from the publisher, i.e., the first one
#pattern can be deleted

docs_ids[, doc:= gsub(paste0("(\\(C\\)\\s\\d{4} \\w|\\(C\\)\\d{4} \\w)(.*)($)"), "", doc, ignore.case = T, perl = T)]

#any(grepl(pattern, docs_ids$doc))
#[1] FALSE


#  3rd copyright pattern Published in --------------------------------------------------
#Publishd in
# docs[grep("Published in", docs, fixed = T)]
# docs <- gsub("Published in 1999 by John Wiley & Sons, Ltd.", "", docs, fixed = T)
# docs <- gsub("Published in 2007 by John Wiley and Sons, Ltd", "", docs, fixed = T)
# docs[grep("Published in", docs, fixed = T)]

#  4th copyright pattern Copyright --------------------------------------------------
pattern <- paste0("(.*)(Copyright)(.*$)")
#length(docs_ids$doc[grep(pattern, docs_ids$doc)])
#1057
docs_tail <- gsub(pattern, "\\2\\3", docs_ids$doc[grep(pattern, docs_ids$doc)], ignore.case = FALSE, perl = T)
#summary(nchar(docs_tail))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 9.0    10.0    10.0    12.1    10.0  1492.0
#docs_tail[which(nchar(docs_tail) > nchar("Copyright "))]
#only one potential critical pattern
#docs_ids$doc[grep("Limited, Australia (CAL), etc., have been used to generate emission figure", docs_ids$doc, fixed = T)]
#replace and rereplace critical pattern
replacement_pattern <- "thisisareplacementpattern"
critical_pattern <- c("Copyright Agency Limited", "Copyright Licensing Agency")
replace_critical_pattern <- paste0(replacement_pattern, c(" Agency Limited", " Licensing Agency"))
docs_ids[, doc:= stri_replace_all_fixed(doc, critical_pattern, replace_critical_pattern, vectorize_all = F)]

#remove copyright pattern
#length(docs_ids$doc[grep("Copyright.*$", docs_ids$doc)])
#1056
docs_ids[, doc:= gsub("Copyright.*$", "", doc)]
#rereplace
docs_ids[, doc:= gsub(replacement_pattern, "Copyright", doc, fixed = T)]
#length(docs_ids$doc[grep("Copyright.*$", docs_ids$doc)])
#[1] 1



# 5th check which docs still have publisher names at tail -----------------
publishers <- publisher_names
publishers <- unique(publishers[ !is.na(publishers) & publishers != ""])
publishers <- publishers[grep(" ", publishers)]
#max(nchar(publishers))

#docs_tails <- gsub("(.*)(.{120}$)", "\\2", docs, perl = T)
#docs_tails[sample(1:length(docs_tails), 50)]
#docs_tails_w_publishers <- stringi::stri_detect_regex(docs_tails, paste(publishers, collapse = "|"))
#docs_tails[docs_tails_w_publishers]

#additional patterns for potential removal from previous check
#;copy \\d{4}
#Published by
#? IWA Publishing 2014."

#directly replace one of them
copyrightsymbol <- stringi::stri_trans_general("\u00A9", "Hex-Any/Name")
#docs[grep(paste0(copyrightsymbol, ".{1,200}$"), docs)]
docs_ids[, doc:= gsub("? IWA Publishing 2014.", "", doc, fixed = T)]

#check for the other one
#docs[grep("copy;.{1,300}$", docs, perl = T)]
#can be removed
docs_ids[, doc:= gsub("copy;.{1,300}$", "", doc, perl = T)]

#check additional pattern
#docs[grep("Published by", docs , perl = T)]
#remove the part that makes sense to be removed
docs_ids[, doc:= gsub("Published by Elsevier Science Ltd.", "", doc, fixed = T)]

#check what is betwewen the last two dots
#docs_tail_last_dots <- gsub("(^.*)(\\.[^\\.]+\\..{0,10}$)", "\\2", docs, perl =T)
#seems like above search pattern that not work correctly...
#docs_tails_w_publishers <- stringi::stri_detect_regex(docs_tail_last_dots, paste(publishers, collapse = "|"))
#docs_tail_last_dots[docs_tails_w_publishers] #looks like there are no tailing publishers anymore
#remove patterns
patterns <- c("The Research Council of Norway and the Norwegian State Housing Bank are financially supporting the project."
              ,"American Society of Agricultural and Biological Engineers"
              ,"co-sponsored by Japan Institute of Energy and Catalysis Society of Japan"
              ,"ASTM International Committee E60 on Sustainability is a significant contributor of technically sound, market-relevant standards addressing the fields of sustainability and sustainable development"

)

docs_ids[, doc:= stri_replace_all_fixed(doc, patterns, rep("", length(patterns)), vectorize_all = FALSE)]


# remove conference proceedings from docs -------------------------------------------
#another potential pattern
#"proceedings"
docs_ids[, grep("proceedings", doc, ignore.case = T)]

#delete all docs beginning with ""The proceedings contain \\d+ papers."
#since these abstracts are only summaries of the content of a specific conference
#substr(docs[grep("^The proceedings contain \\d+ papers", docs)], 1, 200)
delete_idxs <- grep("^The proceedings contain \\d+ papers", docs_ids$doc)
docs_ids <- subset_rows_DT(docs_ids, idxs = delete_idxs, delete_idxs = TRUE)


#also one instance with special error
delete_idxs <- grep("^The proceedings contains \\d+ papers", docs_ids$doc)
docs_ids <- subset_rows_DT(docs_ids, idxs = delete_idxs, delete_idxs = TRUE)

#check pattern again
#docs[grep("^(?=.*conference)(?=.*proceedings)", docs, ignore.case = T, perl = T)]
delete_elements <- c("The present paper is a review of several papers from the Proceedings of the Joint European Thermodynamics Conference"
                     ,"The water, energy and food-security nexus approach put forward by the Bonn2011 Conference highlights the need for an integrative approach towards issues of water")

delete_idxs <- grep(paste(delete_elements, collapse = "|"), docs_ids$doc)
#sum(delete_idxs)
docs_ids <- subset_rows_DT(docs_ids, idxs = delete_idxs, delete_idxs = TRUE)

#check pattern again
#docs[grep("proceedings", docs, ignore.case = T)]
#looks ok now


#check which docs contain longer publisher or affiliation names
aff <- strsplit(affiliation_names, ",")
aff <- unlist(lapply(aff, `[`, 1))
pub_aff <- c(publisher_names, aff)
Encoding(pub_aff) <- "UTF-8"
pub_aff <- pub_aff[ !is.na(pub_aff) & pub_aff != ""]
pub_aff <- unique(pub_aff)

#min(stri_count_fixed(pub_aff, " "))

#pub_aff[ stri_count_fixed(pub_aff, " ") == 0]
#for unigrams it is assumed that they can be replaced if they
#include at least two uppercase letters, hence, if its a special name
#and if they are sourrounded by either brackets or spaces or space and dot/comma/semicolon so that they are not replace within normal words accidentally
pub_aff_unigrams <- pub_aff[ stri_count_fixed(pub_aff, " ") == 0]
pub_aff_unigrams <- pub_aff_unigrams[grep("^.*[A-Z].*[A-Z].*$", pub_aff_unigrams)]
pub_aff_unigrams <- pub_aff_unigrams[order(nchar(pub_aff_unigrams), decreasing = TRUE)]

#replace unigram pub_aff
docs_ids[, doc := gsub(paste0("(\\s|\\()(", paste(pub_aff_unigrams, collapse = "|"), ")(\\s|\\)|\\.|,|;)")
             , " "
             , doc
             , perl = T)
         ]

#quick manual screening looks fine
#docs[grep("   ", docs)]

#check bigrams
pub_aff_bigrams <- pub_aff[ stri_count_fixed(pub_aff, " ") == 1]
#limit to proper names
pub_aff_bigrams <- stri_extract_all_regex(pub_aff_bigrams, "^[A-Z][^\\s]+\\s[A-Z].+$", omit_no_match = TRUE, simplify = TRUE )
pub_aff_bigrams <- pub_aff_bigrams[ pub_aff_bigrams != ""]
pub_aff_bigrams <- pub_aff_bigrams[order(nchar(pub_aff_bigrams))]

#check which patterns to remove from bigrams
#https://stackoverflow.com/questions/116819/regular-expression-to-exclude-set-of-keywords
#pub_aff_bigrams[grep("^(?:(?!University|Department|Institute|College|TU|Laboratory).)*$", pub_aff_bigrams, perl = T)]
#terms include, e.g., "Human Ecology", "Management School", "Environmental Studies", etc.
#it is assumed that with the terms with the uppercase letters are used in the text only
#as true uppercasse letters, one would usually not use, e.g., the term "Urban Studies" as propoer name
#to name the field of "urban studies" within an abstract
#check this:
#docs[grep("Environmental Studies", docs)] #assumption is acceptable
docs_ids[, doc := gsub(paste0(pub_aff_bigrams, collapse = "|")
             , " "
             , doc
             , fixed = T)
         ]


#check trigrams
#pub_aff_trigrams <- pub_aff[ stri_count_fixed(pub_aff, " ") == 2]
#pub_aff_trigrams[grep("^(?:(?!University|Department|Faculty|Institute|College|School|TU|Inst.|Centre|Center|Foundation|Division|Laboratory).)*$"
#                      , pub_aff_trigrams, perl = T)]
#seems reasonable to delete all that
#going further all grams with n>2 may be deleted
pub_aff_2plusgrams <- pub_aff[ stri_count_fixed(pub_aff, " ") >= 2] %>%
                      .[ order(stri_count_fixed(., " "), decreasing = T)] %>% #order by number spaces
                      split(., stri_count_fixed(., " ")) %>%
                      .[length(.):1]  %>% #order has to be reversed since list is ordered from low to high numbers
                      lapply(., function(x) {x[order(nchar(x), decreasing = T)]}) %>%  #order by nchar
                      unlist(use.names = F)

docs_ids[, doc := gsub(paste0(pub_aff_2plusgrams, collapse = "|")
             , " "
             , doc
             , fixed = T)
         ]

return(docs_ids)

}


#not sure why but executing this function resulted in an error
#above function was therefore run manually
#(this was the initial approach anyway, function only to avoid variables/objects in namespace)
# FIXME
#maybe simply the assignment docs_ids <- remove... is missing??
remove_pusblisher_affiliation_names(docs_ids = docs_ids
                                                ,publisher_names = data[, Publisher]
                                                ,affiliation_names = data[, Affiliations])


#save / load image -------------------------------------
# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# save.image(file = "after_pubaff_replacement_not_via_functiont.RData")

# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# load(file = "after_pubaff_replacement_not_via_functiont.RData")
# for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)


# elsevier_publisher_affiliations <- pub_aff
# elsevier_publisher_affiliations <- elsevier_publisher_affiliations[sample(1:length(elsevier_publisher_affiliations), length(elsevier_publisher_affiliations ))]
# writeLines(elsevier_publisher_affiliations, paste0(dirproj, "publishers_affiliations.txt"))

# ids_elsevier <- ids[which(ids %in% elsevier_request_full$id)]
# docs_elsevier <- docs[which(ids %in% elsevier_request_full$id)]
# years_elsevier <- data[id %in% elsevier_request_full$id, Year ]
#
# write.csv(cbind(docs_elsevier,years_elsevier  , ids_elsevier), paste0(dirproj,"data_cleaned.csv"))
#
#
# docs_elsevier_prep <- docs[which(ids %in% elsevier_request_full$id)] %>%
#                  preprocessing_function %>%
#                  stri_replace_all_fixed(., paste0(" ",stopwords, " "), rep(" ", length(stopwords)), vectorize_all = FALSE) %>%
#                  stri_replace_all_regex(., "^the|^this|^many", " ", vectorize_all = FALSE)
#                  gsub("\\s+", " ", .)
# write.csv(cbind(docs_elsevier_prep,years_elsevier  , ids_elsevier), paste0(dirproj,"data_cleaned_prep.csv"))
#
# shuffle <- function(x) {
#   set.seed(length(x))
#   lx <- length(x)
#   x[sample(1:length(x), length(x))]
# }
# docs_elsevier_shuffled <- docs[which(ids %in% elsevier_request_full$id)] %>%
#                           strsplit("\\.") %>%
#                           lapply(., shuffle) %>%
#                           lapply(., function(x) paste(x, collapse = " ")) %>%
#                           unlist
#
# write.csv(cbind(docs_elsevier_shuffled,years_elsevier  , ids_elsevier), paste0(dirproj,"data_cleaned_shuffled.csv"))



#check the histogramm of abstract length again
docs_ids[, ncharAbstract := nchar(doc)]


histncharabstracts_max5k_cleaned <- ggplot(data = docs_ids[ ncharAbstract <= 5000  , c("ncharAbstract", "id")], aes(docs_ids[ ncharAbstract <= 5000  , ncharAbstract])) +
  geom_histogram(bins = 50) +
  # xlab("Year") +
  ylab("Number of characters in abstracts") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=10
                               , angle=90)) +
  scale_x_continuous(breaks=seq(min(docs_ids$ncharAbstract, na.rm = TRUE)
                                , max(docs_ids$ncharAbstract, na.rm = TRUE), 100))

#compare plots
histncharabstracts_max5k_cleaned

histncharabstracts_max5k
#check content of remaining long abstracts
#nrow(data[ nchar(Abstract) >= 5000  & (id %in% ids)  , ])
#[1] 67
#nrow(data[ Publisher == "" , ])
#[1] 13889
#data[ Source.title == "" , ]

#data[ nchar(Abstract) >= 5000 & Source.title == "Energy Policy" , Abstract]
#data[ nchar(Abstract) >= 5000 & id %in% ids , Abstract]
#data[ nchar(Abstract) >= 5000 & id %in% ids  , c("Source.title", "Document.Type", "Publisher", "DOI")]
#some of these long abstracts might be excluded, some not
#it was decided to filter by the quality of the type of the publication (before publiser cleaning step), idea is to exclude reports, proceedings, etc.

#do some manual replacements ------------------------------------------------------
#or corrections of individual words or proper names
#following replacement is known from later analytical steps and was moved here
#for final run of script
docs_ids[, doc:= gsub("chernobil", "chernobyl", doc, ignore.case = T)]


# replace acronyms by words -----------------------------------------------
#extract abbreviations and acronyms to avoid that they are bound
#to the phrase that they stand for in the collocation dectection step

#the following test runs have been performed with non data table object, hence only the doc column

#parallel version for synonym replacement is slightly faster and may be used
#system.time(test1 <- replace_acronyms_by_words(docs[1:1000]))
# User      System verstrichen
# 64.46        0.07       65.61
#second run
# User      System verstrichen
# 81.84        0.06       82.43

#system.time(test2 <- replace_acronyms_by_words_parallel(docs[1:1000], ncores = ncores))
# User      System verstrichen
# 0.56        0.17       55.15
#second run
# User      System verstrichen
# 4.59        0.12       71.63

#identical(test1, test2)
#[1] TRUE


# start1 <- proc.time()
# test1 <- replace_acronyms_by_words(docs[1:1000])
# elapsed1 <- proc.time()-start1
#
# start2 <- proc.time()
# test2 <- replace_acronyms_by_words_parallel(docs[1:1000], ncores = ncores)
# elapsed2 <- proc.time()-start2

#elapsed1
# User      System verstrichen
# 66.27        0.03       68.03
#elapsed2
# User      System verstrichen
# 0.10        0.00       53.84

start_acronym_replacement <- proc.time()
docs_ids[, doc:= replace_acronyms_by_words(doc)]
elapsed_time_acronym_replacement <- proc.time()-start_acronym_replacement
# User      System verstrichen
# 3862.94        0.94     3870.32


#save / load image -------------------------------------
 # setwd("C:/Science/Publication/3_sustainable_energy/data/")
 # save.image(file = "after_acronym_replacement.RData")
 #
 # setwd("C:/Science/Publication/3_sustainable_energy/data/")
 # load(file = "after_acronym_replacement.RData")
 # for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)


# some more manual replacements and document selection ----------------------------------
#check for additional tokens to be removed  in german, french, spanish
docs_ids[grep(" durch | als | entwicklung", doc, ignore.case = T, perl = T),.(doc,id)]
docs_ids[grep("?", doc, fixed = T),.(doc,id)]
docs_ids[grep(" es ", doc, fixed = T),.(doc,id)]
docs_ids[grep(" les | der | die | das | le | el ", doc, ignore.case = T, perl = T),.(doc,id)]
remove_pattern <- c(
  "Werden die internationalen Vereinbarungen zum Ersatz fossiler Energietr?ger durch erneuerbare von der Politik.*$"
  ,"Oberfl?chennahe Geothermie leistet mittlerweile einen wesentlichen Beitrag zur Grundlastversorgung mit W?rmeenergie.*$"
  ,"R?sum?: Les pr?occupations li?es au d?veloppement durable ont influ?.*$"
  ,"Les pays en d?veloppement ont maintenant la possibilit? de choisir entre un syst?me bas? sur le p?trole ou la mise en valeur des.*$"
  ,"La justice environnementale dans le monde des organisations environnementalistes non gouvernementales de Toront.*$"
)

docs_ids[grep(paste(remove_pattern, collapse = "|")
                               , doc
                               , ignore.case = T, perl = T)
                          ,.(doc = gsub(paste0("(^.*)(",paste(remove_pattern, collapse = "|"), ")")
                                        ,"\\2"
                                        ,doc), id)]
#pattern works, remove it, use capturing group \\1 instead of \\2
docs_ids[grep(paste(remove_pattern, collapse = "|")
              , doc
              , ignore.case = T, perl = T)
         , doc := gsub(paste0("(^.*)(",paste(remove_pattern, collapse = "|"), ")")
                       ,"\\1"
                       ,doc)]

#replace typical abstract heading
docs_ids[ , doc:= gsub("Figure not available|see fulltext|Background:|Results:|Methods:|Conclusions:|Conclusion:|Abstract:|Aim:|^Abstract|Main conclusions:"
                       ," ", doc, perl = T)]


#check for additional documents to be removed
docs_ids[grep("^\\w+, ", doc, perl = T),.(substr(doc, 1,50),id)]
tail(docs_ids[grep("^\\w+, ", doc, perl = T),.(substr(doc, 1,50),id)],100)
ids_remove <- c(3824, 28668)
docs_ids[ (id %in% ids_remove),]
docs_ids <- docs_ids[ !(id %in% ids_remove),]
nrow(docs_ids)
#[1] 31708

#remove additional ids on basis of duplicated titles
duplicated_title_ids <- data[duplicated(data[, Title]), id]
docs_ids <- docs_ids[ !(id %in% duplicated_title_ids), ]

#also use titles after removing punctuation and blanks
duplicated_title_ids <- data[duplicated(data[,tolower(gsub("\\W|", "", Title, perl = T))]), id]
docs_ids <- docs_ids[ !(id %in% duplicated_title_ids), ]

# uncontract negations
docs_ids[, doc := uncontract_negations(doc)]

# POS tagging  ------------------------------------------------
time_before_pos_tagging_p <- proc.time()
docs_ids_pos_tagged <- pos_tag_parallel(docs_ids = docs_ids, ncores = 3, chunk_size = 1000)
time_for_pos_tagging_treetagger_p <- proc.time() - time_before_pos_tagging_p
time_for_pos_tagging_treetagger_p
# User      System verstrichen
# 13.81        2.52     1242.55
sum(grepl("SPLITDOCSHERE", docs_ids_pos_tagged$token))
# [1] 31575

#save / load image -------------------------------------
# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# save.image(file = "after_pos_tagging.RData")

# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# load(file = "after_pos_tagging.RData")
# for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)

ids_temp <- data.table(id = unique(docs_ids_pos_tagged$doc_id))
# identical(ids_temp[order(id), id], docs_ids[order(id), id])
# TRUE
#remove marker
docs_ids_pos_tagged <- docs_ids_pos_tagged[!(token %in% "SPLITDOCSHERE"), ]


#use original token  as lemma if lemma is not identified
#correct plural s that has not been removed from tokens with unknown lemma
#this can be the case, e.g., for "biofuels" if in comma separated list
docs_ids_pos_tagged[lemma == "<unknown>" & tag == "NNS", lemma:= gsub("(?<=.)s$", "", token, perl = T)]
docs_ids_pos_tagged[lemma == "<unknown>", lemma:= token]


#specification of ign.comp is somehow ignored in treetag (in the forms I have tried)
#therefore split up hyphenated words
#create token id to avoid removing duplicates when grouping over all columns
docs_ids_pos_tagged[, token_id:= 1:nrow(docs_ids_pos_tagged) ]
#mark lemma of words with hyphen
docs_ids_pos_tagged[ grep("-", token, fixed = T), hyphenated:= TRUE]
docs_ids_pos_tagged <- docs_ids_pos_tagged[, strsplit(as.character(lemma), "-", fixed=TRUE), by = names(docs_ids_pos_tagged)][,
                                                                                                                              lemma := NULL][
                                                                                                                                , setnames(.SD, "V1", "lemma")]

docs_ids_pos_tagged <- docs_ids_pos_tagged[ lemma != "", ]

#basic selection of tags/wclasses to be kept on basis of wclass
wclass_keep <- unique(docs_ids_pos_tagged$wclass)
# [1] "fullstop"      "noun"          "verb"          "adjective"
# [5] "preposition"   "determiner"    "punctuation"   "adverb"
# [9] "pronoun"       "number"        "modal"         "conjunction"
# [13] "comma"         "to"            "name"          "predeterminer"
# [17] "existential"   "listmarker"    "symbol"        "foreign"
# [21] "particle"      "possesive"     "interjection"
wclass_rm <- c("fullstop", "preposition", "determiner", "punctuation", "pronoun", "number", "modal", "conjunction",
                          "comma", "to", "predeterminer", "existential", "listmarker", "symbol", "foreign", "particle", "possesive"
                          ,"interjection")
wclass_keep <- setdiff(wclass_keep, wclass_rm)
#[1] "noun"      "verb"      "adjective" "adverb"    "name"

#check for individual lemma to be kept
lemma_check <- unique(docs_ids_pos_tagged[wclass %in% c("existential", "particle", "determiner", "predeterminer", "preposition"
                                                             ,"modal", "foreign", "to"),lemma ])
lemma_check[order(lemma_check)]
#keep negations and some specific prepositions
lemma_force_keep <- tolower(c("no", "not", "none", "neither", "nor", "offshore", "inland", "downstream"))
#mark lemmas/tokens to keep
docs_ids_pos_tagged[lemma %in% lemma_force_keep, keep := TRUE]
#prune by tag
docs_ids_pos_tagged <- docs_ids_pos_tagged[wclass %in% wclass_keep | keep == TRUE, ]

# final pre-processing and selection of tokens----------------------------------------------------
docs_ids_pos_tagged[ , lemma:= preprocessing_function(lemma, rm_diacritics = TRUE)]

#ensure that some year indications are kept
docs_ids_pos_tagged[ grep("^\\d{4}$|^\\d{4}s$|^\\d{2}th$", token, perl = T)
                     , lemma:= stri_replace_all_fixed(lemma
                                                      ,as.character(0:9)
                                                      ,paste0("_", c("zero", "one", "two", "three", "four", "five", "six", "seven","eight", "nine"))
                                                      , vectorize_all = FALSE
                     )]

#first removal of single punctuation
docs_ids_pos_tagged <- docs_ids_pos_tagged[ !grepl("^[[:punct:]]+$", lemma), ]

docs_ids_pos_tagged[ , lemma:= preprocessing_function(lemma, rm_diacritics = TRUE)]

check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z]", lemma, perl = T), lemma])

#check for some important chemcial compounds
check_non_word_tokens[grep(paste(c("CO2", "SO2", "CH4", "SOx", "NOx", "NH3", "NH4", "NO3", "NO2", "Li", "Ni", "Pb", "H2", "Nd"), collapse = "|")
                          ,check_non_word_tokens
                          ,perl = TRUE)]

#check and replace additional year numbers
check_non_word_tokens[grep("\\d{4}", check_non_word_tokens)]

docs_ids_pos_tagged[ , lemma:= gsub("^November2011$", "November_two_zero_one_one", lemma, perl = T)]
docs_ids_pos_tagged[ , lemma:= gsub("EU2020", "EU_twentytwenty", lemma, perl = T)]
docs_ids_pos_tagged[ , lemma:= gsub("EU2030", "EU_twentythirty", lemma, perl = T)]
docs_ids_pos_tagged[ , lemma:= gsub("EU2050", "EU_twentyfifty", lemma, perl = T)]

#if not tokenized by hyphen
# docs_ids_pos_tagged[ , lemma:= gsub("(post|pre|mid|early)(-)(\\d{4})", "\\1_\\3", lemma, perl = T)]
# docs_ids_pos_tagged[ grep("(post|pre|mid|early)(_)(\\d{4})",lemma, perl = T)
#                      , lemma:= stri_replace_all_fixed(lemma
#                                                       ,as.character(0:9)
#                                                       ,paste0("_", c("zero", "one", "two", "three", "four", "five", "six", "seven","eight", "nine"))
#                                                       , vectorize_all = FALSE
#                      )]
#
check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z]", lemma, perl = T), lemma])
unique(check_non_word_tokens[grep("_", check_non_word_tokens, fixed = T)]) # only the year numbers

check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z_]", lemma, perl = T), lemma])

check_non_word_tokens[grep("\\d{4}", check_non_word_tokens)]
#check everything that is not a chemical compound
check_non_word_tokens[grep("\\d{2}", check_non_word_tokens)]

docs_ids_pos_tagged[ , lemma:= gsub("C02emission", "CO_twoemission", lemma)]

check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z_]", lemma, perl = T), lemma])
check_non_word_tokens[grep("\\d{2}", check_non_word_tokens)]

delete_idxs <- grep("\\d{2}", docs_ids_pos_tagged$lemma, perl = T)
docs_ids_pos_tagged <- subset_rows_DT(docs_ids_pos_tagged, idxs = delete_idxs , delete_idxs = TRUE)

check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z_]", lemma, perl = T), lemma])

check_non_word_tokens[grep("\\d", check_non_word_tokens) & grep()]
unique(docs_ids_pos_tagged[ !grepl("[[:upper:]]", lemma) & grepl("\\d", lemma) , lemma])

#remove lemmas with numbers not a chemical compoudn (marked via at least one uppercase)
docs_ids_pos_tagged <- docs_ids_pos_tagged[ !( !grepl("[[:upper:]]", lemma) & grepl("\\d", lemma) ) , ]

check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z_]", lemma, perl = T), lemma])

#no dashes in words
unique(docs_ids_pos_tagged[grep("_|\\-", lemma, perl = T), lemma])
# only the year numbers

#ensure that some chemical compounds are kept by turning them to "words"
# docs_ids_pos_tagged[ grep(paste(c("CO2", "SO2", "CH4", "SOx", "SO4", "TiO2", "N2O", "NOx", "SiO2", "Cl2", "HF6", "NH3", "NH4", "NO3", "NO2", "Li", "Ni", "Pb", "H2", "Nd")
#                                 , collapse = "|"), lemma)
#                           , lemma:= stri_replace_all_fixed(lemma
#                                                            ,as.character(1:9)
#                                                            ,paste0("_", c("one", "two", "three", "four", "five", "six", "seven","eight", "nine"))
#                                                            , vectorize_all = FALSE
#                           )]

docs_ids_pos_tagged[ , lemma:= stri_replace_all_fixed(lemma
                               ,as.character(1:9)
                               ,paste0("_", c("one", "two", "three", "four", "five", "six", "seven","eight", "nine"))
                               , vectorize_all = FALSE
                          )]

check_non_word_tokens <- unique(docs_ids_pos_tagged[grep("[^A-Za-z_]", lemma, perl = T), lemma])

#remove remaining tokens that inlcudes numbers or punctuation
docs_ids_pos_tagged <- docs_ids_pos_tagged[ !grepl("[^A-Za-z_]",lemma, perl = T) , ]

unique(docs_ids_pos_tagged[ setdiff(grep("[[:punct:]]", lemma, perl = T), grep("_", lemma, perl = T))
                            , lemma])
# character(0)
docs_ids_pos_tagged[, lemma:= tolower(lemma)]

unique(docs_ids_pos_tagged[ tag == "NN" & grepl("[^s]s$", lemma, perl = T), lemma ])
plurals <- c("limitations", "products",  "pumps", "problems", "characteristic", "plants", "emissions",
  "resources", "sources", "materials", "areas", "region", "stakeholders", "manufacturers", "contracts",
  "audits", "effects", "minerals", "scenarios", "households", "limits","foods", "subsystems", "actors"
  , "conflict")

docs_ids_pos_tagged[ lemma %in% plurals, lemma:= gsub("s$", "", lemma, perl = T)]

# pre-process and remove stopwords --------------------------------------------------------
Encoding(stopwords) <- "UTF-8"

stopwords <- c(stopwords,
               pos_tag_parallel(data.table(doc = stopwords, id = 1:length(stopwords)))[,lemma])

stopwords <- unique(stopwords)
stopwords <- stopwords[ nchar(stopwords) > 2]
stopwords <- stopwords[ stopwords != ""]
stopwords <- stopwords[ !grepl("\\W", stopwords)]
stopwords <- tolower(stopwords)

docs_ids_pos_tagged <- docs_ids_pos_tagged[!(lemma %in% stopwords), ]

#system.time(
  # docs_ids[, doc:= preprocessing_function(doc, token_exclude_length = token_exclude_length, erase_patterns = stopphrases)][,
  #      doc:= tokenizers::tokenize_word_stems(doc, language = "english" ,stopwords = NULL ,simplify = FALSE)][,
  #      doc:= lapply(doc , function(x) x[ !(x %in% stopwords) ])][,
  #      doc:= lapply(doc, function(x) paste(x, collapse = " "))][,
  #      doc:= unlist(doc)]
#)
# User      System verstrichen
# 48.27        0.13       49.20


# docs_ids_test <- copy(docs_ids[grep("heat and power system", doc, ignore.case = T),])
# docs_ids_test[, doc:= preprocessing_function(doc, token_exclude_length = token_exclude_length)][,
#          doc:= tokenizers::tokenize_word_stems(doc, language = "english" ,stopwords = NULL ,simplify = FALSE)][,
#           doc:= lapply(doc , function(x) x[ !(x %in% stopwords) ])][,
#            doc:= lapply(doc, function(x) paste(x, collapse = " "))][,
#             doc:= unlist(doc)]

#old approach without data.table docs_ids bound but docs as characters
# docs <- docs %>%
#         preprocessing_function(token_exclude_length = token_exclude_length) %>%
#         tokenizers::tokenize_word_stems(language = "english" ,stopwords = NULL ,simplify = FALSE) %>%
#         lapply(. , function(x) x[ !(x %in% stopwords) ]) %>%
#         lapply(., function(x) paste(x, collapse = " ")) %>%
#         unlist


# settings for vectorization model -------------------------------------------------
#sent_selection_term <- c("resource", "challenge|future") #if several terms shall be searched in the sense of OR use the symbol |, e.g. like "resource|metal"
#offset_selected_sent <- c(-Inf,Inf) #negative and positive offset from selected sentences; c(1,1) would mean, use the selected sentence plus one before and one after it/ set to -Inf,Inf to use full document, this is the default


#save / load image -------------------------------------
# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# save.image(file = "before_collocation_modelling.RData")
#
# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# load(file = "before_collocation_modelling.RData")
# for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)



#issue cleared, see https://github.com/dselivanov/text2vec/issues/230
# # pre test on electric arc furnac collocation
# vocabulary[grep("arc_furnac", vocabulary$term),]
# #   term term_count doc_count
# # 1:        arc_furnac          1         1
# # 2: electr_arc_furnac         41        20
#
# length(docs_ids[grep("\\belectr arc furnac\\b", doc, perl =T), doc])
#
# gsub("arc furnac", " ARCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  ",
#      gsub("electr arc furnac"
#           , "  "
#           , docs_ids[grep("arc furnac", doc, perl =T), doc]
#      )
# )


# collapse pos tagged format to list of tokens ----------------------------
docs_ids <- docs_ids_pos_tagged[, .(doc = list(lemma)), keyby = doc_id]
setnames(docs_ids, "doc_id", "id")
docs_ids <- docs_ids[ids_temp, on = "id"]
docs_ids[is.na(doc), doc := NULL]

#check reamining duplicate docs and if duplication is due to cleaning or really a duplication
duplicated_idxs <- which(duplicated(docs_ids[, doc]))
duplicated_idxs_incl_dups <- which(docs_ids[, doc] %in% docs_ids[duplicated_idxs, doc ])
docs_ids_dup_doc <-  docs_ids[duplicated_idxs_incl_dups,]

data_dups <- data[id %in% docs_ids_dup_doc[, id], ]
#check if really duplicates
data_dups[order(Title), .(substr(Title, 1,100), id)]
data_dups[, .(substr(Abstract, 1,100), id)]
#yes, remove
docs_ids <- docs_ids[ !(id %in% docs_ids[duplicated_idxs, id]), ]

# create subset of POS for collocation model (noun, adjective, name, etc.)--------------------------------------
# wclass_keep #currentyl in documents
# [1] "noun"      "verb"      "adjective" "adverb"    "name"

wclass_rm_cc <- c("verb", "adverb")
wclass_keep_cc <- setdiff(wclass_keep, wclass_rm_cc)
#[1] "noun" "adjective" "name"

#create subset for collocation model
docs_ids_pos_tagged_cc <- docs_ids_pos_tagged[wclass %in% wclass_keep_cc | keep == TRUE, ]

#further remove duplicate ids identified above, i.e., use same ids as in final docs_ids set
docs_ids_pos_tagged_cc <- docs_ids_pos_tagged_cc[ doc_id %in% docs_ids$id , ]

#collapse to tokenized form
ids_temp_cc <- data.table(id = unique(docs_ids_pos_tagged_cc$doc_id))
docs_ids_cc <- docs_ids_pos_tagged_cc[, .(doc = list(lemma)), keyby = doc_id]
setnames(docs_ids_cc, "doc_id", "id")
docs_ids_cc <- docs_ids_cc[ids_temp_cc, on = "id"]
docs_ids_cc[is.na(doc), doc := NULL]

# all(docs_ids_cc$id ==  docs_ids$id)
# TRUE


# check / find cc model parameters ----------------------------------------------------------------
iterator_docs_cc <- itoken(iterable = docs_ids_cc$doc
                           ,ids = docs_ids_cc$id
                           ,progressbar = FALSE
                           ,chunks_number = 10
)

cc_model1 = Collocations$new(collocation_count_min = 5
                             ,pmi_min = 0
                             ,gensim_min = 0
                             ,lfmd_min = -Inf
                             ,llr_min = 0
                             , sep = "_"
)



cc_model1$fit(iterator_docs_cc, n_iter = 10)
# INFO [2018-01-14 16:17:01] iteration 1 - found 74742 collocations
# INFO [2018-01-14 16:17:07] iteration 2 - found 82689 collocations
# INFO [2018-01-14 16:17:13] iteration 3 - found 83112 collocations
# INFO [2018-01-14 16:17:19] iteration 4 - found 83129 collocations
# INFO [2018-01-14 16:17:25] iteration 5 - found 83129 collocations
#...

check1 <- cc_model1$collocation_stat
check1[intersect(grep("^sustainable$", check1$prefix),
                 grep("^energy$", check1$suffix)),]

# prefix suffix   n_i   n_j n_ij     pmi      lfmd   gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1: sustainable energy 28350 91633 3477 1.97622 -17.47099 3.928951 4669.215    48719       147       27545       65

#get a impression of collocation_stat
cc_model1_quantiles <- sapply(c("pmi", "lfmd", "gensim", "llr"), function(x) {
  quantile(cc_model1$collocation_stat[, get(x)])
}, simplify = F)
cc_model1_quantiles
# $pmi
# 0%          25%          50%          75%         100%
# 3.165463e-05 1.127806e+00 2.539256e+00 5.205557e+00 1.916531e+01
#
# $lfmd
# 0%       25%       50%       75%      100%
# -38.33006 -35.72600 -33.60021 -30.26865 -10.45455
#
# $gensim
# 0%          25%          50%          75%         100%
# 0.000000e+00 2.901268e-01 1.402646e+00 8.351916e+00 1.469846e+05
#
# $llr
# 0%          25%          50%          75%         100%
# 7.625204e-09 4.223472e+00 1.714289e+01 5.481515e+01 5.291058e+04

hist(check1$pmi)
hist(check1$lfmd)
hist(check1$llr)
hist(check1$llr[ check1$llr < 200])
hist(check1$gensim)
hist(check1$gensim[ check1$gensim < 2])
hist(check1$gensim[ check1$gensim < .5])
#gensim does not seem to be a good quality measure here,
#too many terms at the lower end, too many ccs might be pruned

check1[c(intersect(grep("sustain", check1$prefix),
                   grep("^energy", check1$suffix)),
         grep("sustainable_energy", check1$prefix),
         grep("sustainable_energy", check1$suffix)),]

tail(check1[c(intersect(grep("sustain", check1$prefix),
                        grep("^energy", check1$suffix)),
              grep("sustainable_energy", check1$prefix),
              grep("sustainable_energy", check1$suffix)),], 90)

#potential pruning options
#pmi > 1.97
#lfmd > -30

head(check1[unique(c(grep("combi[a-z]+_heat", check1$prefix, perl = T),
                     grep("heat$", check1$prefix),
                     grep("heat_power", check1$prefix))),]
     ,100)
#head would be included in above pruning limits for sustainab

tail(check1[unique(c(grep("combi[a-z]+_heat", check1$prefix, perl = T),
                     grep("heat$", check1$prefix),
                     grep("heat_power", check1$prefix))),]
     ,100)
# above pmi value would prune some bad collocations
#unfortunately also "heat network" contained in 17 docs
# above lfmd would prune "heat conductivity" contained in 7 docs

check1[c(intersect(grep("long", check1$prefix),
                   grep("term", check1$suffix)),
         grep("long_term", check1$prefix)
                 ),]


#try one more run with min count = 6 (5 was already ok)
#especially for "...sustain_energi..." there are too many verbs in the phrases
#therefore, increase min CC count to 10 and set pmi as low to only just capture "sustain_energi"
cc_model2 = Collocations$new(collocation_count_min = 8
                             ,pmi_min = 0
                             ,gensim_min = 0
                             ,lfmd_min = -Inf
                             ,llr_min = 0
                             , sep = "_"
)

cc_model2$fit(iterator_docs_cc, n_iter = 6)


check2 <- cc_model2$collocation_stat
cc_model2_quantiles <- sapply(c("pmi", "lfmd", "gensim", "llr"), function(x) {
  quantile(cc_model2$collocation_stat[, get(x)])
}, simplify = F)
cc_model2_quantiles


check2[intersect(grep("^sustainable$", check2$prefix),
                 grep("^energy$", check2$suffix)),]

# prefix suffix   n_i   n_j n_ij      pmi      lfmd   gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1: sustainable energy 28350 91633 3477 1.964684 -17.45946 3.894292 4626.063    26057       157       16153       67

check2[c(intersect(grep("sustainable", check2$prefix),
                   grep("energy", check2$suffix)),
         grep("sustainable_energy", check2$prefix),
         grep("sustainable_energy", check2$suffix)),]


head(check2[c(intersect(grep("sustainable", check2$prefix),
                        grep("energy", check2$suffix)),
              grep("sustainable_energy", check2$prefix),
              grep("sustainable_energy", check2$suffix)),], 50)



tail(check2[c(intersect(grep("sustainable", check2$prefix),
                        grep("energy", check2$suffix)),
              grep("sustainable_energy", check2$prefix),
              grep("sustainable_energy", check2$suffix)),], 50)


head(check2[unique(c(grep("combi[a-z]+_heat", check2$prefix),
                     grep("heat$", check2$prefix),
                     grep("heat_power", check2$prefix))),]
     , 90)


check2[unique(c(grep("fukushima|chernob", check2$prefix),
                grep("fukushima|chernob", check2$suffix)))
       ,]
# prefix   suffix n_i  n_j n_ij       pmi      lfmd   gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1: fukushima  daiichi  64    9    9 15.382573 -21.04272 4746.498 193.2524      240       935         377     4031
# 2: fukushima disaster  64  198    8 10.753216 -26.01193    0.000 104.6369     1629      4752       33996     8238
# 3: fukushima  nuclear  64 2795   17  8.021403 -26.56882  137.555 160.2184     4041      5518        3154     5134


check2[intersect(grep("oil", check2$prefix),
                 grep("cris", check2$suffix))
       ,]
# prefix suffix  n_i n_j n_ij      pmi      lfmd   gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1:    oil crisis 4795 788   38 4.781133 -27.48816 21.70712 180.6429    10290      6921        7034     4385

check2[unique(c(grep("fossil", check2$prefix),
                grep("fossil", check2$suffix)))
       ,]



cc_model3 = Collocations$new(collocation_count_min = 8
                             ,pmi_min = 1.96
                             ,gensim_min = 0
                             ,lfmd_min = -29
                             ,llr_min = 0
                             , sep = "_"
)



cc_model3$fit(iterator_docs_cc, n_iter = 8)


check3 <- cc_model3$collocation_stat
cc_model3_quantiles <- sapply(c("pmi", "lfmd", "gensim", "llr"), function(x) {
  quantile(cc_model3$collocation_stat[, get(x)])
}, simplify = F)
cc_model3_quantiles
# $pmi
# 0%       25%       50%       75%      100%
# 1.961023  5.482190  7.464459  9.574841 18.475698
#
# $lfmd
# 0%       25%       50%       75%      100%
# -28.99954 -27.86648 -26.31830 -23.95497 -10.44302
#
# $gensim
# 0%         25%         50%         75%        100%
# 0.00000    20.14935    58.65829   244.00688 78114.64286
#
# $llr
# 0%         25%         50%         75%        100%
# 66.79926   120.56148   172.91157   312.12277 52832.89869

check3[intersect(grep("^sustainable$", check3$prefix),
                 grep("^energy$", check3$suffix)),]
# prefix suffix   n_i   n_j n_ij      pmi      lfmd   gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1: sustainable energy 28350 91633 3477 1.964684 -17.45946 3.894292 4626.063    11410       155       10546       69

check3[c(intersect(grep("sustainable", check3$prefix),
                   grep("energy", check3$suffix)),
         grep("sustainable_energy", check3$prefix),
         grep("sustainable_energy", check3$suffix)),]


head(check3[c(grep("^fuel$", check3$suffix, perl = T)),], 50)

head(
  check3[c(grep("^power$|^energy$", check3$suffix, perl = T)),]
  ,90)

tail(
  check3[c(grep("^power$|^energy$", check3$suffix, perl = T)),]
  ,90)

head(
  check3[c(grep("solar_energi|solar$", check3$prefix, perl = T)),]
  ,60)

tail(
  check3[c(grep("solar_energi|solar$", check3$prefix, perl = T)),]
  ,60)

head(check3[unique(c(grep("combi[a-z]+_heat", check3$prefix),
                     grep("heat$", check3$prefix),
                     grep("heat_power", check3$prefix))),]
     , 90)


check3[unique(c(grep("fukushima|chernob", check3$prefix),
                grep("fukushima|chernob", check3$suffix)))
       ,]

# prefix   suffix n_i  n_j n_ij       pmi      lfmd    gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1: fukushima  daiichi  70    9    9 15.346415 -21.26513 4629.0159 192.6820      241      1150         477     5009
# 2: fukushima disaster  70  216    8 10.591527 -26.35987    0.0000 102.7239     1931      5778       10934     9797
# 3: fukushima  nuclear  70 2852   18  8.038582 -26.57296  146.0769 169.8898     4788      6140        3730     5819

check3[intersect(grep("oil", check3$prefix),
                 grep("cris", check3$suffix))
       ,]
# prefix suffix  n_i n_j n_ij     pmi     lfmd   gensim      llr rank_pmi rank_lfmd rank_gensim rank_llr
# 1:    oil crisis 5220 804   38 4.72274 -27.7328 20.84606 177.6169     9367      8273        8490     5520
check3[unique(c(grep("fossil", check3$prefix),
                grep("fossil", check3$suffix)))
       ,]

#pruning of final cc_model --------------------------------------------------------
cc_model3$prune(lfmd_min = -28.5)

# set up basic iterator for documents --------------------------------------------------------------
iterator_docs <- itoken(iterable = docs_ids$doc
                        ,ids = docs_ids$id
                        ,progressbar = FALSE
                        ,chunks_number = 10
)



#for untokenized text
# iterator_docs <- itoken(iterable = docs_ids$doc
#                         ,preprocessor = identity
#                         ,tokenizer =  function(x) {tokenizers::tokenize_words(x, lowercase = FALSE, stopwords = NULL, simplify = FALSE)}
#                         ,ids = docs_ids$id
#                         ,progressbar = FALSE
#                         ,chunks_number = 10
# )


# transform iterator by cc_model and create vocabulary for LDA  -------------------
iterator_docs_cc <- cc_model3$transform(iterator_docs)

vocabulary <- create_vocabulary(iterator_docs_cc
                                #,c(1L, 1L)
                                #, sep_ngram ="_"
                                #,stopwords = stopwords
                                )

# initial pruning of vocabulary ---------------------------
#vocabulary[ grepl("^\\w_\\w_|_\\w_\\w$|_\\w_\\w_", vocabulary$term, perl = T), ]
vocabulary <- vocabulary[ !grepl("^\\w_\\w_|_\\w_\\w$|_\\w_\\w_", vocabulary$term, perl = T), ]


# inspect vocabulary ------------------------------------------------------
vocabulary_cc_subset <- vocabulary[grep("_", vocabulary$term),]

vocabulary_cc_subset[vocabulary_cc_subset$doc_count == 1,]
head(vocabulary_cc_subset[vocabulary_cc_subset$term_count == 1,],90)

vocabulary_cc_subset_single_char <- vocabulary[grep("^\\w_|_\\w$|_\\w_", vocabulary$term),]

vocabulary_cc_subset_single_char[ vocabulary_cc_subset_single_char$doc_count == 1, "term" ]
tail(vocabulary_cc_subset_single_char, 100)
head(vocabulary_cc_subset_single_char, 100)
vocabulary_cc_subset_single_char$term

vocabulary[vocabulary$doc_count == 1,]
vocabulary[grep("(^|_)heat_", vocabulary$term), ]
vocabulary[grep("fukushima|chernob", vocabulary$term), ]
vocabulary[grep("oil_cris", vocabulary$term), ]
head(vocabulary[grep("fossil", vocabulary$term),], 100)
vocabulary[grep("action_plan", vocabulary$term),]
vocabulary[grep("heat_", vocabulary$term), ]

# settings for pruning vocabulary -----------------------------------------
nrow(vocabulary)
#[1] 63324

#collocations are not pruned on basis of low term count
#since threshold for collocation binding has already been set
#individual collocations may have count lower than threshold since during iteration
#some of them might be bound to even larger collocations leaving their tail "alone"
#hence only delete collocations with doc_count == 1, the longer collocation remains in the doc
#the actual term count may be -1 for individual documents

vocabulary[ !grepl("_", vocabulary$term) & vocabulary$doc_count < 4 ,]
tail(vocabulary[ !grepl("_", vocabulary$term) & vocabulary$doc_count < 4 ,],100)

vocabulary[ grepl("_", vocabulary$term) & vocabulary$doc_count == 1 & vocabulary$term_count == 1 , "term"]


# prune vocabulary --------------------------------------------------------
# term_count_min = 3 #minimum number of occurences over all documents.
# term_count_max = Inf #maximum number of occurences over all documents.
#
# doc_proportion_per_term_max <- 1.00 #maximum share of docs containing an individual term
# vocab_term_max = Inf #maximum number of terms in vocabulary
#
# doc_proportion_per_term_min <- 0/nrow(docs_ids) #5/nrow(data) #in at least X documents
#
#
# vocabulary_pruned <- prune_vocabulary(vocabulary
#                                ,term_count_min = term_count_min
#                                ,term_count_max = term_count_max
#                                ,doc_proportion_max = doc_proportion_per_term_max
#                                ,doc_proportion_min = doc_proportion_per_term_min
#                                ,vocab_term_max = vocab_term_max
#                                )

#prune only non collocations by doc count > 2
vocabulary[  !grepl("_", vocabulary$term) & (vocabulary$doc_count < 3)  , ]
vocabulary <- vocabulary[ !( !grepl("_", vocabulary$term) & (vocabulary$doc_count < 3) ), ]


#check terms with highest occurrence
nrow(vocabulary)
# [1] 29738

vocabulary <- vocabulary[ !( nchar(vocabulary$term) < 3 ), ]

tail(vocabulary,100)
vocabulary[ (nrow(vocabulary)-199):(nrow(vocabulary)-100),]
#almost only the last 100 words include non informative ones, the rest seems ok
#following words are manually removed
#most of them are typical scientific words
#energy is removed but sustainability or sustainab not to check who uses the concept how
rm_terms <- c("demonstrate", "due", "year", "aim", "evaluate", "important", "require", "find", "case", "propose",
              "make", "include", "present", "base", "result",
              "increase",
              "develop",
              "not", #exclude the negation terms from words_keep (only reasonable for CC)
              "no",
              "none", "analyse", "aspect", "finally", "reveal", "emerge", "perspective", "finding",
              "highlight",
              "model", "methodology",
              "design", "discuss", "lead", "application", "analysis",
              "datum", "apply", "explore",
              ",investigate",  "play", "relate", "face", "work", "key", "basis", "set",
              "focus", "current", "conclude", "purpose",
              "establish", "review", "carry",
              "system", "tool", "determine", "main", "conduct", "significantly", "significant",
              "show", "examine", "exist", "compare", "identify", "investigate",
              "analyze", "term", "type", "address", "test", "result_show"

              ,"sustainable" ,"energy", "process", "high", "production",
              "development", "technology", "high", "low", "new", "research", "project"
              , "achieve", "order", "improve", "year", "sustainability", "aim"
              )

vocabulary <- vocabulary[ !(vocabulary$term %in% rm_terms), ]


tail(vocabulary,100)
vocabulary[ (nrow(vocabulary)-199):(nrow(vocabulary)-100),]
head(vocabulary,100)

#remove the collocations of doc count and term count == 1
#see comment above, why this might be ok
vocabulary <- vocabulary[ !(vocabulary$term_count == 1 & vocabulary$doc_count == 1), ]

tail(vocabulary, 30)

#remove collocations with doc count < 2 that include numbers _one _two, etc.
vocabulary <- vocabulary[ !(grepl(paste0("_", c("one", "two", "three", "four", "five", "six", "seven","eight", "nine"), collapse = "|")
            ,vocabulary$term, perl = T) & vocabulary$doc_count == 1), ]

highest_term_doc_prop <- round(max(vocabulary$doc_count)/nrow(docs_ids), d = 2)
highest_term_term_prop <- round(max(vocabulary$term_count)/nrow(docs_ids), d = 2)
plot(round(vocabulary$doc_count/nrow(docs_ids), d =2))
vocabulary_cc_subset <- vocabulary[grep("_", vocabulary$term),]


# create wordcloud --------------------------------------------------------
wordcloud::wordcloud(as.character(vocabulary$term),
                     as.numeric(vocabulary$doc_count),
                     #scale=c(4,.7),
                     max.words=200,
                     random.order=FALSE,
                     rot.per=0,
                     use.r.layout=FALSE,
                     colors= RColorBrewer::brewer.pal(8, "Dark2")
)

wordcloud::wordcloud(as.character(vocabulary_cc_subset$term),
                     as.numeric(vocabulary_cc_subset$doc_count),
                     #scale=c(4,.7),
                     max.words=200,
                     random.order=FALSE,
                     rot.per=0,
                     use.r.layout=FALSE,
                     colors= RColorBrewer::brewer.pal(8, "Dark2")
)



# transform iterator by cc_model, create vocabulary and create GLOVE model ----------------------------------
vocabulary_glove <- create_vocabulary(iterator_docs
                                      #,c(1L, 1L)
                                      #, sep_ngram ="_"
                                      #,stopwords = stopwords)
)
stopwords_extended <- c(
  #from packages
  tm::stopwords("SMART")
  ,tm::stopwords("en")
  ,tokenizers::stopwords("en")
  ,setdiff(lexicon::sw_buckley_salton, c("ill", "edu", "ignored", "immediate", "novel"))


  #from files
  ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_english_xpo6com.txt"))
  ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_english_ranknl.txt"))

  ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_academic.txt"))

  ,readLines(paste0("C:/Science/R/data/stopwords_en", "/stopwords_msu_jdowell_and_smart_wordsorg.txt"))

  #manual definition
  ,"approach", "methodolology", "paper", "publish", "publication", "abstract", "conclusion", "method"
  ,"original", "article", "issue", "study", "studied", "published"
  ,"study", "specifically", "suggest", "consideration", "consideration", "author"
  #, "relation", "relate"
  ,"issue", "article", "noabstract"
  , "contribution", "contribute"
  , "become"
  #, "estimate"
  ,"br", "br_br", "inf", "inf_inf",

  #followinng stopwords are known from invstigation of vocabulary for dtm creation
  c("demonstrate", "due", "year", "aim", "evaluate", "important", "require", "find", "case", "propose",
    "make", "include", "present", "base", "result",
    "increase",
    "develop",
    "not", #exclude the negation terms from words_keep (only reasonable for CC)
    "no",
    "none", "analyse", "aspect", "finally", "reveal", "emerge", "perspective", "finding",
    "highlight", "introduce", "suggest",
    "model", "methodology",
    "design", "discuss", "lead", "application", "analysis",
    "datum", "apply", "explore",
    ",investigate", "year", "play", "relate", "face", "work", "key", "basis", "set",
    "focus", "current", "conclude", "purpose", "respect",
    "establish", "review", "carry",
    "system", "tool", "determine", "main", "conduct", "significantly", "significant",
    "show", "examine", "exist", "compare", "identify", "investigate",
    "analyze", "term", "type", "address", "test", "result_show")


)

stopwords_extended <- setdiff(stopwords, c("least", "mine", "change", "changes", "whole", "old", "new") )

#remove comment lines and make unique
stopwords_extended <- stopwords[!grepl("^#", stopwords)] %>% #remove comments
{ unlist(stringi::stri_split_regex(., "\\s|\\W|\\t|\\n")) } %>% #get individual lines and terms
  unique() %>%
  .[. != ""] %>% #exclude empty lines
  setdiff(c(
    "no","not", "none","ignored","poorely", "shortcomings", "questionable", "lacking", "reject", "overlooks", "restricted",
    "contested", "controversial", "inadequacies", "inaccurate", "adequate", "abilities", "attempted", "break",
    "government", "governing", "agencies", "agency", "laboratory", "experiment",
    "governments", "strategy", "strategies", "innovative", "innovation",
    "association", "standard", "standardised", "discourse", "discourses",
    "biological", "ground", "biosynthetic", "framework", "criticisms", "embodies", "error", "exposition",
    "measurement", "constitutes", "critics", "emerged", "synthesis" ,"adopted", "assess", "criteron", "critiqued", "nature"
  ) #make sure that some important terms are still included
  )

vocabulary_glove <- vocabulary_glove[ !(vocabulary_glove$term %in% stopwords_extended), ]
vocabulary_glove <- vocabulary_glove[ !( nchar(vocabulary_glove$term) < 3 ), ]
vocabulary_glove <- vocabulary_glove[ !(vocabulary_glove$term_count < 2 & vocabulary_glove$doc_count < 2 ), ]

vectorizer <- vocab_vectorizer(vocabulary_glove)

skip_grams_window <- 5L #x words before and after
skip_grams_window_context <- "symmetric" #or "left" "right"
tcm <- create_tcm(iterator_docs
                  ,vectorizer
                  ,skip_grams_window = skip_grams_window
                  ,skip_grams_window_context = skip_grams_window_context
)

glove = GlobalVectors$new(word_vectors_size = 50
                          ,vocabulary = vocabulary_glove
                          ,x_max = 20
                          # ,learning_rate = 0.15 #AdaGrad will quickly adjust it to optimal
                          # ,alpha = 0.75
                          # ,lambda = 0.0 #for small corpus select small lambda e.g. 1e-5
                          # ,initial = NULL
)

wv_main = glove$fit_transform(tcm
                              ,n_iter = 10
                              ,convergence_tol = 0.01
                              ,n_check_convergence = 1L
                              ,n_threads = 3 #RcppParallel::defaultNumThreads()
)

wv_context = glove$components
word_vectors = wv_main + t(wv_context)

cos_sim_vectors2 <- function(v_add, v_substract = NULL, n = 10, word_vector_set) {
  v_name <- v_add[1]
  v <- t(colSums(word_vector_set[v_add, , drop = FALSE]))
  rownames(v) <- v_name
  if (!is.null(v_substract)) {
    v <- v -  t(colSums(word_vector_set[v_substract, , drop = FALSE]))
  }
  cos_sim <- sim2(x = word_vector_set
                  ,y = v
                  ,method = "cosine", norm = "l2")
  if (!is.null(n)) {
    sort(cos_sim[,1], decreasing = TRUE)[1:n]
  } else {
    sort(cos_sim[,1], decreasing = TRUE)
  }
}

rownames(word_vectors)

cos_sim_vectors2(c("sustainability", "saving"),
                 v_substract = "efficiency",
                 n = 40
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("energy", "save", "saving", "concrete" ,"measure", "sustainable"),
                 v_substract = c("strategy"),
                 n = 40
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("saving", "save", "efficiency", "efficient", "renewable"),
                 v_substract = NULL,
                 n = 20
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("sustainability"),
                 v_substract = NULL,
                 n = 20
                 , word_vector_set = word_vectors[!(rownames(word_vectors) %in% c("view", "order", "define")),])

cos_sim_vectors2(c("sustainability"),
                 v_substract = "environmental",
                 n = 20
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("sustainability"),
                 v_substract = c("economic"),
                 n = 20
                 , word_vector_set = word_vectors[!(rownames(word_vectors) %in% c("view", "order", "define")),])

cos_sim_vectors2(c("sustainability", "urban"),
                 v_substract = "environmental",
                 n = 20
                 , word_vector_set = word_vectors)
cos_sim_vectors2(c("sustainability"),
                 v_substract = "urban",
                 n = 20
                 , word_vector_set = word_vectors)


cos_sim_vectors2(c("sustainability"),
                 v_substract = c("economy"),
                 n = 20
                 , word_vector_set = word_vectors[!(rownames(word_vectors) %in% c("view", "order", "define")),])

cos_sim_vectors2(c("sustainability"),
                 v_substract = c("social"),
                 n = 20
                 , word_vector_set = word_vectors[!(rownames(word_vectors) %in% c("view", "order", "define")),])


cos_sim_vectors2(c("risk", "threat", "global", "international"),
                 v_substract = NULL,
                 n = 20
                 , word_vector_set = word_vectors)


cos_sim_vectors2(c("economy", "nuclear"),
                 v_substract = "renewable",
                 n = 20
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("economy"),
                 v_substract = NULL,
                 n = 20
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("economy"),
                 v_substract = "circular",
                 n = 20
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("economy", "circular"),
                 v_substract = NULL, #"circular",
                 n = 20
                 , word_vector_set = word_vectors)

cos_sim_vectors2(c("economy"),
                 v_substract = "efficiency",
                 n = 20
                 , word_vector_set = word_vectors)


cos_sim_vectors2(c("environmental"),
                 v_substract = c("economic", "social"),
                 n = 20, word_vector_set = word_vectors)

cos_sim_vectors2(c("environment"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)

cos_sim_vectors2(c("environment"),
                 v_substract = c("economy"),
                 n = 20, word_vector_set = word_vectors)
cos_sim_vectors2(c("social"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)
cos_sim_vectors2(c("economic"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)
cos_sim_vectors2(c("sustainable"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)

cos_sim_vectors2(c("nuclear"),
                 v_substract = c("fission"),
                 n = 20, word_vector_set = word_vectors)
cos_sim_vectors2(c("energy"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)
cos_sim_vectors2(c("energy"),
                 v_substract = c("future"),
                 n = 20, word_vector_set = word_vectors)

cos_sim_vectors2(c("nuclear", "power"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)
cos_sim_vectors2(c("nuclear", "power"),
                 v_substract = "future",
                 n = 20, word_vector_set = word_vectors)

cos_sim_vectors2(c("nuclear", "energy"),
                 v_substract = "renewable",
                 n = 20, word_vector_set = word_vectors)

cos_sim_vectors2(c("renewable", "energy"),
                 v_substract = "large",
                 n = 20, word_vector_set = word_vectors)


gsub("nuclear", "NUCELAAAAAAAAAAAAAAAAAAER", data[ which(grepl("renewable", data$Abstract) & grepl("nuclear", data$Abstract))[1:5],
                                                   Abstract], fixed = T)

cos_sim_vectors2(c("renewable"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)


cos_sim_vectors2(c("power", "plant"),
                 v_substract = NULL,
                 n = 20, word_vector_set = word_vectors)







# create vectorizer / dtm in different formats ----------------------------------
vectorizer <- vocab_vectorizer(vocabulary)
dtm <- create_dtm(iterator_docs_cc, vectorizer, type = "dgCMatrix")
dim(dtm)
# [1] 31548 27922
any(Matrix::rowSums(dtm) == 0)
# [1] FALSE
any(Matrix::colSums(dtm) == 0)
# [1] FALSE

if (any(Matrix::rowSums(dtm) == 0)) {

  deleterows <- which(Matrix::rowSums(dtm) == 0)
  warning("Docs with the following ids removed from dtm since these docs were empty after cleaning/pruning: "
          , paste(rownames(dtm)[deleterows], collapse = "; "))

  dtm <- dtm[-deleterows, ]
}


#dtm_l1_norm <- normalize(dtm, "l1")  #seldomly needed

# tfidf.model <- TfIdf$new(
#   smooth_idf = TRUE
#   ,norm = "l1" #norm = c("l1", "l2", "none"),
#   ,sublinear_tf = FALSE)
#
# dtm_tfidf <- tfidf.model$fit_transform(dtm)
#
# dtm_tfidf[1, which(dtm_tfidf[1, ] > 0)]
# dtm[1, which(dtm_tfidf[1, ] > 0)]

#tf <- colSums(dtm)
#tf <- tf[order(tf, decreasing = T)]
#summary(tf)

#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 2.00     3.00     6.00    35.92    16.00 16766.00

#tfidf <- colSums(dtm_tfidf)
#summary(tfidf)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.0594   0.4196   0.7990   2.7109   1.8841 323.9481

#tfidf[tfidf > 100]

# tfidf[grep("fukushi|chernob", names(tfidf))]

# fukushima_prefectur          chernobyl_accid  fukushima_nuclear_crisi            postfukushima
# 0.2785088                0.2506066                0.2822218                0.6903495
# fukushima_nuclear_disast  fukushima_nuclear_accid          fukushima_accid                chernobyl
# 0.5499494                0.8807019                1.0065241                1.2132796
# fukushima_disast        fukushima_daiichi                fukushima
# 0.9292127                1.5336532                3.3837732

# tfidf[grep("^sustain$", names(tfidf))]
# tfidf[grep("^energi$", names(tfidf))]
#
#
# tfidf[grep("fukushi|chernob", names(tfidf))]
#
# tfidf[grep("energi", names(tfidf))]
# min(tfidf[grep("energi", names(tfidf))])
#
#
# tfidf1stquant <- colnames(dtm)[which(tfidf < 0.4)]
# tfidf1stquant[ grepl("_", tfidf1stquant)]
# tfidf1stquant_unigrams <- tfidf1stquant[ !grepl("_", tfidf1stquant)]
#
# tf[1:50]
# head(tf[ !(tf %in% tfidf1stquant) ],50)
# head(tf[ !(tf %in% tfidf1stquant_unigrams) ],50)
#
#
#
#
#
# tfidf1stquant[grep("fukushi|chernob", tfidf1stquant_unigrams)]
# #0
# tfidf1stquant[grep("fukushi|chernob", tfidf1stquant_unigrams)]
#
#
#
# tfidflow <- colnames(dtm)[which(colSums(dtm_tfidf) < 2.5)]
# tfidflow <- tfidflow[ !grepl("_", tfidflow )]
# tfidflow
# tail(tfidflow,100)
#
# tfidflow[grep("energi|sustain", tfidflow)]
# tfidflow[grep("heat|power", tfidflow)]
#
# colnames(dtm)[which(colSums(dtm_tfidf) > 1000)]
#
# energycolumns <- colSums(dtm_tfidf[, grep("energi|heat|power", colnames(dtm_tfidf) )])
# energycolumns <- energycolumns[1:400]
# energycolumns[order(energycolumns, decreasing = F)]
#
# tffidfbelowmedian <- colSums(dtm_tfidf) < tfidfsmy["Median"]
# colSums(dtm_tfidf)[tffidfbelowmedian]


#pruning on basis of tfidf weighting?
#-> no, too much detailed information would be lost that might be used for quality checks, etc.


dtm_tripl <- slam::as.simple_triplet_matrix(dtm)
# save(dtm_tripl, file = paste0(dirres, "dtm_tripl.rda"))
# load(file = paste0(dirres, "dtm_tripl.rda"))


#save / load image -------------------------------------
# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# save.image(file = "dtm_created_before_LDA.RData")
#
# setwd("C:/Science/Publication/3_sustainable_energy/data/")
# load(file = "dtm_created_before_LDA.RData")
# for (l in libraries) library(l,character.only=TRUE,quietly=TRUE,verbose=FALSE)
library(textility)


# set LDA model parameters ------------------------------------------------
#code from
#https://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity
#ntopics <- 10 #to be optimized by perplexity cross validation

#for VEM and Gibbs
#alphaprior <- 0.1 #50/ntopics #50/k
deltaprior <- 0.1
niter <- 2000

convtol <- 0.001
burnin <-  100
thin <- 100

keep <- 0
save <- 0

nstart <- 1
best <- TRUE

seedpar <- c(42)
#seedpar <- c(0,1,42,911,2018)
set.seed(seedpar)


#how many models (folds) per candidate number of topics
candidate_k <- c(5) #candidates for number of topics #seq(from = 2, to = 15, by = 1)
ncores <- (parallel::detectCores(logical = TRUE) - 1) #parallel::detectCores(logical = TRUE)

#for perplexity cross validation
folds <- 3
n <- nrow(dtm_tripl)
splitfolds <- sample(1:folds, n, replace = TRUE)

# set parameters in LDA control lists -------------------------------------

control_LDA_Gibbs_topicmodels <- list(
  #alpha = alphaprior, #prior value for alpha in: theta = Dirichlet(alpha)
  #,estimate.alpha = FALSE #for fixing alpha value in whole model
  #,estimate.beta = TRUE #setting to FALSE for testing a known model/distribution
  delta = deltaprior #starting value for delta in: beta ~ Dirichlet(delta)

  #,verbose = 50
  #,prefix = tempfile()
  #,save = 0

  ,iter = niter
  ,burnin = burnin
  ,thin = thin

  ,keep = keep
  ,nstart = nstart
  ,best = best

  ,seed = seedpar

)


# check if document ids have correclty transported (i.e. correct exclusions) during processing ----------------------------------
#check if all docs have information about year
any(is.na(data$Year))
#[1] FALSE
unique(data$Year) #looks fine

#check if rownames really represent ordered ids
length(rownames(dtm))
all(rownames(dtm) == as.character(1:nrow(dtm)))
#FALSE
rownames(dtm)[240:260]
all(sort(as.integer(rownames(dtm)), decreasing = F) == as.integer(rownames(dtm)))
#TRUE

#check if this corresponds to data for docs
all(as.integer(rownames(dtm)) == as.integer(data[id %in% rownames(dtm),id]))
#TRUE


# find LDA model with optimum number of topics --------------------------------------------------------
#initial guess of topics via number of author keywords
test <- data[, Author.Keywords]
Encoding(test) <- "UTF-8"

keywords <- stri_split_fixed(test, ";") %>%
  unlist() %>%
  stri_split_fixed(" ") %>%
  unlist() %>%
  preprocessing_function() %>%
  tokenizers::tokenize_word_stems() %>%
  unlist() %>%
  table()  %>%
  { .[ . > 1] } %>%
  { .[-which(names(.) %in% stopwords)] }


test <- keywords[grep("?|?", keywords)]
Encoding(test) <- "UTF-8"
test2 <- data[ Abstract %like% "Kolmogorov", Abstract]
test3 <- data[ Author.Keywords %like% "Kolmogorov|???|?", Author.Keywords]
preprocessing_function(test3)

#assuming that the top 10 words are relevant an intial estimate of k can be made as
k_intial_guess <- round(length(keywords)/10, d = 0)
k_intial_guess


# fit LDA models for varying k -----------------------------------------------
#"standard" LDA with topicmodels package
start <- proc.time()
fit_LDA_Gibbs_vary_k_parallel(dtm_tripl = dtm_tripl
                              ,candidate_k = 2
                              ,ctrl_LDA_Gibbs = control_LDA_Gibbs_topicmodels
                              ,ncores = ncores
                              ,model_dir = dirmod
                              )
elapsed <- proc.time()-start
#about 9 minutes for 5 topics
#about 5 minutes for 2 topics



#structured topic model with years as covariates via stm package
#dtm <- tripl_to_sparse(dtm_tripl)
dtm_stm <- sparse_to_stm(dtm)
#names(dtm_stm$documents) <- NULL

str(dtm_stm) #looks fine
start <- proc.time()
fitted_stm <- stm(documents = dtm_stm$documents
                  , vocab =  dtm_stm$vocab
                  , K = 3
                  , prevalence=~Year
                  #,content = NULL
                  , data=as.data.frame(data[id %in% rownames(dtm),"Year"])
                  #spectral initialization would be interesting but uses too much memory...
                  , init.type = "Spectral" #c("Spectral", "LDA", "Random", "Custom")
                  , seed = seedpar
                  , max.em.its = 4
                  , emtol = 1e-05
                  , verbose = TRUE
                  , reportevery = 2
                  , LDAbeta = TRUE #when set to FALSe -> SAGE style topics
                  , interactions = TRUE
                  , ngroups = 1
                  , model = NULL
                  , gamma.prior = "Pooled" #c("Pooled", "L1") #for prevalence covariates
                  , sigma.prior = 0 #strength of ("correlatedness", .i.e ->) regularization towards diagonlized cov matrix
                  , kappa.prior = "L1" #c("L1", "Jeffreys") #for content covariates
                  , control = list(#alpha = 50/K #alphaprior
                                    # burnin = 1,#burnin,
                                     #nits = 1#50 #iterations for initilization
                                    # ,eta = deltaprior
                                    maxV = 3
                                     #,
                                     )
                  )
# fit_LDA_Gibbs_vary_k_parallel(dtm_tripl = dtm_tripl
#                               ,candidate_k = 2
#                               ,ctrl_LDA_Gibbs = control_LDA_Gibbs_topicmodels
#                               ,ncores = ncores
#                               ,model_dir = dirmod
# )
elapsed <- proc.time()-start


fitted_stm2 <- stm(documents = dtm_stm$documents
                  , vocab =  dtm_stm$vocab
                  , K = 2
                  , prevalence=~Year
                  #,content = NULL
                  , data=as.data.frame(data[id %in% rownames(dtm),"Year"])
                  #spectral initialization would be interesting but uses too much memory...
                  , init.type = "LDA" #("Spectral", "LDA", "Random", "Custom")
                  , seed = seedpar
                  , max.em.its = 4#500
                  , emtol = 1e-05
                  , verbose = TRUE#FALSE
                  , reportevery = 1#5
                  , LDAbeta = TRUE #when set to FALSe -> SAGE style topics
                  , interactions = TRUE
                  , ngroups = 1
                  , model = NULL
                  , gamma.prior = "Pooled" #c("Pooled", "L1") #for prevalence covariates
                  , sigma.prior = 0 #strength of ("correlatedness", .i.e ->) regularization towards diagonlized cov matrix
                  , kappa.prior = "L1" #c("L1", "Jeffreys") #for content covariates
                  , control = list(#alpha = 50/K #alphaprior
                    # burnin = 1,#burnin,
                    #nits = 1#50 #iterations for initilization
                    # ,eta = deltaprior
                    maxV = 3000
                    #,
                  )
)




fit_stm_vary_k_parallel(dtm_stm = dtm_stm
                                          ,data = as.data.frame(data[id %in% rownames(dtm),"Year"])
                                          , candidate_k = c(2,300)
                                         # , ctrl_LDA_Gibbs
                                          , ncores = 2
                                          , model_dir = dirmod
                        )


fit_stm_vary_k_parallel <-       function(dtm_stm
                                          ,data
                                                 , candidate_k
                                                # , ctrl_LDA_Gibbs
                                                 , ncores
                                                 , model_dir
) {
  method <-  "stm_CTM"
  #ctrl_meth_list <- ctrl_LDA_Gibbs

  cluster <- makeCluster(ncores)
  registerDoParallel(cluster)

  # load up the needed R package on all the parallel sessions
  clusterEvalQ(cluster, {
    library(stm)
  })

  # export all the needed R objects to the parallel sessions
  clusterExport(cluster, list( ls(environment()) ),  envir = environment())
  # we parallelize by the different number of topics.  A processor is allocated a value
  # of k, and does the cross-validation serially.  This is because it is assumed there
  # are more candidate values of k than there are cross-validation folds, hence it
  # will be more efficient to parallelise

  result_data <- foreach(j = 1:length(candidate_k)
                        # , .export= c("tripl_to_sparse")
                         ) %dopar% {

    k <- candidate_k[j]
    start_time <- as.numeric(proc.time()[3])

    filename <- paste0(model_dir, "k", k, "_", method, "_model.rda")


    fitted  <- stm(documents = dtm_stm$documents
                                      , vocab =  dtm_stm$vocab
                                      , K = k
                                      , prevalence=~Year
                                      #,content = NULL
                                      , data = data
                                      #spectral initialization would be interesting but uses too much memory...
                                      , init.type = "Spectral" #c("Spectral", "LDA", "Random", "Custom")
                                      , seed = 42 #seedpar
                                      , max.em.its = 2000
                                      , emtol = 1e-05
                                      , verbose = FALSE
                                      , reportevery = 500
                                      , LDAbeta = TRUE #when set to FALSe -> SAGE style topics
                                      , interactions = TRUE
                                      , ngroups = 1
                                      , model = NULL
                                      , gamma.prior = "Pooled" #c("Pooled", "L1") #for prevalence covariates
                                      , sigma.prior = 0 #strength of ("correlatedness", .i.e ->) regularization towards diagonlized cov matrix
                                      , kappa.prior = "L1" #c("L1", "Jeffreys") #for content covariates
                                      , control = list(#alpha = 50/K #alphaprior
                                        # burnin = 1,#burnin,
                                        #nits = 1#50 #iterations for initilization
                                        # ,eta = deltaprior
                                        maxV = 300
                                        #,
                                      )
    )





    filename <- gsub("_model.rda$"
                     ,paste0("_hours", as.character(round((as.numeric(proc.time()[3])-start_time)/(60*60), d = 0))
                             , "_model.rda" )
                     ,filename
    )
    save(fitted, file = filename)
    #}
  }
  stopCluster(cluster)
}



# investigate models ------------------------------------------------------

modelfiles <- paste0(gsub("models/", "models2/", dirmod), list.files(gsub("models/", "models2/", dirmod)
                                        , pattern = "k\\d+.+model\\.rda"))
#order by number of k
modelfiles <- modelfiles[order(as.numeric(unlist(stri_extract_all_regex(modelfiles, "(?<=/k)\\d+(?=_)")))
                               , decreasing = F)]

#load(modelfiles[3])
# models <- lapply(modelfiles, function(x) {
#                load(x)
#                fitted
# })
# names(models) <- gsub(paste(dirmod, "_hours.*$", sep = "|"), "", modelfiles, perl = T)

check_model_quality <-                function(  modelfiles
                                                , dtm_tripl
                                                , ncores
                                                , n_topterms_per_topic = 20
) {
  cluster <- makeCluster(ncores)
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
    library(Matrix)
    library(data.table)
    library(textility)
  })
  parallel::clusterExport(cluster,list( ls(environment()) ),  envir = environment())

  result_data <- foreach(j = 1:length(modelfiles)
                        # ,.export= c("function_name")
                         ,.combine = function(...) rbindlist(list(...))
                         ,.multicombine= TRUE
                         ) %dopar% {
    load(modelfiles[j])
    beta <- fitted@beta
    terms <- fitted@terms
    colnames(beta) <- terms
    if (any(!(colnames(dtm_tripl) %in% terms))) {
          stop("Terms of term/topic distribution (model ouput) not identical to terms in document term matrix (model input).")
    }
    #check if desired number of top terms is available in data
    if (any(n_topterms_per_topic > ncol(beta))) {
      n_topterms_per_topic_excluded <- n_topterms_per_topic[(n_topterms_per_topic > ncol(beta))]
      n_topterms_per_topic <- c(n_topterms_per_topic[ !(n_topterms_per_topic >= ncol(beta)) ]
                                , ncol(beta))
      warning(paste0("For k=", k, " terms per topic were less than desired set values. Maximum possible number, i.e., colnames(dtm) was used as upper limit."))
    }



        #get quality measures
model_quality <- data.table(
                  modeltype = class(fitted)
                  ,ntopics = fitted@k
                  ,ntopterms = n_topterms_per_topic
                  ,loglik =  fitted@loglikelihood
                  )


top_term_mat <- make_top_term_matrix(beta = beta, n = n_topterms_per_topic, terms = terms)
tcm_top_terms <- get_cooccurrence(dtm_tripl[,(unique(as.vector(top_term_mat)))])

model_quality <- cbind(model_quality, calc_coherence( tcm = tcm_top_terms
                , top_term_matrix = top_term_mat
                , average_over_topics = TRUE
                #, log_smooth_constant = .01 #default = smaller smoothing constant in paper by Röder #1 would be UMass, #.01 stm package
                , ndocs_tcm = nrow(dtm_tripl))
                )




    return(model_quality)
  }
  stopCluster(cluster)
  return(result_data)
}


model_quality <- do.call(rbindlist, lapply(c(10
                                             #,25
                                             #,50
                                             #,75
                                             ), function(n) {

  check_model_quality( modelfiles = modelfiles
                       , dtm_tripl = dtm_tripl
                       , ncores = parallel::detectCores()
                       , n_topterms_per_topic = n
                        )

}))

model_quality  <- check_model_quality( modelfiles = modelfiles
                                       , dtm_tripl = dtm_tripl
                                       , ncores = parallel::detectCores()
                                       , n_topterms_per_topic = c(10,25,50,75))


#exclusivity does not seem to be a good measure
#values are very close to each other, independent of the number of top words chosen
# exclusivities <- sapply(models, function(x) {
#   stm_adapt_exclusivity(x@beta, 10)
# }, USE.NAMES = T)
# sapply(exclusivities, mean)


normalize_range <- function(x){(x-min(x))/(max(x)-min(x))}



model_quality_normalized <- copy(model_quality)
cols <- names(model_quality_normalized)[3:ncol(model_quality_normalized)]
model_quality_normalized[ , (cols) := lapply(.SD, normalize_range), .SDcols = cols]

plot_data <- data.frame(model_quality_normalized[,2:ncol(model_quality_normalized)])
plot_data <- melt(plot_data, id = "ntopics")
plot_data$variable <- as.factor(plot_data$variable)

  ggplot(plot_data, aes(ntopics, value,  colour = variable)) +
     geom_line() #+

     #theme_bw()


     labs(x = "Number of topics", y = "Logarithmic likelihood")



geom_point(data = result_data, aes(x = k, y = coherence), colour = factor(rep(10, length(coherence))))


fuku <- apply(terms(m3, m3@Dim[2]), 2, function(x) {

  grep("nuclear_accident|nucleat_disaster", x)[1]

})

topic_numbers <- fuku[which(fuku == min(fuku))]

terms(m3, 100)[, "Topic 121"]

fuku_topic <- apply(topics(m3, m3@k), 2, function(x) {

  min(grep("^121$", x, perl = T))

})

which(fuku_topic == min(fuku_topic))


topics(m3, 2)[1:2, 1:5]
topics(m3, 2)[1:2, c("3250", "4776")]

data[ id %in% c("3250", "4776"), Abstract]

test <- topics(m3, m3@k)
class(test)
mode(test)

grep("^121$", c(121, 5343121, 31213), perl = T)


min(as.vector(fuku_topic))


topics(m3, 5)[1:4, 1:5]

oil_cris <- apply(terms(m1, m1@Dim), 2, function(x) {

  grep("oil_cris", x)[1:2]

})



fuku <- apply(terms(m1, 1000), 2, function(x) {

  grep("fukushim|chernob", x)[1:2]

})

terms(m1, 410)[, c("Topic 93") ]

terms(m1, 1000)[c(1:10, 600:610,  970:1000), c("Topic 5", "Topic 6") ]

terms(m2, 15)




# check results of model validation ---------------------------------------






# choose number of topics -------------------------------------------------
k_selected <- 15
#k_selected <- 500



# run LDA with selected (optimum) number of topics ------------------------
fit_topic_models <- function( dtm_tripl
                              ,k_selected
                              ,ctrl_meth_list) {

                  #method has to be passed as character, which input is character?
                  meth_idx <- which(unlist(lapply(ctrl_meth_list, class)) == "character")
                  method <-  ctrl_meth_list[[meth_idx]]
                  ctrl_meth_list <- ctrl_meth_list[[setdiff(1:2, meth_idx)]]
                  rm(meth_idx)

                   if (any(grepl("^cg$", names(ctrl_meth_list)))) {

                     CTM(dtm_tripl,k = k_selected, method = method, control = ctrl_meth_list)

                      } else {

                       LDA(dtm_tripl, k = k_selected ,method = method, control = ctrl_meth_list)

                      }
}



fitted_topic_models <- lapply(modellist, function(m) {

  fit_topic_models(dtm_tripl = dtm_tripl
                   ,k_selected = k_selected
                   ,ctrl_meth_list = m)


})


# analyze quality of LDA results -----------------------------------------------------

#Higher values indicate that the topic distributions are more evenly spread over the topics.
sapply(fitted_topic_models, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))), USE.NAMES = T)



plot_data <- data.frame(posterior = unlist(lapply(fitted_topic_models, function(x) apply(posterior(x)$topics, 1, max))),
                        model = factor(rep(names(fitted_topic_models),
                                           each = nrow(posterior(fitted_topic_models[[1]])$topics)))
                        )

plot_counts_assignment_most_likely_topic <- ggplot(data=plot_data) +
                                             geom_histogram( aes(posterior, group = model), bins = 50) +
                                             facet_wrap(~names(fitted_topic_models))





topics(fitted_topic_models[[1]], k_selected)
topics(fitted_topic_models[[2]], k_selected)
topics(fitted_topic_models[[3]], k_selected)
terms(fitted_topic_models[[1]], 10)
terms(fitted_topic_models[[2]], 10)
terms(fitted_topic_models[[3]], 10)

sort_by_lambda(fitted_topic_models[[3]], lambda = 0.3, number_terms = 10, reorder.topics = TRUE, vocab = vocabulary)


topicmodels_json_ldavis(ldamodel = fitted_topic_models[[1]]
                        ,dtm_dgCT = dtm_tripl
                        ,vocabulary_t2v = vocabulary
                        ,R = 30
                        ,lambda.step = 0.01
                         #,mds.method = jsPCA
                         #,cluster
                         ,plot.opts = list(xlab = "PC1", ylab = "PC2")
)




model_used <- fitted_topic_models[[1]]
model_used <- fitted
topic_names_topterms <- apply(terms(model_used, 5), 2, function(x) {
                                  paste(x, collapse = "#")
                              })
#
# topic_names_manual <- c(t1 = "energy"
#                         ,t2 = "coal power"
#                         )

# topic network -----------------------------------------------------------
library("igraph")

fitted

#http://kateto.net/network-visualization
#https://github.com/trinker/topicmodels_learning

post <- topicmodels::posterior(fitted)

cor_mat <- cor(t(post[["terms"]]))
cor_mat[ cor_mat < .05 ] <- 0
diag(cor_mat) <- 0

graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="lower")
graph <- delete.edges(graph, E(graph)[ weight < 0.05])

E(graph)$edge.width <- E(graph)$weight*20
V(graph)$label <- paste("Topic", V(graph))
V(graph)$size <- colSums(post[["topics"]]) * 15

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(graph, edge.width = E(graph)$edge.width,
            edge.color = "orange", vertex.color = "orange",
            vertex.frame.color = NA, vertex.label.color = "grey30")
title("Strength Between Topics Based On Word Probabilities", cex.main=.8)



topic_mat <- topicmodels::posterior(fitted)[["topics"]]
topic_mat <- cbind(T1 = c(0.1,0.2,0.3,0.4)
                   ,T2 = c(0.1,0.3,0.2,0.4)
                  ,T3 = c(0.4,0.1,0.2,0.3)
                   ,T4 = c(0.1,0.2,0.3,0.4))
rownames(topic_mat) <- c("these", "are", "some", "words")
topic_mat <- t(topic_mat) %*% topic_mat
topic_mat <- topic_mat *100

topic_mat <- topicmodels::posterior(fitted)[["topics"]]
topic_mat <- t(topic_mat) %*% topic_mat
topic_mat <- topic_mat *100

graph <- graph_from_adjacency_matrix(topic_mat, mode = "undirected", diag = FALSE, weighted = T)

E(graph)$edge.width <- ifelse(E(graph)$weight > 29, 10, ifelse(E(graph)$weight < 24,1,5))

plot.igraph(graph, edge.width = E(graph)$edge.width)

plot.igraph(graph)

plot(graph, vertex.shape="none", #vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

l <- layout_in_circle(graph)
plot(graph, layout=l)

l <- layout_with_fr(graph)


plot(graph, vertex.shape="none", #vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85"
     ,layout=l)

plot(graph, vertex.shape="none", #vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85"
     ,layout=layout_with_mds)

plot(graph, vertex.shape="none", #vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85"
     ,layout=layout_nicely)


hist(E(graph)$weight)
mean(E(graph)$weight)
sd(E(graph)$weight)


cut.off <- mean(E(graph)$weight)
graph.cut <- delete_edges(graph, E(graph)[weight<cut.off])

plot(graph.cut, vertex.shape="none", #vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85"
     ,layout=layout_nicely)





##+ principal component analysis ---------------------
dtm.pca <- fitted@gamma
str(dtm.pca)
colnames(dtm.pca) <- paste0("T", 1:300) #topic_names
rownames(dtm.pca) <- paste0("d", 1:nrow(dtm.pca)) #ids

#scaled threshold
#threshold <- mean(apply(dtm.pca,2, mean))/(100/75)
#dtm.pca <- apply(dtm.pca,2, function(x) ifelse(x>=threshold,x,0))

#if principal component produces error, the columns that cannot be scaled
#are removed (error e.g. for word frequencies larger than one, but  only in 1 document)
#dtm.pca <- dtm.pca[,apply(dtm.pca, 2, var, na.rm=TRUE) != 0]


#model.pca.original <- model.pca

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

model.pca <- prcomp(dtm.pca,  scale = F) #scaling not needed, since all content has same dimension

# model.pca <- FactoMineR::PCA(as.data.frame(as.matrix(dtm.pca), stringsAsFactors = F)
#                  , scale.unit = FALSE
#                  , ncp = 5, graph = FALSE)

factoextra::fviz_screeplot(model.pca, addlabels=T)
factoextra::fviz_cos2(model.pca, choice = "var", axes = 1:2)

# factoextra::fviz_pca_var(model.pca
#              ,axes = c(1,2)
#              ,geom = c("text")
#              ,labelsize = 2
#              # ,repel = TRUE
#              # ,addEllipses = TRUE
#              ,col.ind = 1
#              # ,col.var = rgb(0, 0, 0, alpha=0, maxColorValue = 255)
#              #,alpha.ind = "coord" # number, "cos2", "contrib", "coord", "x" or "y"
#              # ,alpha.var = .3
#              #,select.ind = list(name = NULL, cos2 = NULL, contrib = NULL)
#              #,select.var = list(name = NULL, cos2 = .05, contrib = NULL)
#              #,invisible = c("quali")
#              ,var.axes=TRUE
# )

#http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_pca.html
factoextra::fviz_pca_biplot(model.pca
                ,axes = c(1,2)
              # ,geom = c("point", "text")
                ,geom.ind = c("point", "text")
                ,geom.var = c("point", "text")
               # ,label = c("var", "ind")
                ,repel = TRUE
               # ,habillage = anygroups
                 #,addEllipses = TRUE
                ,col.ind = 1
                ,col.var = 4 #rgb(0, 0, 0, alpha=0, maxColorValue = 255)
                #,alpha.ind = "coord" # number, "cos2", "contrib", "coord", "x" or "y"
                #,alpha.var = .3
                #,select.ind = list(name = NULL, cos2 = NULL, contrib = NULL)
                #,select.var = list(name = NULL, cos2 = .05, contrib = NULL)
                #,invisible = c("quali")
                ,var.axes=TRUE
)


#model.pca$rotation <- jitter(model.pca$rotation, amount = 0.01 )
#
# if (jitteramount == FALSE) {
#
#   #do nothing
# } else {
#
#   model.pca$rotation <- jitter(model.pca$rotation, amount = 0.01 )
# }


# model.pca$rotation <- apply(model.pca$rotation,2, function(x) {
#
#
#   items <-  nrow(model.pca$rotation)
#   amount <- (max(x)-min(x))*0.5
#   m <- amount/(2*items)
#   add <- m*(1:items)
#   first
#   x <- x+(add*sign(x))
#
#   return(x)
#
# })


# opar <- par()
# par(mar = rep(2, 4))
# main.label <- paste("sel_word:", sw,
#                     "; sel_range: ", paste(offset_selected_sent, collapse=","),
#                     "; min_word_len: ", token_exclude_length,
#                     "ngram: ",
#                     "term_count_min: ","term_count_min = 10", #minimum number of occurences over all documents.
#                     "\n",
#                     #,term_count_max = 60 #maximum number of occurences over all documents.
#                     "doc_prop_max: ","doc_proportion_max = 0.5" ,
#                     "doc_prop_min: ","doc_proportion_min = 2/length(docs)", #in at least in two documents
#                     #,max_number_of_terms = 200 #maximum number
#                     sep= " ")


pdf(paste0("plot_", gsub("\\W", "", sw), "_plotvariance.pdf")
    #, height=scale_figure[1]
    #,width=scale_figure[2]
)
plot(model.pca
     ,npcs = length(model.pca$sdev)
     ,main = "")

dev.off()




# title( paste("number of principal components: ", length(model.pca$sdev)),
#        cex.main = .8,
#        font.main = 1)
#
# par(opar)

###implement autmatic application of elbow method.... <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TBD

n.elbow <- 4

#facet wrap plot with 3 dimensions
plot(data.frame(model.pca$x[,1:n.elbow]), pch=16, col=rgb(0,0,0,0.5))
title( main.label,
       cex.main = .8,
       font.main = 1)


pdf(paste0("plot_", gsub("\\W", "", sw), "_plotpcascatter.pdf")
    #, height=scale_figure[1]
    #,width=scale_figure[2]
)
plot(data.frame(model.pca$x[,1:n.elbow]), pch=16, col=rgb(0,0,0,0.5))

dev.off()


op <- par()$mar
par(mar = rep(2, 4))


str(model.pca)
plot.pcabiplot <- biplot(model.pca,
                         choices = c(1,2),
                         col = c(0,4),
                         cex = c(0.5,0.5)
                         ,var.axes = F

                         #,expand=1.2
)

plot(model.pca$rotation, type="n"
     # , xlim = c(-1,1)
     # , ylim = c(-1,1)
     ,asp = 1
)

text(model.pca$rotation, rownames(model.pca$rotation), cex=0.8, col=(1:500))

max(model.pca$x[,"PC2"])



pdf(paste0("plot_", gsub("\\W", "", sw), "_plotpcabiplot.pdf")
    #, height=scale_figure[1]
    #,width=scale_figure[2]
)



biplot(model.pca,
       choices = c(1,2),
       col = c(10,4),
       cex = c(0.5,0.5),
       var.axes = F,
       expand=1.2)

par(mar = op)
dev.off()





#estimate number of clusters
library("cluster")

model.agnes <- agnes(as.matrix(dtm) ,method = "ward")

# Compute the gap statistic
gap_stat <- clusGap(as.matrix(dtm.tfidf), FUN = kmeans, nstart = 25,
                    K.max = 12, B = 500)
# Plot the result
library(factoextra)
fviz_gap_stat(gap_stat)

km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df,  geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())
##~ principal component analysis ---------------------

#} #end loop






