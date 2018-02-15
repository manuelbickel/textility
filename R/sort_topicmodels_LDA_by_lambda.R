

#' Sort topics of an lda model from topicmodels by lambda
#'
#' This function is largely a copy of the code parts from the LDAvis package for the function createJSON():
#' https://github.com/cpsievert/LDAvis/blob/master/R/createJSON
#' However, it was customized to accept LDA from from topicmodels package.
#'
#' Documentation still to be done...
#'
#' @param ldamodel
#' @param lambda
#' @param number_terms
#' @param reorder.topics
#' @param vocab
#'
#' @return
#' @export
#'
#' @examples

sort_topicmodels_LDA_by_lambda <- function(ldamodel, lambda = 1, number_terms = 10, reorder.topics = TRUE, vocab = vocabulary) {

#this function is largely a copy of the code parts from the LDAvis package for the function createJSON()
#https://github.com/cpsievert/LDAvis/blob/master/R/createJSON.R

#Some adaptions have been aplied to directly feed the output of ldamodels of the topicmodels package into the function
#The ouput is not a JSON file, but one of the interim outputs within the original createJSON(), i.e.,
#The raw wordlists for the topics sorted by lambda
#The purpose of this kind of less interactive output is to have a possibility to directly process / compare wordlists in R

if (attributes(class(ldamodel))$package != "topicmodels") {

  stop("Function has only been implemented for ldamodels from the package topicmodels.
       Please provide a model created with this package.
       If you use the package text2vec, you can directly use the built-in functions of that package
       to display terms sorted by lambda.")

}



#get corresponding dtm matrix object from call to create ldamodel
docterm <- gsub("(^.*\\()(x = )([^,]+)(,)(.*$)", "\\3", paste(deparse(ldamodel@call), collapse=""), perl=TRUE)

if (class(eval(parse(text = docterm))) == "simple_triplet_matrix") {

  dtm <- eval(parse(text = docterm))

  dtm <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                       dims=c(dtm$nrow, dtm$ncol),
                       dimnames = list(rownames(dtm), colnames(dtm))

                       ,giveCsparse = FALSE
                       )



} else {

dtm <- eval(parse(text = docterm))

}


# calculate initial summary statistics ------------------------------------
phi <- as.matrix(topicmodels::posterior(ldamodel)$terms)
theta <- as.matrix(topicmodels::posterior(ldamodel)$topics)


doc.length <- Matrix::rowSums(dtm)
names(doc.length) <- rownames(dtm)

term.frequency <- vocab$term_count
names(term.frequency) <- vocab$term

dp <- dim(phi)  # should be K x W
dt <- dim(theta)  # should be D x K

N <- sum(doc.length)  # number of tokens in the data
W <- nrow(vocab)  # number of terms in the vocab
D <- length(doc.length)  # number of documents in the data
K <- dt[2] # number of topics in the model


# check that certain input dimensions match
if (dp[1] != K) stop("Number of rows of phi does not match
                     number of columns of theta; both should be equal to the number of topics
                     in the model.")
if (D != dt[1]) stop("Length of doc.length not equal
                     to the number of rows in theta; both should be equal to the number of
                     documents in the data.")
if (dp[2] != W) stop("Number of terms in vocabulary does
                     not match the number of columns of phi (where each row of phi is a
                     probability distribution of terms for a given topic).")
if (length(term.frequency) != W) stop("Length of term.frequency
                                      not equal to the number of terms in the vocabulary.")
if (any(nchar(vocab) == 0)) stop("One or more terms in the vocabulary
                                 has zero characters -- all terms must have at least one character.")


phi.test <- all.equal(rowSums(phi), rep(1, K), check.attributes = FALSE)
theta.test <- all.equal(rowSums(theta), rep(1, dt[1]),
                        check.attributes = FALSE)
if (!isTRUE(phi.test)) stop("Rows of phi don't all sum to 1.")
if (!isTRUE(theta.test)) stop("Rows of theta don't all sum to 1.")

# compute counts of tokens across K topics (length-K vector):
# (this determines the areas of the default topic circles when no term is
# highlighted)
topic.frequency <- colSums(theta * doc.length)
topic.proportion <- topic.frequency/sum(topic.frequency)


# reorder topics by frequency ---------------------------------------------
# re-order the K topics in order of decreasing proportion:
if(reorder.topics) {

  o <- order(topic.proportion, decreasing = TRUE)
} else  {
  o <- seq_along(topic.proportion)
}

phi <- phi[o, ]
theta <- theta[, o]
topic.frequency <- topic.frequency[o]
topic.proportion <- topic.proportion[o]



# individual term counts per topic ---------------------------------------------------------
# token counts for each term-topic combination (widths of red bars)
term.topic.frequency <- phi * topic.frequency

# compute term frequencies as column sums of term.topic.frequency
# we actually won't use the user-supplied term.frequency vector.
# the term frequencies won't match the user-supplied frequencies exactly
# this is a work-around to solve the bug described in Issue #32 on github:
# https://github.com/cpsievert/LDAvis/issues/32
term.frequency <- colSums(term.topic.frequency)
stopifnot(all(term.frequency > 0))

# marginal distribution over terms (width of blue bars)
term.proportion.marginal <- term.frequency/sum(term.frequency)

# Old code to adjust term frequencies. Deprecated for now
# adjust to match term frequencies exactly (get rid of rounding error)
#err <- as.numeric(term.frequency/colSums(term.topic.frequency))
# http://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
#term.topic.frequency <- sweep(term.topic.frequency, MARGIN=2, err, `*`)


# distinctiveness and saliency of the terms -------------------------------
# Most operations on phi after this point are across topics
# R has better facilities for column-wise operations
phi <- t(phi)

# compute the distinctiveness and saliency of the terms:
# this determines the R terms that are displayed when no topic is selected
topic.given.term <- phi/rowSums(phi)  # (W x K)
#comment MWB for learning: weight of a term across topics not only within one topic
# which term has high proportion of representativeness for a certain topic in comparison across topics

#comment MWB for learning: divide each term by the proportion of its topic
#e.g., for topic.given.terms, some terms that are only important in one topic were upweighted
#if the topic is not very important their weight is adjusted again and downwieghted by importance of topic
#the logarithm of this result is used for scaling
kernel <- topic.given.term * log(sweep(topic.given.term, MARGIN=2,
                                       topic.proportion, `/`))
distinctiveness <- rowSums(kernel)
saliency <- term.proportion.marginal * distinctiveness

R <- number_terms
#R <-  nrow(vocab) #number of terms to be displayed

# Order the terms for the "default" view by decreasing saliency:
default.terms <- vocab$term[order(saliency, decreasing = TRUE)][1:R]
counts <- as.integer(term.frequency[match(default.terms, vocab$term)])
Rs <- rev(seq_len(R))
default <- data.frame(Term = default.terms, logprob = Rs, loglift = Rs,
                      Freq = counts, Total = counts, Category = "Default",
                      stringsAsFactors = FALSE)
topic_seq <- rep(seq_len(K), each = R)
category <- paste0("Topic", topic_seq)
lift <- phi/term.proportion.marginal

# Collect R most relevant terms for each topic/lambda combination
# Note that relevance is re-computed in the browser, so we only need
# to send each possible term/topic combination to the browser
find_relevance <- function(i) {
  relevance <- i*log(phi) + (1 - i)*log(lift)
  idx <- apply(relevance, 2,
               function(x) order(x, decreasing = TRUE)[seq_len(R)])
  # for matrices, we pick out elements by their row/column index
  indices <- cbind(c(idx), topic_seq)
  data.frame(Term = vocab$term[idx], Category = category,
             logprob = round(log(phi[indices]), 4),
             loglift = round(log(lift[indices]), 4),
             stringsAsFactors = FALSE)
}

  find_relevance(lambda) %>%
    .[c("Term", "Category")]  %>%
    split(.$Category) %>%
    lapply(`[[`, "Term") %>%
    do.call(rbind, .) %>%
    t()

  #from here on the original createJSON() would proceed with the part on building tinfo...
}



