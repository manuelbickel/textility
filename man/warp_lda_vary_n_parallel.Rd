% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/warp_lda_vary_n_parallel.R
\name{warp_lda_vary_n_parallel}
\alias{warp_lda_vary_n_parallel}
\title{Fit Warp LDA models for varying n in parallel}
\usage{
warp_lda_vary_n_parallel(dtm, n_topics, n_cores, model_dir,
  doc_topic_prior = sapply(as.character(n_topics), function(x) NA_real_,
  USE.NAMES = T), topic_word_prior = sapply(as.character(n_topics),
  function(x) NA_real_, USE.NAMES = T), convtol = 0.001, n_iter = 2000,
  seed = 42)
}
\arguments{
\item{dtm}{The document term matrix to be used in LDA.}

\item{n_topics}{\code{Integer} vector containing the candidate number of topics, e.g., \code{seq(5, 500, 5)}.}

\item{n_cores}{Number of cores to be used.
If computer should be usable during model fitting leave on processor spare via \code{parallel::detectCores()-1}.}

\item{model_dir}{The directory to save the fitted models.
The directory needs to end with "/", e.g., "~/mydir/" (currently no check is implemented).}

\item{doc_topic_prior}{To be passed as a named vector. The prior parameters passed to \code{doc_topic_prior} in \code{text2vec::LDA}.
By default the vector sapply(as.character(n_topics), function(x) NA_real_, USE.NAMES = T)
is used and filled with the values \code{50/n} if the entry in the respective parallel run is \code{NA_real_}.
n is taken from n_topics of the respective parallel run.}

\item{topic_word_prior}{To be passed as a named vector. The prior parameter passed to \code{topic_word_prior} in \code{text2vec::LDA}.
By default the vector sapply(as.character(n_topics), function(x) NA_real_, USE.NAMES = T)
is used and filled with the values \code{1/n} if the entry in the respective parallel run is \code{NA_real_}.
n is taken from n_topics of the respective parallel run.}

\item{convtol}{The convergence tolerance parameter passed to \code{text2vec::LDA}. By default \code{1e-3}.}

\item{n_iter}{The number of iterations parameter passed to \code{text2vec::LDA}. By default \code{2000}.}

\item{seed}{The seed parameter to ensure reproducibility. By default \code{42}.}
}
\value{
For each \code{n_topics} a model is fitted that is put into a list with the resulting doc_topic_distr as list(model = ..., doc_topic_distr = ...)
        Each list is saved via \code{saveRDS} in a file in \code{model_dir}.
        The filenames include information on \code{n} and the elapsed time for fitting the individual model.
        They appear, e.g., as: \code{"n5_Warp_LDA_model_0h_1min.rds"}.
        Especially the initial part \code{"nX_Warp_LDA_model"} may be used for programmatically accessing model files.
}
\description{
LDA models with text2vec are fitted in parallel via \code{foreach} package for a set of candidate number of topics.
Function is not made failsafe, yet, it is just straightforward parallel fitting without safety nets, e.g., regarding overwriting of files.
}
\examples{

# data part of the example is copied from text2vec::LatendDirichletAllocation
library(text2vec)
data("movie_review")
N = 500
tokens = word_tokenizer(tolower(movie_review$review[1:N]))
it = itoken(tokens, ids = movie_review$id[1:N])
v = create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
dtm = create_dtm(it, vocab_vectorizer(v))
# fit one model without parallel pocessing for comparison
# Note that seed and other parameters of the model have to be set
# for comparing to other model from parallel fitting
set.seed(42)
lda_model = LDA$new(n_topics = 5)
doc_topic_distr = lda_model$fit_transform(dtm, n_iter = 20)

modeldir = paste0(getwd(), "/modeldir/")
if (dir.exists(modeldir)) {
  stop("Standard directory used for this example already exists. Please change.")
} else {
  dir.create(modeldir)
}
library(doParallel)
# you might want to check the work load of your processors with your favorite monitor...
warp_lda_vary_n_parallel( dtm = dtm
                          , n_topics = c(3,5,7)
                          , n_iter = 20
                          , n_cores = detectCores()-1
                          , model_dir = modeldir
                          , seed = 42)
list.files(modeldir)
# [1] "n3_Warp_LDA_model_0h_0min.rds"
# [2] "n5_Warp_LDA_model_0h_0min.rds"
# [3] "n7_Warp_LDA_model_0h_0min.rds"
# we compare the model with 5 topics
lda_model_from_parallel = readRDS(list.files(modeldir, full.names = T)[2])
names(lda_model_from_parallel)
# [1] "model"           "doc_topic_distr"
all.equal(doc_topic_distr, lda_model_from_parallel$doc_topic_distr)
# [1] TRUE

# delete directory
unlink(paste0(getwd(), "/modeldir"), recursive=TRUE)
}
