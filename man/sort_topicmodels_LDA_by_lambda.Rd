% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_topicmodels_LDA_by_lambda.R
\name{sort_topicmodels_LDA_by_lambda}
\alias{sort_topicmodels_LDA_by_lambda}
\title{Sort topics of an lda model from topicmodels by lambda}
\usage{
sort_topicmodels_LDA_by_lambda(ldamodel, lambda = 1, number_terms = 10,
  reorder.topics = TRUE, vocab = vocabulary)
}
\arguments{
\item{vocab}{}
}
\description{
This function is largely a copy of the code parts from the LDAvis package for the function createJSON():
https://github.com/cpsievert/LDAvis/blob/master/R/createJSON
However, it was customized to accept LDA from from topicmodels package.
}
\details{
Documentation still to be done...
}
