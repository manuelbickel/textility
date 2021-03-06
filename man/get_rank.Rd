% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rank.R
\name{get_rank}
\alias{get_rank}
\title{Wrapper around base rank allowing for dense rank output}
\usage{
get_rank(x, decreasing = FALSE, na.last = TRUE, ties.method = "average")
}
\arguments{
\item{x}{...}

\item{decreasing}{By default set to FALSE to comply with the logic of base::rank.
Set to TRUE to rank the highest number in x with 1.
This option is currently only implemented for dense ranking.}

\item{na.last}{...}

\item{ties.method}{... amended by "dense".}
}
\value{
...
}
\description{
Internally base::rank is used (hence, same parameters) but amended by the option ties.method = "dense" based on the implementaion of dense_rank in dplyr package.
}
\examples{

x <- sample(1:1000, 100)
all(base::rank(x) == get_rank(x))
#TRUE

x <- c(11,11,12,13)
base::rank(x)
#[1] 1.5 1.5 3.0 4.0
get_rank(x, ties.method = "dense", decreasing = FALSE)
#[1] 1 1 2 3
get_rank(x, ties.method = "dense", decreasing = TRUE)
#[1] 3 3 2 1
}
