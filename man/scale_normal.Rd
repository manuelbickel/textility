% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{scale_normal}
\alias{scale_normal}
\title{Scale values normal with boundaries zero and one}
\usage{
scale_normal(x)
}
\arguments{
\item{x}{}
}
\description{
Scale values normal with boundaries zero and one
}
\examples{
scale_normal(c(0,50,100))
# [1] 0.0 0.5 1.0
scale_normal(c(1,6,11))
# [1] 0.0 0.5 1.0
scale_normal(c(3,3,3))
# [1] 1 1 1
}
