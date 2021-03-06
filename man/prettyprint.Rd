% Generated by roxygen2 (4.0.2): do not edit by hand
\name{prettyprint}
\alias{prettyprint}
\title{Pretty print a dataset}
\usage{
prettyprint(x, labels)
}
\arguments{
\item{x}{a data.frame or a matrix}

\item{labels}{character vector of column headers}
}
\value{
printed output
}
\description{
print like mysql
}
\examples{
x <- matrix(c(7,23,41,43,27,9), nrow=3)
dimnames(x) <- list(c('trt1','trt2','trt3'), c('pass','fail'))
prettyprint(x)
}

