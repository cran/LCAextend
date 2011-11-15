\name{param.ordi}
\alias{param.ordi}
\docType{data}
\title{ parameters to be used for examples in the case of discrete or ordinal measurements }
\description{
  list of cumulative logistic coefficients for each measurement and each class to be used in examples for discrete or ordinal models.
}
\usage{data(param.ordi)}
\details{
  \code{ped.param} is a list of 1 element:
  \code{alpha} a list of \code{d=4} (the number of measurements) entries, each is a matrix of \code{K=3} (the number of classes) times \code{S[j]} 
  (the number of possible values of measurement \code{j}), a row \code{alpha[[j]][k,]} contains the logistic coefficients of the measurement \code{j} for class \code{k}. 
}
\seealso{
See also \code{\link{init.ordi}}
}
\examples{
data(param.ordi)
}
