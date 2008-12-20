\name{init.ordi}
\alias{init.ordi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the initial values for EM algorithm in the case of ordinal measurements }
\description{
  computes the initial values of cumulative logistic coefficients alpha
  for the EM algorithm in the case of ordinal measurements and a product multinomial model.
}
\usage{
init.ordi(y, K, x = NULL, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ y }{ a \code{n} times \code{d} matrix of ordinal (or discrete) measurements, where \code{n} is the number of individuals and \code{d} is the number of 
  measurements. All entries must be finite, if not an error is produced, }
  \item{ K }{ number of latent classes of the model, }
  \item{ x }{ a matrix of covariates if any, default is \code{NULL} (no covariates), }
  \item{ var.list }{ list of integers indicating which covariates (taken from \code{x}) are used for a given measurement (a column of \code{y}). }
}
\details{
  The function allocates every individual to a class and evaluates the
  cumulative logistic coefficients for each measurement and each
  class. Regression coefficients for the covariates are set to 0.
}
\value{
  The function returns a list of one element \code{alpha} which is a list of \code{d} elements, each element \code{alpha[[j]]} is a \code{K} times 
  \code{S-1} matrix, where \code{S} is the number of values of the measurement \code{y[,j]}, a row \code{alpha[[j]][k,]} gives the the cumulative logistic 
  coefficients of class \code{k} and measurement \code{j} using \code{\link{alpha.compute}}.
}

\seealso{ \code{\link{alpha.compute}} }
\examples{
#data
data(ped.ordi)
status <- ped.ordi[,6]
y <- ped.ordi[,7:ncol(ped.ordi)]
#the function
init.ordi(y[status==2,],K=3)
}
