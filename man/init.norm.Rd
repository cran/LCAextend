\name{init.norm}
\alias{init.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes initial values for the EM algorithm in the case of continuous measurements }
\description{
  computes initial values of means and variance-covariance matrices for the EM algorithm in the case of continuous measurements and multinormal model.
}
\usage{
init.norm(y, K, x = NULL, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ y }{ a \code{n} times \code{d} matrix of continuous measurements, where \code{n} is the number of indivduals and \code{d} is the number of measurements.
  All entries must be finite, if not an error is produced, }
  \item{ K }{ number of latent classes of the model, }
  \item{ x }{ a matrix of covariates if any, default is \code{NULL} (no covariates), }
  \item{ var.list }{ a list of integers indicating which covariates (taken from \code{x}) are used for a given measurement (a column of \code{y}). }
}
\details{
  The function allocates every individual to a class by a simple
  clustering of the data and evaluates the means and variance-covariance matrices of
  measurements in each class. Treatment of covariates is not yet implemented, and any
  provided covariate value will be ignored.
}
\value{
  The function returns a list of 2 elements \code{mu} and \code{sigma} of length \code{K} each, \code{mu[k]} is the means vector 
  (of length \code{d}) of measurements in class \code{k} and \code{sigma[k]} is the variances-covariances matrix 
  (of dimension \code{d} times \code{d}) of measurements in class \code{k}.
}
\examples{
#data
data(ped.cont)
status <- ped.cont[,6]
y <- ped.cont[status==2,7:ncol(ped.cont)]
#the function
init.norm(y,K=3)
}
