\name{dens.norm}
\alias{dens.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the multinormal density of a given continuous
  measurement vector for all classes }
\description{
  computes the density of an individual's continuous measurement vector for all latent classes,
  eventually taking covariates into account. This is an internal
  function not meant to be called by the user.
}
\usage{
dens.norm(y.x, param, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y.x}{ a vector \code{y} of values of the measurement followed by the values \code{x} of covariates, if any,}
  \item{param}{ a list of the multinormal density parameters: means \code{mu} and variances-covariances \code{sigma}, }
  \item{var.list}{ a list of integers indicating which covariates (taken from \code{x}) are used for a given type of measurement. }
}
\details{
  For each class \code{k}, the function computes the multinormal density with means \code{param$mu[[k]]} and variances-covariances matrix 
  \code{param$sigma[[k]]} for the individual's measurement
  vector. Treatment of covariates is not yet implemented, and any
  provided covariate value will be ignored.
}
\value{
  The function returns a vector \code{dens} of length \code{K}, where
  \code{dens[k]} is the density of the measurements if the individual belongs to class \code{k}.
}

\examples{
#data
data(ped.cont)
status <- ped.cont[,6]
y <- ped.cont[status==2,7:ncol(ped.cont)]
#param
data(param.cont)
#the function applied for measurement of the first individual in the ped.ordi
dens.norm(y.x=y[1,],param.cont)
}
