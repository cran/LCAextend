\name{dens.prod.ordi}
\alias{dens.prod.ordi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the probability of a given discrete
  measurement vector for all classes under a product of multinomial }
\description{
  computes the probability of an individual's discrete measurement vector for all
  latent classes under a multinomial distribution product, eventually
  taking covariates into account. This is an internal
  function not meant to be called by the user.
}
\usage{
dens.prod.ordi(y.x, param, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ y.x }{ a vector \code{y} of values of the ordinal variables (measurements) followed by the values \code{x} of covariates, if any, }
  \item{ param }{ a list of the parameters alpha (cumulative logistic coefficients), see \code{\link{init.ordi}}, }
  \item{ var.list }{ a list of integers indicating which covariates (taken from \code{x}) are used for a given type of measurement. }
}
\details{
  If there are \code{K} latent classes, \code{d} measurements and each measurement has \code{S[j]} possible values, \code{alpha} is a list of \code{d} 
  elements, each is a \code{K} times \code{S[j]+length{var.list[[j]]}} matrix. For a class \code{C=k}, \code{dens[k]=}\eqn{\displaystyle\prod_{j=1}^d{P(Y_j=y_j|C=k,X_j=x_j)}}{%
   \code{dens[k]=prod_{j=1,...,d}P(Y_j=y_j)}}, where \eqn{P(Y_j=y_j|C=k,X_j=x_j)} is
 computed from the cumulative logistic coefficients \code{alpha[[j]][k,]} and 
   covariates \code{x[var.list[[j]]]},
}
\value{
  The function returns a vector \code{dens} of length \code{K},  where
  \code{dens[k]} is the probability of measurement vector \code{y} with covariates \code{x}, 
  if the individual belongs to class \code{k}.
}


\seealso{  See Also \code{\link{init.ordi}}, }
\examples{
#data
data(ped.ordi)
status <- ped.ordi[,6]
y <- ped.ordi[status==2,7:ncol(ped.ordi)]
#param
data(param.ordi)
#the function applied for measurement of the first individual in the ped.ordi
dens.prod.ordi(y.x=y[1,],param.ordi)
}
