\name{optim.probs}
\alias{optim.probs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the M step of the EM algorithm for the probability parameters }
\description{
  estimates the probability parameters (\code{p}, \code{p.trans}, \code{p0},...) in the M step of the EM algorithm in both cases 
  with and without familial dependence. This is an internal
  function not meant to be called by the user.
}
\usage{
optim.probs(ped, probs, optim.probs.indic = c(TRUE, TRUE, TRUE, TRUE), 
            res.weight, famdep = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ped}{ a matrix of pedigrees data, see \code{\link{e.step}} for more details, }
  \item{probs}{ all probability parameters to be optimized, }
  \item{optim.probs.indic}{ a vector of logical values indicating which probability parameters to be updated, }
  \item{res.weight}{ a matrix of \code{n} times \code{K}, individual weights, where \code{n} is the number of individuals and \code{K} is 
  the total number of latent classes in the model, resulting from the E step of the EM algorithm (see \code{\link{e.step}})}
  \item{famdep}{ a logical variable indicating if familial dependence model is used or not. Default is \code{TRUE}. }
}
\details{
  explicit estimators are computed in function of the weights.
}
\value{
  the function returns the estimated \code{probs} of all probability parameters.
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\examples{
#data
data(ped.cont)
data(peel)
#probs and param
data(probs)
data(param.cont)
#e step
weight <- e.step(ped.cont,probs,param.cont,dens.norm,peel,x=NULL,
                 var.list=NULL,famdep=TRUE)
#the function
optim.probs(ped.cont,probs,weight,optim.probs.indic=
            c(TRUE,TRUE,TRUE,TRUE),famdep=TRUE)
}
