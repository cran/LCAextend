\name{weight.famdep}
\alias{weight.famdep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the computation of triplet and individual weights for a pedigree under familial dependence}
\description{
  computes the triplet and the individual weights of the E step of the EM algorithm for a pedigree in the case of familial dependence. It returns also the 
  overall log-likelihood of the observations. This is an internal
  function not meant to be called by the user.
}
\usage{
weight.famdep(id, dad, mom, status, probs, fyc, peel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{ individual ID of the pedigree, }
  \item{dad}{ dad ID, }
  \item{mom}{ mom ID, }
  \item{status}{ symptom status: (2: symptomatic, 1: without symptoms, 0: missing), }
  \item{probs}{ list of probability parameters of the model, }
  \item{fyc}{ a matrix of \code{n} times \code{K+1} given the density of observations of each individual if allocated to class \code{k}, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model, }
  \item{peel}{ a list of pedigree peeling containing connectors by peeling order and couples of parents }
}
\details{
  the function calls the functions \code{\link{upward}} and
    \code{\link{downward}} which perform the required probability
      computations by processing the pedigree by nuclear family (or
      equivalently by connector) following the peeling order.
}
\value{
  the function returns a list of 3 elements:
  \item{ww}{ triplet weights: an array of \code{n} times 2 times \code{K+1} times \code{K+1} times \code{K+1}, see \code{\link{e.step}}, }
  \item{w}{ individual weights: an array of \code{n} times 2 times \code{K+1}, see \code{\link{e.step}}, }
  \item{ll}{ log-likelihood. }
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\seealso{ See also \code{\link{upward}}, \code{\link{downward}}, \code{\link{e.step}}. }
\examples{
#data
data(ped.cont)
data(peel)
fam <- ped.cont[,1]
id <- ped.cont[fam==1,2]
dad <- ped.cont[fam==1,3]
mom <- ped.cont[fam==1,4]
status <- ped.cont[fam==1,6]
y <- ped.cont[fam==1,7:ncol(ped.cont)]
peel <- peel[[1]]
#probs and param
data(probs)
data(param.cont)
#densities of the observations
fyc <- matrix(1,nrow=length(id),ncol=length(probs$p)+1)
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,
                                      param.cont,NULL))
#the function
weight.famdep(id,dad,mom,status,probs,fyc,peel)
}
