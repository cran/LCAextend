\name{upward}
\alias{upward}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the upward step of the peeling algorithm of a pedigree }
\description{
  computes the probability of observations below connectors
  conditionally to their classes given the model parameters. This is an internal
  function not meant to be called by the user.
}
\usage{
upward(id, dad, mom, status, probs, fyc, peel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{ individual ID of the pedigree, }
  \item{dad}{ dad ID, }
  \item{mom}{ mom ID, }
  \item{status}{ symptom status: (2: symptomatic, 1: without symptoms, 0: missing), }
  \item{probs}{ a list of probability parameters of the model, }
  \item{fyc}{ a matrix of \code{n} times \code{K+1} given the density of observations of each individual if allocated to class \code{k}, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model, }
  \item{peel}{ a list of pedigree peeling result containing connectors by peeling order and couples of parents. }
}
\details{
  This function computes the probability of observations below connectors conditionally to their classes using the function \code{\link{upward.connect}}
}
\value{
  The function returns a list of 2 elements:
  \item{sum.child}{an array of dimension \code{n} times \code{K+1} times \code{K+1} such that \code{sum.child[i,c_1,c_2]} is the probability of 
  individual \code{i} measurements when his parent are assigned to classes \code{c_1} and \code{c_2}, }
  \item{p.yF.c}{ an array of dimension \code{n} times 2 times \code{K+1} giving the probability of all measurements below the individual, 
  depending on his status and his class. }
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\seealso{ See also \code{\link{upward.connect}} }
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
#standardize id to be 1, 2, 3, ...
id.origin <- id
standard <- function(vec) ifelse(vec\%in\%id.origin,which(id.origin==vec),0)
id <- apply(t(id),2,standard)
dad <- apply(t(dad),2,standard)
mom <- apply(t(mom),2,standard)
peel$couple <- cbind(apply(t(peel$couple[,1]),2,standard),
                     apply(t(peel$couple[,2]),2,standard))
for(generat in 1:peel$generation)
peel$peel.connect[generat,] <- apply(t(peel$peel.connect[generat,]),2,standard)
#probs and param
data(probs)
data(param.cont)
#densities of the observations
fyc <- matrix(1,nrow=length(id),ncol=length(probs$p)+1)
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,
                                      param.cont,NULL))
#the function
upward(id,dad,mom,status,probs,fyc,peel)
}
