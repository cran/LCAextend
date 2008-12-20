\name{downward}
\alias{downward}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the downward step of the peeling algorithm and computes unnormalized triplet and individual weights }
\description{
  computes the probability of measurements above connectors and their
  classes given the model parameters, and returns the unnormalized
  triplet and individual weights. This is an internal
  function not meant to be called by the user.
}
\usage{
downward(id, dad, mom, status, probs, fyc, peel, res.upward)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ id }{ individual ID of the pedigree, }
  \item{ dad }{ dad ID, }
  \item{ mom }{ mom ID, }
  \item{ status }{ symptom status: (2: symptomatic, 1: without symptoms, 0: missing), }
  \item{ probs }{ a list of probability parameters of the model, }
  \item{ fyc }{ a matrix of \code{n} times \code{K+1} given the density of observations of each individual if allocated to class \code{k}, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model, }
  \item{ peel }{ a list of pedigree peeling containing connectors by peeling order and couples of parents, }
  \item{ res.upward }{ result of the upward step of the peeling algorithm, see \code{\link{upward}}. }
}
\details{
 This function computes the probability of observations above connectors and their classes using the function \code{\link{downward.connect}}, for each connector, 
 if \code{Y_above(i)} is the observations above connector \code{i} and \code{S_i} and \code{C_i} are his status and his class respectively, the functions computes 
  \code{P(Y_above(i),S_i,C_i)} by computing a downward step for the parent of connector \code{i} who is also a connector. These quantities are used by the function \code{\link{weight.nuc}} to compute the unnormalized triplet weights \code{ww} and the unnormalized 
  individual weights \code{w}.
}
\value{
 The function returns a list of 2 elements:
 \item{ ww }{ unnormalized triplet weights, an array of \code{n} times 2 times \code{K+1} times \code{K+1} times \code{K+1}, where \code{n} is the 
 number of individulas and \code{K} is the total number of latent classes in the model, see \code{\link{e.step}} for more details, }
 \item{ w }{ unnormalized individual weights, an array of \code{n} times 2 times \code{K+1}, see \code{\link{e.step}}. }
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\seealso{  See also \code{\link{downward.connect}}. }
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
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,param.cont,NULL))
#the upward step
res.upward <- upward(id,dad,mom,status,probs,fyc,peel)
#the function
downward(id,dad,mom,status,probs,fyc,peel,res.upward)
}
