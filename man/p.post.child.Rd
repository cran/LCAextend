\name{p.post.child}
\alias{p.post.child}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the posterior probability of observations of a child }
\description{
  computes the posterior probability of measurements of a child for each
  class and each symptom status of the subject given the classes of both
  of his parents. This is an internal function not meant to be called by the user.
}
\usage{
p.post.child(child, c.connect, c.spouse, status, probs, fyc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{child}{ a child in the pedigree, }
  \item{c.connect}{ the class of one parent (who is a connector) of the child, }
  \item{c.spouse}{ the class of the other parent of the child, }
  \item{status}{ the symptom status vector of the whole pedigree, }
  \item{probs}{ a list of all probability parameters of the model, }
  \item{fyc}{ a matrix of \code{n} times \code{K+1} giving the density of measurements of each individual if allocated to class \code{k}, where \code{n} is 
  the number of individuals and \code{K} is the total number of latent classes in the model, }
}
%\details{
  
%}
\value{
  the function returns \code{p.child} a matrix of 2 times \code{K+1} entries such that \code{p.child[s,k]} is the posterior probability of the measurements \code{Y_child} 
  under status \code{S_child=s} and when he is assigned to class \code{k} and his parents are assigned to classes \code{c.connect} and \code{c.spouse}.
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended
  Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\examples{
#data
data(ped.cont)
fam <- ped.cont[,1]
dad <- ped.cont[fam==1,3]
status <- ped.cont[fam==1,6]
y <- ped.cont[fam==1,7:ncol(ped.cont)]
#a child
child <- which(dad!=0)[1]
data(probs)
data(param.cont)
#densities of the observations
fyc <- matrix(1,nrow=nrow(y),ncol=length(probs$p)+1)
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,
                                            param.cont,NULL))
#the function
p.post.child(child,c.connect=1,c.spouse=3,status,probs,fyc)
}
