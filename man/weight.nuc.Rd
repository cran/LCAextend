\name{weight.nuc}
\alias{weight.nuc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the computation of unnormalized triplet and individuals weights for a nuclear family in the pedigree }
\description{
  the weighting algorithm proceeds by nuclear family, the function \code{weight.nuc} computes the unnormalized triplet and individuals weights for a 
  nuclear family in the pedigree. This is an internal
  function not meant to be called by the user.
}
\usage{
weight.nuc(connect, spouse.connect, children.connect, status, 
probs, fyc, p.ybarF.c, ww, w, res.upward)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ connect }{ a connector in the pedigree, }
  \item{ spouse.connect }{ spouse of the connector, }
  \item{ children.connect }{ children of the connector, }
  \item{ status }{ vector of symptom status of the whole pedigree, }
  \item{ probs }{ all probability parameters of the model, }
  \item{ fyc }{ a matrix of \code{n} times \code{K+1} given the density of observations of each individual if allocated to class \code{k}, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model, }
  \item{ p.ybarF.c }{ a array of dimension \code{n} times 2 times \code{K+1} giving the probability of observations above the individual, 
  depending on his status and his class and conditionally on his class, }
  \item{ ww }{ unnormalized triplet weights, an array of \code{n} times 2 times \code{K+1} times \code{K+1} times \code{K+1}, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model, see \code{\link{e.step}}, }
  \item{ w }{ unnormalized individual weights, an array of \code{n} times 2 times \code{K+1}, see \code{\link{e.step}}, }
  \item{ res.upward }{ result of the upward step of the weighting algorithm, see \code{\link{upward}}, }
}
\details{
  updated \code{ww} and \code{w} are computed for the current nuclear family.
}
\value{
 the function returns a list of 2 elements:
 \item{ ww }{ unnormalized triplet weights, an array of \code{n} times 2 times \code{K+1} times \code{K+1} times \code{K+1}, see \code{\link{e.step}}, }
 \item{ w }{ unnormalized individual weights, an array of \code{n} times 2 times \code{K+1}, see \code{\link{e.step}}. }
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\seealso{ See also \code{\link{downward}} }
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
#the first nuclear family
generat <- peel$generation
connect <- peel$peel.connect[generat,]
connect <- connect[connect>0]
spouse.connect <- peel$couple[peel$couple[,1]==connect,2]
children.connect <- union(id[dad==connect],id[mom==connect])
#probs and param
data(probs)
data(param.cont)
#densities of the observations
fyc <- matrix(1,nrow=length(id),ncol=length(probs$p)+1)
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,
                                            param.cont,NULL))
#triplet and individual weights
ww <- array(0,dim=c(length(id),rep(2,3),rep(length(probs$p)+1,3)))
w <- array(0,dim=c(length(id),2,length(probs$p)+1))
#probability of the observations below
p.ybarF.c <- array(1,dim=c(length(id),2,length(probs$p)+1))
p.ybarF.c[connect,,] <- p.post.found(connect,status,probs,fyc)
#the upward step
res.upward <- upward(id,dad,mom,status,probs,fyc,peel)
#the function
weight.nuc(connect,spouse.connect,children.connect,status,probs,fyc,
           p.ybarF.c,ww,w,res.upward)
}
