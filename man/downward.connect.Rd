\name{downward.connect}
\alias{downward.connect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs a downward step for a connector }
\description{
  computes the probability of the measurements above a connector and the
  connector latent class given the model parameters. This is an internal
  function not meant to be called by the user.
}
\usage{
downward.connect(connect, parent1, parent2, bro.connect, status, 
                 probs, fyc, p.ybarF.c, res.upward)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{connect}{ a connector in the pedigree (individual with parents and children in the pedigree), }
  \item{parent1}{ one of the connector parent who is also a connector, }
  \item{parent2}{ the other parent of the connector (not a connector), }
  \item{bro.connect}{ siblings of the connector, }
  \item{status}{ a vector of symptom status, }
  \item{probs}{ a list of all probability parameters of the model, }
  \item{fyc}{ a matrix of \code{n} times \code{K+1} given the density of observations of each individual if allocated to class \code{k}, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model. the \code{K+1} corresponds to the unaffected class, }
  \item{p.ybarF.c}{ a array of dimension \code{n} times 2 times \code{K+1} giving the probability of observations above the individual, 
  depending on his status and his class and conditionally to his class, }
  \item{res.upward}{ the result of the upward step of the peeling algorithm, see \code{\link{upward}}. }
}
\details{
  If \code{Y_above(i)} is the measurements above connector \code{i} and \code{S_i} and \code{C_i} are his status and his class respectively, the function computes 
  \code{P(Y_above(i),S_i,C_i)} by computing a downward step for the parent of connector \code{i} who is also a connector.
}
\value{
  The function returns \code{p.ybarF.c} updated for connector \code{i}.
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
#the 2nd connector
generat <- peel$generation-1
connect <- peel$peel.connect[generat,]
connect <- connect[connect>0][1]
parent1.connect <- intersect(peel$peel.connect[generat+1,],c(dad[id==connect],
                                                             mom[id==connect]))
parent2.connect <- setdiff(c(dad[id==connect],mom[id==connect]),parent1.connect)
bro.connect <- union(id[dad==parent1.connect],id[mom==parent1.connect])
bro.connect <- setdiff(bro.connect,connect)
#probs and param
data(probs)
data(param.cont)
#densities of the observations
fyc <- matrix(1,nrow=length(id),ncol=length(probs$p)+1)
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,param.cont,NULL))
#probability of the observations below
p.ybarF.c <- array(1,dim=c(length(id),2,length(probs$p)+1))
#the upward step
res.upward <- upward(id,dad,mom,status,probs,fyc,peel)
#the function
downward.connect(connect,parent1.connect,parent2.connect,bro.connect,status,
                 probs,fyc,p.ybarF.c,res.upward)
}
