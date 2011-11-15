\name{upward.connect}
\alias{upward.connect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the upward step for a connector }
\description{
  computes the probability of the measurements below a connector
  conditionally to the connector latent class given the model
  parameters. This is an internal function not meant to be called by the user.
}
\usage{
upward.connect(connect, spouse.connect, children.connect, status,
probs, p.yF.c, fyc, sum.child)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{connect}{ a connector in the pedigree, }
  \item{spouse.connect}{ spouse of the connector, }
  \item{children.connect}{ children of the connector, }
  \item{status}{ a vector of symptom status of the whole pedigree, }
  \item{probs}{ a list of probability parameters of the model, }
  \item{p.yF.c}{ an array of dimension \code{n} times 2 times \code{K+1} giving the probability of measurements below the individual, 
  depending on his status and his class, where \code{n} is the 
  number of individuals and \code{K} is the total number of latent classes in the model, }
  \item{fyc}{ a matrix of \code{n} times \code{K+1} given the density of measurements of each individual if allocated to class \code{k}, }
  \item{sum.child}{ an array of dimension \code{nber.indiv} times \code{K+1} times \code{K+1} such that \code{sum.child[i,c_1,c_2]} is the probability of 
  individual \code{i} measurements when his parent are assigned to classes \code{c_1} and \code{c_1}. }
}
\details{
  If \code{Y_above(i)} is the observations below connector \code{i} and \code{C_i} is his class, the functions computes \code{P(Y_below(i)|C_i)}.
}
\value{
  The function returns a list of 2 elements:
  \item{sum.child}{an array of dimension \code{n} times \code{K+1} times \code{K+1} such that \code{sum.child[i,c_1,c_2]} is the probability of 
  individual \code{i} observations when his parent are assigned to classes \code{c_1} and \code{c_2}, }
  \item{p.yF.c}{a array of dimension \code{n} times 2 times \code{K+1} giving the probability of measurements below the individual, 
  depending on his status and his class, updated for the current connector. }
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
\seealso{ See also \code{\link{upward}} }
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
#a nuclear family
#connector in the pedigree 1
connect <- peel$peel.connect[1,1]
#soupse of connector connect
spouse.connect <- peel$couple[peel$couple[,1]==connect,2]
#children of connector connect
children.connect <- union(id[dad==connect],id[mom==connect])
#probs and param
data(probs)
data(param.cont)
#probabilitiy of observations above
p.yF.c <- matrix(1,nrow=length(id),ncol=length(probs$p)+1)
#densities of the observations
fyc <- matrix(1,nrow=length(id),ncol=length(probs$p)+1)
fyc[status==2,1:length(probs$p)] <- t(apply(y[status==2,],1,dens.norm,
                                      param.cont,NULL))
#sums over childs
sum.child <- array(0,c(length(id),length(probs$p)+1,length(probs$p)+1))
#the function
upward.connect(connect,spouse.connect,children.connect,status,probs,
               p.yF.c,fyc,sum.child)
}
