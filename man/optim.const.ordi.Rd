\name{optim.const.ordi}
\alias{optim.const.ordi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the M step for the measurement distribution parameters
  in multinomial case, with an ordinal constraint on the parameters }
\description{
  Estimates the cumulative logistic coefficients \code{alpha} in the
  case of multinomial (or ordinal) data with an ordinal constraint on
  the parameters.
}
\usage{
optim.const.ordi(y, status, weight, param, x = NULL, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{y}{ a matrix of discrete (or ordinal) measurements (only for symptomatic subjects), }
  \item{status}{ symptom status of all individuals, }
  \item{weight}{ a matrix of \code{n} times \code{K} of individual weights, where \code{n} is the number of individuals and \code{K} is 
  the total number of latent classes in the model, }
  \item{param}{ a list of measurement density parameters, here is a list of \code{alpha}, }
  \item{x}{ a matrix of covariates (optional). Default id \code{NULL}, }
  \item{var.list}{ a list of integers indicating which covariates (taken from \code{x}) are used for a given type of measurement }
}
\details{
  the constraint on the parameters is that, for a symptom \code{j}, the rows \code{alpha[[j]][k,]} are equal for all classes \code{k} except the first values. 
  Therefore, maximum likelihood estimators are not explicit and the
  function \code{lrm} of the package \code{Design} is used to perform a
  numerical optimization.
}
\value{
  The function returns a list of estimated parameters \code{param} satisfying the constraint.
}
%\references{ Labbe et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. }
\examples{
#data
data(ped.ordi)
status <- ped.ordi[,6]
y <- ped.ordi[,7:ncol(ped.ordi)]
data(peel)
#probs and param
data(probs)
data(param.ordi)
#e step
weight <- e.step(ped.ordi,probs,param.ordi,dens.prod.ordi,peel,x=NULL,
                 var.list=NULL,famdep=TRUE)$w
weight <- matrix(weight[,1,1:length(probs$p)],nrow=nrow(ped.ordi),
                 ncol=length(probs$p))
#the function
optim.const.ordi(y[status==2,],status,weight,param.ordi,x=NULL,
                 var.list=NULL)
}
