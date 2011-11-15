\name{optim.noconst.ordi}
\alias{optim.noconst.ordi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the M step for the measurement distribution parameters in multinomial case without constraint on the parameters }
\description{
  Estimates the cumulative logistic coefficients \code{alpha} in the case of multinomial (or ordinal) data without constraint on the coefficients.
}
\usage{
optim.noconst.ordi(y, status, weight, param, x = NULL, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a matrix of discrete (or ordinal) measurements (only for symptomatic subjects), }
  \item{status}{ symptom status of all individuals, }
  \item{weight}{ a matrix of \code{n} times \code{K} of individual weights, where \code{n} is the number of individuals and \code{K} is 
  the total number of latent classes in the model, }
  \item{param}{ a list of measurement distribution parameters, here is a list \code{alpha} of cumulative logistic coefficients, }
  \item{x}{ a matrix of covariates (optional). Default is \code{NULL}, }
  \item{var.list}{ a list of integers indicating which covariates (taken from \code{x}) are used for a given type of measurment. }
}
\details{
  The values of explicit estimators are computed by logistic
  transformation of weighted empirical frequencies.
}
\value{
  the function returns a list of estimated parameters \code{param}.
}
%\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }
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
optim.noconst.ordi(y[status==2,],status,weight,param.ordi,x=NULL,
                   var.list=NULL)
}
