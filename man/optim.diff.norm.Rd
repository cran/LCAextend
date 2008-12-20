\name{optim.diff.norm}
\alias{optim.diff.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the M step for measurement density parameters in multinormal case }
\description{
  Estimates the mean
  \code{mu} and parameters of the variance-covariance matrix
  \code{sigma} of a multinormal distribution for the measurements with
  a general variance-covariance matrix identical for all classes.
}
\usage{
optim.diff.norm(y, status, weight, param, x = NULL, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ y }{ a matrix of continuous measurements (only for symptomatic subjects), }
  \item{ status }{ symptom status of all individuals, }
  \item{ weight }{ a matrix of \code{n} times \code{K} of individual weights, where \code{n} is the number of individuals and \code{K} is 
  the total number of latent classes in the model, }
  \item{ param }{ a list of measurement density parameters, here is a list of \code{mu} and \code{sigma}, }
  \item{ x }{ a matrix of covariates (optional). Default id \code{NULL}, }
  \item{ var.list }{ a list of integers indicating which covariates (taken from \code{x}) are used for a given type of measurement. }
}
\details{
  The values of explicit estimators are computed for both \code{mu} and
  \code{sigma}. The variance-covariance matrices \code{sigma} are
  identical  for all classes. Treatment of covariates is not yet implemented, and any
  provided covariate value will be ignored.
}
\value{
  The function returns a list of estimated parameters \code{param}.
}
\examples{
#data
data(ped.cont)
status <- ped.cont[,6]
y <- ped.cont[,7:ncol(ped.cont)]
data(peel)
#probs and param
data(probs)
data(param.cont)
#e step
weight <- e.step(ped.cont,probs,param.cont,dens.norm,peel,x=NULL,
                 var.list=NULL,famdep=TRUE)$w
weight <- matrix(weight[,1,1:length(probs$p)],nrow=nrow(ped.cont),
                 ncol=length(probs$p))
#the function
optim.diff.norm(y[status==2,],status,weight,param.cont,x=NULL,
                 var.list=NULL)
}
