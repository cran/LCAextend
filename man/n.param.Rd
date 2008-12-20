\name{n.param}
\alias{n.param}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the number of parameters of a model }
\description{
  computes the number of free parameters of a model, depending in the number of classes, the type of parameter optimization and the used of 
  familial dependence, to be used in BIC model selection. This is an internal function not meant to be called by the user.
}
\usage{
n.param(y, K, trans.const = TRUE, optim.param, 
        optim.probs.indic = c(TRUE, TRUE, TRUE, TRUE), famdep = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ y }{ a matrix of measurements, }
  \item{ K }{ an integer, the number of latent classes of a candiate model, }
  \item{ trans.const }{ a logical variable indicating if the parental constraint is used. Parental constraint means that the class of a subject can be only one 
  of his parents classes. Default is \code{TRUE}, }
  \item{ optim.param }{ a function used for parameter optimization, see \code{\link{lca.model}} for more details, }
  \item{ optim.probs.indic }{ a vector of logical values indicating which probability parameters to be updated, see \code{\link{lca.model}} for more details, }
  \item{ famdep }{ a logical variable indicating if familial dependence model is used or not. Default is \code{TRUE}. }
}
%\details{  
%}
\value{
  The function returns the number of free parameters (of the measurement
  distribution and the probabilities of the latent classes).
}
\seealso{ See also \code{\link{model.select}}. }
\examples{
data(ped.cont)
y <- ped.cont[,7:ncol(ped.cont)]
n.param(y,K=3,trans.const=TRUE,optim.indep.norm, 
        optim.probs.indic=c(TRUE,TRUE,TRUE,TRUE),famdep=TRUE)
}
