\name{ped.cont}
\alias{ped.cont}
\docType{data}
\title{ pedigrees with continuous data to be used for examples }
\description{
  data set of 48 pedigrees: a matrix of pedigrees data with continuous observations to be used for examples.
}
\usage{data(ped.cont)}
%\format{}
\details{
  \code{ped} is a matrix of 10 columns:
  \code{ped[,1]} family ID, 
  \code{ped[,2]} subject ID, 
  \code{ped[,3]} father ID, 0 for founders (i.e. subjects having no parents in the pedigree),
  \code{ped[,4]} mother ID, 0 for founders (i.e. subjects having no parents in the pedigree), 
  \code{ped[,5]} subject sex: 1 male, 2 female, 
  \code{ped[,6]} symptom status (2: symptomatic, 1: without symptoms, 0: missing), 
  \code{ped[,7:10]} continuous observations, \code{NA} for
  missing and without symptoms, 
}
\examples{
data(ped.cont)
}
