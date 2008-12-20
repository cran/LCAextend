\name{ped.ordi}
\alias{ped.ordi}
\docType{data}
\title{ pedigrees with discrete or ordinal data to be used for examples }
\description{
  data set of 48 pedigrees: a matrix of pedigrees data with discrete or ordinal observations to be used for examples.
}
\usage{data(ped.ordi)}
%\format{}
\details{
  \code{ped} is a matrix of 10 columns:
  \code{ped[,1]} family ID, 
  \code{ped[,2]} subject ID, 
  \code{ped[,3]} father ID, 0 for founders (i.e. subjects having no parents in the pedigree),
  \code{ped[,4]} mother ID, 0 for founders (i.e. subjects having no parents in the pedigree), 
  \code{ped[,5]} subject sex: 1 male, 2 female, 
  \code{ped[,6]} symptom status (2: symptomatic, 1: without symptoms, 0: missing),  
  \code{ped[,7:10]} discrete or ordinal observations,
  \code{NA} for missing and without symptoms,  
}
\examples{
data(ped.ordi)
}
