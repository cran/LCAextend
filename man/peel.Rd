\name{peel}
\alias{peel}
\docType{data}
\title{ peeling order of pedigrees and couples in pedigrees }
\description{
  \code{peel} is a list of 48 entries, each gives the peeling order of the pedigrees and lists the couples in the 48 pedigrees of \code{ped.cont} and \code{peed.ordi}.
}
\usage{data(peel)}
\value{
  For a pedigree \code{f} in the data \code{ped.cont} or \code{ped.ordi},
  \code{peel[[f]]} is a list of three entries:
\describe{
  \item{\code{generation}}{ the number of generations in the pedigree, }
  \item{\code{peel.connect}}{ a matrix of \code{generation} rows, each giving the connectors of the generation in the order of peeling, }
  \item{\code{couple}}{ a matrix of two columns, giving the couples in the pedigree. }
 }
}
\seealso{
See also \code{\link{ped.cont}} and \code{\link{ped.ordi}}.
}
\examples{
data(peel)
}
