\name{attrib.dens}
\alias{attrib.dens}
\title{ associates to a function of density parameter optimization an attribute to distinguish between ordinal and normal cases }
\description{ associates to a function of density parameter optimization an attribute to distinguish between ordinal and normal cases. This is an internal function not meant to be called by the user. }
\usage{
attrib.dens(optim.param)
}
\arguments{
  \item{optim.param}{the function used to estimate the parameters of the measurements. }
}
\details{
Available \code{optim.param} functions are \code{\link{optim.noconst.ordi}}, \code{\link{optim.const.ordi}} for ordinal measurements and \code{\link{optim.indep.norm}}, \code{\link{optim.diff.norm}}, \code{\link{optim.equal.norm}} and \code{\link{optim.gene.norm}} for multinormal measurements. The attribution uses the internal function \code{attr} and the attribute name used is \code{type}. The user can make his own \code{optima.param} function and has to associate an attribute \code{type} to it to be used instead of the available ones.
}
\value{
  The function returns the same \code{optim.param} with an attribute \code{type} taking values in \code{ordi} or \code{norm}.
}
\examples{
optim.param <- optim.indep.norm
optim.param <- attrib.dens(optim.param)
}
