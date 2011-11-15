\name{param.cont}
\alias{param.cont}
\docType{data}
\title{ parameters to be used for examples in the case of continuous measurements }
\description{
  means and variance-covariance matrices for each class to be used in examples with continuous measurements.
}
\usage{data(param.cont)}
\details{
  \code{ped.param} is a list of 2 elements:
\describe{
  \item{\code{mu}}{ a list of \code{K=3} (the number of latent classes) entries, each represents the means of the measurement multinormal density in the class, }
  \item{\code{sigma}}{ a list of \code{K=3} entries, each is the
  variance-covariance matrix of the measurement multinormal density in the class. }}
The dimension (the number of multinormal measurements) used in the dataset is 4.
}
\seealso{
See also \code{\link{init.norm}}
}
\examples{
data(param.cont)
}
