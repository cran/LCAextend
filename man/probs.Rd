\name{probs}
\alias{probs}
\docType{data}
\title{ probabilities parameters to be used for examples }
\description{
  a list of probability parameters such as the probability that a
  founder is assigned to each class, the transition probabilities and the probability that a child is symptomatic. 
}
\usage{data(probs)}
%\format{}
\details{
  \code{probs } a list of probability parameters: 

  For models with familial dependence:
  \describe{
    \item{\code{p}}{ a probability vector, each \code{p[c]} is the probability that an symptomatic founder is in class  \code{c} for \code{c>=1}, }
  \item{\code{p0}}{ the probability that a founder without symptoms is in class 0, }
  \item{\code{p.trans}}{ an array of dimension \code{K} times \code{K+1}
    times \code{K+1}, where \code{K} is the number of latent classes of
    the model, and is such that \code{p.trans[c_i,c_1,c_2]} is the
    conditional probability that a symptomatic individual
  \code{i} is in class \code{c_i} given that his parents are in classes
  \code{c_1} and \code{c_2}, }
  \item{\code{p0connect}}{ a vector of length \code{K}, where
    \code{p0connect[c]} is the probability that a connector without
    symptoms is in class \code{0}, 
  given that one of his parents is in class \code{c>=1} and the other in class 0, }
  \item{\code{p.found}}{ the probability that a founder is symptomatic, }
  \item{\code{p.child}}{ the probability that a child is symptomatic, }
}

  For models without familial dependence, all individuals are independent:
  \describe{
  \item{\code{p}}{ a probability vector, each \code{p[c]} is the probability that an symptomatic individual is in class \code{c} for \code{c>=1}, }
  \item{\code{p0}}{ the probability that an individual without symptoms is in class 0, }
  \item{\code{p.aff}}{ the probability that an individual is symptomatic, }
}
}
\examples{
data(probs)
}
