\name{alpha.compute}
\alias{alpha.compute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes cumulative logistic coefficients using probabilities }
\description{
  computes cumulative logistic coefficients using probabilities.
}
\usage{
alpha.compute(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{a vector of probabilities (positive entries summing to 1). }
}
\details{
  If \code{p} has one value (equal to 1) \code{alpha.compute} returns \code{NA}, if it has \code{S (S>=2)} values, \code{alpha.compute} returns \code{S-1} coefficients 
  \code{alpha} such that if \code{Y} is a random variable taking values in \code{{1,...,S}} with probabilities \code{p}, coefficients \code{alpha[i]} are given by:
  
  \deqn{ p_1+...+p_i=P(Y\leq i)=\frac{\exp(\alpha_1+...+\alpha_i)}{1+\exp(\alpha_1+...+\alpha_i)},  }{%
  \code{p[1]+...+p[i]=P(Y<=i)=exp(alpha[1]+...+alpha[i])/(1+exp(alpha[1]+...+alpha[i]))}, }
  
  for all \code{i<=S-1}.
}
\value{
  The function returns \code{alpha}:  a vector of \code{S-1} cumulative logistic coefficients.
}

\seealso{ \code{alpha.compute} is the inverse function of \code{\link{p.compute}} }
\examples{
# a vector of probability
p <- c(0,0.2,0,0,0.3,0.4,0.1,0,0)
alpha.compute(p)
#gives -Inf -1.38 0 0 1.38 0 2.19 Inf Inf
p.compute(alpha.compute(rep(1/5,5)))
}
