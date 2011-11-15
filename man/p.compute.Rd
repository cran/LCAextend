\name{p.compute}
\alias{p.compute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ computes the probability vector using logistic coefficients}
\description{
  computes the probability vector using cumulative logistic coefficients
}
\usage{
p.compute(alpha,decal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{ a vector of cumulative logistic coefficients, the first value can
    be \code{-Inf}, followed by, eventually, only one negative value, 
  then only positive values. It can end by \code{Inf} values.}
  \item{decal}{ offset term to be applied to sums of logistic coefficients}
}
\details{
  If \code{alpha} has \code{S-1} values, \code{p.compute} returns \code{p} of length \code{S}. If \code{Y} is a random variable taking values in \code{\{1,...,S\}} with 
  probabilities \code{p}, coefficients \code{alpha[i]} are given by:
  
  \deqn{ p_1+...+p_i=P(Y\leq i)=\frac{\exp(\alpha_1+...+\alpha_i)}{(1+\exp(\alpha_1+...+\alpha_i)} }{%
  p[1]+...+p[i]=P(Y<=i)=exp(alpha[1]+...+alpha[i])/(1+exp(alpha[1]+...+alpha[i]))}
  
  for all \code{i<=S-1}.
}
\value{
  \code{p}:  a probability vector 
}
\seealso{ \code{p.compute} is the inverse function of \code{\link{alpha.compute}} }
\examples{
# a vector of probability
p <- c(0,0.2,0,0,0.3,0.4,0.1,0,0)
alpha <- alpha.compute(p)
#gives alpha= -Inf -1.38 0 0 1.38 0 2.19 Inf Inf
p.compute(alpha) #gives p
}
