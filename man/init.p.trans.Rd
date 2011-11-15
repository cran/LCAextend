\name{init.p.trans}
\alias{init.p.trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ initializes the transition probabilities }
\description{
  initializes the marginal transition probabilities with or without parental constraint.
}
\usage{
init.p.trans(K, trans.const = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ K }{ number of latent classes, }
  \item{ trans.const }{ a logical variable indicating if the parental constraint is used. Parental constraint means that the class of a subject can be only one 
  of his parents classes. Default is \code{TRUE}. }
}
\details{
All non-zero transition probabilities are set to be equal. The
parental constraint indicator determines which transition probabilities
are non-zero.
}
\value{
  the function returns \code{p.trans} an array of dimension \code{K} times \code{K+1} times \code{K+1}: \code{p.trans[c_i,c_1,c_2]} is the probability that 
  the subject \code{i} is assigned to class \code{c_i} and his parents to classes \code{c_1} and \code{c_2}.
}

\examples{
init.p.trans(3) #parental constraint is TRUE,
init.p.trans(3,trans.const=FALSE) #parental constraint is FALSE.
}
