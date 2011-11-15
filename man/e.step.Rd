\name{e.step}
\alias{e.step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ performs the E step of the EM algorithm for a single pedigree for both cases with and without familial dependence}
\description{
  computes triplet and individual weights the E step of the EM algorithm
  for all pedigrees in the data, in both cases with and without familial
  dependence. This is an internal
  function not meant to be called by the user.
}
\usage{
e.step(ped, probs, param, dens, peel, x = NULL, var.list = NULL, 
       famdep = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ ped }{ a matrix representing pedigrees and measurements: \code{ped[,1]} family ID, \code{ped[,2]} subjects ID, \code{ped[,3]} dad ID, 
  \code{ped[,4]} mom ID, \code{ped[,5]} sex, \code{ped[,6]} symptom status: (2: symptomatic, 1: without symptoms, 0: missing), 
  \code{ped[,7:ncol(ped)]} measurements, each column corresponds to a phenotypic measurement, }
    \item{ probs }{ a list of probability parameters of the model, see below for more details, }
  \item{ param }{ a list of measurement distribution parameters of the model, see below for more details, }
  \item{ dens }{ distribution of the mesurements, used in the model (multinormal, multinomial,...)}
  \item{ peel }{ a list of pedigree peeling containing connectors by peeling order and couples of parents, }
  \item{ x }{ covariates, if any. Default is \code{NULL}, }
  \item{ var.list }{ a list of integers indicating which covariates (taken from \code{x}) are used for a given type of measurement. Default is \code{NULL}, }
  \item{ famdep }{ a logical variable indicating if familial dependence model is used or not. Default is \code{TRUE}. In models without familial dependence, individuals 
  are treated as independent and pedigree structure is meaningless. In models with familial dependence, a child class depends in his parents classes via a 
  triplet-transition probability, }
}
\details{
 \code{probs } is a list of initial probability parameters:
  
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

  \code{param} is a list of measurement density parameters: the coefficients \code{alpha} (cumulative logistic coefficients see \code{\link{alpha.compute}}) in 
  the case of discrete or ordinal data, and means \code{mu} and variances-covariances matrices \code{sigma} in the case of continuous data,
}
\value{
  The function returns a list of 3 elements:
  \item{ ww }{ triplet posterior probabilities, an array of \code{n} (the number of individuals) times 2 times \code{K+1} times \code{K+1} times \code{K+1}, 
  where \code{K} is the total number of latent classes of the model. For an individual \code{i}, the triplet probability \code{ww[i,s,c,c_1,c_2]} is the 
  posterior probability that individual \code{i} belongs to class \code{c} when his symptom status is \code{s} and given that his parents classes are \code{c_1} 
  and \code{c_2}, where \code{s} takes two values 1 for affected and 2 for unaffected. In particular, all \code{ww[,2,,,]} are zeros for affected individuals and 
  all \code{ww[,1,,,]} are zeros for unaffected individuals. For missing individuals (unkown symptom status), both \code{ww[,1,,,]} and \code{ww[,2,,,]} are full,}

  \item{ w }{ individual posterior probabilities, an array of \code{n} times 2 times \code{K+1} , where \code{n} is the number of individuals and is such that 
  \code{w[i,s,c]} is the posterior probability that individual \code{i} belongs to class \code{c} when his symptom status is \code{s}, where 
  \code{s} takes two values 1 for affected and 2 for unaffected. In particular, all \code{w[,2,]} are zeros for affected individuals and all \code{w[,1,]} are 
  zeros for unaffected individuals. For missing individuals (unkown symptom status), both \code{w[,1,]} and \code{w[,2,]} are full, }

  \item{ ll }{ log-likelihood of the considered model and parameters. }
}
\references{ TAYEB et al.: Solving Genetic Heterogeneity in Extended Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2. }

\seealso{ See also \code{\link{weight.famdep}}, \code{\link{lca.model}}. }
\examples{
#data
data(ped.cont)
data(peel)
#probs and probs
data(probs)
data(param.cont)
#the function
e.step(ped.cont,probs,param.cont,dens.norm,peel,x=NULL,var.list=NULL,
       famdep=TRUE)
}
