\name{lca.model}
\alias{lca.model} %- Also NEED an '\alias' for EACH other topic documented here.
\title{fits latent class models for phenotypic measurements in pedigrees
  with or without 
  familial dependence using an Expectation-Maximization (EM) algorithm }
\description{
  This is the main function for fitting latent class models. It performs some checks of the pedigrees (it exits if an individual has only one 
  parent in the pedigree, if no children is in the pedigree or if there
  are not enough individuals for parameters estimation) and of the
  initial values (positivity of probabilites and their summation to
  one). For models with familial dependence, the child latent class
  depends on his parents classes via 
  triplet-transition probabilities. In the case of models without
  familial dependence, it performs the classical Latent 
  Class Analysis (LCA) where all individuals are supposed independent 
  and the pedigree structure is meaningless. The EM algorithm stops when
  the difference between log-likelihood is smaller then \code{tol} that
  is fixed by the user. 
}
\usage{
lca.model(ped, probs, param, optim.param, fit = TRUE, 
optim.probs.indic = c(TRUE, TRUE, TRUE, TRUE), tol = 0.001, 
x = NULL, var.list = NULL, famdep = TRUE, modify.init = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ ped }{ a matrix or data frame representing pedigrees and measurements: \code{ped[,1]} family ID, \code{ped[,2]} subjects ID, \code{ped[,3]} dad ID, 
  \code{ped[,4]} mom ID, \code{ped[,5]} sex, \code{ped[,6]} symptom
  status (2: symptomatic, 1: without symptoms, 0: missing), 
  \code{ped[,7:ncol(ped)]} measurements, each column corresponds to a phenotypic measurement. If the measurement distribution specified with \code{optim.param} is multinomial, then these columns must either be of type \code{integer} of \code{factor},  }
  \item{ probs }{ a list of initial probability parameters (see below
    for more details). The function \code{\link{init.p.trans}} can be
    used to compute an initial value  
  of the component \code{p.trans} of \code{probs}, }
  \item{ param }{ a list of initial measurement distribution parameters (see below for more details). The function \code{\link{init.ordi}} can be used to compute an initial 
  value of \code{param} in the case of discrete or ordinal data (product
  multinomial distribution) and \code{\link{init.norm}} in the case of
  continous data (mutivariate normal distribution), }
  \item{ optim.param }{ a variable indicating how measurement distribution parameter optimization is performed (see below for more details), }
  \item{ fit }{ a logical variable, if \code{TRUE}, the EM algorithm is
    performed, if \code{FALSE}, only computation of weights and
    log-likelihood are performed with the initial parameter values
    without log-likelihood maximization, }
  \item{ optim.probs.indic }{ a vector of logical values indicating which probability parameters to estimate, }
  \item{ tol }{ a small number governing the stopping rule of the EM algorithm. Default is 0.001, }
  \item{ x }{ a matrix of covariates (optional), default is \code{NULL}, }
  \item{ var.list }{ a list of integers indicating the columns of
    \code{x} containing the covariates to use for a given phenotypic measurement, default is \code{NULL}, }
  \item{ famdep }{ a logical variable indicating if familial dependence model is used or not. Default is \code{TRUE}. In models without familial dependence, individuals 
  are treated as independent and pedigree structure is meaningless. In models with familial dependence, a child class depends in his parents classes via a 
  triplet-transition probability, }
  \item{ modify.init }{ a function to modify initial values of the EM algorithm, or \code{NULL}, default is \code{NULL}. }
}
\details{
  The symptom status vector (column 6 of \code{ped}) takes value 1 for
  subjects that have been
  examined and show no symptoms (i.e. completely unaffected
  subjects). When applying the LCA to
  measurements available on all subjects, the status vector must take the
  value of 2 for every individual with measurements.
  
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

  \code{param} is a list of measurement distribution parameters: the coefficients \code{alpha} (cumulative logistic coefficients see \code{\link{alpha.compute}}) in 
  the case of discrete or ordinal data, and means \code{mu} and variances-covariances matrices \code{sigma} in the case of continuous data,

  \code{optim.param} is a variable indicating how the measurement distribution parameter estimation of the M step is performed. Two possibilities, 
  \code{\link{optim.noconst.ordi}} and \code{\link{optim.const.ordi}}, are now available in the case of discrete or ordinal measurements, and four possibilities 
  \code{\link{optim.indep.norm}} (measurements are independent, diagonal variance-covariance matrix), 
  \code{\link{optim.diff.norm}} (general variance-covariance matrix but equal for all classes), 
  \code{\link{optim.equal.norm}} (variance-covariance matrices are different for each class but equal variance and equal covariance for a class) and  
  \code{\link{optim.gene.norm}} (general variance-covariance matrices for all classes), are now available in the case of continuous measurements, 
  One of the allowed values of \code{optim.param} must be entered without quotes. 

  \code{optim.probs.indic} is a vector of logical values of length 4 for
  models with familial dependence and 2 for models without familial
  dependence.
  
  For models with familial dependence:
  \describe{
  \item{\code{optim.probs.indic[1]}}{ indicates whether \code{p0} will be estimated or not, }
  \item{\code{optim.probs.indic[2]}}{ indicates whether \code{p0connect} will be estimated or not, }
  \item{\code{optim.probs.indic[3]}}{ indicates whether \code{p.found} will be estimated or not, }
  \item{\code{optim.probs.indic[4]}}{ indicates whether \code{p.connect} will
    be estimated or not. }
  } 

  For models without familial dependence:
  \describe{
  \item{\code{optim.probs.indic[1]}}{ indicates whether \code{p0} will be estimated or not, }
  \item{\code{optim.probs.indic[2]}}{ indicates whether \code{p.aff} will be
    estimated or not. }
  }
  All defaults are \code{TRUE}. If the dataset contains only nuclear families, there is no information to estimate p0connect and p.connect, and these parameters will not be estimated, irrespective of the indicator value.
}
\value{
  The function returns a list of 4 elements:
  \item{ param }{ the Maximum Likelihood Estimator (MLE) of the
    measurement  distribution parameters if \code{fit=TRUE} or the input
    \code{param} if \code{fit=FALSE}, } 
  \item{ probs }{ the MLE of probability parameters if \code{fit=TRUE} or the input \code{probs} if \code{fit=FALSE}, }
  
   When measurements are available on all subjects, the probability parameters \code{p0} and \code{p0connect} are degenerated to 0 and 
   \code{p.afound}, \code{p.child} and \code{p.aff} to 1 in the output. 
  
  \item{ weight }{ an array of dimension \code{n} (the number of individuals) times 2 times \code{K+1} (\code{K} being the number of latent classes in the selected 
  model and the \code{K+1}th class being the unaffected class) giving
  the individual posterior probabilities. 
  \code{weight[i,s,c]} is the
  posterior probability  
  that individual \code{i} belongs to class \code{c} when his symptom
  status is \code{s}, where  
  \code{s} takes two values: 1 for symptomatic and 2 for without
  symptom. In particular, all \code{weight[,2,]} are 0 for symptomatic
  individuals and all \code{weight[,1,]} are  
  0 for individuals without symptoms. For missing individuals (unkown
  symptom status), both \code{weight[,1,]} and \code{weight[,2,]} may be
  greater than 0. }
  
  \item{ ll }{ the maximum log-likelihood value (log-ML) if \code{fit=TRUE} or the log-likelihood computed with the input values of \code{param} 
  and \code{probs} if \code{fit=FALSE}, } 
}
\references{ TAYEB, A. LABBE, A., BUREAU, A. and MERETTE, C. (2011) Solving Genetic Heterogeneity in Extended
  Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 26(3): 539-560. DOI: 10.1007/s00180-010-0224-2, \cr

  LABBE, A., BUREAU, A. et MERETTE, C. (2009) Integration of Genetic Familial Dependence Structure in Latent Class Models. The International Journal of Biostatistics, 5(1): Article 6. }

\examples{
#data
data(ped.ordi)
fam <- ped.ordi[,1]
#probs and param
data(param.ordi)
data(probs)
#the function applied only to two first families of ped.ordi
lca.model(ped.ordi[fam\%in\%1:2,],probs,param.ordi,optim.noconst.ordi,
          fit=TRUE,optim.probs.indic=c(TRUE,TRUE,TRUE,TRUE),tol=0.001,x=NULL,
          var.list=NULL,famdep=TRUE,modify.init=NULL)
}
