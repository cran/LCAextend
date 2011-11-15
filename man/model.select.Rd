\name{model.select}
\alias{model.select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ selects a latent class model for pedigree data }
\description{
  Performs selection of a latent class model for phenotypic measurements
  in pedigrees based on one of
  two possible methods: likelihood-based cross-validation or Bayesian
  Information Criterion (BIC) selection. This is the top-level
  function to perform a Latent Class Analysis (LCA), which calls the
  model fitting function
  \code{lca.model}. Model selection is performed among models within one of two
  types: with and without familial dependence. Two families of
  distributions are currently implemented: product multinomial for discrete (or
  ordinal) data and mutivariate 
  normal for continuous data.
}
\usage{
model.select(ped, distribution, trans.const = TRUE, optim.param,
             optim.probs.indic = c(TRUE, TRUE, TRUE, TRUE), 
             famdep = TRUE, selec = "bic", H = 5, K.vec = 1:7, 
             tol = 0.001, x = NULL, var.list = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ ped }{ a matrix containing variables coding the pedigree
    structure and the phenotype measurements: \code{ped[,1]} family ID, \code{ped[,2]} subjects ID, \code{ped[,3]} dad ID, 
  \code{ped[,4]} mom ID, \code{ped[,5]} sex, \code{ped[,6]} symptom
  status (2: symptomatic, 1: without symptoms, 0: missing), 
  \code{ped[,7:ncol(ped)]} measurements, each column corresponds to a
  phenotypic measurement, }
  \item{ distribution }{ a character variable taking the value \code{"normal"} for multivariate normal measurements and \code{"multinomial"} for ordinal or discrete 
  multinomial measurements, }
  \item{ trans.const }{ a logical variable indicating if the parental constraint is used. Parental constraint means that the class of a subject must be one 
  of his parents classes. Default is \code{TRUE}, }
  \item{ optim.param }{ a variable indicating how the measurement distribution parameter optimization is performed (see below for more details), }
  \item{ optim.probs.indic }{ a vector of logical values indicating which probability parameters to estimate (see below for more details), }
  \item{ famdep }{ a logical variable indicating if the familial dependence model is used or not. Default is \code{TRUE}. In models without familial dependence, individuals 
  are treated as independent and pedigree structure is meaningless. In models with familial dependence, a child class depends in his parents classes via a 
  triplet transition probability, }
  \item{ selec }{ a character variables taking the value \code{bic} if BIC selection is used and the value \code{cross} if cross-validation is used, }
  \item{ H }{ an integer giving the number of equal parts into which data
    will be splitted for the likelihood-based cross-validation model
    selection (see below for more details), }
  \item{ K.vec }{ a vector of integers, the number of latent classes of
    candidate models, if \code{K.vec} has one value, only models with that
    number of classes will be fitted, }
  \item{ tol }{ a small number governing the stopping rule of the EM algorithm. Default is 0.001, }
  \item{ x }{ a matrix of covariates (optional), default is \code{NULL}, }
  \item{ var.list }{ a list of integers indicating the columns of
    \code{x} containing the covariates to use for a given phenotypic measurement, default is \code{NULL}. }
}
\details{
  In the case of cross-validation based-likelihood method, data is
  splitted into \code{H} parts: \code{H-1} parts as a training set and one part as a 
  test set. For each model, a validation log-likelihood is obtained by
  evaluating the log-likelihood of the test set data using the parameter
  values estimated in the training set. This is repeated \code{H} times
  using a different part as training set each time, and a total
  validation log-likelihood is obtained by summation over the \code{H}
  test sets. The best model is the one having the largest 
  validation log-likelihood. In the case of BIC selection method, the
  BIC is computed for each candidate model. The model with the smallest
  BIC is selected.

  The symptom status vector (column 6 of \code{ped}) takes value 1 for
  subjects that have been
  examined and show no symptoms (i.e. completely unaffected
  subjects). When applying the LCA to
  measurements available on all subjects, the status vector must take the
  value of 2 for every individual with measurements. If covariates are used, covariate values must be provided for subjects with symptom status 0 (missing) but not for subjects with symptom status 1 (if covariate values are provided, they will be ignored).


    \code{optim.param} is a variable indicating how the measurement
  distribution parameter optimization of the M step is performed. Two
  possibilities, 
  \code{\link{optim.noconst.ordi}} and \code{\link{optim.const.ordi}}, are now available in the case of discrete or ordinal measurements, and four possibilities, 
  \code{\link{optim.indep.norm}} (measurements are independent, diagonal variance-covariance matrix), 
  \code{\link{optim.diff.norm}} (general variance-covariance matrix but equal for all classes), 
  \code{\link{optim.equal.norm}} (variance-covariance matrices are different for each class but equal variance and equal covariance for a class) and  
  \code{\link{optim.gene.norm}} (general variance-covariance matrices for all classes), in the case of continuous measurements. 
  One of the allowed values of \code{optim.param} must be entered without quotes.

  \code{optim.probs.indic} is a vector of logical values of length 4 for
  models with familial dependence and 2 for models without familial
  dependence indicating which probability parameters to estimate. See the
  help page for \code{\link{lca.model}} for a definition of the parameters. 

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
  All defaults are \code{TRUE}.
}
\value{
  The function returns a list of 5 elements, the first 3 elements are common for BIC and cross-validation model selection methods and are:

  \item{ param }{ the Maximum Likelihood Estimator (MLE) of the measurement distribution parameters of the selected model, }
  \item{ probs }{ the Maximum Likelihood Estimator (MLE) of the probability parameters of the selected model, }
  \item{ weight }{ an array of dimension \code{n} (the number of
    individuals) times 2 times \code{K+1} (\code{K} being the number of latent classes in the selected 
  model and the \code{K+1}th class being the unaffected class) giving
  the individual posterior probabilities. \code{weight[i,s,c]} is the
  posterior probability  
  that individual \code{i} belongs to class \code{c} when his affection
  status is \code{s}, where  
  \code{s} takes two values: 1 for symptomatic and 2 for without
  symptom. In particular, all \code{weight[,2,]} are 0 for symptomatic
  individuals and all \code{weight[,1,]} are  
  0 for individuals without symptoms. For missing individuals (unkown
  symptom status), both \code{weight[,1,]} and \code{weight[,2,]} may be
  greater than 0. }
  
  If the cross-validation selection method is used, the function returns also 
  \item{ ll }{ the value of the maximum log-likelihood (log-ML) of the selected model, }
  \item{ ll.valid }{  the total cross-validation log-likelihood of all candidate models, }
  and if the Bayesian Information Criterion selection method is used, the function returns also 
  \item{ ll }{ the value of maximum log-likelihood (log-ML) of all candidate models, }
  \item{ bic }{ the Bayesian Information Criterion
    \code{BIC=-2*log(ll)+m*log(n)} of all candidate models, where \code{m} is the number of free parameters of 
  the model and \code{n} the total number of individuals. }
}
\references{ TAYEB, A. LABBE, A., BUREAU, A. and MERETTE, C. (2011) Solving Genetic Heterogeneity in Extended
  Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 26(3): 539-560. DOI: 10.1007/s00180-010-0224-2, \cr
  
  LABBE, A., BUREAU, A. et MERETTE, C. (2009) Integration of Genetic Familial Dependence Structure in Latent Class Models. The International Journal of Biostatistics, 5(1): Article 6. }
\seealso{ See also \code{\link{lca.model}}. }
\examples{
#data
data(ped.cont)
fam <- ped.cont[,1]
#the function applied for the two first families of ped.cont
model.select(ped.cont[fam\%in\%1:2,],distribution="normal",trans.const=TRUE,
             optim.indep.norm,optim.probs.indic=c(TRUE,TRUE,TRUE,TRUE),
             famdep=TRUE,selec="bic",K.vec=1:3,tol=0.001,x=NULL,var.list=NULL)
}
