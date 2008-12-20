\name{LCAextend-package}
\alias{LCAextend-package}
\alias{LCAextend}
\docType{package}
\title{
Latent Class Analysis (LCA) and model selection for pedigree data
}
\description{
Description: This package performs a Latent Class Analysis of phenotypic measurements in pedigrees and a model selection based on one of two methods: likelihood-based cross-validation and Bayesian Information Criterion. It computes also individual and triplet child-parents weights in a pedigree using an upward-downward algorithm. It takes into account the familial dependence defined by the pedigree structure by considering that a class of a child depends on his parents classes via triplet-transition probabilities of the classes. The package handles the case where measurements are available on all subjects and the case where measurements are available only on symptomatic (i.e. affected) subjects. Distributions for discrete (or ordinal) and continous data are currently implemented. The package can deal with missing data.
}
\details{
\tabular{ll}{
Package: \tab LCAextend\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-09-25\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}

}
\author{
Arafat TAYEB <arafat.tayeb@crulrg.ulaval.ca>, Alexandre BUREAU <alexandre.bureau@msp.ulaval.ca> and Aurelie Labbe <aurelie.labbe@mcgill.ca> \cr
Maintainer: Arafat TAYEB <arafat.tayeb@crulrg.ulaval.ca>
}
\references{TAYEB, A. et al.: Solving Genetic Heterogeneity in Extended
  Families by Identifying Sub-types of Complex Diseases. Computational Statistics, 2011, DOI: 10.1007/s00180-010-0224-2, \cr
  
  LABBE, A., BUREAU, A. et MERETTE, C. (2009). Integration of Genetic Familial Dependence Structure in Latent Class Models. The International Journal of Biostatistics, 5(1): Article 6. 

}
\keyword{ package }
%\seealso{
%}
\examples{
#data
data(ped.cont)
fam <- ped.cont[,1]
#the function applied for the two first families of ped.cont
res <- model.select(ped.cont[fam\%in\%1:2,],distribution="normal",trans.const=TRUE,
             optim.indep.norm,optim.probs.indic=c(TRUE,TRUE,TRUE,TRUE),
             famdep=TRUE,selec="bic",K.vec=1:3,tol=0.001,x=NULL,var.list=NULL)
plot(1:3,res$bic,type="l",col="blue",xlab="K",ylab="BIC",main="model 
    selection using BIC")
}
