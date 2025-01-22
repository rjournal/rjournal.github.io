### Prediction, Bootstrapping and Monte Carlo Analyses 
### Based on Linear Mixed Models with QAPE 2.0 Package
### Alicja Wolny-Dominiak, Tomasz Zadlo


# PACKAGES AND DATASET --------------------------------------------------------


update.packages("qape")
update.packages("dplyr")
update.packages("lme4")
update.packages("HLMdiag")
update.packages("psych")

library(qape)
library(dplyr)
library(lme4)
library(HLMdiag)  # radon data
library(psych) # geometric mean 


# RADON DATA AND THE MODEL ----------------------------------------------------


# We use the following model for radon data
# with two correlated random effects:

Ypop <- radon$log.radon
radon.model <-
  lmer(log.radon ~ basement + uranium + (basement |
                                           county), data = radon)

# the lack of normality of random effects and random components:
normCholTest(radon.model, shapiro.test) 


# EXAMPLE 1 -------------------------------------------------------------------


## Input arguments ------------------------------------------------------------

# It is assumed that observations from county 26 
# from the first floor are unavailable.
con <- rep(1, nrow(radon))
con[radon$county == 26 & radon$basement == 1] <- 0

fixed.part <- 'basement + uranium'
random.part <- '(basement|county)' 
reg <- select(radon, -log.radon) # population matrix of auxiliary variables
p <- c(0.75, 0.9) # orders of Quantiles of Absolute Prediction Error
estMSE <- TRUE # to compute the naive MSE estimator of the EBLUP

# for prediction of the mean in county 26 using the EBLUP
gamma <-
  (1 / sum((radon$county == 26))) * ifelse((radon$county == 26), 1, 0)

# for prediction of the arithmetic mean, geometric mean and median 
# in county 26 using the plug-in predictor
thetaFun <- function(x) {
  c(mean(x[radon$county == 26]), psych::geometric.mean(x[radon$county == 26]),
    median(x[radon$county == 26]))
  }

backTransExp <- function(x) exp(x)

# observations of the variable of interest assumed to be available
YS <- Ypop[con == 1]


## Predictors -----------------------------------------------------------------


### EBLUP predictor -----------------------------------------------------------

myeblup <- EBLUP(YS, fixed.part, random.part, reg, con, gamma,
                 weights = NULL, estMSE)
class(myeblup)
# the value of the predictor of the arithmetic mean of 
# logarithms of radon measurements:
myeblup
myeblup$thetaP
myeblup$neMSE # the value of the naive MSE estimator of the EBLUP
print(myeblup)
summary(myeblup)

### PLUG-IN predictor ---------------------------------------------------------

myplugin <- plugInLMM(YS, fixed.part, random.part, reg, con, weights = NULL, 
                    backTrans = backTransExp, thetaFun)
class(myplugin)
# values of the predictor of the arithmetic mean, geometric mean
# and median of radon measurements:
myplugin
myplugin$thetaP 
print(myplugin)
summary(myplugin)


# EXAMPLE 2 -------------------------------------------------------------------


B <- 5 # number of bootstrap iterations

## Residual bootstrap ---------------------------------------------------------

# accuracy measures estimates based on the residual bootstrap 
set.seed(1056) 
residBoot <- bootRes(myplugin, B, p, correction = TRUE)
# values of estimated RMSEs of the predictor of three characteristics:  
# the arithmetic mean, geometric mean and median of radon measurements:
residBoot$estRMSE
# values of estimated QAPEs (of order 0.75 in the first row, 
# and of order 0.9 in the second row) of the predictor of three characteristics:   
# the arithmetic mean, geometric mean and median of radon measurements:
residBoot$estQAPE


# EXAMPLE 2 (ADDITIONAL RESULTS) ----------------------------------------------


## Parametric bootstrap -------------------------------------------------------

# parametric bootstrap accuracy measures estimates:
# (We present the code for illustrative purposes only. The parametric bootstrap
# should not be used because the normality assumption is not met.)
set.seed(1056) 
paramBoot <- bootPar(myplugin, B, p)
paramBoot$estRMSE
paramBoot$estQAPE

## Parallel computing ---------------------------------------------------------

### Residual bootstrap --------------------------------------------------------

# accuracy measures estimates based on 
# the residual bootstrap with the correction procedure
# (parallel computing is applied):
set.seed(1056)
residBootFuture <- bootResFuture(myplugin, B, p, correction = TRUE)
residBootFuture$estRMSE
residBootFuture$estQAPE

### Parametric bootstrap ------------------------------------------------------

# parametric bootstrap accuracy measures estimates 
# (parallel computing is applied):
# (We present the code for illustrative purposes only. The parametric bootstrap
# should not be used because the normality assumption is not met.)
set.seed(1056) 
paramBootFuture <- bootParFuture(myplugin, B, p)
paramBootFuture$estRMSE
paramBootFuture$estQAPE


# EXAMPLE 3 -------------------------------------------------------------------


## Input arguments ------------------------------------------------------------

# Let us define a misspecified predictor:
fixed.part.mis <- '1'
random.part.mis <- '(1|county)'
myplugin.mis <- plugInLMM(YS, fixed.part.mis, random.part.mis, reg, con, 
                        weights = NULL, backTrans = backTransExp, thetaFun)

## Residual bootstrap ---------------------------------------------------------

set.seed(1056)
residBootMis <- bootResMis(myplugin, myplugin.mis, B, p, correction = TRUE)
# residual bootstrap with the correction RMSE estimators 
# of 'plugin' of: the arithmetic mean, geometric mean and median
# of radon measurements in county 26: 
residBootMis$estRMSElmm
# residual bootstrap with the correction RMSE estimators 
# of 'plugin.mis' of: the arithmetic mean, geometric mean and median
# of radon measurements in county 26: 
residBootMis$estRMSElmmMis
# residual bootstrap with the correction QAPE estimators of order 0.75 and 0.9 
# of 'plugin' of: the arithmetic mean, geometric mean and median
# of radon measurements in county 26:
residBootMis$estQAPElmm
# residual bootstrap with the correction QAPE estimators of order 0.75 and 0.9 
# of 'plugin.mis' of: the arithmetic mean, geometric mean and median
# of radon measurements in county 26: 
residBootMis$estQAPElmmMis


# EXAMPLE 3 (ADDITIONAL RESULTS) ----------------------------------------------


## Parametric bootstrap -------------------------------------------------------

# (We present the code for illustrative purposes only. The parametric bootstrap
# should not be used because the normality assumption is not met.)
set.seed(1056) 
paramBootMis <- bootParMis(myplugin, myplugin.mis, B, p)
# parametric bootstrap RMSE estimators of 'plugin' 
# of 'plugin' of: the arithmetic mean, geometric mean and median
# of radon measurements in county 26: 
paramBootMis$estRMSElmm
# parametric bootstrap RMSE estimators of 'plugin.mis' 
# of 'plugin.mis' of: the arithmetic mean, geometric mean and median
# of radon measurements in county 26: 
paramBootMis$estRMSElmmMis
# parametric bootstrap QAPE estimators of order 0.75 and 0.9 of 'plugin' of 
# the arithmetic mean, geometric mean and median of 
# radon measurements in county 26:
paramBootMis$estQAPElmm
# parametric bootstrap QAPE estimators of order 0.75 and 0.9 of 'plugin.mis' of
# the arithmetic mean, geometric mean and median of
# radon measurements in county 26:
paramBootMis$estQAPElmmMis


# EXAMPLE 4 -------------------------------------------------------------------


## Input arguments ------------------------------------------------------------

predictorLMMmis <- myplugin # to define the model
predictorLMM <- myplugin # which properties are assessed in the simulation study
predictorLMM2 <- myplugin  # which properties are assessed in the sim. study
K <- 5 # the number of MC iterations
# diag. elements of the covariance matrix of random components are divided by:
ratioR <- 1
# diag. elements of the covariance matrix of random effects are divided by:
ratioG <- 1

## Monte Carlo analysis -------------------------------------------------------

set.seed(1086)
MC <- mcLMMmis(Ypop, predictorLMMmis, predictorLMM, predictorLMM2, 
                       K, p, ratioR, ratioG)

# relative bias of 'predictorLMM' 
# of the arithmetic mean, geometric mean and median in county 26 (in %):
MC$rBlmm
# relative RMSE of 'predictorLMM' 
# of the arithmetic mean, geometric mean and median in county 26 (in %):
MC$rRMSElmm
# QAPE of order 0.75 and 0.9 of 'predictorLMM' 
# of the arithmetic mean, geometric mean and median in county 26:
MC$QAPElmm


# EXAMPLE 4 (ADDITIONAL RESULTS) ----------------------------------------------


## Input arguments ------------------------------------------------------------

# Let us define another predictor:
fixed.part.mis2 <- 'uranium'
random.part.mis2 <- '(1|county)'
myplugin.mis2 <- plugInLMM(YS, fixed.part.mis2, random.part.mis2, reg, con, 
                          weights = NULL, backTrans = backTransExp, thetaFun)

predictorLMMmis <- myplugin # to define the model
predictorLMM <- myplugin.mis # which properties are assessed in the MC study
predictorLMM2 <- myplugin.mis2  # which properties are assessed in the MC study
K <- 5 # the number of MC iterations
# diag. elements of the covariance matrix of random components are divided by:
ratioR <- 1
# diag. elements of the covariance matrix of random effects are divided by:
ratioG <- 1

## Monte Carlo analysis -------------------------------------------------------

set.seed(1086)
MC2 <- mcLMMmis(Ypop, predictorLMMmis, predictorLMM, predictorLMM2, 
                        K, p, ratioR, ratioG)

# relative bias of 'predictorLMM' 
# of the arithmetic mean, geometric mean and median in county 26 (in %):
MC2$rBlmm
# relative bias of 'predictorLMM2' 
# of the arithmetic mean, geometric mean and median in county 26 (in %):
MC2$rBlmm2
# relative RMSE of 'predictorLMM' 
# of the arithmetic mean, geometric mean and median in county 26 (in %):
MC2$rRMSElmm
# relative RMSE of 'predictorLMM2' 
# of the arithmetic mean, geometric mean and median in county 26 (in %):
MC2$rRMSElmm2
# QAPE of order 0.75 and 0.9 of 'predictorLMM' 
# of the arithmetic mean, geometric mean and median in county 26:
MC2$QAPElmm
# QAPE of order 0.75 and 0.9 of 'predictorLMM2' 
# of the arithmetic mean, geometric mean and median in county 26:
MC2$QAPElmm2