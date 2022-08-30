###############################################################
## This file contains the code used in the paper "garchx:
## Flexible and Robust GARCH-X Modelling" by Genaro Sucarrat.
##
## First created 12 April 2020, Oslo.
##
## Contents:
##
## Section 2: The garchx package
## Section 3: Large sample properties
## Section 4: A speed comparison
##
## Note: R version 3.6.3 used throughout.
##
###############################################################


###############################################################
## Section 2: The garchx package
###############################################################

##load data used for illustration
##===============================

library(xts)
data(spyreal, package="rugarch")
eps <- spyreal[,"SPY_OC"]*100
plot(eps)

##illustrate garchx function
##==========================

library(garchx)
garchx(eps)

##s3 methods
##==========

mymod <- garchx(eps)
coef(mymod)       #coefficient estimates
fitted(mymod)     #fitted conditional variance
logLik(mymod)     #log-likelihood (i.e., not the average log-likelihood)
nobs(mymod)       #no. of observations
predict(mymod)    #generate predictions of the conditional variance
print(mymod)      #print of estimation result
quantile(mymod)   #fitted quantile(s), the default corresponds to 97.5% value-at-risk
residuals(mymod)  #standardized residuals
toLatex(mymod)    #generate LaTeX code
vcov(mymod)       #coefficient covariance

##order argument
##==============
	
garchx(eps, order=c(1,1,1))  #garch(1,1) w/asymmetry
garchx(eps, order=c(1,2))    #garch(2,1)

##xreg argument
##=============

x <- spyreal[,"SPY_RK"]*100
xlagged <- lag(x)  #note: this lags, since x is an xts object
xlagged[1] <- 0    #replace NA-value with 0

garchx(eps, xreg=xlagged)

##vcov.type argument
##==================

garchx(eps, xreg=xlagged, vcov.type="robust")

mymod <- garchx(eps, xreg=xlagged)
vcov(mymod, vcov.type="robust")


##inference under nullity
##=======================

mymod <- garchx(eps, xreg=xlagged)
ttest0(mymod, k=2)
ttest0(mymod)

r <- cbind(c(0,0))
R <- rbind(c(0,1,0,0),c(0,0,1,0))
waldtest0(mymod, r = r, R = R)


##zero-coefficient restrictions via omission
##==========================================

garchx(eps, order=c(0,0), arch=1, garch=1)
garchx(eps, asym=1)
garchx(eps, garch=0)

garchx(eps, arch=c(1,2), garch=c(1,2))
garchx(eps, arch=2, garch=2)

garchx(eps, arch=0, xreg=xlagged)

garchx(eps, arch=0)


##numerical optimisation
##======================

mymod <- garchx(eps, asym = 1)
mymod$initial.values


###############################################################
## Section 3: Large sample properties
###############################################################

## Clean workspace and load required R packages
## The garchxAvar function
## Bias and standard error of estimates

## Clean workspace and load required R packages
##=============================================

##clean workspace:
rm(list = ls())

library(tseries)
library(fGarch)
library(rugarch)
library(garchx)


## The garchxAvar function
##========================

##note: the following exercise is similar to the one
##undertaken in Francq and Zakoian (2019, pp. 180-181)

##simulate epsilon:
n <- 10000000
omega <- 1; alpha1 <- 0.1
set.seed(123)
eta <- rnorm(n)
eps <- garchxSim(n, intercept = omega, arch = alpha1, garch = NULL,
	innovations = eta)

##compute asymptotic coefficient covariance:
epslagged2 <- eps[-length(eps)]^2
epslagged4 <- epslagged2^2
J <- matrix(NA, 2, 2)
J[1,1] <- mean( 1/( (omega+alpha1*epslagged2)^2 ) )
J[1,2] <- mean( epslagged2/( (omega+alpha1*epslagged2)^2 ) )
J[2,1] <- J[1,2]
J[2,2] <- mean( epslagged4/( (omega+alpha1*epslagged2)^2 ) )
Eeta4 <- 3
Avar1 <- (Eeta4-1)*solve(J)
Avar1

##garchxAvar function:
Avar2 <- garchxAvar(c(omega,alpha1), arch=1, Eeta4=3, n=n,
  innovations=eta)
Avar2

##relative comparison:
Avar2/Avar1


## Bias and standard error of estimates
##=====================================

##asymptotic coefficient-covariances
##----------------------------------

n <- 10000000
pars <- c(0.2, 0.1, 0.8)
set.seed(123)
##normal
AvarNormal <- garchxAvar(pars, arch=1, garch=1, Eeta4=3, n=n)
AvarNormal
##t(5):
eta <- rt(n, df=5)/sqrt(5/3)
Avart5 <- garchxAvar(pars, arch=1, garch=1, Eeta4=9, n=n,
  innovations=eta)
Avart5


##ase associated with T=10000
##---------------------------

sqrt( diag(AvarNormal/10000) )
sqrt( diag(Avart5/10000) )

##Monte Carlo simulations
##-----------------------

##Note: the code below undertakes the Monte Carlo simulations.
##The results from those appear in the paper under two different
##subsections entitled, respectively:
## - "Bias and standard errors of estimates"
## - "Coefficient-covariance estimate"
##Note: for tseries, the results in "Bias and ..." are with the
##default initial parameter values, whereas the results in
##"Coefficient-covariance estimate" are with the non-default
##initial parameter values.

##setup:
iReps <- 1000 #no. of replications
iT <- 10000 #no. of observations
omega <- 0.2; alpha1 <- 0.1; beta1 <- 0.8
etaDGP <- "N" #"N" or "t"
package <- "tseries" #"tseries", "fGarch", "rugarch" or "garchx"

##parameter matrix:
mPars <- matrix(NA, iReps, 3)
colnames(mPars) <- c("omega", "alpha1", "beta1")

##vcov array:
aAvar <- array(dim=c(3,3,iReps))

##loop:
for(i in 1:iReps){

  ##simulate:
  if( etaDGP=="N" ){ eta <- rnorm(iT) }
  if( etaDGP=="t" ){ eta <- rt(iT, df=5)/sqrt(5/3) }
  eps <- garchxSim(iT, intercept=omega, arch=alpha1, garch=beta1,
    innovations=eta)

  ##tseries:
  if( package=="tseries" ){
    mymod <- garch(eps)
##simulations with non-default initial parameter values:
#    mymod <- garch(eps, control=garch.control(start=c(0.1,0.1,0.7)))
    mPars[i,] <- coef(mymod)
    aAvar[,,i] <- vcov(mymod)*iT
  }

  ##fGarch:
  if( package=="fGarch" ){
    mymod <- garchFit(include.mean=FALSE, data=eps, cond.dist="QMLE")
    mPars[i,] <- mymod@fit$matcoef[,1]
    aAvar[,,i] <- mymod@fit$cvar*iT
  }

  ##rugarch:
  if( package=="rugarch" ){
    spec <-
      ugarchspec( mean.model=list(armaOrder=c(0,0), include.mean=FALSE))
    mymod <- ugarchfit(data=eps, spec=spec)
##with non-default solver:
#    mymod <- ugarchfit(data=eps, spec=spec, solver="nlminb")
    if(!is.null(coef(mymod))){
      mPars[i,] <- coef(mymod)
      aAvar[,,i] <- mymod@fit$robust.cvar*iT
    }
  }

  ##garchx:
  if( package=="garchx" ){
    mymod <- garchx(eps)
    mPars[i,] <- coef(mymod)
    aAvar[,,i] <- vcov(mymod)*iT
  }
  
  ##print loop no.:
  if(any(i/iReps == c(0.01, 0.02, 0.04, 0.1, 0.2, 0.5, 0.7, 0.9, 0.99))){
    cat("Replication ", i, " at ", substr(Sys.time(), 12, 19),
      "\n", sep="")
  }

}

##how many times did estimation fail?
whichNAs <- which( is.na(mPars[,1]) )
whichNAs
if( length(whichNAs)>0 ){
  mPars <- mPars[ -whichNAs,] #remove rows with NAs
  aAvar <- aAvar[,,-whichNAs] #remove matrices with NAs
}

##means of estimates:
round( colMeans(mPars), digits=3)
  
##empirical standard errors
round( sqrt( diag(var(mPars)) ) , digits=3)
 
##histograms:
hist(mPars[,1])
hist(mPars[,2])
hist(mPars[,3])


##Coefficient-covariance estimate
##-------------------------------

if(etaDGP=="N"){ Avar <- AvarNormal }
if(etaDGP=="t"){ Avar <- Avart5 }

##estimated AVar:
AvarEst <- matrix(NA,3,3)
colnames(AvarEst) <- colnames(Avar)
rownames(AvarEst) <- rownames(Avar)
for(i in 1:3){
  for(j in 1:3){
    AvarEst[i,j] <- mean(aAvar[i,j,])
  }
}
round(AvarEst, digits=4)

##true Avar:
round(Avar, digits=4)

##ratio:
round( AvarEst/Avar, digits=4)


###############################################################
## Section 4: A speed comparison
###############################################################

## Clean workspace and load required R packages
## Prepare experiment
## tseries
## fGarch
## rugarch
## garchx
## Compare

## Clean workspace and load required R packages
##=============================================

##clean workspace:
rm(list = ls())

library(tseries)
library(fGarch)
library(rugarch)
library(garchx)
library(microbenchmark)

## Prepare experiment
##===================

iT <- 1000
iTimes <- 200 #number of times to repeate by microbenchmark
dgp <- 1 #1, 2, 3 or 4
mResults <- matrix(NA, 1, 4)
rownames(mResults) <- "mean"
colnames(mResults) <-
  c("tseries", "fGarch", "rugarch", "garchx")

##dgp parameters:
omega <- 0.2
alpha1 <- 0.1
beta1 <- 0.8
gamma1 <- 0.05
lambda1 <- 0.3

##generate the series to evaluate:
set.seed(123)
if( dgp==1 || dgp==2 ){
  eps <- garchxSim(iT, intercept=omega, arch=alpha1, garch=beta1)
}
if( dgp==3 ){
  eps <- garchxSim(iT, intercept=omega, arch=alpha1, garch=beta1,
    asym=gamma1)
}
if( dgp==4 ){
  x <- arima.sim(list(ar=lambda1), iT, innov=0.1*rnorm(iT))
  xlagged <- c(0, x[-length(x)])
  eps <- garchxSim(iT, intercept=omega, arch=alpha1, garch=beta1,
    xreg=lambda1*xlagged)
}


## tseries
##========

if( dgp==1 ){ order <- c(1,1) }
if( dgp==2 ){ order <- c(2,2) }
if( dgp==1 || dgp==2 ){
  res <- microbenchmark( mymod <- garch(eps, order=order) , times=iTimes)
  mResults[1,"tseries"] <- mean(res$time)/1000000
}


## fGarch
##=======

if( dgp==1 ){
  res <- microbenchmark(
            mymod <- garchFit(include.mean=FALSE, data=eps),
            times=iTimes)
  mResults[1,"fGarch"] <- mean(res$time)/1000000
}
if( dgp==2 ){
  res <- microbenchmark(
            mymod <- garchFit(include.mean=FALSE, formula=~garch(2,2), data=eps),
            times=iTimes)
  mResults[1,"fGarch"] <- mean(res$time)/1000000
}
if( dgp==3 ){
  res <- microbenchmark(
    mymod <- garchFit(include.mean=FALSE, formula=~aparch(1,1), include.delta=FALSE, data=eps),
            times=iTimes)
  mResults[1,"fGarch"] <- mean(res$time)/1000000
}


## rugarch
##========

if( dgp==1 ){
  spec <-
    ugarchspec( mean.model=list(armaOrder=c(0,0), include.mean=FALSE))
}
if( dgp==2 ){
  spec <- ugarchspec( mean.model=list(armaOrder=c(0,0),
    include.mean=FALSE), variance.model=list(garchOrder=c(2, 2)))
}
if( dgp==3 ){
  spec <-
    ugarchspec( mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
    variance.model=list(model="gjrGARCH") )
}
if( dgp==4 ){
  spec <-
    ugarchspec( mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
    variance.model=list(external.regressors=cbind(xlagged)) )
}
res <-
  microbenchmark( mymod <- ugarchfit(data=eps, spec=spec), times=iTimes)
mResults[1,"rugarch"] <- mean(res$time)/1000000


## garchx
##=======

if( dgp==1 ){ arch <- 1; garch <- 1; asym <- 0; xreg=NULL}
if( dgp==2 ){ arch <- 1:2; garch <- 1:2; asym <- 0; xreg=NULL }
if( dgp==3 ){ arch <- 1; garch <- 1; asym <- 1; xreg=NULL }
if( dgp==4 ){ arch <- 1; garch <- 1; asym <- 0; xreg=xlagged}
res <- microbenchmark(
  mymod <- garchx(eps, arch=arch, garch=garch, asym=asym, xreg=xreg),
  times=iTimes)
mResults[1,"garchx"] <- mean(res$time)/1000000


## Compare
##========

round( mResults/min(mResults, na.rm=TRUE) , digits=2)
