


 Executed with R4.02

#Data

library(NHPoisson)
library(IndTestPP)

data(TxBHZ)
attach(TxBHZ)



#Building the point processes of the extreme events in the three series

auxZ<-POTevents.fun(TxZ, thres=37.8)
posZ<-auxZ$Px
auxH<-POTevents.fun(TxH, thres=36.4)
auxB<-POTevents.fun(TxB, thres=31.3)
posB<-auxB$Px
posH<-auxH$Px

T<-length(TxZ)
PlotMargP(list(posB, posH, posZ), T=T, cex.axis=0.6,cex=0.6, cex.main=0.7, cex.lab=0.7)
	

# Correlation and Extreme dependence measures and Dutilleul plots


cor(TxZ, TxB, use='complete.obs')
cor(TxZ, TxH, use='complete.obs')
cor(TxB, TxH, use='complete.obs')

	
aux<- depchi(TxB,TxZ,indgraph=FALSE,xlegend='topright',thresval=c(9000:9975)/10000)

aux<- depchi(TxB,TxH,indgraph=FALSE,xlegend='topright',thresval=c(9000:9975)/10000)
aux<- depchi(TxZ,TxH,indgraph=FALSE,xlegend='topright',thresval=c(9000:9975)/10000)

aux<-CountingCor(posB,posZ, ll=10, T=T, method='kendall')
aux
aux<-CountingCor(posB,posH, ll=10, T=T, method='kendall')
aux
aux<-CountingCor(posH,posZ, ll=10, T=T, method='kendall')
aux

aux<-BinPer(posB,posZ, ll=10, T=T)
aux<-BinPer(posB,posH, ll=10, T=T)
aux<-BinPer(posH,posZ, ll=10, T=T)

par(mfrow=c(1,3))
lambdaEB<-emplambda.fun(posE=posB, t=c(1:T), lint=100, plot=F)$emplambda
aux<-DutilleulPlot(posZ, posB, lambdaEB, main="Zaragoza-Barcelona", cex.main=0.9)
lambdaEH<-emplambda.fun(posE=posH,t=c(1:T), lint=100, plot=F)$emplambda
aux<-DutilleulPlot(posB, posH, lambdaEH, main="Barcelona-Huesca", cex.main=0.9)
aux<-DutilleulPlot(posZ, posH, lambdaEH, main="Zaragoza-Huesca", cex.main=0.9)


#Fitting the univariate Poisson processes to the three series

ss<-sin(2*pi*dayyear/366)
cc<-cos(2*pi*dayyear/366)
covZ <- cbind(ss,cc, Txm15Z, Txm15Z**2 )
dimnames(covZ) <- list(NULL, c("Sin", "Cos", "Txm15", "Txm152"))
ModZ<-fitPP.fun(covariates = covZ, posE = posZ,  inddat = auxZ$inddat , 
      tit = "Sin+Cos+Txm15+Txm152", start = list(b0 = 1, b1=-1,b2=1, b3=0, b4=0), dplot=F)
lambdaZ<-ModZ@lambdafit


covB <- cbind(ss, cc, Txm15B, Txm15B*ss,Txm15B*cc, Txm15B**2 )
dimnames(covB) <- list(NULL, c("Sin", "Cos", "Txm15", "sTxm15", "cTxm15","Txm152"))
ModB<-fitPP.fun(covariates = covB, posE = posB,  inddat = auxB$inddat , tit = "Sin+Cos+Txm15+sTxm15+cTxm15+Txm152", 
	start = list(b0 = 1, b1=-1,b2=1, b3=0, b4=0,b5=0,b6=0), dplot=F)

covH <- cbind(ss,cc, Txm15H, Txm15H**2 )
dimnames(covH) <- list(NULL, c("Sin", "Cos", "Txm15", "Txm152"))
ModH<-fitPP.fun(covariates = covH, posE = posH,  inddat = auxH$inddat , tit = "", 
      start = list(b0 = 1, b1=-1,b2=1, b3=0, b4=0), dplot=F)

lambdaB<-ModB@lambdafit
lambdaH<-ModH@lambdafit

#Testing independence  and Dutilleul plots given the marginal intensities

	#Zaragoza and Barcelona

aux<-CondTest(posZ, posB, lambday=lambdaB, r=15)
aux$pvN


PBZB<-TestIndNH(posZ, posB, nsim = 5000, type = "Poisson",lambdaMarg =cbind(lambdaB),
      fixed.seed=35)  
PBZB$pv

auxZB<-NHK(lambdaZ, lambdaB, posC=posZ, posD=posB, r=c(1:15), typePlot='Kfun',
        cores=2,fixed.seed=36)
auxZB$pv

	#Barcelona and Huesca

aux<-CondTest(posB, posH, lambday=lambdaH, r=15)
aux$pvN

PBBH<-TestIndNH(posB, posH, nsim = 5000, type = "Poisson",lambdaMarg =cbind(lambdaH),
      fixed.seed=36)  
PBBH$pv

auxBH<-NHK(lambdaB, lambdaH, posC=posB, posD=posH, r=c(1:10), typePlot='Kfun',
        cores=2,fixed.seed=25)
auxBH$pv

	#Zaragoza and Huesca

aux<-CondTest(posZ, posH, lambday=lambdaH, r=15)
aux$pvN

PBZH<-TestIndNH(posZ, posH, nsim = 5000, type = "Poisson",lambdaMarg =cbind(lambdaH),
      fixed.seed=36)  
PBZH$pv


auxZH<-NHK(lambdaZ, lambdaH, posC=posZ, posD=posH, r=c(1:10), typePlot='Kfun',
        cores=2,fixed.seed=36)
auxZH$pv


	#Removing influent point in Barcelona

posBb<-posB[-4]  #902
auxBbH<-NHK(lambdaB, lambdaH, posC=posBb, posD=posH, r=c(1:10), typePlot='Kfun',cores=2, fixed.seed=130)
auxBbH$pv

par(mfrow=c(1,3))
aux<-DutilleulPlot(posZ, posB, ModB@lambdafit,main="Barcelona-Zaragoza", cex.main=0.9)
aux<-DutilleulPlot(posB, posH, ModH@lambdafit,main="Barcelona-Huesca", cex.main=0.9)
aux<-DutilleulPlot(posZ, posH, ModH@lambdafit,main="Zaragoza-Huesca", cex.main=0.9)


PBBHZ<-TestIndNH(posB, posH, posZ, nsim = 1000, type = "Poisson",
         lambdaMarg =cbind(lambdaH, lambdaZ),fixed.seed=65, cores=2)  
PBBHZ$pv


#Model taking into account Dependence between TxZ and TxH

aux<-CPSPPOTevents(TxZ, TxH, thres1=37.8, thres2=36.4)
possZ<-aux$Px1
possH<-aux$Px2
posZH<-aux$Px12



#Inference on the CPSP for TxZ and TxH

firstt<-function(posNH){minpos<-min(unlist(posNH))}
lambdaiZH<-cbind(lambdaOZ,lambdaOH,lambdaZH)

aux<-IntMPP(funMPP.name="DepNHCPSP", funMPP.args=list(lambdaiM=lambdaiZH, d=2, dplot=FALSE), 
            fun.name="firstt", fun.args = NULL, clevel = 0.95, cores = 2, fixed.seed = 125) 

aux<-IntMPP(funMPP.name="DepNHCPSP", funMPP.args=list(lambdaiM=lambdaiZH, d=2, dplot=FALSE),
    fun.name="NumI", fun.args = list(I=c(1,459)),  fixed.seed = 125)
  
  aux<-IntMPP(funMPP.name="DepNHCPSP", funMPP.args=list(lambdaiM=lambdaiZH, d=2, dplot=FALSE),
    fun.name="NumI", fun.args = list(I=c(7803,8262)),  fixed.seed = 125)


#Simulating and characterizing  dependence in a vector of processes

set.seed(123)
lambdaParent<-runif(2000)/10

aux<-DepNHNeyScot(lambdaParent=lambdaParent, d=2, lambdaNumP = c(3,4), dist = "normal", 
    sigmaC = c(3,2),fixed.seed=123, dplot=F)
posxd<- aux$posNH$N1
posyd<- aux$posNH$N2

aux <-IndNHNeyScot(lambdaParent=lambdaParent, d=2, lambdaNumP = c(3,4),dist = "normal",
      sigmaC =c(3,2),fixed.seed=123, dplot=F)
posxi<- aux$N1
posyi<- aux$N2

par(mfrow=c(1,3))
distxyd<- nearestdist(posxd , posyd)
hist(distxyd , main='Dependent processes' , xlab='Nearest dist',xlim=c(0,60),
	ylim=c(0,270),breaks=seq(0,60, by=4) )
distxyi<- nearestdist(posxi , posyi)
hist(distxyi , main='Independent processes' , xlab='Nearest dist',xlim=c(0,60),
	ylim=c(0,270),breaks=seq(0,60, by=4) )
qqplot(distxyi,distxyd,xlab='Independent processes', ylab='Dependent processes')
lines(distxyd,distxyd, col="red")


