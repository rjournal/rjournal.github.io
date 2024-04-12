library(SIMEXBoost)
library(MASS)
data = read.table("C://bankruptcy_data.csv",sep=",",head=T)
data = data[,-94]
Y = data[,95]
Y = as.numeric(Y)
Xstar = t(as.matrix(data[,-95]))

Xstar=scale(Xstar)

p = dim(Xstar)[1]
n = dim(Xstar)[2]
R = c(0.1, 0.3, 0.5)

set.seed(202270)
##naive
EST<-NULL
naive<-Boost_VSE(Y,t(Xstar),type="binary",Iter = 50,Lambda = 0)$BetaHat
EST<-cbind(EST,naive)

##linear
for(i in R){
  correctL <- SIMEXBoost(Y,t(Xstar),zeta=c(0,0.25,0.5,0.75,1),B=50, type="binary",
                         sigmae=diag(i,p), Iter=50, Lambda=0,Extrapolation="linear")$BetaHatCorrect
  
  EST = cbind(EST,correctL)
}

##Quadratic
for(i in R){
  correctQ <- SIMEXBoost(Y,t(Xstar),zeta=c(0,0.25,0.5,0.75,1),B=50, type="binary",
                         sigmae=diag(i,p), Iter=50, Lambda=0,Extrapolation="quadratic")$BetaHatCorrect

  EST = cbind(EST,correctQ)
}

round(EST,3)

