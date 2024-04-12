library(SIMEXBoost)
library(MASS)
NAIVE = NULL; SIMEXL = NULL; SIMEXQ = NULL
for(p in c(200,500)) {

for(sigma in c(0.1,0.3,0.5)) {

set.seed(202270)
  beta0=c(1,1,1,rep(0,p-3))

 X = matrix(rnorm((p)*400),nrow=400,ncol=p,byrow=TRUE)

 Sig = diag(sigma^3,dim(X)[2])
 data=ME_Data(X=X,beta=beta0,type="AFT-normal",sigmae=Sig)
 Y = data$response
 Xstar = data$ME_covariate

 
 naive = SIMEXBoost(Y,Xstar,zeta=c(0.9,1),B=2, type="AFT-normal",sigmae=diag(0,dim(X)[2]),
  Iter=100, Lambda=0, Extrapolation="linear")$BetaHatCorrect

 correctL = SIMEXBoost(Y,Xstar,zeta=c(0,0.25,0.5,0.75,1),B=50, type="AFT-normal",sigmae=Sig,
  Iter=50, Lambda=0, Extrapolation="linear")$BetaHatCorrect
   correctL[which(abs(correctL)<0.5)]=0
 correctQ = SIMEXBoost(Y,Xstar,zeta=c(0,0.25,0.5,0.75,1),B=50, type="AFT-normal",sigmae=Sig,
  Iter=50, Lambda=0, Extrapolation="quadratic")$BetaHatCorrect
   correctQ[which(abs(correctQ)<0.5)]=0

#############

Sen0 = which(beta0 !=0); Spe0 =  which(beta0 ==0)

## results for the naive estimator
 naive = as.numeric(naive)
 L1_norm = sum(abs(naive - beta0))
 L2_norm = sqrt(sum((naive - beta0)^2) )
 Spe = length(which(naive[Spe0]==0)) / length(Spe0)
 Sen = length(which(naive[Sen0]!=0)) / length(Sen0)

## results for the error-corrected estimator based on Extrapolation="linear" 
 L1_norm_l = sum(abs(correctL - beta0))
 L2_norm_l = sqrt(sum((correctL - beta0)^2) )
 Spe_l = length(which(correctL[Spe0]==0)) / length(Spe0)
 Sen_l = length(which(correctL[Sen0]!=0)) / length(Sen0)

## results for the error-corrected estimator based on Extrapolation="quadratic" 
 L1_norm_q = sum(abs(correctQ - beta0))
 L2_norm_q = sqrt(sum((correctQ - beta0)^2) )
 Spe_q = length(which(correctQ[Spe0]==0)) / length(Spe0)
 Sen_q = length(which(correctQ[Sen0]!=0)) / length(Sen0)


#############

NAIVE = rbind(NAIVE,c(L1_norm,L2_norm,Spe,Sen))
SIMEXL = rbind(SIMEXL,c(L1_norm_l,L2_norm_l,Spe_l,Sen_l))
SIMEXQ = rbind(SIMEXQ,c(L1_norm_q,L2_norm_q,Spe_q,Sen_q))
}
 }


