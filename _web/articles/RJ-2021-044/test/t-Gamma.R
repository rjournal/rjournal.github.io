rm(list=ls(all=TRUE))

library(fitdistrplus)
library(xtable)
library(goftest)
library(OneStep)

set.seed(12345)

n<-10000
theta<-c(2,3)

M<-10000


system.time(resgamma4 <- benchonestep.replicate(n, M, "gamma", shape=theta[1], rate=theta[2], echo=FALSE, ncpus=4))

save(resgamma4, file=paste0("res-gamma-n",max(n), "-M", max(M),".RData"))

if(FALSE)
  load(paste0("res-gamma-n10000-M10000.RData"))

tabMLE <- resgamma4[c("shape","rate"),"mle", ]
tabME <- resgamma4[c("shape","rate"),"mme", ]
tabLCE <- resgamma4[c("shape","rate"),"onestep", ]

###Visualisation et Information de Fisher

res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabME-matrix(rep(theta,M),2,M))
res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))



tabtime<-data.frame(MLE=sum(resgamma4["time","mle", ]), 
                    ME = sum(resgamma4["time","mme", ]), 
                    LCE = sum(resgamma4["time","onestep", ]))


I<-matrix(0,2,2)
I[1,1]<-trigamma(theta[1])
I[2,1]<--1/theta[2]
I[1,2]<--1/theta[2]
I[2,2]<-theta[1]/theta[2]^2

covlim<-solve(I)

## Moments variance
J<-matrix(c(1/theta[2], -theta[1]/theta[2]^2, (1+2*theta[1])/theta[2]^2, -2*theta[1]*(theta[1]+1)/theta[2]^3),2,2,byrow=TRUE)
A<-matrix(c(theta[1]/theta[2]^2, 2*theta[1]*(theta[1]+1)/theta[2]^3, 2*theta[1]*(theta[1]+1)/theta[2]^3, (4*theta[1]+6)*theta[1]*(theta[1]+1)/theta[2]^4),2,2)
Jinv<-solve(J)
Sigma<-Jinv%*%A%*%t(Jinv)

pdf("fig-Gamma.pdf", width = 10, height=7)
layout(matrix(1:6,2,3,byrow=TRUE))
hist(res[1,],freq=FALSE,nclass=40,xlim=c(-10,10),ylim=c(0,0.17),main=expression(list("MLE", alpha)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[1,1]))
lines(x,y,col="red")
hist(res2[1,],freq=FALSE,nclass=40,xlim=c(-10,10),ylim=c(0,0.17),main=expression(list("ME", alpha)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(Sigma[1,1]))
lines(x,y2,col="blue")
hist(res3[1,],freq=FALSE,nclass=40,xlim=c(-10,10),ylim=c(0,0.17),main=expression(list("LCE", alpha)),xlab="rescaled error")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.12),main=expression(list("MLE", beta)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
hist(res2[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.12),main=expression(list("ME", beta)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(Sigma[2,2]))
lines(x,y2,col="blue")
hist(res3[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.12),main=expression(list("LCE", beta)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
dev.off()

row.names(tabtime)<-"Computation time (s)"

T11<-cvm.test(res[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic
T12<-cvm.test(res2[1,],null="pnorm",mean=0,sd=sqrt(Sigma[1,1]))$statistic
T13<-cvm.test(res3[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic

T1<-data.frame(MLE=T11,LCE=T13,ME=T12)
row.names(T1)<-"CvM statistic alpha"

T21<-cvm.test(res[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic
T22<-cvm.test(res2[2,],null="pnorm",mean=0,sd=sqrt(Sigma[2,2]))$statistic
T23<-cvm.test(res3[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic

T2<-data.frame(MLE=T21,LCE=T23,ME=T22)
row.names(T2)<-"CvM statistic beta"

capture.output(xtable(rbind(tabtime,T1,T2), label="tab:gamma:time:cvm",
                      caption="Computation time and CvM statistics"),
               file="tab-gamma.tex")
