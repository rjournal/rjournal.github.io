rm(list=ls(all=TRUE))

library(fitdistrplus)
library(xtable)
library(goftest)
library(OneStep)

set.seed(1234)

n<-10000
theta<-c(1,5)

M<-10000

system.time(resnbinom4 <- benchonestep.replicate(n, M, "nbinom", size=theta[1], mu=theta[2], echo=FALSE, ncpus=4))


save(resnbinom4, file=paste0("res-nbinom-n",max(n), "-M", max(M),".RData"))

if(FALSE)
  load(paste0("res-nbinom-n10000-M10000.RData"))

tabMLE <- resnbinom4[c("size","mu"),"mle", ]
tabME <- resnbinom4[c("size","mu"),"mme", ]
tabLCE <- resnbinom4[c("size","mu"),"onestep", ]

###Visualisation et Information de Fisher

res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabME-matrix(rep(theta,M),2,M))
res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))



tabtime<-data.frame(MLE=sum(resnbinom4["time","mle", ]), 
                    ME = sum(resnbinom4["time","mme", ]), 
                    LCE = sum(resnbinom4["time","onestep", ]))


## Moments variance
r<-theta[1]
mu<-theta[2]

J<-matrix(c(0,1,-mu^2/r^2,1+2*mu+2*mu/r),2,2,byrow=TRUE)

p<-r/(mu+r)
q<-1-p
mu2<-r*q/p^2
mu3<-r*q*(q+1)/p^3
mu4<-(6*q+p^2+3*r*q)*r*q/p^4

A11<-mu2

m2<-mu2+mu^2
m3<-mu3+3*m2*mu-3*mu*mu^2+mu^3

A12<-m3-mu*m2

m4<-mu4+4*m3*mu-6*m2*mu^2+4*mu*mu^3-mu^4

A22<-m4-(m2)^2

A<-matrix(c(A11,A12,A12,A22),2,2,byrow=TRUE)
Jinv<-solve(J)
Sigma<-Jinv%*%A%*%t(Jinv)




pdf("fig-nbinom.pdf", width = 10, height=7) 

layout(matrix(1:6,2,3,byrow=TRUE))


hist(res[1,],freq=FALSE,nclass=40,xlim=c(-8,8),ylim=c(0,0.25), main=expression(list("MLE", r)),xlab="rescaled error")
x<-seq(-8,8,length=100)
y<-dnorm(x,mean=0,sd=sd(res[1,]))
lines(x,y,col="red")
hist(res2[1,],freq=FALSE,nclass=40,xlim=c(-8,8),ylim=c(0,0.25), main=expression(list("ME", r)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(Sigma[1,1]))
lines(x,y2,col="blue")
hist(res3[1,],freq=FALSE,nclass=40,xlim=c(-8,8),ylim=c(0,0.25),main=expression(list("LCE", r)),xlab="rescaled error")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.12),main=expression(list("MLE", mu)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sd(res[2,]))
lines(x,y,col="red")
hist(res2[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.12),main=expression(list("ME", mu)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(Sigma[2,2]))
lines(x,y2,col="blue")
hist(res3[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.12),main=expression(list("LCE", mu)),xlab="rescaled error")
lines(x,y,col="red")

dev.off()

row.names(tabtime)<-"Computation time (s)"

T11<-cvm.test(res[1,],null="pnorm",mean=0,sd=sd(res[1,]))$statistic
T12<-cvm.test(res2[1,],null="pnorm",mean=0,sd=sqrt(Sigma[1,1]))$statistic
T13<-cvm.test(res3[1,],null="pnorm",mean=0,sd=sd(res[1,]))$statistic

T1<-data.frame(MLE=T11,LCE=T13,ME=T12)
row.names(T1)<-"CvM statistic r"

T21<-cvm.test(res[2,],null="pnorm",mean=0,sd=sd(res[2,]))$statistic
T22<-cvm.test(res2[2,],null="pnorm",mean=0,sd=sqrt(Sigma[2,2]))$statistic
T23<-cvm.test(res3[2,],null="pnorm",mean=0,sd=sd(res3[2,]))$statistic

T2<-data.frame(MLE=T21,LCE=T23,ME=T22)
row.names(T2)<-"CvM statistic mu"


capture.output(xtable(rbind(tabtime,T1,T2), label="tab:nbinom:time:cvm",
                      caption="Computation time and CvM statistics"),
               file="tab-nbinom.tex")