rm(list=ls(all=TRUE))

library(fitdistrplus)
library(actuar)
library(xtable)
library(goftest)
library(OneStep)

set.seed(12345)

n<-10000
theta<-c(0.8,3)

shape <- theta[1]
rate <- theta[2]
scale <- 1/rate

M<- 10000
#M <-100

tabMLE<-matrix(0,2,M)
tabOLS<-matrix(0,2,M)
tabLCE<-matrix(0,2,M)
tabGLCE<-matrix(0,2,M)

timeMLE<-0
timeOLS<-0
timeLCE<-0
timeGLCE<-0

for (i in 1:M){
  
  #Simulate the sample
  o.sample<-rweibull(n,shape = theta[1],scale = 1/theta[2])
  
  # #MLE
  # ttmp<-proc.time()
  # esttmp<-mledist(o.sample,"weibull")$estimate
  # ttmp<-proc.time()-ttmp
  # 
  # tabMLE[,i]<-c(esttmp[1],1/esttmp[2])
  # 
  # timeMLE<-timeMLE + ttmp
  
  #OLS
  
  ttmp<-proc.time()
  Y<- log(sort(o.sample))
  X<- log(-log(1-(1:n)/(n+1))) #ASTM Procedure
  res<-as.numeric(lm(Y~X)$coef)

  ttmp<-proc.time()-ttmp
  timeOLS<-timeOLS + ttmp
  
  shape<-1/res[2]
  rate<-1/exp(res[1])
  tabOLS[,i]<-c(shape,rate)
  
  # #OSMLE (Fisher scoring)
  # 
  # ttmp<-proc.time()
  # esttmp<-onestep(o.sample,"weibull")$estimate
  # ttmp<-proc.time()-ttmp
  # 
  # tabLCE[,i]<-c(esttmp[1],1/esttmp[2])
  # timeLCE<-timeLCE  + ttmp
  
  # GLCE
  ttmp<-proc.time()
  esttmp<-onestep(o.sample,"weibull",method="numeric")$estimate
  ttmp<-proc.time()-ttmp
  
  tabGLCE[,i]<-c(esttmp[1],1/esttmp[2])
  timeGLCE<-timeGLCE  + ttmp


 if (i%%100==0) print(i)

}

system.time(resweibull4 <- benchonestep.replicate(n, M, "weibull", shape=theta[1], scale=theta[2], methods=c("mle", "onestep"), ncpus=4))

save(resweibull4, tabOLS, tabGLCE, timeOLS, timeGLCE, file=paste0("res-weibull-n",max(n), "-M", max(M),".RData"))

if(FALSE)
  load(paste0("res-weibull-n10000-M10000.RData"))

tabMLE <- resweibull4[c("shape","scale"),"mle", ]
tabLCE <- resweibull4[c("shape","scale"),"onestep", ]

###Visualisation et Information de Fisher

res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabOLS-matrix(rep(theta,M),2,M))
res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))
res4<-sqrt(n)*(tabGLCE-matrix(rep(theta,M),2,M))

#tabtime<-data.frame(MLE=timeMLE[2],OLS=timeOLS[2],LCE=timeLCE[2])
#tabtime2<-data.frame(MLE=timeMLE[2],LCE=timeLCE[2],GLCE=timeGLCE[2])

tabtime2<-data.frame(MLE=sum(resweibull4["time","mle", ]), 
                    LCE = sum(resweibull4["time","onestep", ]),
                    GLCE=timeGLCE[3])
tabtime<-data.frame(MLE=sum(resweibull4["time","mle", ]), 
                    OLS=timeOLS[3],
                    LCE = sum(resweibull4["time","onestep", ]))



I<-matrix(0,2,2)
I[1,1]<- 1/shape^2*(trigamma(1)+digamma(2)^2)
I[2,1]<- 1/rate*digamma(2) 
I[1,2]<- 1/rate*digamma(2)  
I[2,2]<- shape^2/rate^2

covlim<-solve(I)


## Moments variance
J <- matrix(c(-digamma(1+1/shape)*gamma(1+1/shape)/(rate*shape^2),
              -2*digamma(1+2/shape)*gamma(1+2/shape)/(rate^2*shape^2),
              -gamma(1+1/shape)/(rate^2),
              -2*gamma(1+2/shape)/(rate^3)),2,2)
A <- matrix(c(gamma(1+2/shape)/(rate^2)-(gamma(1+1/shape)^2)/(rate^2),
              (gamma(1+3/shape)-gamma(1+1/shape)*gamma(1+2/shape))/(rate^3),
              (gamma(1+3/shape)-gamma(1+1/shape)*gamma(1+2/shape))/(rate^3),
              (gamma(1+4/shape)-gamma(1+2/shape)^2)/(rate^4)),2,2)

Jinv<-solve(J)
Sigma<-Jinv%*%A%*%t(Jinv)



pdf("fig-Weibull.pdf", width = 10, height=7)
layout(matrix(1:6,2,3,byrow=TRUE))
hist(res[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7), main=expression(list("MLE", tau)),xlab="rescaled error")
x<-seq(-5,5,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[1,1]))
lines(x,y,col="red")
hist(res2[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main=expression(list("OLS", tau)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=mean(res2[1,]),sd=sd(res2[1,]))
lines(x,y2,col="blue")
hist(res3[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main=expression(list("LCE", tau)),xlab="rescaled error")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main=expression(list("MLE", beta)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
hist(res2[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main=expression(list("OLS", beta)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=mean(res2[2,]),sd=sd(res2[2,]))
lines(x,y2,col="blue")
hist(res3[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main=expression(list("LCE", beta)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
dev.off()

row.names(tabtime)<-"Computation time (s)"

T11<-cvm.test(res[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic
T12<-cvm.test(res2[1,],null="pnorm",mean=0,sd=sd(res2[1,]))$statistic
T13<-cvm.test(res3[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic

cvm.test(res2[1,],null="pnorm",mean=mean(res2[1,]),sd=sd(res2[1,]))$statistic


T1<-data.frame(MLE=T11,LCE=T13,OLS=T12)
row.names(T1)<-"CvM statistic tau"

T21<-cvm.test(res[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic
T22<-cvm.test(res2[2,],null="pnorm",mean=0,sd=sd(res2[2,]))$statistic
T23<-cvm.test(res3[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic

T2<-data.frame(MLE=T21,LCE=T23,OLS=T22)
row.names(T2)<-"CvM statistic beta"


capture.output(xtable(rbind(tabtime,T1,T2), label="tab:weibull:time:cvm",
                      caption="Computation time and CvM statistics"),
               file="tab-weibull.tex")


## another output ##


cvm.test(res2[1,],null="pnorm",mean=mean(res2[1,]),sd=sd(res2[1,]))$statistic
cvm.test(res2[2,],null="pnorm",mean=mean(res2[2,]),sd=sd(res2[2,]))$statistic


pdf("fig-Weibull-generic.pdf", width = 10, height=7)
layout(matrix(1:6,2,3,byrow=TRUE))
hist(res[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main=expression(list("MLE", tau)),xlab="rescaled error")
x<-seq(-5,5,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[1,1]))
lines(x,y,col="red")
hist(res3[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main=expression(list("LCE", tau)),xlab="rescaled error")
lines(x,y,col="red")
hist(res4[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main=expression(list("numerical LCE", tau)),xlab="rescaled error")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main=expression(list("MLE", beta)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
hist(res3[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main=expression(list("LCE", beta)),xlab="rescaled error")
lines(x,y,col="red")
hist(res4[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main=expression(list("numerical LCE", beta)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
dev.off()

row.names(tabtime2)<-"Computation time (s)"

T11<-cvm.test(res[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic
T12<-cvm.test(res3[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic
T13<-cvm.test(res4[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic

T1<-data.frame(MLE=T11,LCE=T12,GLCE=T13)
row.names(T1)<-"CvM statistic tau"

T21<-cvm.test(res[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic
T22<-cvm.test(res3[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic
T23<-cvm.test(res4[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic

T2<-data.frame(MLE=T21,LCE=T22,GLCE=T23)
row.names(T2)<-"CvM statistic beta"



capture.output(xtable(rbind(tabtime2,T1,T2), label="tab:weibull:generic:time:cvm",
                      caption="Computation time and CvM statistics"),
               file="tab-weibull-generic.tex")


cvm.test(res4[2,],null="pnorm",mean=mean(res2[2,]),sd=sqrt(covlim[2,2]))$statistic


