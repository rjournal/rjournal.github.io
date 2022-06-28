
library(fitdistrplus)
library(actuar)
library(xtable)

library(OneStep)

n<-10000
theta<-c(0.8,3)

shape <- theta[1]
rate <- theta[2]
scale <- 1/rate

M<- 10000

tabMLE<-matrix(0,2,M)
tabME<-matrix(0,2,M)
tabLCE<-matrix(0,2,M)
tabGLCE<-matrix(0,2,M)

timeMLE<-0
timeME<-0
timeLCE<-0
timeGLCE<-0

dweibull2 <- function(x,shape,scale,log=FALSE)
  return(dweibull(x = x,shape = shape,scale = scale,log = log))

memp<-function(x,order){x^order}



for (i in 1:M){
  
  #Simulate the sample
  o.sample<-rweibull(n,shape = theta[1],scale = 1/theta[2])
  
  #MLE
  ttmp<-Sys.time()
  esttmp<-mledist(o.sample,"weibull")$estimate
  ttmp<-Sys.time()-ttmp
  
  tabMLE[,i]<-c(esttmp[1],1/esttmp[2])
  
  timeMLE<-timeMLE + ttmp
  
  
  #ME
  m <- mean(log(o.sample))
  v <- var(log(o.sample))
  shape.start <- 1.2/sqrt(v)
  scale.start <- exp(m + 0.572/shape)
  start <- list(shape = shape.start, scale = scale.start)
  
  ttmp<-Sys.time()
  esttmp<-mmedist(o.sample,"weibull",order=1:2,memp=memp,start = start,checkstartfix = TRUE,lower = c(0,0))$estimate
  ttmp<-Sys.time()-ttmp
  
  tabME[,i]<-c(esttmp[1],1/esttmp[2])
  timeME<-timeME + ttmp
  
  #OSMLE (Fisher scoring)
  
  ttmp<-Sys.time()
  esttmp<-onestep(o.sample,"weibull")$estimate
  ttmp<-Sys.time()-ttmp
  
  tabLCE[,i]<-c(esttmp[1],1/esttmp[2])
  timeLCE<-timeLCE  + ttmp
  
  # GLCE
  ttmp<-Sys.time()
  esttmp<-onestep(o.sample,"weibull",method="numeric")$estimate
  ttmp<-Sys.time()-ttmp
  
  tabGLCE[,i]<-c(esttmp[1],1/esttmp[2])
  timeGLCE<-timeGLCE  + ttmp
  
  if (i%%100==0) print(i)
  
}

###Visualisation et Information de Fisher

res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabME-matrix(rep(theta,M),2,M))
res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabME-matrix(rep(theta,M),2,M))
res4<-sqrt(n)*(tabGLCE-matrix(rep(theta,M),2,M))



tabtime<-data.frame(MLE=timeMLE,LCE=timeLCE,ME=timeME,GLCE=timeGLCE)


I<-matrix(0,2,2)
I[1,1]<- 1/shape^2*(trigamma(1)+digamma(2))
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

pdf("Weibull-generic.pdf")
layout(matrix(1:6,2,3,byrow=TRUE))
hist(res[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main="MLE theta1",xlab="")
x<-seq(-5,5,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[1,1]))
lines(x,y,col="red")
hist(res2[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main="ME theta1",xlab="")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(Sigma[1,1]))
lines(x,y2,col="blue")
hist(res4[1,],freq=FALSE,nclass=40,xlim=c(-3,3),ylim=c(0,0.7),main="LCE theta1",xlab="")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main="MLE theta2",xlab="")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
hist(res2[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main="ME theta2",xlab="")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(Sigma[2,2]))
lines(x,y2,col="blue")
hist(res4[2,],freq=FALSE,nclass=40,xlim=c(-15,15),ylim=c(0,0.13),main="LCE theta2",xlab="")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y,col="red")
dev.off()


capture.output(xtable(tabtime),file="Weibull-generic.out")
