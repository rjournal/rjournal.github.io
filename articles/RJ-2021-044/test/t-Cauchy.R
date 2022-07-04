rm(list=ls(all=TRUE))

library(fitdistrplus)
library(xtable)
library(goftest)
library(OneStep)

### Cauchy
set.seed(1234)

n<-10000
theta<-c(2,3)

M<-10000
#M <- 100

tabMLE<-matrix(0,2,M)
tabQME<-matrix(0,2,M)
tabLCE<-matrix(0,2,M)

timeMLE<-0
timeQME<-0
timeLCE<-0

for (i in 1:M){

  #Simulate the sample
	o.sample<-rcauchy(n,location=theta[1],scale=theta[2])
		
	#MLE
# 	ttmp<-proc.time()
#   tabMLE[,i]<-mledist(o.sample,"cauchy")$estimate
# 	#tabMLE[,i]<-c(1,1)
# 	ttmp<-proc.time()-ttmp
# 	
# 	timeMLE<-timeMLE + ttmp

	#Characteristic function
	
	#t<-0.3
	
	#ttmp<-Sys.time()
	#Phi<-mean(exp(1i*t*o.sample))
	#S<-Re(Phi)
	#Z<-Im(Phi)
	
	#tabQME[,i]<-c(atan(Z/S)/t,-log(sqrt(S^2+Z^2))/t)
	#ttmp<-Sys.time()-ttmp
	
	#timeQME<-timeQME + ttmp
	
	#Quantile matching
	
	ttmp<-proc.time()
	tabQME[,i]<-c(median(o.sample),as.numeric(diff(quantile(o.sample,c(1/4,3/4))))/2)
	ttmp<-proc.time()-ttmp
	
	timeQME<-timeQME + ttmp
	
  #LCE (Fisher scoring)
 
# 	ttmp<-proc.time()
#   	tabLCE[,i]<-onestep(o.sample,"cauchy")$estimate
#   	ttmp<-proc.time()-ttmp
#   
# 	timeLCE<-timeLCE  + ttmp

}



system.time(rescauchy4 <- benchonestep.replicate(n, M, "cauchy", location=theta[1], scale=theta[2], methods=c("mle", "onestep"), ncpus=4))

save(rescauchy4, tabQME, timeQME, file=paste0("res-cauchy-n",max(n), "-M", max(M),".RData"))

if(FALSE)
  load(paste0("res-cauchy-n10000-M10000.RData"))

tabMLE <- rescauchy4[c("location","scale"),"mle", ]
tabLCE <- rescauchy4[c("location","scale"),"onestep", ]


###Visualisation et Information de Fisher

res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabQME-matrix(rep(theta,M),2,M))
res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))

tabtime<-data.frame(MLE=sum(rescauchy4["time","mle", ]), 
                    QME=timeQME[3],
                    LCE = sum(rescauchy4["time","onestep", ]))


covlim = matrix ( 0, nrow=2,ncol=2)

#Characteristic function
#covlim[1,1]<-(exp(2*theta[2]*abs(t))-1)/(2*t^2)
#covlim[2,2]<-(exp(2*theta[2]*abs(t))-1)/(2*t^2)

#Quantile matching

fm<-dcauchy(theta[1],location=theta[1],scale=theta[2])

q1<-qcauchy(0.25,location=theta[1],scale=theta[2])
f1<-dcauchy(q1,location=theta[1],scale=theta[2])
q3<-qcauchy(0.75,location=theta[1],scale=theta[2])
f3<-dcauchy(q3,location=theta[1],scale=theta[2])

covlim[1,1]<-1/4/fm^2
covlim[2,2]<-1/4/16*(3/f1^2+3/f3^2 -2/f1/f3) 

covlim[2,1]<- 1/16/fm*(1/f3-1/f1)
covlim[1,2]<-covlim[2,1]

mean(res2[1,]*res2[2,])
covlim[1,2]

invIfisher = matrix ( 0, nrow=2,ncol=2)
invIfisher[1,1] = (2*theta[2]^2)
invIfisher[2,2] = (2*theta[2]^2)

invIfisher 

pdf("fig-Cauchy.pdf", width = 10, height=7)
layout(matrix(1:6,2,3,byrow=TRUE))
hist(res[1,],freq=FALSE,nclass=40,xlim=c(-13,13),ylim=c(0,0.17),main=expression(list("MLE", m)),xlab="rescaled error")
x<-seq(-13,13,length=100)
y<-dnorm(x,mean=0,sd=sqrt(invIfisher[1,1]))
lines(x,y,col="red")
hist(res2[1,],freq=FALSE,nclass=40,xlim=c(-13,13),ylim=c(0,0.17),main=expression(list("QME", m)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(covlim[1,1]))
lines(x,y2,col="blue")
hist(res3[1,],freq=FALSE,nclass=40,xlim=c(-13,13),ylim=c(0,0.17),main=expression(list("LCE", m)),xlab="rescaled error")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=40,xlim=c(-13,13),ylim=c(0,0.17),main=expression(list("MLE", d)),xlab="rescaled error")
x<-seq(-13,13,length=100)
y<-dnorm(x,mean=0,sd=sqrt(invIfisher[2,2]))
lines(x,y,col="red")
hist(res2[2,],freq=FALSE,nclass=40,xlim=c(-13,13),ylim=c(0,0.17),main=expression(list("QME", d)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(covlim[2,2]))
lines(x,y2,col="blue")
hist(res3[2,],freq=FALSE,nclass=40,xlim=c(-13,13),ylim=c(0,0.17),main=expression(list("LCE", d)),xlab="rescaled error")
x<-seq(-13,13,length=100)
y<-dnorm(x,mean=0,sd=sqrt(invIfisher[2,2]))
lines(x,y,col="red")
dev.off()

row.names(tabtime)<-"Computation time (s)"

T11<-cvm.test(res[1,],null="pnorm",mean=0,sd=sqrt(invIfisher[1,1]))$statistic
T12<-cvm.test(res2[1,],null="pnorm",mean=0,sd=sqrt(covlim[1,1]))$statistic
T13<-cvm.test(res3[1,],null="pnorm",mean=0,sd=sqrt(invIfisher[1,1]))$statistic

T1<-data.frame(MLE=T11,LCE=T13,QME=T12)
row.names(T1)<-"CvM statistic m"

T21<-cvm.test(res[2,],null="pnorm",mean=0,sd=sqrt(invIfisher[2,2]))$statistic
T22<-cvm.test(res2[2,],null="pnorm",mean=0,sd=sqrt(covlim[2,2]))$statistic
T23<-cvm.test(res3[2,],null="pnorm",mean=0,sd=sqrt(invIfisher[2,2]))$statistic

T2<-data.frame(MLE=T21,LCE=T23,QME=T22)
row.names(T2)<-"CvM statistic d"

capture.output(xtable(rbind(tabtime,T1,T2), label="tab:cauchy:time:cvm",
                      caption="Computation time and CvM statistics"),
               file="tab-cauchy.tex")

