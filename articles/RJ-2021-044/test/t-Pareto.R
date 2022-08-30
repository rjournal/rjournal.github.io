rm(list=ls(all=TRUE))

library(fitdistrplus)
library(actuar)
library(xtable)
library(goftest)
library(OneStep)

set.seed(1234)

n<-10000
theta <- c(1.1,0.3)
delta<-0.9
shape <- theta[1]
scale <- theta[2]

M<-10000
#M <- 500

tabMLE<-matrix(0,2,M)
tabMLE2<-matrix(0,2,M)
tabLCE<-matrix(0,2,M)
tabLCEinit<-matrix(0,2,M)

timeMLE<-0
timeMLE2<-0
timeLCE<-0



for (i in 1:M){
  
  #Simulate the sample
  o.sample<-rpareto(n,shape=theta[1],scale=theta[2])
  
  # #MLE
  # ttmp<-proc.time()
  # esttmp<- mledist(data = o.sample,distr = "pareto")$estimate 
  # ttmp<-proc.time()-ttmp
  # 
  # tabMLE[,i]<-c(esttmp[1],esttmp[2])
  # timeMLE<-timeMLE + ttmp
  
  #MLE o nsubsample
  ttmp<-proc.time()
  esttmp<- mledist(data = o.sample[1:as.integer(n^delta)],distr = "pareto")$estimate 
  ttmp<-proc.time()-ttmp
  
  tabMLE2[,i]<-c(esttmp[1],esttmp[2])
  timeMLE2<-timeMLE2 + ttmp

  #OSMLE (Fisher scoring)
  # 
  # ttmp<-proc.time()
  # esttmp<-onestep(o.sample,"pareto")$estimate
  # ttmp<-proc.time()-ttmp
  # 
  # tabLCE[,i]<-c(esttmp[1],esttmp[2])
  # timeLCE<-timeLCE  + ttmp


  #tabLCEinit[,i]<-onestep(o.sample,"pareto",init=list(shape=theta[1],scale=theta[2]))$estimate

 if (i%%100==0) print(i)

}


system.time(respareto4 <- benchonestep.replicate(n, M, "pareto", shape=theta[1], scale=theta[2], 
                                                 methods=c("mle", "onestep"), ncpus=1))

save(respareto4, tabMLE2, timeMLE2, file=paste0("res-pareto-n",max(n), "-M", max(M),".RData"))

if(FALSE)
  load(paste0("res-pareto-n10000-M10000.RData"))

tabMLE <- respareto4[c("shape","scale"),"mle", ]
tabLCE <- respareto4[c("shape","scale"),"onestep", ]


###Visualisation et Information de Fisher

res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
res2<-sqrt(n)*(tabMLE2-matrix(rep(theta,M),2,M))
res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))
#res4<-sqrt(n)*(tabLCEinit-matrix(rep(theta,M),2,M))

tabtime<-data.frame(MLE=sum(respareto4["time","mle", ]), 
                    MLEsub=timeMLE2[3],
                    LCE = sum(respareto4["time","onestep", ]))

scale  <- theta[2]   
shape <- theta[1]  


Info <- matrix(0,2,2)
Info[1,1]<- 1/(shape^2)
Info[2,1]<- -1/(scale*(shape+1))
Info[1,2]<- -1/(scale*(shape+1)) 
Info[2,2]<- shape/(scale^2*(shape+2))
covmat<-solve(Info)

### Marginales
pdf("fig-Pareto.pdf", width = 10, height=7)
nclass <- 400
nclass2 <- 100
layout(matrix(1:6,2,3,byrow=TRUE))
hist(res[1,],freq=FALSE,nclass=nclass,xlim=c(-10,10),ylim=c(0,0.2),main=expression(list("MLE", alpha)),xlab="rescaled error")
x<-seq(-15,15,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covmat[1,1]))
lines(x,y,col="red")
hist(res2[1,],freq=FALSE,nclass=nclass2,xlim=c(-10,10),ylim=c(0,0.2),main=expression(list("sub-MLE", alpha)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(as.integer(n^(1-delta))*covmat[1,1]))
lines(x,y2,col="blue")
hist(res3[1,],freq=FALSE,nclass=nclass2,xlim=c(-10,10),ylim=c(0,0.2),main=expression(list("LCE", alpha)),xlab="rescaled error")
lines(x,y,col="red")

hist(res[2,],freq=FALSE,nclass=nclass,xlim=c(-4,4),ylim=c(0,0.4),main=expression(list("MLE", sigma)),xlab="rescaled error")
x<-seq(-6,6,length=100)
y<-dnorm(x,mean=0,sd=sqrt(covmat[2,2]))
lines(x,y,col="red")
hist(res2[2,],freq=FALSE,nclass=nclass2,xlim=c(-4,4),ylim=c(0,0.4),main=expression(list("sub-MLE", sigma)),xlab="rescaled error")
lines(x,y,col="red")
y2<-dnorm(x,mean=0,sd=sqrt(as.integer(n^(1-delta))*covmat[2,2]))
 lines(x,y2,col="blue")
hist(res3[2,],freq=FALSE,nclass=nclass2,xlim=c(-4,4),ylim=c(0,0.4),main=expression(list("lCE", sigma)),xlab="rescaled error")
lines(x,y,col="red")

dev.off()

row.names(tabtime)<-"Computation time (s)"

T11<-cvm.test(res[1,],null="pnorm",mean=0,sd=sqrt(covmat[1,1]))$statistic
T12<-cvm.test(res2[1,],null="pnorm",mean=0,sd=sqrt(as.integer(n^(1-delta))*covmat[1,1]))$statistic
T13<-cvm.test(res3[1,],null="pnorm",mean=0,sd=sqrt(covmat[1,1]))$statistic

T1<-data.frame(MLE=T11,LCE=T13,MLEsub=T12)
row.names(T1)<-"CvM statistic alpha"

T21<-cvm.test(res[2,],null="pnorm",mean=0,sd=sqrt(covmat[2,2]))$statistic
T22<-cvm.test(res2[2,],null="pnorm",mean=0,sd=sqrt(as.integer(n^(1-delta))*covmat[2,2]))$statistic
T23<-cvm.test(res3[2,],null="pnorm",mean=0,sd=sqrt(covmat[2,2]))$statistic

T2<-data.frame(MLE=T21,LCE=T23,MLEsub=T22)
row.names(T2)<-"CvM statistic sigma"

capture.output(xtable(rbind(tabtime,T1,T2), label="tab:pareto:time:cvm",
                      caption="Computation time and CvM statistics"),
               file="tab-pareto.tex")


cvm.test(res2[1,],null="pnorm",mean=mean(res2[1,]),sd=sqrt(as.integer(n^(1-delta))*covmat[1,1]))$statistic
cvm.test(res3[1,],null="pnorm",mean=mean(res3[1,]),sd=sqrt(covmat[1,1]))$statistic
#cvm.test(res4[1,],null="pnorm",mean=0,sd=sqrt(covmat[1,1]))$statistic


cvm.test(res2[2,],null="pnorm",mean=mean(res2[2,]),sd=sqrt(as.integer(n^(1-delta))*covmat[2,2]))$statistic
cvm.test(res3[2,],null="pnorm",mean=mean(res3[2,]),sd=sqrt(covmat[2,2]))$statistic
#cvm.test(res4[2,],null="pnorm",mean=0,sd=sqrt(covmat[2,2]))$statistic


