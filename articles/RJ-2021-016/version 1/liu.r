rm(list=ls())
install.packages("NTS")
install.packages("TSA")
install.packages("tsDyn")
library(NTS)
library(TSA)
library(tsDyn)
##################################################################
####Compare three functions, results are shown in table 2 in the paper

iter=200
n=200
thr=0.2
est=matrix(0,iter,3)


t0=proc.time()
for(i in 1:iter){
	set.seed(i)
	y=uTAR.sim(nob=n, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=thr, cnst=c(1,-1),sigma=c(1, 1))$series
	est1=uTAR.grid(y=y, p1=2, p2=2, d=2, thrQ=c(0,1),Trim=c(0.1,0.9),include.mean=T)
	est[i,1]=est1$thr
}
t1=proc.time()-t0

t0=proc.time()
for(i in 1:iter){
	set.seed(i)
	y=uTAR.sim(nob=n, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=thr, cnst=c(1,-1),sigma=c(1, 1))$series
	est2=uTAR(y=y,p1=2,p2=2,d=2,k0=50,thrQ=c(0,1),Trim=c(0.1,0.9),include.mean=T)
	est[i,2]=est2$thr
}
t2=proc.time()-t0


t0=proc.time()
for(i in 1:iter){
	set.seed(i)
	y=uTAR.sim(nob=n, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=thr, cnst=c(1,-1),sigma=c(1, 1))$series
	est3=tar(y=y,p1=2,p2=2,d=2,is.constant1 = T, is.constant2 = T,print=F,method="CLS")
	est[i,3]=est3$thd
}
t3=proc.time()-t0


print("Elapsed time for uTAR.grid with sample size 200:")
print(t1)
print("Elapsed time for uTAR with sample size 200:")
print(t2)
print("Elapsed time for tar with sample size 200:")
print(t3)
print("MSE for three functions:")
print(apply(abs(est-thr)^2,2,mean))



iter=200
n=2000
thr=0.2
est=matrix(0,iter,3)
t0=proc.time()
for(i in 1:iter){
	set.seed(i)
	y=uTAR.sim(nob=n, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=thr, cnst=c(1,-1),sigma=c(1, 1))$series
	est1=uTAR.grid(y=y, p1=2, p2=2, d=2, thrQ=c(0,1),Trim=c(0.1,0.9),include.mean=T)
	est[i,1]=est1$thr
}
t1=proc.time()-t0

t0=proc.time()
for(i in 1:iter){
	set.seed(i)
	y=uTAR.sim(nob=n, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=thr, cnst=c(1,-1),sigma=c(1, 1))$series
	est2=uTAR(y=y,p1=2,p2=2,d=2,k0=50,thrQ=c(0,1),Trim=c(0.1,0.9),include.mean=T)
	est[i,2]=est2$thr
}
t2=proc.time()-t0


t0=proc.time()
for(i in 1:iter){
	set.seed(i)
	y=uTAR.sim(nob=n, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=thr, cnst=c(1,-1),sigma=c(1, 1))$series
	est3=tar(y=y, p1=2, p2=2, d=2, is.constant1 = T, is.constant2 = T, print=F,method="CLS")
	est[i,3]=est3$thd
}
t3=proc.time()-t0


print("Elapsed time for uTAR.grid with sample size 2000:")
print(t1)
print("Elapsed time for uTAR with sample size 2000:")
print(t2)
print("Elapsed time for tar with sample size 2000:")
print(t3)
print("MSE for three functions:")
print(apply(abs(est-thr)^2,2,mean))



###############################################################################
### Example to show how to use uTAR.est, uTAR.pred, backTAR
set.seed(1687)
y=uTAR.sim(nob=2000, arorder=c(2,2), phi=t(matrix(c(-0.3, 0.5,0.6,-0.3),2,2)), d=2, thr=0.2, cnst=c(1,-1), sigma=c(1, 1))

#pdf("SETAR.pdf",height=4,width=8)
plot(y$series[1:200],type='l',xlab='Time',ylab='',main='Time series plot of a SETAR process')
#dev.off()
thr.est=uTAR.grid(y=y$series, p1=2, p2=2, d=2, thrQ=c(0,1),Trim=c(0.1,0.9))
est=uTAR.est(y=y$series, arorder=c(2,2), thr=thr.est$thr,d=2)
uTAR.pred(mode=est, orig=2000, h=1, iteration=100, ci=0.95, output=TRUE)

set.seed(11)
backTAR(est,50,1,3000)


thr.est2=uTAR.grid(y=y$series, p1=2, p2=2, d=1, thrQ=c(0,1),Trim=c(0.1, 0.9), include.mean=T)
est2=uTAR.est(y=y$series, arorder=c(2,2), thr=thr.est2$thr, d=1)
set.seed(11)
backTAR(est2,50,1,3000)



##############################################################################
###ACMx
set.seed(12)
x=rnorm(1000)*0.1
y=matrix(0,1000,1)
y[1]=2
lambda=matrix(0,1000,1)
for (i in 2:1000){
	lambda[i]=2+0.2*y[i-1]/exp(x[i-1])+0.5*lambda[i-1]
	set.seed(i)
	y[i]=rpois(1,exp(x[i])*lambda[i])
}
ACMx(y,order=c(1,1),x,"po")

###############################################################################
#####CFAR
phi_func= function(x)
{
	return(dnorm(x, mean=0, sd=0.1))
}
set.seed(101)
t=1000; N=50;
x=g_cfar(t, rho=5, phi_func, sigma=1, ini=100)
f=x$cfar[,seq(1,1001,1000/N)]
F_test_cfar(f, p.max=2, df_b=10, grid=1000)
model=est_cfar(f, p=1, df_b=10, grid=1000)
print(c(model$rho, model$sigma))

num_obs = rep(N+1, t); x_pos = matrix(rep(seq(0, 1, 1/N), each = t), t, N+1);
weight0=function(x){return(rep(1,length(x)))}
F_test_cfarh(f, weight0,p.max=2, df_b=10, grid=1000,num_obs, x_pos)
modelh=est_cfarh(f, weight0, p=1, df_b=10, grid=1000, num_obs,x_pos)
print(c(modelh$rho, modelh$sigma))


#pdf("cfar.pdf",height=5,width=8)
plot(seq(-1,1,0.01),phi_func(seq(-1,1,0.01)),type='l', ylab='',xlab='')
points(seq(-1,1,1/1000),model$phi_func,type='l',lty=2, col=1)
legend.txt=c("True", "Estimated")
legend("topright",legend.txt,lty=1:2,col=c(1,1))
#dev.off()
pred=p_cfar(model, f, m = 3)



#########################################################################
#####SMC

simuRW=function(nobs,ssw,ssv,xx0){
 	xx = 1:nobs
 	ww = rnorm(nobs)*ssw
 	vv = rnorm(nobs)*ssv
 	xx[1] = xx0+rnorm(1)
 	for(i in 2:nobs){
 	 	xx[i] = xx[i-1]+ww[i]
 	}
 	yy = xx+vv
 	return(list(xx=xx,yy=yy))
}

### function of SIS step with full information
SISstep.RW.Full=function(mm,xx,logww,yyy,par,xdim,ydim,resample.sch){
 	ssw2 = par$ssw**2; ssv2 = par$ssv**2
 	tau2 = 1/(1/ssw2+1/ssv2); tau = sqrt(tau2)
 	mu = (xx/ssw2+yyy/ssv2)*tau2
 	logww = logww-xx**2/2/ssw2-yyy**2/2/ssv2+mu**2/2/tau2
 	logww = logww-max(logww)
 	if(resample.sch==0) r.index=rep(0,mm)
 	if(resample.sch==1){
 	 	ww = exp(logww)
 	 	ww = ww/sum(ww)
 	 	r.index = sample.int(mm,size=mm,replace=T,prob=ww)
 	 	xx = xx[r.index]
 	 	mu = mu[r.index]
 	 	logww = logww*0
 	 }
 	xx = mu+ rnorm(mm)*tau
 	return(list(xx=xx,logww=logww,r.index=r.index))
}

### function of SIS step Bootstrap filter
SISstep.RW.B=function(mm,xx,logww,yyy,par,xdim,ydim){
 	ee = rnorm(mm)*par$ssw
 	xx = xx+ee
 	logww = logww+(xx*yyy-xx**2/2)/par$ssv**2
 	logww = logww-max(logww)
 	return(list(xx=xx,logww=logww))
}

### function of RW SIS step independent filter
SISstep.RW.I=function(mm,xx,logww,yyy,par,xdim,ydim){
 	ee = rnorm(mm)*par$ssv
 	xx.new = matrix(yyy-ee,nrow=1)
 	logww = logww-(xx.new-xx)**2/2/par$ssw**2
 	logww = logww-max(logww)
 	return(list(xx=xx.new,logww=logww))
}

### function KF for tracking in clutter with known indicator I_t
RW.KF=function(nobs,ssw,ssv,yy){
 	xhat = 1:nobs; bb = 0; cc = 0
 	HH = matrix(1,ncol=1,nrow=1,byrow=T)
 	GG = matrix(1,ncol=1,nrow=1,byrow=T)
 	WW= ssw; VV= ssv; mu = 0; SS = 1
 	for(i in 1:nobs){
 	 	outP = KF1pred(mu,SS,HH,bb,WW)
 	 	mu = outP$mu
 	 	SS = outP$SS
 	 	outU = KF1update(mu,SS,yy[i],GG,cc,VV)
 	 	mu = outU$mu
 	 	SS = outU$SS
 	 	xhat[i] = mu
 	 	}
 	 	return(list(xhat=xhat))
}


KF1pred=function(mu,SS,HH,bb,WW){
  mu=HH%*%mu+bb
  SS=HH%*%SS%*%t(HH)+WW%*%t(WW)
  return(list(mu=mu,SS=SS))
}
KF1update=function(mu,SS,yy,GG,cc,VV){
  KK=t(solve(GG%*%SS%*%t(GG)+VV%*%t(VV),t(SS%*%t(GG))))
  mu=mu+KK%*%(yy-cc-GG%*%mu)
  SS=SS-KK%*%GG%*%SS
  return(list(mu=mu,SS=SS))
}

nobs=200; xdim=1;ydim=1
mm=100;
set.seed(100)
xx.init=rnorm(mm);
sch=c(1,5,200); ss=matrix(c(1,1,1,4,4,1),ncol=2,byrow=T)
xx0=0; nsimu=50
SSE1=SSE2=SSE3=matrix(0,ncol=3,nrow=3); SSE4=rep(0,3)
for(iss in 1:3){
 	ssw=ss[iss,1]
 	ssv=ss[iss,2]
	set.seed(iss)
 	simu=simuRW(nobs,ssw,ssv,xx0)
 	par=list(ssw=ssw,ssv=ssv)
 	for(i in 1:nsimu){
 	 	for(isch in 1:3){
 	 	 	resample.sch=rep(c(rep(0,sch[isch]-1),1),nobs/sch[isch])
			set.seed(i)
 	 	 	out1=SMC.Full(SISstep.RW.Full,nobs,simu$yy,mm,par,xx.init,
 	 	 	xdim,ydim,resample.sch,delay=0)
			set.seed(i)
 	 	 	out2=SMC(SISstep.RW.B,nobs,simu$yy,mm,par,xx.init,xdim,ydim,resample.sch,delay=0)
			set.seed(i)
 	 	 	out3=SMC(SISstep.RW.I,nobs,simu$yy,mm,par,xx.init,xdim,ydim,resample.sch,delay=0)
 	 	 	SSE1[iss,isch]=SSE1[iss,isch]+sum((simu$xx-out1$xhat[1,,1])**2)
 	 	 	SSE2[iss,isch]=SSE2[iss,isch]+sum((simu$xx-out2$xhat[1,,1])**2)
 	 	 	SSE3[iss,isch]=SSE3[iss,isch]+sum((simu$xx-out3$xhat[1,,1])**2)
 	 	}
 	 	out4=RW.KF(nobs,ssw,ssv,simu$yy)
 	 	SSE4[iss]=SSE4[iss]+sum((simu$xx-out4$xhat)**2)
 	 }
}
MSE1=SSE1/nsimu/nobs; MSE2=SSE2/nsimu/nobs
MSE3=SSE3/nsimu/nobs; MSE4=SSE4/nsimu/nobs

SISstep.Smooth.RW=function(mm,xxt,xxt1,ww,vv,par){
	ssw = par$ssw
	uu = 1:mm
	aa = 1:mm
	for(i in 1:mm){
		res = (xxt[i]-xxt1)/ssw
		aa[i] = sum(exp(-res**2/2)*ww)
	}
	for(j in 1:mm){
		res = (xxt-xxt1[j])/ssw
		uu[j] = sum(exp(-res**2/2)*vv/aa)
	}
	vv = ww*uu
	return(list(vv=vv))
}

resample.sch=rep(1,nobs)
SSE5=matrix(0,3,3); SSE6=matrix(0,3,3)
sch=c(1,5,10);
for(iss in 1:3){
 	ssw=ss[iss,1]
 	ssv=ss[iss,2]
	set.seed(iss)
 	simu=simuRW(nobs,ssw,ssv,xx0)
 	par=list(ssw=ssw,ssv=ssv)
 	for(i in 1:nsimu){
 	 	for(isch in 1:3){
 	 	 	resample.sch=rep(c(rep(0,sch[isch]-1),1),nobs/sch[isch])
			set.seed(i)
			out4=SMC.Smooth(SISstep.RW.B,SISstep.Smooth.RW,nobs,simu$yy,mm,par,xx.init,xdim,ydim,resample.sch)
			set.seed(i)
			out5=SMC.Smooth(SISstep.RW.I,SISstep.Smooth.RW,nobs,simu$yy,mm,par,xx.init,xdim,ydim,resample.sch)
 		 	SSE5[iss,isch]=SSE5[iss,isch]+sum((simu$xx-out4$xhat[1,])**2)
 		 	SSE6[iss,isch]=SSE6[iss,isch]+sum((simu$xx-out5$xhat[1,])**2)
		}
	}
}
MSE5=SSE5/nsimu/nobs; MSE6=SSE6/nsimu/nobs




