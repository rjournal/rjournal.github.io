#This code reproduces all figures, tables and code examples in the manuscript.

library(TULIP)


# -------------------------------------------------------------------------------
#Code in manuscript

set.seed(123456)
dat.vec<-sim.bi.vector(1000)

obj <- dsda(dat.vec$x, y=dat.vec$y, lambda=seq(0.005, 0.3, length.out=20))
pred <- predict(obj, dat.vec$testx)
err<- apply(pred, 2, function(x){mean(x!=dat.vec$testy)})
print(min(err)) #0.111
print(obj$lambda[which.min(err)]) #0.02052632



obj <- dsda(dat.vec$x, y=dat.vec$y,testx=dat.vec$testx)
err<- apply(obj$pred, 2, function(x){mean(x!=dat.vec$testy)})
print(min(err)) #0.107
print(obj$lambda[which.min(err)])  #0.03180946



set.seed(1234) #
dat.vec<-sim.bi.vector(100)

x <- exp(dat.vec$x)
testx <- exp(dat.vec$testx)
obj.SeSDA <- SeSDA(dat.vec$x, y=dat.vec$y)
pred.SeSDA <- predict(obj.SeSDA, testx)
err <- apply(pred.SeSDA, 2, function(x){mean(x!=dat.vec$testy)})
min(err) #0.28

#
# obj.dsda <- dsda(dat.vec$x, y=dat.vec$y, testx=testx)
# err <- apply(obj.dsda$pred, 2, function(x){mean(x!=dat.vec$testy)})
# min(err) #0.34




obj.dsda <- dsda(dat.vec$x, y=dat.vec$y, lambda=seq(0.1, 0.5, length.out=20))
obj.road <- ROAD(dat.vec$x, y=dat.vec$y, lambda=seq(0.1, 0.5, length.out=20))
obj.sos <- SOS(dat.vec$x, y=dat.vec$y, lambda=seq(0.1, 0.5, length.out=20))


data(GDS1615)
x <- GDS1615$x
y <- GDS1615$y
set.seed(123456)
teindex <- c(sample(which(y==1), sum(y==1)/3), sample (which(y==2), sum(y==2)/3), sample(which(y==3), sum(y==3)/3))
obj <- msda(x[-teindex, ], y=y[-teindex], testx=x[teindex, ])
err <- apply(obj$pred, 2, function(x){mean(x!=y[teindex])})
min(err)
paste(min(err), obj$lambda[which.min(err)], obj$df[which.min(err)] ) #"0.0487804878048781 1.44687193087759 19"


set.seed(123456)
dat.ten<-sim.tensor.cov(1000)

obj <- catch(dat.ten$x, dat.ten$z, dat.ten$y, dat.ten$testx, dat.ten$testz)
pred <- obj$pred
err <- apply(pred, 2, function(x){mean(x!=dat.ten$testy)})
min(err) #0.167
obj$lambda[which.min(err)] #0.4270712


data(csa)
x <- csa$IDLH
y <- csa$y
teindex <- seq(1,147,7)
obj <- catch(x[-teindex, ], y=y[-teindex], testx=x[teindex, ], nlambda=10)
err <- apply(obj$pred, 2, function(x){mean(x!=y[teindex])})
print(err) #"0.9523810 0.1904762 0.0952381 0.0000000 0.0952381 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000"

obj <- dsda.all(dat.vec$x, dat.vec$y, dat.vec$testx, dat.vec$testy, nfolds = 10)
print(obj$err) #0.116

obj.dsda <- cv.dsda(dat.vec$x, dat.vec$y, nfolds = 10)
obj.catch <- cv.catch(dat.ten$x, dat.ten$z, dat.ten$y, lambda.opt="min")

obj <- catch(dat.ten$x, dat.ten$z, dat.ten$y, dat.ten$testx, dat.ten$testz)
obj.adj <- adjten(dat.ten$x, dat.ten$z, dat.ten$y, dat.ten$testx,
                     dat.ten$testz)
obj.fit <- catch(dat.ten$x, dat.ten$z, dat.ten$y)
pred <- predict(obj.fit, obj.adj$testxres, dat.ten$z, dat.ten$testz,
                   obj.adj$gamma)

# -------------------------------------------------------------------------------------
# Code for Figure 4
library(ggplot2)

set.seed(123456)
dat <- sim.bi.vector(tesize=100)
trx=dat$x
try=dat$y

obj <- dsda(trx, y=try, lambda = seq(0.25, 0.55, length.out=100))
nlambda = length(obj$lambda)
coef = matrix(0, nrow=nlambda, ncol=5)

for (i in 1:nlambda){
  coef[i,1]=obj$beta[24,i]
  coef[i,2]=obj$beta[57,i]
  coef[i,3]=obj$beta[128,i]
  coef[i,4]=obj$beta[236,i]
  coef[i,5]=obj$beta[267,i]
}

datour=data.frame(var=as.factor(c(rep(1,nlambda),rep(2,nlambda),rep(3,nlambda),rep(4,nlambda),rep(5,nlambda))),lambda=rep(obj$lambda,5),coefficient=c(coef[,1],coef[,2],coef[,3],coef[,4],coef[,5]))
g=ggplot(data=datour, aes(x=lambda, y=coefficient, group=var)) +  geom_line(aes(color=var))+theme_bw()
g=g+labs(title='Solution path of DSDA',x='lambda')
g=g+theme(legend.position="none")
g

# -------------------------------------------------------------------------------------
# Code for Figure 5

set.seed(123456)
sigma <- matrix(0.3, 500, 500)
diag(sigma) <- 1
dsigma <- t(chol(sigma))
#define beta and mean
beta <- matrix(0, nrow = 500, ncol = 1)
beta[1:10,1] <- 0.5
M <- matrix(0, nrow = 2, ncol = 500)
M[2,] <- sigma%*%beta
y <- c(rep(1, 75), rep(2, 75))
#generate test data
telabel <- ceiling(runif(1000)*2)
vec_x <- matrix(rnorm(150*500),ncol = 500)
x <- vec_x%*%t(dsigma)
x[y==2, ] <- x[y==2, ] + M[2,]
vec_testx <- matrix(rnorm(1000*500), ncol = 500)
testx <- vec_testx %*% t(dsigma)
testx[telabel==2, ] <- testx[telabel==2, ] + M[2, ]
dat.vec <- list(x = x, y = y, testx = testx, testy = telabel)

#original
par(mfrow=c(1,2))
expx <- exp(x)
d<-density(expx[which(y==1),1], bw=0.75)
plot(d, main="Class 1")
d<-density(expx[which(y==2),1], bw=0.75)
plot(d, main="Class 2")


#pooled
par(mfrow=c(1,2))
obj.norm <- getnorm(expx[,1],y)
xnew = obj.norm$x.norm
xtmp=xnew[which(y==1)]
d<-density(xtmp, bw=0.75)
plot(d, main="Pooled_class 1")
xtmp=xnew[which(y==2)]
d<-density(xtmp, bw=0.75)
plot(d, main="Pooled_class 2")

#naive
par(mfrow=c(1,2))
obj.norm <- getnorm(expx[,1],y, type="naive")
xnew = obj.norm$x.norm
xtmp=xnew[which(y==1)]
d<-density(xtmp, bw=0.75)
plot(d, main="Naive_class 1")
xtmp=xnew[which(y==2)]
d<-density(xtmp, bw=0.75)
plot(d, main="Naive_class 2")

# ------------------------------------------------------------------------------------
# Code for Figure 6

set.seed(123456)
dat <- sim.bi.vector(tesize=100)
obj.dsda <- dsda(dat.vec$x, y=dat.vec$y, lambda=seq(0.1,0.45,length.out=20))
lam.dsda <- obj.dsda$lambda
obj.sos <- SOS(dat.vec$x, y=dat.vec$y, lambda=lam.dsda)
lam.sos <- obj.sos$lambda
obj.road <- ROAD(dat.vec$x, y=dat.vec$y, lambda=lam.dsda)
lam.road <- obj.road$lambda

plot(x=lam.dsda, y=lam.road, line='-')
dat=data.frame(lam.dsda=lam.dsda, lam.road=lam.road)
g<-ggplot(data=dat, aes(x=lam.dsda, y=lam.road))
g<-g+geom_point()+theme_bw()
g<-g+xlab("lam.dsda (=2*lam.sos)")+ggtitle("Parameter among three methods")
g




# ------------------------------------------------------------------------------------
# Code for Figure 7
library(TULIP)
library(ggplot2)
data("GDS1615")
x <- GDS1615$x
y <- GDS1615$y
set.seed(13456)
teindex <- c(sample(which(y==1), sum(y==1)/3), sample(which(y==2), sum(y==2)/3), sample(which(y==3), sum(y==3)/3))
obj <- msda(x[-teindex,], y=y[-teindex], testx=x[teindex,])
err <- apply(obj$pred, 2, function(x){mean(x!=y[teindex])})

theta <- obj$beta[24][[1]]

w=x[teindex,]%*%theta
pcaob=prcomp(w,scale=TRUE)
newx=pcaob$x[,1:2]
PC1=newx[,1]
PC2=newx[,2]


class=as.factor(y[teindex])
dat<-data.frame(PC1,PC2,class)
myplot<-ggplot(dat,aes(x=PC1,y=PC2,shape=class))+scale_shape_manual(values=c(1, 3, 17))
myplot<-myplot+geom_point()+theme_bw()
#myplot<-myplot+theme(legend.position="none")
myplot<-myplot+ggtitle('GDS1615 clusters')+ theme(plot.title = element_text(hjust = 0.5))
myplot+xlab('MSDA-PC1')+ylab('MSDA-PC2')

myplot

#----------------------------------------------------------------------------------------
#ADHD real data binary case
set.seed(123456)
library(R.matlab)
library(glmnet)
library(Matrix)
library(MASS)
library(tensr)
library(TULIP)
library(sparseLDA)

load("bitrans.RData")
pathname<-file.path('./ADHD_test_resize0.mat')
testdat<-readMat(pathname)
Xtesttem=testdat$testX
pathname<-file.path('./ADHD_train_resize0.mat')
traindat<-readMat(pathname)
Xtraintem=traindat$X


########################transformation 1
minx=min(Xtraintem[Xtraintem!=0])
Xtraintem[which(Xtraintem==0)]=minx/2
Xtesttem[which(Xtesttem==0)]=minx/2
Xtraintem=log(Xtraintem)
Xtesttem=log(Xtesttem)
############################################

dimen=dim(Xtesttem)[1:3]
nvars=prod(dimen)
nclass=length(unique(traindat$y))
ntrain=length(traindat$y)
ntest=length(testdat$testy)
n=ntrain+ntest
vec_x=matrix(0,nrow=nvars,ncol=n)
x=array(list(),ntrain+ntest)
for (i in 1:ntrain){
  x[[i]]=Xtraintem[,,,i]
  vec_x[,i]=matrix(x[[i]],nrow=nvars,ncol=1)
}
for (i in 1:ntest){
  x[[i+ntrain]]=Xtesttem[,,,i]
  vec_x[,i+ntrain]=matrix(x[[i+ntrain]],nrow=nvars,ncol=1)
}
vec_x=t(vec_x)


y=c(traindat$y,testdat$testy)
y=y+1
y=as.vector(y)
y[y==4]=1
y[y==3]=2
age=c(traindat$age,testdat$testage)
gender=c(traindat$gender,testdat$testgender)
hand=c(traindat$hand,testdat$testhand)
z=cbind(age,hand)
vec_x=cbind(vec_x,z)


sd=apply(vec_x,2,sd)
sd[sd==0]=1
vec_x=scale(vec_x,center=TRUE,scale=sd)
for (i in 1:dim(vec_x)[1]){
  x[[i]]=array(vec_x[i,1:nvars],dimen)
}


nf=sum(gender)
nm=n-nf  #suppose male correspondes to z[,1]=0
indexm=which(gender==0)
indexf=which(gender==1)
xm=x[indexm]
xf=x[indexf]
vec_xm=vec_x[indexm,]
vec_xf=vec_x[indexf,]
ym=y[indexm]
yf=y[indexf]
zm=z[indexm,]
zf=z[indexf,]




run=20 #100 for recording error rates
tlog=0
tcatch=0
tdsda=0
errlogm=rep(0,run)
errlogf=rep(0,run)
errlog=rep(0,run)
errcatchm=rep(0,run)
errcatchf=rep(0,run)
errcatch=rep(0,run)
errdsdam=rep(0,run)
errdsdaf=rep(0,run)
errdsda=rep(0,run)
errsesda=rep(0,run)
tsesda=0
errsosm=rep(0,run)
errsosf=rep(0,run)
errsos=rep(0,run)
t=0
for (irun in 1:run){

  print(irun)

  #male
  teindex=teindexm[[irun]]

  vec_xtrm=vec_xm[-teindex,]
  vec_xtem=vec_xm[teindex,]
  ytrm=ym[-teindex]
  ytem=ym[teindex]
  xtrm=xm[-teindex]
  xtem=xm[teindex]
  ztrm=zm[-teindex,]
  ztem=zm[teindex,]
  ntrm=length(ytrm)


  #female
  teindex=teindexf[[irun]]

  vec_xtrf=vec_xf[-teindex,]
  vec_xtef=vec_xf[teindex,]
  ytrf=yf[-teindex]
  ytef=yf[teindex]
  xtrf=xf[-teindex]
  xtef=xf[teindex]
  ztrf=zf[-teindex,]
  ztef=zf[teindex,]
  ntrf=length(ytrf)




  #l1 logistic
  obj<-cv.glmnet(vec_xtrm,ytrm,family="binomial")
  pred=predict(obj,vec_xtem,type='class',s='lambda.min')
  t1=Sys.time()
  obj <- glmnet(vec_xtrm,ytrm,family="binomial",lambda=obj$lambda)
  tlog=tlog+Sys.time()-t1
  errlogm[irun]=sum(pred!=ytem)/length(ytem)
  errlog[irun]=sum(pred!=ytem)

  obj<-cv.glmnet(vec_xtrf,ytrf,family="binomial")
  pred=predict(obj,vec_xtef,type='class',s='lambda.min')
  t1=Sys.time()
  obj <- glmnet(vec_xtrf,ytrf,family="binomial",lambda=obj$lambda)
  tlog=tlog+Sys.time()-t1
  errlogf[irun]=sum(pred!=ytef)/length(ytef)
  errlog[irun]=errlog[irun]+sum(pred!=ytef)
  errlog[irun]=errlog[irun]/(length(ytem)+length(ytef))


  #catch
  #male
  obj=cv.catch(xtrm,ztrm,y=ytrm)
  t1=Sys.time()
  obj=catch(xtrm,ztrm,y=ytrm,lambda=obj$lambda.min)
  tcatch=tcatch+Sys.time()-t1
  pred=predict(obj,newx=xtem,newz=ztem)
  errcatchm[irun]=sum(pred!=ytem)/length(ytem)
  errcatch[irun]=sum(pred!=ytem)



  obj=cv.catch(xtrf,ztrf,y=ytrf)
  t1=Sys.time()
  obj=catch(xtrf,ztrf,y=ytrf,lambda=obj$lambda.min)
  tcatch=tcatch+Sys.time()-t1
  pred=predict(obj,newx=xtef,newz=ztef)
  errcatchf[irun]=sum(pred!=ytef)/length(ytef)
  errcatch[irun]=errcatch[irun]+sum(pred!=ytef)
  errcatch[irun]=errcatch[irun]/(length(ytem)+length(ytef))


  #dsda
  obj<-cv.dsda(vec_xtrm,ytrm,nfolds=5)
  t1=Sys.time()
  obj<-dsda(vec_xtrm,y=ytrm,lambda=obj$lambda.min)
  tdsda=tdsda+Sys.time()-t1
  pred<-as.vector(predict(obj,vec_xtem))
  errdsdam[irun]=sum(pred!=ytem)/length(ytem)
  errdsda[irun]=sum(pred!=ytem)




  obj<-cv.dsda(vec_xtrf,ytrf,nfolds=5)
  t1=Sys.time()
  obj<-dsda(vec_xtrf,y=ytrf,lambda=obj$lambda.min)
  tdsda=tdsda+Sys.time()-t1
  pred<-as.vector(predict(obj,vec_xtef))
  errdsdaf[irun]=sum(pred!=ytef)/length(ytef)
  errdsda[irun]=(sum(pred!=ytef)+errdsda[irun])/(length(ytem)+length(ytef))


  #sesda
  obj<-cv.SeSDA(vec_xtrm,ytrm)
  t1=Sys.time()
  obj<-SeSDA(vec_xtrm,ytrm,lambda=obj$objdsda$lambda.min)
  tsesda=tsesda+Sys.time()-t1
  pred=predict.SeSDA(obj,vec_xtem)
  errsesda[irun]=sum(pred!=ytem)



  obj<-cv.SeSDA(vec_xtrf,ytrf)
  t1=Sys.time()
  obj<-SeSDA(vec_xtrf,ytrf,lambda=obj$objdsda$lambda.min)
  tsesda=tsesda+Sys.time()-t1
  pred=predict.SeSDA(obj,vec_xtef)

  errsesda[irun]=errsesda[irun]+sum(pred!=ytef)
  errsesda[irun]=errsesda[irun]/(length(ytem)+length(ytef))

  #sos
  #SOS
  sym=matrix(0,nrow=ntrm,ncol=2)
  syf=matrix(0,nrow=ntrf,ncol=2)
  for (i in 1:ntrm){
    sym[i,ytrm[i]]=1
  }
  for (i in 1:ntrf){
    syf[i,ytrf[i]]=1
  }
  nam=paste("Class",1:2,sep="")
  colnames(sym)<-c(nam)
  colnames(syf)<-c(nam)

  Xcm=normalize(vec_xtrm)
  Xcf=normalize(vec_xtrf)
  svec_x_allm=Xcm$Xc
  svec_x_allf=Xcf$Xc

  svec_testxm=normalizetest(vec_xtem,Xcm)
  svec_testxf=normalizetest(vec_xtef,Xcf)

  #male
  all.folds<-split(sample(1:ntrm),rep(1:5,length=ntrm))
  lamrange=seq(-30,-6,3)
  errcv=rep(0,length(lamrange))
  for (fold in 1:5){
    print(fold)
    omit=all.folds[[fold]]
    svec_vaxm=svec_x_allm[omit,]
    svec_xm=svec_x_allm[-omit,]
    sytrm=sym[-omit,]
    yvam=ytrm[omit]
    for (ilam in 1:length(lamrange)){
      outs<-sda(data.frame(svec_xm),sytrm,lambda=1e-6,stop=lamrange[ilam],trace=FALSE)
      testvam<-predict(outs,data.frame(svec_vaxm))
      predvam<-as.numeric(testvam$class)
      errcv[ilam]=errcv[ilam]+sum(predvam!=yvam)/length(yvam)
    }
  }
  lamtunem=lamrange[which.min(errcv)]
  t1=Sys.time()
  obj=sda(data.frame(svec_x_allm),sym,lambda=1e-6,stop=lamtunem,trace=FALSE)
  t=t+Sys.time()-t1
  testm<-predict(obj,data.frame(svec_testxm))
  predsm<-as.numeric(testm$class)
  errsosm[irun]=sum(predsm!=ytem)/length(ytem)
  errsos[irun]=sum(predsm!=ytem)



  #female
  all.folds<-split(sample(1:ntrf),rep(1:5,length=ntrf))
  lamrange=seq(-30,-6,3)
  errcv=rep(0,length(lamrange))
  for (fold in 1:5){
    print(fold)
    omit=all.folds[[fold]]
    svec_vaxf=svec_x_allf[omit,]
    svec_xf=svec_x_allf[-omit,]
    sytrf=syf[-omit,]
    yvaf=ytrf[omit]
    for (ilam in 1:length(lamrange)){
      outs<-sda(data.frame(svec_xf),sytrf,lambda=1e-6,stop=lamrange[ilam],trace=FALSE)
      testvaf<-predict(outs,data.frame(svec_vaxf))
      predvaf<-as.numeric(testvaf$class)
      errcv[ilam]=errcv[ilam]+sum(predvaf!=yvaf)/length(yvaf)
    }
  }
  lamtunef=lamrange[which.min(errcv)]
  t1=Sys.time()
  obj=sda(data.frame(svec_x_allf),syf,lambda=1e-6,stop=lamtunef,trace=FALSE)
  t=t+Sys.time()-t1
  testf<-predict(obj,data.frame(svec_testxf))
  predsf<-as.numeric(testf$class)
  errsosf[irun]=sum(predsf!=ytef)/length(ytef)
  errsos[irun]=errsos[irun]+sum(predsf!=ytef)
  errsos[irun]=errsos[irun]/(length(ytem)+length(ytef))


}

print('logistic')
print(tlog)
print(mean(errlog))
print(sd(errlog))

print('catch')
print(tcatch)
print(mean(errcatch))
print(sd(errcatch))

print('dsda')
print(tdsda)
print(mean(errdsda))
print(sd(errdsda))

print('sesda')
print(mean(errsesda))
print(sd(errsesda))
print(tsesda)

print('sos')
print(mean(errsos))
print(sd(errsos))
print(t)


# -----------------------------------------------------------------------------------------
# ADHD multi-class case
set.seed(123456)
library(R.matlab)
library(glmnet)
library(Matrix)
library(MASS)
library(tensr)
library(TULIP)
library(sparseLDA)

load("multrans.RData")


pathname<-file.path('./ADHD_test_resize0.mat')
testdat<-readMat(pathname)
Xtesttem=testdat$testX
pathname<-file.path('./ADHD_train_resize0.mat')
traindat<-readMat(pathname)
Xtraintem=traindat$X


########################transformation 1
minx=min(Xtraintem[Xtraintem!=0])
Xtraintem[which(Xtraintem==0)]=minx/2
Xtesttem[which(Xtesttem==0)]=minx/2
Xtraintem=log(Xtraintem)
Xtesttem=log(Xtesttem)
############################################

dimen=dim(Xtesttem)[1:3]
nvars=prod(dimen)
nclass=length(unique(traindat$y))
ntrain=length(traindat$y)
ntest=length(testdat$testy)
n=ntrain+ntest
vec_x=matrix(0,nrow=nvars,ncol=n)
x=array(list(),ntrain+ntest)
for (i in 1:ntrain){
  x[[i]]=Xtraintem[,,,i]
  vec_x[,i]=matrix(x[[i]],nrow=nvars,ncol=1)
}
for (i in 1:ntest){
  x[[i+ntrain]]=Xtesttem[,,,i]
  vec_x[,i+ntrain]=matrix(x[[i+ntrain]],nrow=nvars,ncol=1)
}
vec_x=t(vec_x)


y=c(traindat$y,testdat$testy)
y=y+1
y=as.vector(y)
y[y==4]=3
age=c(traindat$age,testdat$testage)
gender=c(traindat$gender,testdat$testgender)
hand=c(traindat$hand,testdat$testhand)
z=cbind(age,hand)
vec_x=cbind(vec_x,z)


sd=apply(vec_x,2,sd)
sd[sd==0]=1
vec_x=scale(vec_x,center=TRUE,scale=sd)
for (i in 1:dim(vec_x)[1]){
  x[[i]]=array(vec_x[i,1:nvars],dimen)
}


nf=sum(gender)
nm=n-nf  #suppose male correspondes to z[,1]=0
indexm=which(gender==0)
indexf=which(gender==1)
xm=x[indexm]
xf=x[indexf]
vec_xm=vec_x[indexm,]
vec_xf=vec_x[indexf,]
ym=y[indexm]
yf=y[indexf]
zm=z[indexm,]
zf=z[indexf,]



run=20
tlog=0
tcatch=0
tmsda=0
errlogm=rep(0,run)
errlogf=rep(0,run)
errlog=rep(0,run)
errcatchm=rep(0,run)
errcatchf=rep(0,run)
errcatch=rep(0,run)
errmsdam=rep(0,run)
errmsdaf=rep(0,run)
errmsda=rep(0,run)
errsosm=rep(0,run)
errsosf=rep(0,run)
errsos=rep(0,run)
t=0
for (irun in 1:run){
  
  print(irun)
  
  #male
  teindex=teindexm[[irun]]
  vec_xtrm=vec_xm[-teindex,]
  vec_xtem=vec_xm[teindex,]
  ytrm=ym[-teindex]
  ytem=ym[teindex]
  xtrm=xm[-teindex]
  xtem=xm[teindex]
  ztrm=zm[-teindex,]
  ztem=zm[teindex,]
  ntrm=length(ytrm)
  
  
  #female
  teindex=teindexf[[irun]]
  vec_xtrf=vec_xf[-teindex,]
  vec_xtef=vec_xf[teindex,]
  ytrf=yf[-teindex]
  ytef=yf[teindex]
  xtrf=xf[-teindex]
  xtef=xf[teindex]
  ztrf=zf[-teindex,]
  ztef=zf[teindex,]
  ntrf=length(ytrf)
  
  
  
  #l1 logistic
  obj<-cv.glmnet(vec_xtrm,ytrm,family="multinomial")
  pred=predict(obj,vec_xtem,type='class',s='lambda.min')
  t1=Sys.time()
  obj <- glmnet(vec_xtrm,ytrm,family="multinomial",lambda=obj$lambda)
  tlog=tlog+Sys.time()-t1
  errlogm[irun]=sum(pred!=ytem)/length(ytem)
  errlog[irun]=sum(pred!=ytem)
  
  obj<-cv.glmnet(vec_xtrf,ytrf,family="multinomial")
  pred=predict(obj,vec_xtef,type='class',s='lambda.min')
  t1=Sys.time()
  obj <- glmnet(vec_xtrf,ytrf,family="multinomial",lambda=obj$lambda)
  tlog=tlog+Sys.time()-t1
  errlogf[irun]=sum(pred!=ytef)/length(ytef)
  errlog[irun]=errlog[irun]+sum(pred!=ytef)
  errlog[irun]=errlog[irun]/(length(ytem)+length(ytef))
  
  
  
  
  
  
  #catch 
  #male
  obj=cv.catch(xtrm,ztrm,y=ytrm)
  t1=Sys.time()
  obj=catch(xtrm,ztrm,y=ytrm,lambda=obj$lambda.min)  
  tcatch=tcatch+Sys.time()-t1
  pred=predict(obj,newx=xtem,newz=ztem)
  errcatchm[irun]=sum(pred!=ytem)/length(ytem)
  errcatch[irun]=sum(pred!=ytem)
  
  
  
  obj=cv.catch(xtrf,ztrf,y=ytrf)
  t1=Sys.time()
  obj=catch(xtrf,ztrf,y=ytrf,lambda=obj$lambda.min)
  tcatch=tcatch+Sys.time()-t1
  pred=predict(obj,newx=xtef,newz=ztef)
  errcatchf[irun]=sum(pred!=ytef)/length(ytef)
  errcatch[irun]=errcatch[irun]+sum(pred!=ytef)
  errcatch[irun]=errcatch[irun]/(length(ytem)+length(ytef))
  
  
  #Mmsda
  
  obj<-cv.msda(vec_xtrm, ytrm, model='multi.modified', lambda.factor=0.6, nlambda=50, perturb=0.01)
  t1=Sys.time()
  obj=msda(vec_xtrm, y=ytrm, model='multi.modified', lambda=obj$lambda.min)  
  tmsda=tmsda+Sys.time()-t1
  pred=predict(obj,vec_xtem)
  errmsdam[irun]=sum(pred!=ytem)/length(ytem)
  errmsda[irun]=sum(pred!=ytem)
  
  
  obj<-cv.msda(vec_xtrf, ytrf, model='multi.modified', lambda.factor=0.6, nlambda=50,perturb=0.01)
  t1=Sys.time()
  obj=msda(vec_xtrf,y=ytrf, model='multi.modified', lambda=obj$lambda.min)  
  tmsda=tmsda+Sys.time()-t1
  pred=predict(obj,vec_xtef)
  errmsdaf[irun]=sum(pred!=ytef)/length(ytef)
  errmsda[irun]=(errmsda[irun]+sum(pred!=ytef))/(length(ytef)+length(ytem))
  
  
  #SOS
  sym=matrix(0,nrow=ntrm,ncol=3)
  syf=matrix(0,nrow=ntrf,ncol=3)
  for (i in 1:ntrm){
    sym[i,ytrm[i]]=1
  }
  for (i in 1:ntrf){
    syf[i,ytrf[i]]=1
  }
  nam=paste("Class",1:3,sep="")
  colnames(sym)<-c(nam)
  colnames(syf)<-c(nam)
  
  Xcm=normalize(vec_xtrm)
  Xcf=normalize(vec_xtrf)
  svec_x_allm=Xcm$Xc
  svec_x_allf=Xcf$Xc
  
  svec_testxm=normalizetest(vec_xtem,Xcm)
  svec_testxf=normalizetest(vec_xtef,Xcf)
  
  #male
  all.folds<-split(sample(1:ntrm),rep(1:5,length=ntrm))
  lamrange=seq(-30,-6,3)
  errcv=rep(0,length(lamrange))
  for (fold in 1:5){
    print(fold)
    omit=all.folds[[fold]]
    svec_vaxm=svec_x_allm[omit,]
    svec_xm=svec_x_allm[-omit,]
    sytrm=sym[-omit,]
    yvam=ytrm[omit]
    for (ilam in 1:length(lamrange)){
      outs<-sda(data.frame(svec_xm),sytrm,lambda=1e-6,stop=lamrange[ilam],trace=FALSE)
      testvam<-predict(outs,data.frame(svec_vaxm))
      predvam<-as.numeric(testvam$class)
      errcv[ilam]=errcv[ilam]+sum(predvam!=yvam)/length(yvam)
    }
  }
  lamtunem=lamrange[which.min(errcv)]
  t1=Sys.time()
  obj=sda(data.frame(svec_x_allm),sym,lambda=1e-6,stop=lamtunem,trace=FALSE)
  t=t+Sys.time()-t1
  testm<-predict(obj,data.frame(svec_testxm))
  predsm<-as.numeric(testm$class)
  errsosm[irun]=sum(predsm!=ytem)/length(ytem)
  errsos[irun]=sum(predsm!=ytem)
  
  
  
  
  #female
  all.folds<-split(sample(1:ntrf),rep(1:5,length=ntrf))
  lamrange=seq(-30,-6,3)
  errcv=rep(0,length(lamrange))
  for (fold in 1:5){
    print(fold)
    omit=all.folds[[fold]]
    svec_vaxf=svec_x_allf[omit,]
    svec_xf=svec_x_allf[-omit,]
    sytrf=syf[-omit,]
    yvaf=ytrf[omit]
    for (ilam in 1:length(lamrange)){
      outs<-sda(data.frame(svec_xf),sytrf,lambda=1e-6,stop=lamrange[ilam],trace=FALSE)
      testvaf<-predict(outs,data.frame(svec_vaxf))
      predvaf<-as.numeric(testvaf$class)
      errcv[ilam]=errcv[ilam]+sum(predvaf!=yvaf)/length(yvaf)
    }
  }
  lamtunef=lamrange[which.min(errcv)]
  t1=Sys.time()
  obj=sda(data.frame(svec_x_allf),syf,lambda=1e-6,stop=lamtunef,trace=FALSE)
  t=t+Sys.time()-t1
  testf<-predict(obj,data.frame(svec_testxf))  
  predsf<-as.numeric(testf$class)
  errsosf[irun]=sum(predsf!=ytef)/length(ytef)
  errsos[irun]=errsos[irun]+sum(predsf!=ytef)
  errsos[irun]=errsos[irun]/(length(ytem)+length(ytef))
  
  
}


print('multinomial')
print(tlog)
print(mean(errlog))
print(sd(errlog))

print('catch')
print(tcatch)
print(mean(errcatch))
print(sd(errcatch))

print('MSDA')
print(tmsda)
print(mean(errmsda))
print(sd(errmsda))


print('sos')
print(mean(errsos))
print(sd(errsos))
print(t)





