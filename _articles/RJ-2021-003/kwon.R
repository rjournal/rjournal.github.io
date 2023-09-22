
  rm(list=ls())
  library(ncpen, ncvreg)
  
  ###########################################################
  ### example code in the section of "The R package ncpen"
  ###########################################################
  
  ## Linear regression with scad penalty
  n=100; p=10
  sam = sam.gen.ncpen(n=n,p=p,q=5,cf.min=0.5,cf.max=1,corr=0.5,family="gaussian")
  x.mat = sam$x.mat; y.vec = sam$y.vec
  fit = ncpen(y.vec=y.vec,x.mat=x.mat,family="gaussian", penalty="scad")
  coef(fit); plot(fit)
  
  # prediction from a new dataset
  test.n=20
  newx = matrix(rnorm(test.n*p),nrow=test.n,ncol=p)
  predict(fit, new.x.mat=newx, type="")
  
  # selection of the optimal model based on the cross-validation
  cv.fit = cv.ncpen(y.vec=y.vec,x.mat=x.mat,family="gaussian", penalty="scad")
  coef(cv.fit)  
  
  # selection of the optimal model based on the generalized information criterion
  fit = ncpen(y.vec=y.vec,x.mat=x.mat,family="gaussian", penalty="scad")
  gic.ncpen(fit)  
  
  
  ###########################################################
  ###  codes for Table 1 and 2
  ###########################################################
  
  ## 1. fixed n, vary p
  n=500; nrep=100
  p.vec = 2^(0:5)*200
  
  method = c("scad", "mcp", "tlp", "classo","sridge","mbridge","mlog")
  fam="gaussian"
  # fam="binomial" # for the results of the logistic regression 
  
  out = matrix(Inf,nrow=length(p.vec),ncol=length(method)+1)
  colnames(out) = c("ncvreg",method)
  rownames(out) = p.vec
  
  for(i in 1:length(p.vec)){
    
    p = p.vec[i]
    
    tmp.mat = matrix(Inf,ncol=length(method)+1,nrow=nrep)
    colnames(tmp.mat) = c("ncvreg",method)
    for(rr in 1:nrep){
      
      set.seed(rr+0404)
      
      s0 = sam.gen.ncpen(n=n,p=p,q=10,family=fam)
      x.mat = s0$x.mat
      y.vec = s0$y.vec
      
      tmp.mat[rr,1] = system.time(n0<-ncvreg(x.mat,y.vec,dfmax=50,family=fam,penalty="SCAD"))[3]
      L.vec = n0$lambda
      tmp.mat[rr,2] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="scad"))[3]
      tmp.mat[rr,3] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="mcp"))[3]
      tmp.mat[rr,4] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="tlp"))[3]
      
      cv.ncpen.fit = cv.ncpen(y.vec,x.mat,penalty="lasso")
      opt = which.min(cv.ncpen.fit$like)
      opt.gam = cv.ncpen.fit$lambda[opt]
      
      tmp.mat[rr,5] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,gam=opt.gam,penalty="classo"))[3]
      tmp.mat[rr,6] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,gam=opt.gam,penalty="sridge"))[3]
      tmp.mat[rr,7] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="mbridge"))[3]
      tmp.mat[rr,8] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="mlog"))[3]
      
      cat("The number is ",rr,"///","p=",p,"\n")
      
    } # rr 1 to nrep: replication
    out[i,] = colMeans(tmp.mat)
  } # p.vec
  
  lin.out.p = out 
  
  ## 2. fixed p, vary n
  p=500; nrep=100
  n.vec = 2^(0:5)*200
  
  out = matrix(Inf,nrow=length(n.vec),ncol=length(method)+1)
  colnames(out) = c("ncvreg",method)
  rownames(out) = n.vec
  
  for(i in 1:length(n.vec)){
    
    n = n.vec[i]
    
    tmp.mat = matrix(Inf,ncol=length(method)+1,nrow=nrep)
    colnames(tmp.mat) = c("ncvreg",method)
    for(rr in 1:nrep){
      
      set.seed(rr+0404)
      
      s0 = sam.gen.ncpen(n=n,p=p,q=10,family=fam)
      x.mat = s0$x.mat
      y.vec = s0$y.vec
      
      tmp.mat[rr,1] = system.time(n0<-ncvreg(x.mat,y.vec,dfmax=50,family=fam,penalty="SCAD"))[3]
      L.vec = n0$lambda
      tmp.mat[rr,2] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="scad"))[3]
      tmp.mat[rr,3] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="mcp"))[3]
      tmp.mat[rr,4] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="tlp"))[3]
      
      cv.ncpen.fit = cv.ncpen(y.vec,x.mat,penalty="lasso")
      opt = which.min(cv.ncpen.fit$like)
      opt.gam = cv.ncpen.fit$lambda[opt]
      
      tmp.mat[rr,5] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,gam=opt.gam,penalty="classo"))[3]
      tmp.mat[rr,6] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,gam=opt.gam,penalty="sridge"))[3]
      tmp.mat[rr,7] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="mbridge"))[3]
      tmp.mat[rr,8] = system.time(ncpen(y.vec,x.mat,df.max=50,lambda=L.vec,family=fam,penalty="mlog"))[3]
      
      cat("The number is ",rr,"///","n=",n,"\n")
      
    } # rr 1 to nrep: replication
    out[i,] = colMeans(tmp.mat)
    
  } # n.vec
  
  lin.out.n = out 
  
  
  ###########################################################
  ###  codes for Figure 2 and 3
  ###########################################################
  
  data(diabetes, package="lars")
  x.mat = diabetes$x2; #x.mat = diabetes$x;  
  n = dim(x.mat)[1]; p = dim(x.mat)[2]
  x.mat = matrix(as.numeric(x.mat),n,p); y.vec = diabetes$y 
  
  plot.coef = function(b.mat,lam.vec,story){
    plot(lam.vec,b.mat[1,],type="n",ylim=c(min(b.mat),max(b.mat)),
         main=story, ylab="",xlab=expression(log(lambda)))
    for(pos in 1:dim(b.mat)[1]){ lines(lam.vec,b.mat[pos,],col=pos,lty=pos,lwd=1) }
  }
  
  ## Figure 1: ncvreg vs ncpen for SCAD 
# pdf("diabetes-scad.pdf", height=6)  
postscript("diabetes-scad.ps",horizontal=F, height=6)  
  
  par(mar=c(4, 4, 2, 4))
  par(mfrow=c(2,2))
  
  # scad (ncpen)
  ncp.fit = ncpen(y.vec,x.mat,family="gaussian",penalty="scad",x.standardize=TRUE) 
  lam.vec = ncp.fit$lambda
  
  # lasso 
  ncv.fit = ncvreg(y=y.vec,X=x.mat,family="gaussian",penalty="lasso") 
  plot.coef(ncv.fit$beta[-1,],log(ncv.fit$lambda),"LASSO (ncvreg, standardization)") 
  
  # scad (ncvreg)
  ncv.fit = ncvreg(X=x.mat,y=y.vec,family="gaussian",penalty="SCAD",gamma=3.7,lambda=lam.vec) 
  plot.coef(ncv.fit$beta[-1,],log(ncv.fit$lambda),"SCAD (ncvreg, standardization)")
  
  # scad (ncpen)
  ncp.fit = ncpen(y.vec,x.mat,family="gaussian",penalty="scad",
                  lambda=lam.vec,x.standardize=TRUE) 
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"SCAD (ncpen, standardization)")
  
  # scad (ncpen no std)
  ncp.fit = ncpen(y.vec,x.mat,family="gaussian",penalty="scad",
                  lambda=NULL,x.standardize=FALSE)
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"SCAD (ncpen, no standardization)") 
dev.off()
  
  
  ## Figure 2: other penalties
# pdf("diabetes-others.pdf", height=9)  
postscript("diabetes-others.ps",horizontal=F, height=9)  
  
  par(mar=c(4, 4, 2, 4))
  par(mfrow=c(3,2))
  
  ncpen.fit = ncpen(y.vec,x.mat,family="gaussian",penalty="lasso")
  lam.vec = ncpen.fit$lambda
  
  # mcp 
  ncp.fit = ncpen(y.vec,x.mat,lambda=lam.vec,tau=3.7,family="gaussian",penalty="mcp") 
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"MCP")
  
  # tlp 
  ncp.fit = ncpen(y.vec,x.mat,lambda=lam.vec,tau=3.7,family="gaussian",penalty="tlp")
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"truncated L1 penalty") 
  
  #plot.cv.ncpen(cv.ncpen.fit)
  opt.gam = 0.01
  lam.vec = lam.vec[lam.vec>opt.gam]
  ncp.fit = ncpen(y.vec,x.mat,lambda=lam.vec,gam=opt.gam,tau=3.7,family="gaussian",penalty="classo")
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"clipped LASSO") 
  
  # sridge 
  opt.gam = 0.01
  lam.vec = lam.vec[lam.vec>opt.gam]
  ncp.fit = ncpen(y.vec,x.mat,lambda=lam.vec,gam=opt.gam,tau=3.7,family="gaussian",penalty="sridge")
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"sparse ridge") 
  
  # mlog
  ncp.fit = ncpen(y.vec,x.mat,lambda=lam.vec,tau=3.7,family="gaussian",penalty="mlog")
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"modified log") 
  
  # mbridge 
  ncp.fit = ncpen(y.vec,x.mat,lambda=lam.vec,tau=3.7,family="gaussian",penalty="mbridge")
  plot.coef(ncp.fit$beta[-1,],log(ncp.fit$lambda),"modified bridge") 
dev.off()
  
  
  ###########################################################
  ###  code for Figure 4
  ########################################################### 
  
  data(prostate, package="spls")
  y.vec = prostate$y 
  x.mat = prostate$x
  
  f.vec = rep(0,dim(x.mat)[2])
  for(id in 1:dim(x.mat)[2]){
    up = 0; dn = 0 
    for(iid in c(0,1)){ tem = x.mat[y.vec==iid,id]; up = up+length(tem)*(mean(tem)-mean(x.mat[,id]))^2; dn = dn+sum((tem-mean(tem))^2) }
    f.vec[id] = up/dn
  }
  x.mat = x.mat[,order(f.vec,decreasing=T)]
  x.mat = x.mat[,1:50] 
  p = dim(x.mat)[2]
  
  tem.plot.ncpen = function (x,log.scale=T,...){
    ncpen.fit = x
    b.mat = ncpen.fit$beta[-1, ]
    lambda = ncpen.fit$lambda
    if (log.scale == TRUE) 
      lambda = log(lambda)
    plot(lambda, b.mat[1, ], type = "n", ylim = c(min(b.mat), 
                                                  max(b.mat)), ylab = "", 
         ...)
    for (pos in 1:dim(b.mat)[1]) {
      lines(lambda, b.mat[pos, ], col = pos, lty = pos)
    }
  }
  
#pdf("ridge-eff.pdf", height=6)  
  postscript("ridge-eff.ps",horizontal=F, height=6)  
  
  par(mar=c(4,2,2,1), mfrow=c(2,4))
  for(A in c(1,0.7,0.3,0)){
    ncpen.fit = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="scad",x.standardize=T,alpha=A,df.max=100)
    tem.plot.ncpen(ncpen.fit,main=bquote(alpha==.(A)),xlab="")
  }
  for(A in c(1,0.7,0.3,0)){
    ncpen.fit = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="mbridge",x.standardize=T,alpha=A,df.max=100)
    tem.plot.ncpen(ncpen.fit,main=bquote(alpha==.(A)),xlab=expression(log(lambda)))
  }
dev.off() 
  
  
  ###########################################################
  ###  codes for Table 3 and Figure 5
  ########################################################### 
  
  data(prostate, package="spls")
  y.vec = prostate$y 
  x.mat = scale(prostate$x)
  
  f.vec = rep(0,dim(x.mat)[2])
  for(id in 1:dim(x.mat)[2]){
    up = 0; dn = 0 
    for(iid in c(0,1)){ tem = x.mat[y.vec==iid,id]; up = up+length(tem)*(mean(tem)-mean(x.mat[,id]))^2; dn = dn+sum((tem-mean(tem))^2) }
    f.vec[id] = up/dn
  }
  x.mat = x.mat[,order(f.vec,decreasing=T)]
  x.mat = x.mat[,1:500] 
  p = dim(x.mat)[2]
  n =dim(x.mat)[1]
  
  ## simulations
  nrep=300
  mm = c("scad","mcp","tlp","classo","sridge","mbridge","mlog")
  method = c("lasso","ncvreg",paste(mm,0,sep=""),paste(mm,1,sep=""))
  mis.mat = num.mat = matrix(Inf,nrow=nrep,ncol=length(method))
  
  for(kk in 1:nrep){
    set.seed(kk+1234)
    a = sample(which(y.vec==1),sum(y.vec==1)*0.3)
    b = sample(which(y.vec==0),sum(y.vec==0)*0.3)
    idx = c(a,b)  
    
    X = x.mat[-idx,]; Y = y.vec[-idx]
    tX = x.mat[idx,]; tY = y.vec[idx]
    tX0 = cbind(1,tX)
    
    n0 = cv.ncvreg(X=X,y=Y,family="binomial",penalty="SCAD",df.max=50)
    ncvreg = coef(n0)
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="lasso",df.max=50)  
    ini = lasso = coef(n0,type="like")$beta    
    
    ## warm start
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="scad",df.max=50,local=F,local.initial=NULL)  
    scad0 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="mcp",df.max=50,local=F,local.initial=NULL)  
    mcp0 = coef(n0,type="like")$beta
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="tlp",df.max=50,local=F,local.initial=NULL)  
    tlp0 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="classo",df.max=50,local=F,local.initial=NULL)  
    classo0 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="sridge",df.max=50,local=F,local.initial=NULL)  
    sridge0 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="mbridge",df.max=50,local=F,local.initial=NULL)  
    mbridge0 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="mlog",df.max=50,local=F,local.initial=NULL)  
    mlog0 = coef(n0,type="like")$beta    
    
    
    ## global initial
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="scad",df.max=50,local=T,local.initial=ini)  
    scad1 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="mcp",df.max=50,local=T,local.initial=ini)  
    mcp1 = coef(n0,type="like")$beta
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="tlp",df.max=50,local=T,local.initial=ini)   
    tlp1 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="classo",df.max=50,local=T,local.initial=ini)  
    classo1 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="sridge",df.max=50,local=T,local.initial=ini)   
    sridge1 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="mbridge",df.max=50,local=T,local.initial=ini)    
    mbridge1 = coef(n0,type="like")$beta    
    
    n0 = cv.ncpen(y.vec=Y,x.mat=X,family="binomial",penalty="mlog",df.max=50,local=T,local.initial=ini)  
    mlog1 = coef(n0,type="like")$beta    
    
    ## summarize results
    Coef = cbind(lasso,ncvreg,
                 scad0,mcp0,tlp0,classo0,sridge0,mbridge0,mlog0
                 ,scad1,mcp1,tlp1,classo1,sridge1,mbridge1,mlog1)
    
    colnames(Coef) == method
    
    for(i in 1:ncol(Coef)){
      B = Coef[,i]
      mis.mat[kk,i] = mean(tY!=(tX0%*%B>0.5)*1)
      num.mat[kk,i] = sum(B[-1]!=0)
    }
  }

  ## Table 3
  res0 = data.frame(method,100*colMeans(mis.mat),colMeans(num.mat))
  colnames(res0) = c("method","MIS","NUM")
  res = data.frame(res0[3:9,], res0[10:16,-1])
  
  res0 = data.frame(method,100*apply(mis.mat,2,sd)/sqrt(300),apply(num.mat,2,sd)/sqrt(300))
  colnames(res0) = c("method","MIS","NUM")
  res.sd = data.frame(res0[3:9,], res0[10:16,-1])

  ## Figure 5
  plot.coef = function(b.mat,lam.vec,story){
    plot(lam.vec,b.mat[1,],type="n",ylim=c(min(b.mat),max(b.mat)),
         main=story, ylab="",xlab=expression(log(lambda)))
    for(pos in 1:dim(b.mat)[1]){ lines(lam.vec,b.mat[pos,],col=pos,lty=pos,lwd=1) }
  }
  
  n0 = cv.ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="lasso",df.max=50)
  lasso = coef(n0,type="like")$beta  
  n0 = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="lasso",df.max=50)
  L.vec = n0$lambda
  
#pdf("initial-eff.pdf", height=8)  
  postscript("initial-eff.ps",horizontal=F, height=8)  
  
  par(mar=c(4,4,2,4), mfrow=c(3,2))
  
  n0 = ncvreg(y=y.vec,X=x.mat,family="binomial",penalty="SCAD",dfmax=100)
  plot.coef(n0$beta[-1,],log(n0$lambda),"SCAD (ncvreg, warm start)")
  
  n0 = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="lasso",df.max=50)
  plot.coef(n0$beta[-1,],log(n0$lambda),"LASSO")
  
  n0 = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="scad",df.max=50)
  plot.coef(n0$beta[-1,],log(n0$lambda),"SCAD (warm start)") 
  
  n0 = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="scad",df.max=50,local=T,local.initial=lasso)
  plot.coef(n0$beta[-1,],log(n0$lambda),"SCAD (global initial)") 
  
  n0 = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="mcp",df.max=50)
  plot.coef(n0$beta[-1,],log(n0$lambda),"MCP (warm start)") 
  
  n0 = ncpen(y.vec=y.vec,x.mat=x.mat,family="binomial",penalty="mcp",df.max=50,local=T,local.initial=lasso)
  plot.coef(n0$beta[-1,],log(n0$lambda),"MCP (global initial)") 
dev.off() 
  
  
  
#############################################################
### Code for Figure 1: various penalty function 
#############################################################

get.pen.fun = function(pen){
  if(pen=="lasso"){ 
    pen.fun = function(b.vec,lam,gam,tau){ return(lam*abs(b.vec)) }
    pen.grad.fun = function(b.vec,lam,gam,tau){ return(lam*sign(b.vec)) }
  }
  if(pen=="scad"){
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec)*(ab.vec<lam)
      tem1.vec = ((tau*lam*(ab.vec-lam)-(ab.vec^2-lam^2)/2)/(tau-1)+lam^2)*(ab.vec>=lam)*(ab.vec<tau*lam)
      tem2.vec = ((tau+1)*lam^2/2)*(ab.vec>=tau*lam)
      return(tem0.vec+tem1.vec+tem2.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec);
      tem0.vec = (lam)*(ab.vec<lam); tem1.vec = ((tau*lam-ab.vec)/(tau-1))*(ab.vec>=lam)*(ab.vec<tau*lam)
      return((tem0.vec+tem1.vec)*sb.vec)
    }
  }
  if(pen=="mcp"){
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec-ab.vec^2/2/tau)*(ab.vec<tau*lam); 
      tem1.vec = (tau*lam^2/2)*(ab.vec>=tau*lam); return(tem0.vec+tem1.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec); tem0.vec = (lam-ab.vec/tau)*(ab.vec<tau*lam)
      return(tem0.vec*sb.vec)
    }
  }
  if(pen=="tlp"){
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec)*(ab.vec<tau); tem1.vec = (lam*tau)*(ab.vec>=tau)
      return(tem0.vec+tem1.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec); tem0.vec = (lam)*(ab.vec<tau)
      return(tem0.vec*sb.vec)
    }
  }
  if(pen=="classo"){
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); tem0.vec = (-ab.vec^2/tau/2+lam*ab.vec)*(ab.vec<tau*(lam-gam))
      tem1.vec = (gam*ab.vec-tau^2*(lam-gam)^2/tau/2+lam*tau*(lam-gam)-tau*gam*(lam-gam))*(ab.vec>=tau*(lam-gam))
      return(tem0.vec+tem1.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec)
      tem0.vec = (lam-ab.vec/tau)*(ab.vec<tau*(lam-gam)); tem1.vec = (gam) * (ab.vec>=tau*(lam-gam))
      return((tem0.vec+tem1.vec)*sb.vec)
    }
  }
  if(pen=="sridge"){ 
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec)
      tem0.vec = (-ab.vec^2/2/tau+lam*ab.vec)*(ab.vec<tau*lam/(1+tau*gam))
      tem1.vec = (gam*ab.vec^2/2+tau*lam^2/(1+tau*gam)/2)*(ab.vec>=tau*lam/(1+tau*gam))
      return(tem0.vec+tem1.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec)
      tem0.vec = (lam-ab.vec/tau)*(ab.vec<tau*lam/(1+tau*gam))
      tem1.vec = gam*ab.vec*(ab.vec>=tau*lam/(1+tau*gam))
      return((tem0.vec+tem1.vec)*sb.vec)
    }
  }
  if(pen=="mbridge"){
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); pen.vec = rep(0,length(b.vec))
      pen.vec[ab.vec< tau] = lam*ab.vec[ab.vec<tau]
      pen.vec[ab.vec>=tau] = lam*(2*sqrt(tau*ab.vec[ab.vec>=tau])-tau)
      return(pen.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec); grad.vec = rep(0,length(b.vec))
      grad.vec[ab.vec< tau] = lam
      grad.vec[ab.vec>=tau] = lam*sqrt(tau/ab.vec[ab.vec>=tau])
      return(grad.vec*sb.vec)
    }
  }
  if(pen=="mlog"){
    pen.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); pen.vec = rep(0,length(b.vec))
      pen.vec[ab.vec< tau] = lam*ab.vec[ab.vec<tau]
      pen.vec[ab.vec>=tau] = lam*tau*(1+log(ab.vec[ab.vec>=tau]/tau))
      return(pen.vec)
    }
    pen.grad.fun = function(b.vec,lam,gam,tau){
      ab.vec = abs(b.vec); sb.vec = sign(b.vec); grad.vec = rep(0,length(b.vec))
      grad.vec[ab.vec< tau] = lam; grad.vec[ab.vec>=tau] = lam*tau/ab.vec[ab.vec>=tau]
      return(grad.vec*sb.vec)
    }
  }
  return(list(pen.fun=pen.fun,pen.grad.fun=pen.grad.fun))
}


## code for figure 1
t.vec = seq(-7,7,length.out=1000) 
p.vec = c("scad","mcp","tlp","classo","sridge","mlog","mbridge")
name = c("SCAD","MCP","truncated L1 penalty","clipped LASSO","sparse ridge","modified log","modified bridge")
lam = 1
gam = 0.5 
tau = 3 

setwd("C:/Users/user/Dropbox/Desktop/Paper/NCPEN/text/R Journal/revision-20190626/ncpen")
# pdf("penplot.pdf", width=12, height=8) 
postscript("penplot.ps",height=6,horizontal=F) 

plot(t.vec,abs(t.vec),type="n",ylab="J(t)",xlab="t",
     cex.axis=1.5, cex.lab=1.5, ylim=c(0,5))
for(id in 1:length(p.vec)){
  pen.fun = get.pen.fun(p.vec[id])[[1]]
  lines(t.vec,pen.fun(t.vec,lam,gam,tau),type="l",col=id,lwd=2, lty=id)
}
legend("top",legend=name,col=1:length(p.vec),lwd=2,cex=1,lty=1:length(p.vec)) 


dev.off() 

  
  
  
  

