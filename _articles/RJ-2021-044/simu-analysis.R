
library(OneStep)


oldsimu <- function(n, M)
{
  set.seed(1234)
  
  #n<-10000
  theta<-c(0.5,1.5)
  
  #M<-10000
  
  tabMLE<-matrix(0,2,M)
  tabME<-matrix(0,2,M)
  tabLCE<-matrix(0,2,M)
  
  timeMLE<-0
  timeME<-0
  timeLCE<-0
  
  for (i in 1:M){
    
    #Simulate the sample
    o.sample<-rbeta(n,shape1=theta[1],shape2=theta[2])
    
    #MLE
    ttmp<-proc.time()
    tabMLE[,i]<-mledist(o.sample,"beta")$estimate
    ttmp<-proc.time()-ttmp
    
    timeMLE<-timeMLE + ttmp
    
    
    #ME
    ttmp<-proc.time()
    tabME[,i]<-mmedist(o.sample,"beta")$estimate
    ttmp<-proc.time()-ttmp
    
    timeME<-timeME + ttmp
    
    #OSMLE (Fisher scoring)
    
    ttmp<-proc.time()
    tabLCE[,i]<-onestep(o.sample,"beta")$estimate
    ttmp<-proc.time()-ttmp
    
    timeLCE<-timeLCE  + ttmp
    
    
    if (i%%100==0){print(i)}
  }
  
  res<-sqrt(n)*(tabMLE-matrix(rep(theta,M),2,M))
  res2<-sqrt(n)*(tabME-matrix(rep(theta,M),2,M))
  res3<-sqrt(n)*(tabLCE-matrix(rep(theta,M),2,M))
  list(cbind(tabMLE, tabME, tabLCE), cbind(timeMLE, timeME, timeLCE), list(res, res2, res3))
}


n <- 1e4
system.time(benchonestep(rbeta(n, 1/2, 3/2), "beta", c("mme", "mle", "one")))
system.time(oldsimu(n, 1))


####   G3 <- c("gamma", "beta", "nbinom") # Distributions with a default MME initial guess estimator ####   
benchreplicate <- function(nsample, nbsimu, distr,..., echo=FALSE, cores=1)
{
  rdistr <- paste0("r", distr)
  theoval <- list(...)
  
  if(cores == 1)
  {
    bench1 <- function()
    {
      x <- do.call(rdistr, c(list(n=nsample), theoval))
      res <- benchonestep(x, distr, c("mme", "mle", "one")) 
      error <- res[rownames(res) != "time", ] - unlist(theoval)
      rownames(error) <- paste0("error-", rownames(error))
      rbind(res, error)
    }
    res <- replicate(nbsimu, bench1())
    dimnames(res)[[3]] <- paste0("simu", 1:nbsimu)
    if(echo)
      cat("sample size", nsample, "\n")
  }else
  {
    bench1 <- function(xyz)
    {
      x <- do.call(rdistr, c(list(n=nsample), theoval))
      res <- benchonestep(x, distr, c("mme", "mle", "one")) 
      error <- res[rownames(res) != "time", ] - unlist(theoval)
      rownames(error) <- paste0("error-", rownames(error))
      rbind(res, error)
    }
    require(parallel)
    cl <- parallel::makeCluster(getOption("cl.cores", cores))
    parallel::clusterExport(cl, list("nbsimu", "n"))
    parallel::clusterEvalQ(cl, library(OneStep))
    res <- parallel::parLapply(cl, 1:nbsimu, bench1)
    parallel::stopCluster(cl)
    res <- simplify2array(res)
    dimnames(res)[[3]] <- paste0("simu", 1:nbsimu)
  }
  res
}
benchall <- function(nsample, nbsimu, distr, ..., echo=TRUE, cores=1)
{
  if(cores == 1)
  {
    res <- lapply(nsample, function(n)
      benchreplicate(n, nbsimu, distr,..., echo=echo))
    names(res) <- paste0("n=", nsample)
  }else
  {
    require(parallel)
    cl <- parallel::makeCluster(getOption("cl.cores", cores))
    parallel::clusterExport(cl, list("benchreplicate", "nbsimu"))
    parallel::clusterEvalQ(cl, library(OneStep))
    f <- function(n)
      benchreplicate(n, nbsimu, distr,..., echo=echo)
    res <- parallel::parLapply(cl, nsample, f)
    parallel::stopCluster(cl)
    names(res) <- paste0("n=", nsample)
  }
  simplify2array(res)
}

#check
# benchreplicate(1e3, 2, "gamma", shape=pi, rate=sqrt(3))
# benchall(10^(2:3), 2, "gamma", shape=pi, rate=sqrt(3))

n <- floor(10^(2:6/2))
n <- 1e4
nbsimu <- 1e3
system.time(resgamma <- benchall(n, nbsimu, "gamma", shape=pi, rate=sqrt(3), 
                                 cores=2, echo=FALSE))
system.time(resbeta <- benchall(n, nbsimu, "beta", shape1=pi, shape2=1/pi, 
                                 cores=4, echo=FALSE))
system.time(resbeta <- benchall(n, nbsimu, "beta", shape1=pi, shape2=1/pi, 
                                cores=1, echo=FALSE))
system.time(resbetaold <- oldsimu(n, nbsimu))

nbsimu <- 1000
system.time(resbeta <- benchreplicate(n, nbsimu, "beta", shape1=1/2, shape2=3/2, echo=FALSE, cores=1))
system.time(resbeta4 <- benchreplicate(n, nbsimu, "beta", shape1=1/2, shape2=3/2, echo=FALSE, cores=4))
system.time(resbetaold <- oldsimu(n, nbsimu))
str(resbeta4)
str(resbeta)

dimnames(resbeta4)

xlim <- c(-2,2)
par(mfrow=1:2)
hist(resbeta4[c("error-shape1"), "mle", ] * sqrt(n), xlim=xlim)
hist(resbetaold[[3]][[1]][1,], xlim=xlim)

par(mfrow=1:2)
hist(resbeta4[c("error-shape1"), "mme", ] * sqrt(n), xlim=xlim)
hist(resbetaold[[3]][[2]][1,], xlim=xlim)


par(mfrow=1:2)
hist(resbeta4[c("error-shape1"), "onestep", ] * sqrt(n), xlim=xlim)
hist(resbetaold[[3]][[3]][1,], xlim=xlim)


save(resgamma, file=paste0("resgamma-n",max(n), "-m", max(m),".RData"))
save(resbeta, file=paste0("resbeta-n",max(n), "-m", max(m),".RData"))

dimnames(resgamma)



mean.graph <- function(object, value, theo)
{  
  object.mean <- apply(object, c(1:2, 4), mean, na.rm=TRUE)
  n <- as.numeric(substr(dimnames(object)[[4]], 3, nchar(dimnames(object)[[4]])))
  print(cbind(n, t(object.mean[value, , ])))
  matplot(n, t(object.mean[value, , ]), type="l", log="x", ylab=value,
          main=paste("average value of", value), xlab="sample size")
  legend("topright", lty=1:3, col=1:3, leg=dimnames(object)[[2]])
  if(length(grep("error", value, invert=TRUE)) == 1)
    abline(h=theo[value], col="grey")
  else 
    abline(h=0, col="grey")
}
par(mfrow=1:2, mar=c(4,4,2,1))
mean.graph(resgamma, "shape", theo=c("shape"=pi, "rate", sqrt(3)))
mean.graph(resgamma, "rate", theo=c("shape"=pi, "rate", sqrt(3)))

par(mfrow=1:2, mar=c(4,4,2,1))
mean.graph(resbeta, "shape1", theo=c("shape1"=pi, "shape2", 1/pi))
mean.graph(resbeta, "shape2", theo=c("shape1"=pi, "shape2", 1/pi))




density.graph <- function(object, value, theo, ...)
{
  n <- as.numeric(substr(dimnames(object)[[4]], 3, nchar(dimnames(object)[[4]])))
  if(length(n) <= 3)
    par(mfrow=c(1,length(n)), ...)
  else
    par(mfrow=c(2,length(n)/2), ...)
  for(j in 1:length(n))
  {
    dens.list <- sapply(1:3, function(i)
      density(object[value, i, , j]))
    plot(dens.list[,1], xlab=dimnames(object)[[4]][j], main=value,
         xlim= range(dens.list["x",]), ylim=range(dens.list["y",]), type="l")
    for(i in 2:3)
      lines(dens.list[,i], col=i, lty=i)
    legend("topright", lty=1:3, col=1:3, leg=dimnames(object)[[2]])
    if(length(grep("error", value, invert=TRUE)) == 1)
      abline(v=theo[value], col="grey")
    else 
      abline(v=0, col="grey")
  }
}

density.graph(resgamma, "shape", theo=c("shape"=pi, "rate", sqrt(3)))
density.graph(resgamma, "error-shape")
density.graph(resgamma, "error-rate")


density.graph(resbeta, "shape1", theo=c("shape1"=pi, "shape2", 1/pi))
density.graph(resbeta, "shape2", theo=c("shape1"=pi, "shape2", 1/pi))

