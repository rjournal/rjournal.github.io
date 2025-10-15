library(rqPen)
library(conquer)

getPenObj <- function(x,y,beta,tau,lambda){
  fits <- cbind(1,x)%*%beta
  resids <- y - fits
  loss <- rqPen:::check(resids,tau=tau)
  obj <- apply(loss,2,mean)
  penbeta <- beta[-1,]
  penval <- lambda*apply(abs(penbeta),2,sum)
  list(penRho=obj+penval,rho=obj,pen=penval, fits=fits, resids=resids)
}


simFunc <- function(i){
  set.seed(i)
  times <- obj <- NULL
  for(n in c(200,2000,20000)){
    for(p in c(30,100,300)){
      #for(i in 1:50){
        print(paste("working on i",i,"n",n,"p",p))
        bstar <- c(rnorm(10), rep(0,p-10))
        x <- matrix(rnorm(n*p),n,p)
        y <- as.numeric(x%*%bstar + rnorm(n)) #rt(n,3)

        lambda <- seq(.25,.05,length=50)
        rtime <- system.time(r1 <- rq.pen(x,y,lambda=lambda, scalex=FALSE, lambda.discard=FALSE))
        lptime <- system.time(r2 <- rq.pen(x,y,lambda=lambda, alg="br", scalex=FALSE, lambda.discard=FALSE))
        ctime <- system.time(c1 <- conquer.reg(x,y,lambda=lambda, iteMax=5000))

        robj  <- getPenObj(x,y,coefficients(r1),.5,lambda)
        lpobj <- getPenObj(x,y,coefficients(r2),.5,lambda)
        cobj  <- getPenObj(x,y,c1$coeff,.5,c1$lambda)
        cobj$penRho <- rev(cobj$penRho)

        times <- rbind(times, c(n,p,i,rtime[3],"rqPen-huber"),
                              c(n,p,i,lptime[3],"rqPen-br"),
                              c(n,p,i,ctime[3],"conquer"))

        obj <- rbind(obj, cbind(n,p,i,robj$penRho/lpobj$penRho,lambda, "rqPen-huber"),
                          cbind(n,p,i,cobj$penRho/lpobj$penRho,lambda, "conquer"),
                          cbind(n,p,i,robj$penRho/cobj$penRho,lambda,"rq-conquer"))

      #}
    }
  }
  list(times=times,obj=obj)
}
#library(parallel)
#ncores <- 17
#mc_results <- mclapply(1:50,simFunc, mc.cores=ncores)

mc_results <- list()
for(i in 1:50){
  mc_results[[i]] <- simFunc(i)
}


save.image("lassoSpeedCompareBen2.Rdata")
4+4

load("lassoSpeedCompareBen2.Rdata")
library(ggplot2)
library(plyr)

obj <- times <- NULL
for(i in 1:50){
  obj <- rbind(obj, mc_results[[i]]$obj)
  times <- rbind(times, mc_results[[i]]$times)
}

#obj <- data.frame(obj, method=rep( c(rep("rq-huber",30), rep("conquer",30)),750))
times <- data.frame(times)
colnames(times) <- c("n","p","i","time","method")

for(i in 1:( ncol(times) -1)){
  if(i !=2){
    times[,i] <- as.numeric(times[,i])
  }
}

times$p <- revalue(times$p, c("30"="p=30","100"="p=100","300"="p=300"))
times$p <- factor(times$p, levels=c("p=30","p=100","p=300"))

obj <- data.frame(obj)
colnames(obj) <- c("n","p","i","ratio","lambda","method")

for(i in 1:((ncol(obj)-1))){
  if(i !=2){
    obj[,i] <- as.numeric(paste(obj[,i]))
  }
}

obj$p <- revalue(obj$p, c("30"="p=30","100"="p=100","300"="p=300"))
obj$p <- factor(obj$p, levels=c("p=30","p=100","p=300"))

#pdf("lassoSpeedCompare2.pdf")
p <- ggplot(subset(times,method!="rqPen-br"), aes(x=as.factor(n),y=log(time+.01,10), fill=method)) + geom_boxplot()+facet_wrap(.~p)+ylab("Log Base 10 Time in seconds")+
  xlab("n")
#dev.off()
saveRDS(p, file = "lassoSpeedCompare2.rds")

#pdf("lassoObjCompare2.pdf")
p <- ggplot(subset(obj,method!="rq-conquer"), aes(x=as.factor(n),y=ratio, fill=method)) + geom_boxplot()+facet_wrap(.~p)+ylab("Objective Function Ratios")+
  xlab("n")
#dev.off()
saveRDS(p, file = "lassoObjCompare2.rds")

#
#
# ggplot(subset(times,p==30 & method !="rq-br"), aes(x = n, y = time, fill = method))  + geom_boxplot() + labs(y="Time in seconds",title="Time comparison for p=30")
# ggplot(subset(times,p==100 & method !="rq-br"), aes(x = n, y = time, fill = method))  + geom_boxplot() + labs(y="Time in seconds",title="Time comparison for p=100")
# ggplot(subset(times,p==300 & method !="rq-br"), aes(x = n, y = time, fill = method))  + geom_boxplot() + labs(y="Time in seconds",title="Time comparison for p=300")
#
# ggplot(subset(a,p==30), aes(x = as.factor(n), y = ratio, fill = method))  + geom_boxplot() + labs(y="Ratio of objective function at solution with rq-br",title="Objective function solution comparison for p=30")
# ggplot(subset(a,p==100), aes(x = as.factor(n), y = ratio, fill = method))  + geom_boxplot() + labs(y="Ratio of objective function at solution with rq-br",title="Objective function solution comparison for p=300")
# ggplot(subset(a,p==300), aes(x = as.factor(n), y = ratio, fill = method))  + geom_boxplot() + labs(y="Ratio of objective function at solution with rq-br",title="Objective function solution comparison for p=300")
#
