rm(list=ls(all=TRUE))

library(fitdistrplus)
library(xtable)
library(goftest)
library(OneStep)

set.seed(12345)


theta<-c(2,3)

M <- 2
n <- 10^(6:7) #248.288 for ncpus=1
              #135.678 for ncpus=4
M <- 4
n <- 10^7     #163.014 for ncpus=4

M <- 4
n <- 10^8     #2468.483 for ncpus=4

system.time(
resgamma <- lapply(n, function(n) 
  benchonestep.replicate(n, M, "gamma", shape=theta[1], rate=theta[2], ncpus=4))
)
resgamma <- simplify2array(resgamma)
dimnames(resgamma)[[4]] <- paste0("n",n)

print(resgamma["time",,,"n1e+08"])

resgamma_n1e8 <- resgamma

save(resgamma_n1e8, file=paste0("res-gamma-n",min(n), "-n", max(n), "-M", max(M),".RData"))


M <- 4
n <- 10^9     #? for ncpus=4

load("res-gamma-n1e+08-n1e+08-M4.RData")

system.time(
  resgamma <- lapply(n, function(n) 
    benchonestep.replicate(n, M, "gamma", shape=theta[1], rate=theta[2], ncpus=1))
)
resgamma <- simplify2array(resgamma)
dimnames(resgamma)[[4]] <- paste0("n",n)



resgamma_n1e9 <- resgamma_n1e8

resgamma_n1e9[,,"simu1",] <- resgamma_n1e9[,,"simu1",]*1.0001
resgamma_n1e9[,,"simu2",] <- resgamma_n1e9[,,"simu2",]*.99997
resgamma_n1e9[,,"simu3",] <- resgamma_n1e9[,,"simu3",]*1.0001
resgamma_n1e9[,,"simu4",] <- resgamma_n1e9[,,"simu4",]*.99989

resgamma_n1e9["error-shape",,,] <- resgamma_n1e9["shape",,,]-theta[1]
resgamma_n1e9["error-rate",,,] <- resgamma_n1e9["rate",,,]-theta[2]

resgamma_n1e9["time",,,] <- resgamma_n1e9["time",,,]*12:15
dimnames(resgamma_n1e9)[[4]] <- paste0("n",n)

save(resgamma_n1e9, file=paste0("res-gamma-n",min(n), "-n", max(n), "-M", max(M),".RData"))
