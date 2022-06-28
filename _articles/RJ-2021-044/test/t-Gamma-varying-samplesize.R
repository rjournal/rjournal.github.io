rm(list=ls(all=TRUE))

library(fitdistrplus)
library(xtable)
library(goftest)
library(OneStep)

set.seed(12345)

n <- 10^3
theta<-c(2,3)

M<-10


system.time()

avgtime <- function(n, M)
{
  resgamma <- benchonestep.replicate(n, M, "gamma", shape=theta[1], rate=theta[2])
  apply(resgamma["time", , ], 1, mean)
}

n <- 10^(5:9)
resgamma <- sapply(n, function(n) avgtime(n, M))
colnames(resgamma) <- n

save(resgamma, file=paste0("res-gamma-n",min(n), "-n", max(n), "-M", max(M),".RData"))

if(FALSE)
  load("res-gamma-n1000-n1e+07-M10.RData")





tabtime<-resgamma[-1, ]
row.names(tabtime)<- c("MLE", "LCE")
colnames(tabtime) <- paste0("10^", 3:7)

capture.output(xtable( tabtime, label="tab:gamma:time:varying:size",
                      caption="Average computation time (s)", digits=4),
               file="tab-gamma-varying-size.tex")
