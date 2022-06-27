## Power and Sample Size for Longitudinal Models in R - The longpower Package and
## Shiny App

## by Samuel Iddi and Michael C. Donohue for the Alzheimer's Disease Neuroimaging Initiative

rm(list=ls())
# install.packages("longpower")
library(longpower)

## Illustration 'Edland' method
t <- seq(0,1.5,0.25)
edland.linear.power(delta=1.5, t=t, sig2.s = 24, sig2.e = 10, sig.level=0.05,
                      power = 0.80)
## Alternatively
lmmpower(delta=1.5, t=t, sig2.s = 24, sig2.e = 10, sig.level=0.05,
         power = 0.80,method="edland")

##### Illustration: 'Diggle' method
n <- 3
t <- c(0,2,5)
rho <- c(0.2, 0.5, 0.8)
sigma2 <- c(100, 200, 300)
tab <- outer(rho, sigma2,
              Vectorize(function(rho, sigma2){
                ceiling(diggle.linear.power(
                  delta=0.5,
                  t=t,
                  sigma2=sigma2,
                  R=rho,
                  alternative="one.sided",
                  power = 0.80)$n[1])}))
colnames(tab) = paste("sigma2 =", sigma2)
rownames(tab) = paste("rho =", rho)
tab

### Illustration: 'Liu-Liang' method
u <- list(u1 = rep(1,n), u2 = rep(0,n)) #a list of covariate vectors or
                          #matrices associated with the parameter of interest
v <- list(v1 = rep(1,n), v2 = rep(1,n)) #a respective list of covariate
                    #vectors or matrices associated with the nuisance parameter
rho<-c(0.2, 0.5, 0.8) #correlations
delta <- c(20, 30, 40, 50)/100
tab <- outer(rho, delta,Vectorize(function(rho, delta){
  ceiling(liu.liang.linear.power(
    delta=delta, u=u, v=v,
    sigma2=1,
    R=rho, alternative="one.sided",
    power=0.80)$n[1])}))
colnames(tab) = paste("delta =", delta)
rownames(tab) = paste("rho =", rho)
tab

#### Illustration 'mmrm' method
Ra <- matrix(0.25, nrow = 4, ncol = 4)
diag(Ra) <- 1 #exchangeable correlation matrix for group A
ra <- c(1, 0.90, 0.80, 0.70)#retention in group A
sigmaa <- 1 #standard deviation for group A
power.mmrm(Ra = Ra, ra = ra, sigmaa = sigmaa, delta = 0.5, power = 0.80)


### For app illustration: please visit the publicly available 'power' app at:
## https://github.com/atrihub/power/

