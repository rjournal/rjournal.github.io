#=====================================================================#
# Code script | DChaos: An R Package for Chaotic Time Series Analysis
#=====================================================================#
install.packages("DChaos")
library(DChaos)
require(highfrequency);require(tseriesChaos);require(nonlinearTseries)
set.seed(34)

## ********************************************************************
## 1.Read time-series data
## ********************************************************************
## Simulates time-series data from the Logistic map with chaos
ts        <- DChaos::logistic.sim(n=1000, a=4)
show(head(ts, 5))

## Simulates tick-by-tick data from bid quote price for Starbucks company
ts2       <- highfrequency::sbux
show(head(ts2, 5))

## Simulates time-series data from four well-known chaotic dynamic systems
## adding a measurement noise with several variance values
dgp.noise <- function(model,nset,noise,I){

  # Settings
  X           <- matrix(NA, nrow=nset, ncol=I)

  # Data generating processes
  for (i in 1:I)   {
    if (model=="logistic"){
    # Logistic map without chaos
    X[,i]     <- DChaos::logistic.sim(n=nset, a=3.2, s=noise)
    } else

    if (model=="logistic.chaos"){
    # Logistic map with chaos
    X[,i]     <- DChaos::logistic.sim(n=nset, a=4, s=noise)
    } else

    if (model=="gauss"){
    # Gauss map without chaos
    X[,i]     <- DChaos::gauss.sim(n=nset, alpha=4.9, beta=-0.58, s=noise)
    } else

    if (model=="gauss.chaos"){
    # Gauss map with chaos
    X[,i]     <- DChaos::gauss.sim(n=nset, alpha=6.2, beta=-0.5, s=noise)
    } else

    if (model=="henon"){
    # Henon map without chaos
    X[,i]     <- DChaos::henon.sim(n=nset, a=1.2, b=0.1, s=noise)[,1]
    } else

    if (model=="henon.chaos"){
    # Henon map with chaos
    X[,i]     <- DChaos::henon.sim(n=nset, a=1.4, b=0.3, s=noise)[,1]
    }

    if (model=="rossler"){
    # Rossler system without chaos
    x.ts      <- DChaos::rossler.sim(a=0.1, b=0.1, c=7, x0=rnorm(1), y0=rnorm(1), z0=rnorm(1), time=seq(0,100,0.01))
    X[,i]     <- x.ts[c(1:nset),3]
    } else

    if (model=="rossler.chaos"){
    # Rossler system with chaos
    x.ts      <- DChaos::rossler.sim(a=0.2, b=0.2, c=5.7, x0=rnorm(1), y0=rnorm(1), z0=rnorm(1), time=seq(0,100,0.01))
    X[,i]     <- x.ts[c(1:nset),3]
    }
  }

  # Output
  return(X)
}

## ********************************************************************
## 2.State space reconstruction from time-series data
## ********************************************************************
## Provides the uniform delayed-coordinate embedding vectors (Forward)
data      <- tseriesChaos::embedd(ts, m=5, d=2)
show(head(data, 5))

## Provides the uniform delayed-coordinate embedding vectors (Backward)
data      <- DChaos::embedding(ts, m=5, lag=2, timelapse="FIXED")
show(head(data, 5))

## Provides the non-uniform delayed-coordinate embedding vectors (Backward)
data      <- DChaos::embedding(ts2, m=3, lag=4, timelapse="VARIABLE")
show(head(data, 5))

## ********************************************************************
## 3.Estimating the Lyapunov exponents from time-series data
## ********************************************************************
## Provides the best-fitted neural network models for certain parameter set
model    <- DChaos::netfit(ts, m=1:4, lag=1:3, timelapse="FIXED", h=2:10)

## Summary method for a nnet object
## Provides the best set of weights found for the best-fitted neural net model
summary(model)

## Computes analytically the partial derivatives from the best-fitted neural net model estimated previously
jacobian <- DChaos::jacobian.net(model=model)
show(head(jacobian$jacobian))

## Partial derivatives are calculated analytically without setting previously any neural net model
jacobian <- DChaos::jacobian.net(data=ts, m=3:3, lag=1:1, timelapse="FIXED", h=2:10)
show(head(jacobian$jacobian))

## Provides the largest Lyapunov exponent by the Norma-2 procedure considering the bootstrap
## blocking method from the best-fitted neural net model and the partial derivatives showed
## in the jacobian.net example
exponent <- DChaos::lyapunov.max(data=jacobian, blocking="BOOT", doplot=FALSE)

## Summary method for a lyapunov object
## Provides summary statistics of the results given in an object of class lyapunov
summary(exponent)

## Provides the Lyapunov exponent spectrum by the QR decomposition procedure considering the
## bootstrap blocking method from the best-fitted neural net model and the partial derivatives
## showed in the jacobian.net example as well.
exponent <- DChaos::lyapunov.spec(data=jacobian, blocking="BOOT", doplot=FALSE)

## Summary method for a lyapunov object
## Provides summary statistics of the results given in an object of class lyapunov
summary(exponent)

## Provides the Lyapunov exponent spectrum by several blocking methods (all-in-one)
exponent <- DChaos::lyapunov(ts, m=3:3, lag=1:1, timelapse="FIXED", h=2:10, w0maxit=100,
                     wtsmaxit=1e6, pre.white=TRUE, lyapmethod="SLE", blocking="ALL",
                     B=100, trace=1, seed.t=TRUE, seed=56666459, doplot=FALSE)

## Summary method for a lyapunov object
## Provides summary statistics of the results given in an object of class lyapunov
summary(exponent)

## ********************************************************************
## 4.Analysis of chaotic time-series data adding a measurement noise
## ********************************************************************
## Figure 1: Logistic map attractor adding a measurement noise with several variance values (0,0.01,0.02,0.03,0.04,0.05)
par(mfrow=c(4,3))
noise  <-  c(0,0.01,0.02,0.03,0.04,0.05,0.1,0.15,0.2,0.25,0.3,0.5)
for (i in 1:length(noise)){
  X    <-  DChaos::logistic.sim(a=4, n=1000, s=noise[i])
           plot(X[c(1:999)],X[c(2:1000)], cex=.4, xlab= "X(t-1)",ylab="X(t)",
           main=paste("Measurement noise","s =",noise[i]),col="indianred1",col.main="darkgray")
}

## Table 4: The mean square error (MSE) values based on the estimation of the largest Lyapunov exponent from direct methods
## provided by the tseriesChaos (D1) and nonlinearTseries (D2) packages are showed. Also those obtained by the jacobian indirect
## methods through the DChaos library are presented (N2 for lyapunov.max and QR for lyapunov.spec).
B                        <-  100
mset                     <-  list("logistic.chaos","gauss.chaos","henon.chaos","rossler.chaos")
noise                    <-  c(0,0.01,0.02,0.03,0.04,0.05)
nset                     <-  1000
N                        <-  length(noise)
M                        <-  length(mset)
d1                       <-  array(dim=c(B,M,N))
d2                       <-  array(dim=c(B,M,N))
net1                     <-  array(dim=c(B,M,N))
net2                     <-  array(dim=c(B,M,N))
# We select different noise levels and models
for (i in 1:N){
  for (k in 1:M){
    # We generate different B series for each noise level and model
    X <-  dgp.noise(model=mset[k],nset=nset,noise=noise[i],I=B)
    # We estimate the Lyapunov exponent by 4 methods for the simulated B series
    for (j in 1:B){
      d1[j,k,i]          <- tseriesChaos::lyap(lyap_k(series = X[,j], m = 3, d = 1, ref = nset*.1, t = nset*.1, s = nset*.1, eps = sd(X[,j])), 0.73, 2.47)[2]
      tau.ami            <- nonlinearTseries::timeLag(X[,j], technique = "ami", lag.max = 100, do.plot = T)
      emb.dim            <- nonlinearTseries::estimateEmbeddingDim(X[,j], time.lag = tau.ami,max.embedding.dim = 15)
      d2[j,k,i]          <- nonlinearTseries::estimate(nonlinearTseries::maxLyapunov(time.series = X[,j], min.embedding.dim = emb.dim, max.embedding.dim = emb.dim+3, time.lag = tau.ami, radius = 1,do.plot = F))
      derivada           <- DChaos::jacobian.net(data = X[,j],m = 1:4,lag = 1:1,h = 2:10)
      net1[j,k,i]        <- DChaos::lyapunov.max(derivada, blocking = "BOOT", B=1000, doplot = F)$exponent.median[1,1]
      net2[j,k,i]        <- DChaos::lyapunov.spec(derivada, blocking = "BOOT", B=1000, doplot = F)$exponent.median[1,1]
      print(c(j,k,i))
    }
  }
}

## Table 5: Rejection percentages at the 5% significance level from the dynamic systems considered with a chaotic behaviour.
##          These results provide the size of our hypothesis test.
B                        <-  100
mset                     <-  list("logistic.chaos","gauss.chaos","henon.chaos","rossler.chaos")
noise                    <-  c(0,0.01,0.02,0.03,0.04,0.05)
nset                     <-  c(50,100,200)
N                        <-  length(noise)
M                        <-  length(mset)
L                        <-  length(nset)
pnet1                    <-  array(dim=c(B,M,N,L))
pnet2                    <-  array(dim=c(B,M,N,L))
# We select different noise levels and models
for (i in 1:N){
  for (k in 1:M){
    for (z in 1:L){
      # We generate different B series for each noise level and model
      X <-  dgp.noise(model=mset[k],nset=nset[z],noise=noise[i],I=B)
      # We estimate the Lyapunov exponent only by the jacobian indirect methods for the simulated B series
      for (j in 1:B){
        derivada              <- jacobian.net(data = X[,j],m = 1:5,lag = 1:1,h = 2:10)
        pnet1[j,k,i,z]        <- lyapunov.max(derivada, blocking = "BOOT", B=1000, doplot = F)$exponent.median[1,4]
        pnet2[j,k,i,z]        <- lyapunov.spec(derivada, blocking = "BOOT", B=1000, doplot = F)$exponent.median[1,4]
        print(c(j,k,i,z))
      }
    }
  }
}

## Table 6: Rejection percentages at the 5% significance level from the dynamic systems considered with a non-chaotic behaviour.
##          These results provide the power of our hypothesis test.
B                        <-  100
mset                     <-  list("logistic","gauss","henon","rossler")
noise                    <-  c(0,0.01,0.02,0.03,0.04,0.05)
nset                     <-  c(50,100,200)
N                        <-  length(noise)
M                        <-  length(mset)
L                        <-  length(nset)
pnet1                    <-  array(dim=c(B,M,N,L))
pnet2                    <-  array(dim=c(B,M,N,L))
# We select different noise levels and models
for (i in 1:N){
  for (k in 1:M){
    for (z in 1:L){
      # We generate different B series for each noise level and model
      X <-  dgp.noise(model=mset[k],nset=nset[z],noise=noise[i],I=B)
      # We estimate the Lyapunov exponent only by the jacobian indirect methods for the simulated B series
      for (j in 1:B){
        derivada              <- jacobian.net(data = X[,j],m = 1:5,lag = 1:1,h = 2:10)
        pnet1[j,k,i,z]        <- lyapunov.max(derivada, blocking = "BOOT", B=1000, doplot = F)$exponent.median[1,1]
        pnet2[j,k,i,z]        <- lyapunov.spec(derivada, blocking = "BOOT", B=1000, doplot = F)$exponent.median[1,1]
        print(c(j,k,i,z))
      }
    }
  }
}
