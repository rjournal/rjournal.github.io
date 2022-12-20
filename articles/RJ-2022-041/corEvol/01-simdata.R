## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2013/2017 Felix Schönbrodt

## ======================================================================
## This file runs the Monte-Carlo simulations on a multicore machine.
## The numbers of used processor cores is defined in nCore. It can also be set to 1.
## Note: The original simulations from Schönbrodt & Perugini (2013)
## unfortunately did not use a seed; hence the reported results are not
## fully reproducible and the script here will produce slightly different
## results.
## ======================================================================


library(dplyr)
library(doParallel)

nCore <- 1
registerDoParallel(cores=nCore)
#print(paste("Registered multicores: ", getDoParWorkers()))

# Load Ruscio's function for inducing a correlation into a bivariate data set with empirical marginal distributions
source("00-Ruscio_GenData.R")

## ======================================================================
## Simulation parameters
## ======================================================================

popsize <- 1000000	# size of population from which samples are drawn
n.max <- 1000		# maximum sample size
n.min <- 20			# minimum sample size
B <- 10000 		# number of bootstrapped trajectories
rs <- seq(.1, .1, by=.1)	# which correlations should be induced in the population?


## ======================================================================
## Load marginal distributions: Should be a list of length two with raw data. Each list element contains one vector of data which defines the marginal distribution. List elements can have differing lengths.
## ======================================================================

# Simulate Gaussian distributions
NORMAL <- list(rnorm(popsize), rnorm(popsize))

# Or, load empirical distributions, e.g. the data set provided by Ted Micceri:
# http://www.freewebs.com/tedstats/Files/Real_Data.zip

# TESTDIST is used in the functions below - point it to any list of desired distributions
TESTDIST <- NORMAL
#load("micc.RData")
#TESTDIST <- list(micc.data[[5]], micc.data[[10]])

## ======================================================================
## The functions for the simulation
## ======================================================================


# calculate all correlations when sample size increases from n.min to n.max
corEvol <- function(df, n.min=20, stepsize=1) {
  res <- matrix(NA, nrow=length(seq(n.min, nrow(df), by=stepsize)), ncol=2)
  for (i in seq(n.min, nrow(df), by=stepsize)) {
    res[i-n.min+1, 1:2] <- c(n=i, R=cor(df[1:i,1], df[1:i,2]))
  }
  colnames(res) <- c("n", "R")
  return(res)
}


#' @param DIST A list of two vectors containing the marginal distributions
#' @param n.max Maximum sample size to be simulated
#' @param B Number of bootstrapped trajectories
#' @param rho Correlation to be imposed on the bivariate data set
#' @param replace Sample with or without replacement?
simCorEvol <- function(DIST, rho, n.min=20, n.max=1000, B=10, replace=TRUE, nCore=1, popsize=1000000) {

  if (nCore > 1 & nCore > getDoParWorkers()) stop("nCore does not match the number of registered multicores!")

  # Replications are split up in nCore pieces to harvest multicores
  if (B %% nCore != 0) {
    B <- ceiling(B/nCore)*nCore
    print(paste("Warning: Replications is not dividible by", nCore, "(the number of used processor cores) - set replications to", B))
  }

  # Generate population with specified rho
  #cat("\nImposing correlation on data ...\n")
	set.seed(123)
  tic <- Sys.time()
  pop <- GenData(DIST, rho=rho, N=popsize)
  toc <- Sys.time()
  #cat("\ntook me ", toc-tic)

  #cat("Running multicore simulations ...\n")

  # outer loop for the distribution of replications across cores
  res1 <- foreach(batch=1:nCore, .combine=rbind) %dopar% {

		set.seed(123+batch)

    maxcount <- (n.max-n.min+1)*(B/nCore)
    res <- matrix(NA, ncol=4, nrow=maxcount)

    counter <- 1

    for (rep in 1:(B/nCore)) {		# rep = replication

      # draw a boostrap sample from the population
      sam <- pop[sample(1:nrow(pop), size=n.max, replace=replace), ]
      f <- corEvol(sam)
      f <- cbind(f, rho = rho)
      f <- cbind(f, unique=(batch-1)*(B/nCore) + counter)
      res[(((counter-1)*nrow(f))+1):(counter*nrow(f)), 1:4] <- f
      counter <- counter + 1

      # poor man's status bar ...
      #if (counter %% 1000 == 0) print(paste0("Finished simulation ", counter, "..."))
    }

    #res <- data.frame(res)
    colnames(res) <- c("n", "r", "rho", "unique")
    return(res)
  }

  #cat("Simulations done.\n")

  return(res1)
}


dir.create("simData", showWarnings = FALSE)

for (r in rs) {
  #print('####################################')
  #print(Sys.time())
  #print(paste("Computing rho",r))
  #print('####################################')
  #tic <- Sys.time()
  sim <- simCorEvol(TESTDIST, n.max=n.max, B=B, rho=r, replace=TRUE, nCore=nCore)
  #toc <- Sys.time()
  #print(toc-tic)
  save(sim, file=paste0("simData/sim",r*10,".RData"), compress="gzip")
  rm(sim)
  gc(reset=TRUE)
}

