## ======================================================================
## These functions are slightly adopted versions of the function published in Ruscio, J., & Kaczetow, W. (2008). Simulating Multivariate Nonnormal Data Using an Iterative Algorithm. Multivariate Behavioral Research, 43(3), 355–381. doi:10.1080/00273170802285693
## ======================================================================



################################################################################################################
GenData <- function(Pop, rho, N=min(sapply(Pop, length)), N.Factors = 0, Max.Trials = 10, Initial.Multiplier = 1, seed = NA)
{
# Initialize variables and (if applicable) set random number seed (step 1) -------------------------------------

	k <- length(Pop)
   Data <- matrix(0, nrow = N, ncol = k)            # Matrix to store the simulated data
   Iteration <- 0                                   # Iteration counter
   Best.RMSR <- 1                                   # Lowest RMSR correlation
   Trials.Without.Improvement <- 0                  # Trial counter
   if (!is.na(seed)) set.seed(seed)                    # If user specified a nonzero seed, set it
	   Distributions <- matrix(NA, nrow=N, ncol=k)
   
   Target.Corr <- matrix(c(1, rho, rho, 1), nrow=2)

# Generate distribution for each variable (step 2) -------------------------------------------------------------

   for (i in 1:k) {
	   Distributions[, i] <- sort(sample(Pop[[i]], N, replace = TRUE))
   }

#     This implementation of GenData bootstraps each variable's score distribution from a supplied data set.
#     Users should modify this block of the program, as needed, to generate the desired distribution(s).
#
#     For example, to sample from chi-square distributions with 2 df, replace the 2nd line in this block with:
#        Distributions[,i] <- sort(rchisq(N, df = 2))
#
#     Or, one can drop the loop and use a series of commands that samples variables from specified populations:
#        Distributions[,1] <- sort(rnorm(N, 0, 1))          # Standard normal distribution
#        Distributions[,2] <- sort(runif(N, 0, 1))          # Uniform distribution ranging from 0 - 1
#        Distributions[,3] <- sort(rlnorm(N, 0, 1))         # Log-normal distribution, log scale M = 0, SD = 1
#        Distributions[,4] <- sort(rexp(N, rate = 1))       # Exponential distribution with rate = 1
#        Distributions[,5] <- sort(rpois(N, lambda = 4))    # Poisson distribution with lambda = 4
#        Distributions[,6] <- sort(rbinom(N, 10, .25)       # Binominal distribution, size = 10 and p = .25
#        Distributions[,7] <- sort(rbinom(N, 2, .25)        # Binary distribution with p = .25
#
#     All of the commands shown above draw random samples from specified population distributions.  As an
#     alternative, one can reproduce distributions without sampling error.  For example, working with a
#     supplied data set, one can replace the 2nd line in this block with:
#        Disrributions[,i] <- Supplied.Data[,i]
#     Alternatively, idealized distributions can be reproduced.  For example, uniform quantiles can be
#     created and used to generate data from common distributions:
#        Uniform.Quantiles <- seq(from = 0, to = 1, length = (N + 2))[2:(N + 1)]   # quantiles 0, 1 dropped
#        Distributions[,1] <- qnorm(Uniform.Quantiles, 0, 1)      # Standard normal distribution
#        Distributions[,2] <- qunif(Uniform.Quantiles, 0, 1)      # Uniform distribution ranging from 0 to 1
#        Distributions[,3] <- qchisq(Uniform.Quantiles, df = 2)   # Chi-square distribution with 2 df
#
#     Note that when score distributions are generated from specified populations rather than bootstrapped from
#     a supplied data set, the user must provide the target correlation matrix (see the next block).  This is
#     true regardless of whether the distributions incorporate sampling error.

# Calculate and store a copy of the target correlation matrix (step 3) -----------------------------------------

   #Target.Corr <- cor(Supplied.Data)
   Intermediate.Corr <- Target.Corr

#     This implementation of GenData calculates the target correlation matrix from a supplied data set.
#     Alternatively, the user can modify the program to generate data with user-defined sample size, number of 
#     variables, and target correlation matrix by redefining the function as follows:
#        GenData <- function(N, k, Target.Corr, N.Factors = 0, Max.Trials = 5, Initial.Multiplier = 1, seed = 0)
#     In this case, one would also remove the program lines that calculate N, k, and Target.Corr.
#     To generate data in which variables are uncorrelated, one would remove the “sort” function from step 2
#        and terminate the program before step 3 begins by returning the Distributions object as the data set.

# If number of latent factors was not specified, determine it through parallel analysis (step 4) ---------------

   if (N.Factors == 0) 
   {
      Eigenvalues.Observed <- eigen(Intermediate.Corr)$values
      Eigenvalues.Random <- matrix(0, nrow = 100, ncol = k)
      Random.Data <- matrix(0, nrow = N, ncol = k)
      for (i in 1:100)
      {
         for (j in 1:k)
            Random.Data[,j] <- sample(Distributions[,j], size = N, replace = TRUE)
         Eigenvalues.Random[i,] <- eigen(cor(Random.Data))$values
      }
      Eigenvalues.Random <- apply(Eigenvalues.Random, 2, mean) # calculate mean eigenvalue for each factor
      N.Factors <- max(1, sum(Eigenvalues.Observed > Eigenvalues.Random))
   }

# Generate random normal data for shared and unique components, initialize factor loadings (steps 5, 6) --------

   Shared.Comp <- matrix(rnorm(N * N.Factors, 0, 1), nrow = N, ncol = N.Factors)
   Unique.Comp <- matrix(rnorm(N * k, 0, 1), nrow = N, ncol = k)
   Shared.Load <- matrix(0, nrow = k, ncol = N.Factors)
   Unique.Load <- matrix(0, nrow = k, ncol = 1)

# Begin loop that ends when specified number of iterations pass without improvement in RMSR correlation --------

   while (Trials.Without.Improvement < Max.Trials)
   {
      Iteration <- Iteration + 1

# Calculate factor loadings and apply to reproduce desired correlations (steps 7, 8) ---------------------------

      Fact.Anal <- Factor.Analysis(Intermediate.Corr, Corr.Matrix = TRUE, N.Factors = N.Factors)
      if (N.Factors == 1) {
		  Shared.Load[,1] <- Fact.Anal$loadings
	  } else {
		  Shared.Load <- Fact.Anal$loadings
	  }
      Shared.Load[Shared.Load > 1] <- 1
      Shared.Load[Shared.Load < -1] <- -1
      if (Shared.Load[1,1] < 0) Shared.Load <- Shared.Load * -1
      Shared.Load.sq <- Shared.Load * Shared.Load
      for (i in 1:k)
         if (sum(Shared.Load.sq[i,]) < 1) {
			 Unique.Load[i,1] <- (1 - sum(Shared.Load.sq[i,]))
		 } else {
			 Unique.Load[i,1] <- 0
		 }
      Unique.Load <- sqrt(Unique.Load)

      for (i in 1:k) {
         Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]
	 }
      # the %*% operator = matrix multiplication, and the t() function = transpose (both used again in step 13)

	  # Replace normal with nonnormal distributions (step 9) ---------------------------------------------------------

      for (i in 1:k)
      {
         Data <- Data[sort.list(Data[,i]),]
         Data[,i] <- Distributions[,i]
      }

# Calculate RMSR correlation, compare to lowest value, take appropriate action (steps 10, 11, 12) --------------

      Reproduced.Corr <- cor(Data)
      Residual.Corr <- Target.Corr - Reproduced.Corr
      RMSR <- sqrt(sum(Residual.Corr[lower.tri(Residual.Corr)] * Residual.Corr[lower.tri(Residual.Corr)]) / 
              (.5 * (k * k - k)))
      if (RMSR < Best.RMSR) {
         Best.RMSR <- RMSR
         Best.Corr <- Intermediate.Corr
         Best.Res <- Residual.Corr
         Intermediate.Corr <- Intermediate.Corr + Initial.Multiplier * Residual.Corr
         Trials.Without.Improvement <- 0
      } else {
         Trials.Without.Improvement <- Trials.Without.Improvement + 1
         Current.Multiplier <- Initial.Multiplier * .5 ^ Trials.Without.Improvement
         Intermediate.Corr <- Best.Corr + Current.Multiplier * Best.Res
      }
   }  # end of the while loop
 
# Construct the data set with the lowest RMSR correlation (step 13) --------------------------------------------

   Fact.Anal <- Factor.Analysis(Best.Corr, Corr.Matrix = TRUE, N.Factors = N.Factors)
   if (N.Factors == 1) {
	   Shared.Load[,1] <- Fact.Anal$loadings
   } else {
	   Shared.Load <- Fact.Anal$loadings
   }
   Shared.Load[Shared.Load > 1] <- 1
   Shared.Load[Shared.Load < -1] <- -1
   if (Shared.Load[1,1] < 0) {Shared.Load <- Shared.Load * -1}
   Shared.Load.sq <- Shared.Load * Shared.Load
   for (i in 1:k) {
      if (sum(Shared.Load.sq[i,]) < 1) {
		  Unique.Load[i,1] <- (1 - sum(Shared.Load.sq[i,]))
	  } else {
		  Unique.Load[i,1] <- 0
	  }
   }
   Unique.Load <- sqrt(Unique.Load)
   for (i in 1:k) {
	   Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]
   }
   Data <- apply(Data, 2, scale) # standardizes each variable in the matrix
   for (i in 1:k)
   {
      Data <- Data[sort.list(Data[,i]),]
      Data[,i] <- Distributions[,i]
   }
   Data <- Data[sample(1:N, N, replace = FALSE), ] # randomize order of cases

# Report the results and return the simulated data set (step 14) -----------------------------------------------

   Iteration <- Iteration - Max.Trials
   #cat("\nN =",N,", k =",k,",",Iteration,"Iterations,",N.Factors,"Factors, RMSR r =",round(Best.RMSR,3),"\n")
   #cat("Target correlation rho =", rho, "; obtained correlation =", round(cor(Data)[1, 2], 5))
   return(Data)
}

################################################################################################################
Factor.Analysis <- function(Data, Corr.Matrix = FALSE, Max.Iter = 50, N.Factors = 0)
{
   Data <- as.matrix(Data)
   k <- dim(Data)[2]
   if (N.Factors == 0) N.Factors <- k
   if (!Corr.Matrix) Cor.Matrix <- cor(Data)
   else Cor.Matrix <- Data
   Criterion <- .001
   Old.H2 <- rep(99, k)
   H2 <- rep(0, k)
   Change <- 1
   Iter <- 0
   Factor.Loadings <- matrix(nrow = k, ncol = N.Factors)
   while ((Change >= Criterion) & (Iter < Max.Iter))
   {
      Iter <- Iter + 1
      Eig <- eigen(Cor.Matrix)
      L <- sqrt(Eig$values[1:N.Factors])
      for (i in 1:N.Factors)
         Factor.Loadings[,i] <- Eig$vectors[,i] * L[i]
      for (i in 1:k)
         H2[i] <- sum(Factor.Loadings[i,] * Factor.Loadings[i,])
      Change <- max(abs(Old.H2 - H2))
      Old.H2 <- H2
      diag(Cor.Matrix) <- H2
   }
   if (N.Factors == k) N.Factors <- sum(Eig$values > 1)
   return(list(loadings = Factor.Loadings[,1:N.Factors], factors = N.Factors))
}




