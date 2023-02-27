
########## BayesPPD Replication Script ###################


library(BayesPPD)


############# Examples: design of a non-inferiority trial for medical devices ###############

#### 1. Design using the power prior with fixed a0
historical <- matrix(0, ncol=3, nrow=2)
historical[1,] <- c(44, 535, 0.3)
historical[2,] <- c(33, 304, 0.3)

# This function computes power when treatment group sample size 750.
# A loop can be written to generate the power in Table 3 for fixed a0.  
set.seed(1)
power <- 
  power.two.grp.fixed.a0(data.type="Bernoulli", n.t=750, n.c=round(750/3), 
                         historical=historical,
                         samp.prior.mu.t=0.092, samp.prior.mu.c=0.092,
                         prior.mu.t.shape1=0.0001, prior.mu.t.shape2=0.0001, 
                         prior.mu.c.shape1=0.0001,prior.mu.c.shape2=0.0001,
                         delta=0.041, N=10000) 
power$`power/type I error` # 0.8428


# The following loop is used to generate the type I error rates in Table 3 for fixed a0.
set.seed(1)
lis <- c(750, 810, 900, 960, 1110)
results <- NULL
for(i in 1:length(lis)){
  error <- power.two.grp.fixed.a0(data.type="Bernoulli", n.t=lis[i], n.c=round(lis[i]/3), 
                                  historical=historical,
                                  samp.prior.mu.t=0.133, samp.prior.mu.c=0.092,
                                  prior.mu.t.shape1=0.0001, prior.mu.t.shape2=0.0001, 
                                  prior.mu.c.shape1=0.0001,prior.mu.c.shape2=0.0001,
                                  delta=0.041, N=10000) 
  res <- error$`power/type I error`
  results <- c(results, res)
} 
results # 0.0299 0.0274 0.0315 0.0298 0.0316



#### 2. Design using the normalized power prior
historical <- matrix(0, ncol=2, nrow=2)
historical[1,] <- c(44, 535)
historical[2,] <- c(33, 304)


# This function computes power when treatment group sample size 750.
# It takes about an hour to run on an average Mac laptop.  
# A loop can be written to generate the power and type I error rates in Table 3 for random a0.  
set.seed(1)
power <- 
  power.two.grp.random.a0(data.type="Bernoulli", n.t=750, n.c=round(750/3),
                          historical=historical,
                          samp.prior.mu.t=0.092, samp.prior.mu.c=0.092,
                          prior.mu.t.shape1=0.0001, prior.mu.t.shape2=0.0001, 
                          prior.mu.c.shape1=0.0001,prior.mu.c.shape2=0.0001,
                          prior.a0.shape1=1,prior.a0.shape2=1,
                          lower.limits=rep(0, 10), upper.limits=rep(1, 10),
                          slice.widths=rep(0.1, 10), delta=0.041, gamma=0.95,
                          nMC=20000, nBI=250, N=10000)
power$`power/type I error` # 0.864





############# Examples: study of acquired immunodeficiency syndrome (AIDS) ###############


#### 1. Analysis using the power prior with fixed a0 for GLMs

# Transform age and CD4 count. 
# Define Y0, X0, Y and X. 
data(actg019)
data(actg036)
Y0 <- actg019$outcome
X0 <- actg019[,-1]
X0$age_std <- scale(X0$age)
X0$T4_log <- log(X0$T4count)
X0 <- as.matrix(X0[,c("age_std","race","T4_log")])

Y <- actg036$outcome
X <- actg036[,-1]
X$age_std <- scale(X$age)
X$T4_log <- log(X$T4count)
X <- as.matrix(X[,c("treat","age_std","race","T4_log")])


# This code computes the posterior mean of beta using a power prior with a0 fixed at 0.5. 
# Three columns of Table 5 can be obtained by changing the value of a0 in the code below.  
set.seed(1)
# Put historical data in a list of one list
historical <- list(list(y0=Y0, x0=X0, a0=0.5))
result <- glm.fixed.a0(data.type="Bernoulli", data.link="Logistic", y=Y, x=X, 
                       historical=historical, nMC=10000, nBI=250)
colMeans(result$posterior.samples) # 4.8931870 -0.9459501  0.3645510  0.7201122 -1.4784046


#### 2. Design using the normalized power prior for GLMs


# We first generate the sampling priors.

library(truncnorm)
# Generate sampling priors for parameters other than beta_1.
set.seed(1)
historical.sp <- list(list(y0=Y0, x0=X0, a0=1))
result <- glm.fixed.a0(data.type="Bernoulli", data.link="Logistic", 
                       historical=historical.sp,
                       nMC=10000, nBI=250, current.data = FALSE)
beta.sp <- result$posterior.samples
nSP <- 10000
# The sampling prior is fixed at the posterior mean of the parameter given 
# the historical data.
mat.sp <- matrix(rep(colMeans(beta.sp), each=nSP), nrow=nSP)
# Generate sampling priors for beta_1.
beta1.sp <- rtruncnorm(nSP, a=-2, b=-0.1, mean=-0.5)
samp.prior.beta <- cbind(mat.sp[,1], beta1.sp, mat.sp[,2:4])


# Then we obtain a rough estimate of the sample size required to achieve a power of 0.8.
set.seed(1)
# We experiment with sample sizes 800, 1000 and 1200. 
sample.sizes <- c(800,1000,1200)
historical <- list(list(y0=Y0, x0=X0, a0=0.5))
results <- NULL
for(i in 1:length(sample.sizes)){
  result <- power.glm.fixed.a0(data.type="Bernoulli", data.size=sample.sizes[i],
                               historical=historical,
                               samp.prior.beta=samp.prior.beta, 
                               delta=0, gamma=0.95, approximate=TRUE, N=10000)
  results <- c(results, result$`power/type I error`)
}

results # 0.8037 0.8177 0.8391
# We observe that to reach a power of 0.8, the sample size should be approximately 800 
# when a0 is fixed at 0.5.


# Finally, we calculate the exact power using the normalized power prior 
# with a0 modeled as random.

# We first estimate the normalizing constant required for computing the normalized power prior.
# Since there is only one historical dataset, the grid is a matrix with one column. 
grid <- matrix(seq(0.05,1,by=0.1))
historical <- list(list(y0=Y0, x0=X0))
a0_coef <- normalizing.constant(grid=grid, historical=historical,
                                data.type="Bernoulli", data.link="Logistic")

# We compute the power for sample size of 800 using the normalized power prior. 
# This function takes a few days to run. We recommend running it on the computing cluster. 
# This code can be repeated for a range of sample sizes to generate Figure 1.
result <- power.glm.random.a0(data.type="Bernoulli", data.link="Logistic", 
                              data.size=800, historical=historical,
                              samp.prior.beta=samp.prior.beta,
                              a0.coefficients = a0_coef, 
                              delta=0, nMC=25000, nBI=250, N=10000) 
result$`power/type I error` # 0.7936



