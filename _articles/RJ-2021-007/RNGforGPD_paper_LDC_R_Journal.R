# This files includes the R code used in the paper, entitled 
# "RNGforGPD: An R Package for Generation of Univariate and 
# Multivariate Generalized Poisson Data".

library(RNGforGPD); library(mvtnorm); library(ggplot2)
library(MixAll); library(corpcor)

########################################################
###################### Section 3 #######################
########################################################
set.seed(3406)
CorrNNGpois(c(0.1, 10), c(0.1, 0.2), 0.5)
#> [1] 0.8016437

lambda.vec <- c(-0.2, 0.2, -0.3)
theta.vec <- c(1, 3, 4) 
M <- c(0.352, 0.265, 0.342) 
N <- diag(3)  
N[lower.tri(N)] <- M 
TV <- N + t(N)
diag(TV) <- 1  
cstar<- CmatStarGpois(TV, theta.vec, lambda.vec, verbose = FALSE) 
cstar
#           [,1]       [,2]       [,3]
# [1,] 1.0000000  0.3943785  0.2946171
# [2,] 0.3943785  1.0000000  0.3601862
# [3,] 0.2946171  0.3601862  1.0000000

QuantileGpois(0.98, 1, -0.2, details = TRUE)
#> x = 0, P(X = x) = 0.3678794 , P(X <= x) = 0.3678794 
#> x = 1, P(X = x) = 0.449329 , P(X <= x) = 0.8172084 
#> x = 2, P(X = x) = 0.1646435 , P(X <= x) = 0.9818519 
#> When lambda is negative, we need to account for truncation error
#> The adjusted CDF are: 0.3746792 0.83231331
#> [1] 2

set.seed(86634)
ComputeCorrGpois(c(3, 2, 5, 4), c(0.3, 0.2, 0.5, 0.6), verbose = FALSE)
# $min
#            [,1]       [,2]       [,3]       [,4]
# [1,]         NA -0.8441959 -0.8523301 -0.8040863
# [2,] -0.8441959         NA -0.8364747 -0.7861681
# [3,] -0.8523301 -0.8364747         NA -0.7966635
# [4,] -0.8040863 -0.7861681 -0.7966635         NA
# 
# $max
#           [,1]      [,2]      [,3]      [,4]
# [1,]        NA 0.9838969 0.9937024 0.9869316
# [2,] 0.9838969        NA 0.9872343 0.9819031
# [3,] 0.9937024 0.9872343        NA 0.9941324
# [4,] 0.9869316 0.9819031 0.9941324        NA


ValidCorrGpois(matrix(c(1, 0.9, 0.9, 1), byrow = TRUE, nrow = 2), 
               c(0.5, 0.5), c(0.1, 0.105), verbose = FALSE)
#> [1] TRUE

########################################################
########## Artificial Data Example, Section 4 ##########
########################################################

set.seed(12345)

theta.vec <- c(2, 3, 5, 55)
lambda.vec <- c(0, 0.4, 0.5, -0.25)

M <- c(0.1521111, 0.2652423, 0.2427688, -0.6475432, 0.1644882, -0.2522349)
N <- diag(4); N[lower.tri(N)] <- M
TV <- N + t(N); diag(TV) <- 1
cstar <- CmatStarGpois(TV, theta.vec, lambda.vec, verbose = FALSE)

#########################################
## Four generalized Poisson variables, ##
## 1000 iterations with sample size of ##
## 2000 each iteration                 ##
#########################################

no.gpois <- 4
sample.size <- 2000
iter <- 1000

m1 <- numeric(iter); v1 <- numeric(iter)
m2 <- numeric(iter); v2 <- numeric(iter)
m3 <- numeric(iter); v3 <- numeric(iter)
m4 <- numeric(iter); v4 <- numeric(iter)
corrs <- matrix(NA, iter, 16)
simulate.dat <- list()

for (i in 1:iter){
  dat <- GenMVGpois(sample.size, no.gpois, cstar, theta.vec, lambda.vec, details = FALSE)
  m1[i] <- mean(dat[, 1]); v1[i] <- var(dat[, 1])
  m2[i] <- mean(dat[, 2]); v2[i] <- var(dat[, 2])
  m3[i] <- mean(dat[, 3]); v3[i] <- var(dat[, 3])
  m4[i] <- mean(dat[, 4]); v4[i] <- var(dat[, 4])
  corrs[i, ] <- as.vector(cor(dat))
  simulate.dat[[i]] <- dat
}

#################### empirical theta and lambda ######################

theta.emp <- function(x, y) {
  sqrt( x^3 / y )
}

lambda.emp <- function (x, y) {
  1 - sqrt(x) / sqrt(y)
}

theta1 <- theta.emp(m1, v1)
lambda1 <- lambda.emp(m1, v1)

theta2 <- theta.emp(m2, v2)
lambda2 <- lambda.emp(m2, v2)

theta3 <- theta.emp(m3, v3)
lambda3 <- lambda.emp(m3, v3)

theta4 <- theta.emp(m4, v4)
lambda4 <- lambda.emp(m4, v4)

#######################################################
########## empirical and specified quantities #########
#######################################################

## theta and lambda

emp.theta <- c(mean(theta1), mean(theta2), 
               mean(theta3), mean(theta4))
emp.lambda <- c(mean(lambda1), mean(lambda2), 
                mean(lambda3), mean(lambda4))

# > emp.theta
# [1]  1.999379  3.004133  4.992992 55.115466
# > emp.lambda
# [1]  0.0003688401  0.3996088777  0.5004439777 -0.2526939913

#########################
######## Table 4 ########
#########################
avg.corr <- apply(corrs, 2, mean)
emp.corr <- rbind(avg.corr[1:4], 
                  avg.corr[5:8], 
                  avg.corr[9:12], 
                  avg.corr[13:16])

## empirical and specified correlation matrix

round(TV, 4); round(emp.corr, 4)

#        [,1]    [,2]    [,3]    [,4]
# [1,] 1.0000  0.1521  0.2652  0.2428
# [2,] 0.1521  1.0000 -0.6475  0.1645
# [3,] 0.2652 -0.6475  1.0000 -0.2522
# [4,] 0.2428  0.1645 -0.2522  1.0000
#        [,1]    [,2]    [,3]    [,4]
# [1,] 1.0000  0.1520  0.2676  0.2445
# [2,] 0.1520  1.0000 -0.6499  0.1654
# [3,] 0.2676 -0.6499  1.0000 -0.2517
# [4,] 0.2445  0.1654 -0.2517  1.0000

#########################
######## Table 5 ########
#########################

## empirical and specified theta vectors
theta.vec; round(emp.theta, 4)
## empirical and specified lambda vectors
lambda.vec; round(emp.lambda, 4)

#################### Four Moments ######################

GPDmean <- function (theta, lambda) {
  theta / (1 - lambda)
}

GPDvariance <- function (theta, lambda) {
  theta / ((1 - lambda)^3)
}

GPDskewness <- function (theta, lambda) {
  (1 + 2 * lambda) / sqrt(theta * (1 - lambda))
}

GPDkurtosis <- function (theta, lambda) {
  (1 + 8 * lambda + 6 * lambda^2) / (theta * (1 - lambda))
}

ratio <- function (empirical, theoretical) {
  abs((empirical - theoretical) / theoretical)
}

# Variable 1
mean.spe1 <- GPDmean(theta.vec[1], lambda.vec[1])
mean.emp1 <- GPDmean(emp.theta[1], emp.lambda[1])

var.spe1 <- GPDvariance(theta.vec[1], lambda.vec[1])
var.emp1 <- GPDvariance(emp.theta[1], emp.lambda[1])

skew.spe1 <- GPDskewness(theta.vec[1], lambda.vec[1])
skew.emp1 <- GPDskewness(emp.theta[1], emp.lambda[1])

kur.spe1 <- GPDkurtosis(theta.vec[1], lambda.vec[1])
kur.emp1 <- GPDkurtosis(emp.theta[1], emp.lambda[1])

# Per cent change

mean.change.1 <- ratio(mean.emp1, mean.spe1)
var.change.1 <- ratio(var.emp1, var.spe1)
skew.change.1 <- ratio(skew.emp1, skew.spe1)
kur.change.1 <- ratio(kur.emp1, kur.spe1)

# Variable 2
mean.spe2 <- GPDmean(theta.vec[2], lambda.vec[2])
mean.emp2 <- GPDmean(emp.theta[2], emp.lambda[2])

var.spe2 <- GPDvariance(theta.vec[2], lambda.vec[2])
var.emp2 <- GPDvariance(emp.theta[2], emp.lambda[2])

skew.spe2 <- GPDskewness(theta.vec[2], lambda.vec[2])
skew.emp2 <- GPDskewness(emp.theta[2], emp.lambda[2])

kur.spe2 <- GPDkurtosis(theta.vec[2], lambda.vec[2])
kur.emp2 <- GPDkurtosis(emp.theta[2], emp.lambda[2])

# Per cent change

mean.change.2 <- ratio(mean.emp2, mean.spe2)
var.change.2 <- ratio(var.emp2, var.spe2)
skew.change.2 <- ratio(skew.emp2, skew.spe2)
kur.change.2 <- ratio(kur.emp2, kur.spe2)

# Variable 3
mean.spe3 <- GPDmean(theta.vec[3], lambda.vec[3])
mean.emp3 <- GPDmean(emp.theta[3], emp.lambda[3])

var.spe3 <- GPDvariance(theta.vec[3], lambda.vec[3])
var.emp3 <- GPDvariance(emp.theta[3], emp.lambda[3])

skew.spe3 <- GPDskewness(theta.vec[3], lambda.vec[3])
skew.emp3 <- GPDskewness(emp.theta[3], emp.lambda[3])

kur.spe3 <- GPDkurtosis(theta.vec[3], lambda.vec[3])
kur.emp3 <- GPDkurtosis(emp.theta[3], emp.lambda[3])

# Per cent change

mean.change.3 <- ratio(mean.emp3, mean.spe3)
var.change.3 <- ratio(var.emp3, var.spe3)
skew.change.3 <- ratio(skew.emp3, skew.spe3)
kur.change.3 <- ratio(kur.emp3, kur.spe3)

# Variable 4
mean.spe4 <- GPDmean(theta.vec[4], lambda.vec[4])
mean.emp4 <- GPDmean(emp.theta[4], emp.lambda[4])

var.spe4 <- GPDvariance(theta.vec[4], lambda.vec[4])
var.emp4 <- GPDvariance(emp.theta[4], emp.lambda[4])

skew.spe4 <- GPDskewness(theta.vec[4], lambda.vec[4])
skew.emp4 <- GPDskewness(emp.theta[4], emp.lambda[4])

kur.spe4 <- GPDkurtosis(theta.vec[4], lambda.vec[4])
kur.emp4 <- GPDkurtosis(emp.theta[4], emp.lambda[4])

# Per cent change

mean.change.4 <- ratio(mean.emp4, mean.spe4)
var.change.4 <- ratio(var.emp4, var.spe4)
skew.change.4 <- ratio(skew.emp4, skew.spe4)
kur.change.4 <- ratio(kur.emp4, kur.spe4)

#########################
######## Table 6 ########
#########################

tab.6 <- round(data.frame(
  Variable.1 = c(mean.spe1, mean.emp1, var.spe1, var.emp1, 
                 skew.spe1, skew.emp1, kur.spe1, kur.emp1),
  Variable.2 = c(mean.spe2, mean.emp2, var.spe2, var.emp2, 
                 skew.spe2, skew.emp2, kur.spe2, kur.emp2),
  Variable.3 = c(mean.spe3, mean.emp3, var.spe3, var.emp3, 
                 skew.spe3, skew.emp3, kur.spe3, kur.emp3),
  Variable.4 = c(mean.spe4, mean.emp4, var.spe4, var.emp4, 
                 skew.spe4, skew.emp4, kur.spe4, kur.emp4)), 4)

rownames(tab.6) <- c(
  "Sepcified mu",
  "Empirical mu",
  "Specified variance",
  "Empirical variance",
  "Specified skewness",
  "Empirical skewness",
  "Specified kurtosis",
  "Empirical kurtosis"
)

tab.6

#########################
######## Table 7 ########
#########################

tab.7 <- round(data.frame(
  Variable.1 = c(mean.change.1, var.change.1, 
                 skew.change.1, kur.change.1),
  Variable.2 = c(mean.change.2, var.change.2,
                 skew.change.2, kur.change.2),
  Variable.3 = c(mean.change.3, var.change.3, 
                 skew.change.3, kur.change.3),
  Variable.4 = c(mean.change.4, var.change.4,
                 skew.change.4, kur.change.4)), 4)

rownames(tab.7) <- c("Mean", "Variance", "Skewness", "Kurtosis")

tab.7

##########################
######## Figure 1 ########
##########################

# use entries (1,2), (1, 3), (2, 3) and (3, 4) of the specified correlation matrix
# to generate the plot

plot.corr <- as.data.frame(cbind(corrs[, c(2, 3, 7, 12)], 1:1000))

ggplot(data = plot.corr) +
  geom_line(aes(x = plot.corr[, 5], y = plot.corr[, 1]), color = "red") +
  geom_line(aes(x = plot.corr[, 5], y = plot.corr[, 2]), color = "dark blue") + 
  geom_line(aes(x = plot.corr[, 5], y = plot.corr[, 3]), color = "dark green") +
  geom_line(aes(x = plot.corr[, 5], y = plot.corr[, 4]), color = "brown") +
  geom_hline(aes(yintercept = 0.1521111), color = "red", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,0.1521111,label = 0.152, hjust= 0.4, vjust = 4.0),size = 3, color = "red") +
  geom_hline(aes(yintercept = 0.2652423), color = "dark blue", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,0.26535423,label = 0.265, hjust= 0.4, vjust = -2.0),size = 3, color = "dark blue") +
  geom_hline(aes(yintercept = -0.6475432), color = "dark green", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,-0.6475432,label = -0.648, hjust= 0.4, vjust = 2.5),size = 3, color = "dark green") +
  geom_hline(aes(yintercept = -0.2522349), color = "brown", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,-0.2522349,label = -0.252, hjust= 0.4, vjust = 2.5),size = 3, color = "brown") +
  xlab("Iteration Number") +
  ylab("Correlation") +
  ylim(-0.7, 0.5) +
  # ggtitle('Empirical Correlation Values across 1000 Iterations') +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        legend.position = "none")

##################################################
########## Real Data Example, Section 4 ##########
##################################################

###########################################
########### Univariate Epilepsy ###########
###########################################


# baseline: Baseline seizure counts
# Y1: Seizure counts at the first followup
# Y2: Seizure counts at the second followup
# Y3: Seizure counts at the third followup
# Y4: Seizure counts at the fourth followup


data(epilepsy, package = "robustbase")
robustbase.epilepsy <- as.data.frame(epilepsy)
my.epilepsy <- data.frame(baseline = robustbase.epilepsy$Base,
                          Y1 = robustbase.epilepsy$Y1,
                          Y2 = robustbase.epilepsy$Y2,
                          Y3 = robustbase.epilepsy$Y3,
                          Y4 = robustbase.epilepsy$Y4)

#######################################
#######################################
# Baseline Seizure Counts Simulations #
#######################################
#######################################

# Method of moments estimation
MOM.genpois <- function(data) {
  
  mu <- mean(data); sigma2 <- var(data)
  
  theta <- sqrt(mu^3 / sigma2)
  lambda <- 1 - sqrt(mu / sigma2)
  
  return(data.frame(
    theta = theta,
    lambda = lambda
  ))
}

# calculates the true rate and dispersion parameters 
# by fitting vector generalized linear models 

true.theta <- MOM.genpois(my.epilepsy[, "baseline"])$theta
true.lambda <- MOM.genpois(my.epilepsy[, "baseline"])$lambda

#############################################################
## using an iteration size of 1000 and a sample size of 59 ##
#############################################################

samp.size <- nrow(my.epilepsy); iter.size <- 1000

####################################
# set a seed
####################################

set.seed(12345)

#########################################################
# Baseline Seizure Counts Simulations : using branching #
#########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)

for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Branching")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
Branching.sim.theta <- mean(sim.thetas)
Branching.sim.lambdas <- mean(sim.lambdas)

########################################################
# Baseline Seizure Counts Simulations: using inversion #
########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Inversion")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
Inversion.sim.theta <- mean(sim.thetas)
Inversion.sim.lambdas <- mean(sim.lambdas)

########################################################
# Baseline Seizure Counts Simulations: using build up  #
########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Build-Up")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
BuildUp.sim.theta <- mean(sim.thetas)
BuildUp.sim.lambdas <- mean(sim.lambdas)

########################################################
# Baseline Seizure Counts Simulations: using Chop-Down #
########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Chop-Down")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
ChopDown.sim.theta <- mean(sim.thetas)
ChopDown.sim.lambdas <- mean(sim.lambdas)

#####################################################
# Baseline Seizure Counts Simulations: using Normal #
#####################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Normal-Approximation")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
NormalApproximation.sim.theta <- mean(sim.thetas)
NormalApproximation.sim.lambdas <- mean(sim.lambdas)

#########################
######## Table 8 ########
#########################

baseline.univariate.epilepsy.small.sim.res <- data.frame(
  Specified = c(true.theta, true.lambda),
  Branching = c(Branching.sim.theta, Branching.sim.lambdas),
  Inversion = c(Inversion.sim.theta, Inversion.sim.lambdas),
  BuildUp = c(BuildUp.sim.theta, BuildUp.sim.lambdas),
  ChopDown = c(ChopDown.sim.theta, ChopDown.sim.lambdas),
  Normal = c(NormalApproximation.sim.theta, NormalApproximation.sim.lambdas))

round(baseline.univariate.epilepsy.small.sim.res, 4)

############################################################
# using an iteration size of 1000 and a sample size of 500 #
############################################################

samp.size <- 2000; iter.size <- 1000

####################################
# set a seed
####################################
set.seed(12345)

#########################################################
# Baseline Seizure Counts Simulations : using branching #
#########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Branching")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
Branching.sim.theta <- mean(sim.thetas)
Branching.sim.lambdas <- mean(sim.lambdas)

########################################################
# Baseline Seizure Counts Simulations: using inversion #
########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Inversion")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
Inversion.sim.theta <- mean(sim.thetas)
Inversion.sim.lambdas <- mean(sim.lambdas)

########################################################
# Baseline Seizure Counts Simulations: using Build-Up #
#######################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Build-Up")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
BuildUp.sim.theta <- mean(sim.thetas)
BuildUp.sim.lambdas <- mean(sim.lambdas)

########################################################
# Baseline Seizure Counts Simulations: using Chop-Down #
########################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Chop-Down")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
ChopDown.sim.theta <- mean(sim.thetas)
ChopDown.sim.lambdas <- mean(sim.lambdas)

#####################################################
# Baseline Seizure Counts Simulations: using Normal #
#####################################################

sim.thetas <- numeric(iter.size); sim.lambdas <- numeric(iter.size)
for (i in 1:iter.size) {
  sim.res <- GenUniGpois(true.theta, true.lambda, samp.size, 
                         details = FALSE, method = "Normal-Approximation")
  sim.thetas[i] <- sim.res$empirical.theta
  sim.lambdas[i] <- sim.res$empirical.lambda

}
NormalApproximation.sim.theta <- mean(sim.thetas)
NormalApproximation.sim.lambdas <- mean(sim.lambdas)

#########################
######## Table 9 ########
#########################
baseline.univariate.epilepsy.large.sim.res <- data.frame(
  Specified = c(true.theta, true.lambda),
  Branching = c(Branching.sim.theta, Branching.sim.lambdas),
  Inversion = c(Inversion.sim.theta, Inversion.sim.lambdas),
  BuildUp = c(BuildUp.sim.theta, BuildUp.sim.lambdas),
  ChopDown = c(ChopDown.sim.theta, ChopDown.sim.lambdas),
  Normal = c(NormalApproximation.sim.theta, NormalApproximation.sim.lambdas))

round(baseline.univariate.epilepsy.large.sim.res, 4)

###############################################
########## DebTrivedi Data Example ############
###############################################

##################################################
########### Four Dimensional Scenario ############
##################################################

data(DebTrivedi)

# Method of moments estimation
MOM.genpois <- function(data) {
  
  mu <- mean(data); sigma2 <- var(data)
  
  theta <- sqrt(mu^3 / sigma2)
  lambda <- 1 - sqrt(mu / sigma2)
  
  return(data.frame(
    theta = theta,
    lambda = lambda
  ))
}

new.opp <- DebTrivedi[, 3] + 1 # avoid the computational complexities

# visits to a physician in an office setting (OFP)
true.theta.1 <- MOM.genpois(DebTrivedi[, 1])$theta
true.lambda.1 <- MOM.genpois(DebTrivedi[, 1])$lambda

# number of years of education (SCHOOL)
true.theta.2 <- MOM.genpois(DebTrivedi$school)$theta
true.lambda.2 <- MOM.genpois(DebTrivedi$school)$lambda

# visits to a physician in a hospital outpatient setting (OPP)
true.theta.3 <- MOM.genpois(new.opp)$theta
true.lambda.3 <- MOM.genpois(new.opp)$lambda

# number of chronic conditions (NUMCHRON)
true.theta.4 <- MOM.genpois(DebTrivedi$numchron)$theta
true.lambda.4 <- MOM.genpois(DebTrivedi$numchron)$lambda

####################################################
theta.vec <- as.vector(c(true.theta.1, true.theta.2, true.theta.3, true.theta.4))
lambda.vec <- as.vector(c(true.lambda.1, true.lambda.2, true.lambda.3, true.lambda.4))
TV <- matrix(c(1, cor(DebTrivedi[, 1], DebTrivedi$school),
               cor(DebTrivedi[, 1], new.opp),
               cor(DebTrivedi[, 1], DebTrivedi$numchron),
               cor(DebTrivedi$school, DebTrivedi[, 1]),
               1,cor(DebTrivedi$school, new.opp),
               cor(DebTrivedi$school, DebTrivedi$numchron),
               cor(new.opp,DebTrivedi[, 1]),
               cor(new.opp,DebTrivedi$school),
               1, cor(new.opp,DebTrivedi$numchron),
               cor(DebTrivedi$numchron, DebTrivedi[, 1]),
               cor(DebTrivedi$numchron, DebTrivedi$school),
               cor(DebTrivedi$numchron, new.opp), 1),
             4, 4, byrow = TRUE)

cmat.star <- CmatStarGpois(TV, theta.vec, lambda.vec, verbose = FALSE)

set.seed(12345)
iter <- 1000; sample.size <- nrow(DebTrivedi); no.gpois <- 4
m1 <- numeric(iter); v1 <- numeric(iter)
m2 <- numeric(iter); v2 <- numeric(iter)
m3 <- numeric(iter); v3 <- numeric(iter)
m4 <- numeric(iter); v4 <- numeric(iter)
corrs <- matrix(NA, iter, 16)
for (i in 1:iter) {
  dat <- GenMVGpois(sample.size, no.gpois, cmat.star, theta.vec, lambda.vec, details = FALSE)
  m1[i] <- mean(dat[, 1]); v1[i] <- var(dat[, 1])
  m2[i] <- mean(dat[, 2]); v2[i] <- var(dat[, 2])
  m3[i] <- mean(dat[, 3]); v3[i] <- var(dat[, 3])
  m4[i] <- mean(dat[, 4]); v4[i] <- var(dat[, 4])
  corrs[i, ] <- as.vector(cor(dat))
}

#################### empirical theta and lambda ######################

theta.emp <- function(x, y) {
  sqrt(x^3 / y)
}

lambda.emp <- function (x, y) {
  1 - sqrt(x) / sqrt(y)
}

theta1 <- theta.emp(m1, v1)
lambda1 <- lambda.emp(m1, v1)

theta2 <- theta.emp(m2, v2)
lambda2 <- lambda.emp(m2, v2)

theta3 <- theta.emp(m3, v3)
lambda3 <- lambda.emp(m3, v3)

theta4 <- theta.emp(m4, v4)
lambda4 <- lambda.emp(m4, v4)

#######################################################
########## empirical and specified quantities #########
#######################################################

## theta and lambda

emp.theta <- c(mean(theta1), mean(theta2),
               mean(theta3), mean(theta4))
emp.lambda <- c(mean(lambda1), mean(lambda2),
                mean(lambda3), mean(lambda4))

##########################
######## Table 10 ########
##########################

## empirical and specified theta vectors
round(theta.vec, 4); round(emp.theta, 4)
## empirical and specified lambda vectors
round(lambda.vec, 4); round(emp.lambda, 4)

##########################
######## Table 11 ########
##########################

## correlation

avg.corr <- apply(corrs, 2, mean)
emp.corr <- rbind(avg.corr[1:4], avg.corr[5:8], 
                  avg.corr[9:12], avg.corr[13:16])
## empirical and specified correlation matrix
round(TV, 4); round(emp.corr, 4)

#################### Four Moments ######################

GPDmean <- function (theta, lambda) {
  theta / (1 - lambda)
}

GPDvariance <- function (theta, lambda) {
  theta / ((1 - lambda)^3)
}

GPDskewness <- function (theta, lambda) {
  (1 + 2 * lambda) / sqrt(theta * (1 - lambda))
}

GPDkurtosis <- function (theta, lambda) {
  (1 + 8 * lambda + 6 * lambda^2) / (theta * (1 - lambda))
}

ratio <- function (empirical, theoretical) {
  abs((empirical - theoretical) / theoretical)
}

# ofp
mean.spe.ofp <- GPDmean(theta.vec[1], lambda.vec[1])
mean.emp.ofp <- GPDmean(emp.theta[1], emp.lambda[1])

var.spe.ofp <- GPDvariance(theta.vec[1], lambda.vec[1])
var.emp.ofp <- GPDvariance(emp.theta[1], emp.lambda[1])

skew.spe.ofp <- GPDskewness(theta.vec[1], lambda.vec[1])
skew.emp.ofp <- GPDskewness(emp.theta[1], emp.lambda[1])

kur.spe.ofp <- GPDkurtosis(theta.vec[1], lambda.vec[1])
kur.emp.ofp <- GPDkurtosis(emp.theta[1], emp.lambda[1])

mom.spe.vec.ofp <- c(mean.spe.ofp, var.spe.ofp, skew.spe.ofp, kur.spe.ofp)
mom.emp.vec.ofp <- c(mean.emp.ofp, var.emp.ofp, skew.emp.ofp, kur.emp.ofp)

# Per cent change

mean.change.ofp <- ratio(mean.emp.ofp, mean.spe.ofp)
var.change.ofp <- ratio(var.emp.ofp, var.spe.ofp)
skew.change.ofp <- ratio(skew.emp.ofp, skew.spe.ofp)
kur.change.ofp <- ratio(kur.emp.ofp, kur.spe.ofp)

# school
mean.spe.s <- GPDmean(theta.vec[2], lambda.vec[2])
mean.emp.s <- GPDmean(emp.theta[2], emp.lambda[2])

var.spe.s <- GPDvariance(theta.vec[2], lambda.vec[2])
var.emp.s <- GPDvariance(emp.theta[2], emp.lambda[2])

skew.spe.s <- GPDskewness(theta.vec[2], lambda.vec[2])
skew.emp.s <- GPDskewness(emp.theta[2], emp.lambda[2])

kur.spe.s <- GPDkurtosis(theta.vec[2], lambda.vec[2])
kur.emp.s <- GPDkurtosis(emp.theta[2], emp.lambda[2])

mom.spe.vec.s <- c(mean.spe.s, var.spe.s, skew.spe.s, kur.spe.s)
mom.emp.vec.s <- c(mean.emp.s, var.emp.s, skew.emp.s, kur.emp.s)

# Per cent change

mean.change.s <- ratio(mean.emp.s, mean.spe.s)
var.change.s <- ratio(var.emp.s, var.spe.s)
skew.change.s <- ratio(skew.emp.s, skew.spe.s)
kur.change.s <- ratio(kur.emp.s, kur.spe.s)

# opp
mean.spe.opp <- GPDmean(theta.vec[3], lambda.vec[3])
mean.emp.opp <- GPDmean(emp.theta[3], emp.lambda[3])

var.spe.opp <- GPDvariance(theta.vec[3], lambda.vec[3])
var.emp.opp <- GPDvariance(emp.theta[3], emp.lambda[3])

skew.spe.opp <- GPDskewness(theta.vec[3], lambda.vec[3])
skew.emp.opp <- GPDskewness(emp.theta[3], emp.lambda[3])

kur.spe.opp <- GPDkurtosis(theta.vec[3], lambda.vec[3])
kur.emp.opp <- GPDkurtosis(emp.theta[3], emp.lambda[3])

mom.spe.vec.opp <- c(mean.spe.opp, var.spe.opp, skew.spe.opp, kur.spe.opp)
mom.emp.vec.opp <- c(mean.emp.opp, var.emp.opp, skew.emp.opp, kur.emp.opp)

# Per cent change

mean.change.opp <- ratio(mean.emp.opp, mean.spe.opp)
var.change.opp <- ratio(var.emp.opp, var.spe.opp)
skew.change.opp <- ratio(skew.emp.opp, skew.spe.opp)
kur.change.opp <- ratio(kur.emp.opp, kur.spe.opp)

# numchron
mean.spe.n <- GPDmean(theta.vec[4], lambda.vec[4])
mean.emp.n <- GPDmean(emp.theta[4], emp.lambda[4])

var.spe.n <- GPDvariance(theta.vec[4], lambda.vec[4])
var.emp.n <- GPDvariance(emp.theta[4], emp.lambda[4])

skew.spe.n <- GPDskewness(theta.vec[4], lambda.vec[4])
skew.emp.n <- GPDskewness(emp.theta[4], emp.lambda[4])

kur.spe.n <- GPDkurtosis(theta.vec[4], lambda.vec[4])
kur.emp.n <- GPDkurtosis(emp.theta[4], emp.lambda[4])

mom.spe.vec.n <- c(mean.spe.n, var.spe.n, skew.spe.n, kur.spe.n)
mom.emp.vec.n <- c(mean.emp.n, var.emp.n, skew.emp.n, kur.emp.n)

# Per cent change

mean.change.n <- ratio(mean.emp.n, mean.spe.n)
var.change.n <- ratio(var.emp.n, var.spe.n)
skew.change.n <- ratio(skew.emp.n, skew.spe.n)
kur.change.n <- ratio(kur.emp.n, kur.spe.n)

##########################
######## Table 12 ########
##########################

tab.12 <- round(data.frame(
  OFP = c(mean.spe.ofp, mean.emp.ofp, var.spe.ofp, var.emp.ofp, 
          skew.spe.ofp, skew.emp.ofp, kur.spe.ofp, kur.emp.ofp),
  SCHOOL = c(mean.spe.s, mean.emp.s, var.spe.s, var.emp.s, 
             skew.spe.s, skew.emp.s, kur.spe.s, kur.emp.s),
  OPP = c(mean.spe.opp, mean.emp.opp, var.spe.opp, var.emp.opp, 
          skew.spe.opp, skew.emp.opp, kur.spe.opp, kur.emp.opp),
  NUMCHRON = c(mean.spe.n, mean.emp.n, var.spe.n, var.emp.n, 
               skew.spe.n, skew.emp.n, kur.spe.n, kur.emp.n)), 4)

rownames(tab.12) <- c(
  "Sepcified mu",
  "Empirical mu",
  "Specified variance",
  "Empirical variance",
  "Specified skewness",
  "Empirical skewness",
  "Specified kurtosis",
  "Empirical kurtosis"
)
colnames(tab.12)[3] <- "OPP + 1"

tab.12

##########################
######## Table 13 ########
##########################

tab.13 <- round(data.frame(
  OFP = c(mean.change.ofp, var.change.ofp, 
                 skew.change.ofp, kur.change.ofp),
  SCHOOL = c(mean.change.s, var.change.s,
                 skew.change.s, kur.change.s),
  OPP = c(mean.change.opp, var.change.opp, 
                 skew.change.opp, kur.change.opp),
  NUMCHRON = c(mean.change.n, var.change.n,
                 skew.change.n, kur.change.n)), 4)

rownames(tab.13) <- c("Mean", "Variance", "Skewness", "Kurtosis")
colnames(tab.13)[3] <- "OPP + 1"

# suppress scientific notation
format(tab.13, scientific = FALSE)

##########################
######## Figure 2 ########
##########################

# use entries (1,2), (1, 4), and (2, 4) of the specified correlation matrices
# to generate the plot

plot.corr.Deb <- as.data.frame(cbind(corrs[, c(2, 4, 8)], 1:1000))

ggplot(data = plot.corr.Deb) +
  geom_line(aes(x = plot.corr.Deb[, 4], y = plot.corr.Deb[, 1]), color = "red") +
  geom_line(aes(x = plot.corr.Deb[, 4], y = plot.corr.Deb[, 2]), color = "dark blue") + 
  geom_line(aes(x = plot.corr.Deb[, 4], y = plot.corr.Deb[, 3]), color = "dark green") +
  geom_hline(aes(yintercept = 0.26188577), color = "dark blue", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,0.26188577,label = 0.262, hjust= 0.5, vjust = 2.8),size = 3, color = "dark blue") +
  geom_hline(aes(yintercept = 0.06443261), color = "red", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,0.06443261,label = 0.064, hjust= 0.5, vjust = 2.8),size = 3, color = "red") +
  geom_hline(aes(yintercept = -0.06582921), color = "dark green", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,-0.06582921,label = -0.066, hjust= 0.5, vjust = 3.2),size = 3, color = "dark green") +
  xlab("Iteration Number") +
  ylab("Correlation") +
  ylim(-0.5, 0.5) +
  # ggtitle('Empirical Correlation Values across 1,000 Iterations') +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        legend.position = "none")

###########################################################
### Figure 3: Theoretical vs. Empirical : marginal PMFs ###
###########################################################

### Using the artificial data

########################################################
########## Artificial Data Example, Section 4 ##########
########################################################

set.seed(12345)

theta.vec <- c(2, 3, 5, 55)
lambda.vec <- c(0, 0.4, 0.5, -0.25)

M <- c(0.1521111, 0.2652423, 0.2427688, -0.6475432, 0.1644882, -0.2522349)
N <- diag(4); N[lower.tri(N)] <- M
TV <- N + t(N); diag(TV) <- 1
cstar <- CmatStarGpois(TV, theta.vec, lambda.vec, verbose = FALSE)

#########################################
## Four generalized Poisson variables, ##
## 1000 iterations with sample size of ##
## 2000 each iteration                 ##
#########################################

no.gpois <- 4
sample.size <- 2000
iter <- 1000

m1 <- numeric(iter); v1 <- numeric(iter)
m2 <- numeric(iter); v2 <- numeric(iter)
m3 <- numeric(iter); v3 <- numeric(iter)
m4 <- numeric(iter); v4 <- numeric(iter)
corrs <- matrix(NA, iter, 16)
simulate.dat <- list()

for (i in 1:iter){
  dat <- GenMVGpois(sample.size, no.gpois, cstar, theta.vec, lambda.vec, details = FALSE)
  m1[i] <- mean(dat[, 1]); v1[i] <- var(dat[, 1])
  m2[i] <- mean(dat[, 2]); v2[i] <- var(dat[, 2])
  m3[i] <- mean(dat[, 3]); v3[i] <- var(dat[, 3])
  m4[i] <- mean(dat[, 4]); v4[i] <- var(dat[, 4])
  corrs[i, ] <- as.vector(cor(dat))
  simulate.dat[[i]] <- dat
}

probs <- function(x, theta, lambda) {
  theta * (theta + lambda * x)^(x - 1) * exp(-theta - lambda * x) / factorial(x)
}

x <- y <- numeric(iter)
range <- matrix(nrow = no.gpois, ncol = 2)

prob.ls <- list() ## list to store probabilities for each variable
p <- list() ## list to store plots

for (i in 1:no.gpois) {
  
  ## calculate range of each variable across iterations
  
  for (j in 1:iter) {
    x[j] <- max(simulate.dat[[j]][, i])
    y[j] <- min(simulate.dat[[j]][, i])
  }
  
  range[i, ] <- c(min(y), max(x))
  
  ## calculate empirical distributions
  
  prob.mat <- matrix(nrow = iter, ncol = (range[i, 2] - range[i, 1] + 1))
  colnames(prob.mat) <- range[i, 1] : range[i, 2]
  
  for (j in 1:iter) {
    
    prob <- table(simulate.dat[[j]][, i]) / sample.size
    prob.mat[j, colnames(prob.mat) %in% names(prob)] <- c(as.numeric(prob))
    
  }
  prob.ls[[i]] <- prob.mat
  
  ## plot empirical vs. theoretical PMFs. 
  
  if (i == 1) k <- 0:15 
  # make this range divisible by 5, so that x axis will not be in continuous display
  
  else {
    
    k <- range[i, 1] : range[i, 2]
    
  }
  
  prob <- data.frame(probs(k, theta.vec[i], lambda.vec[i]))
  prob1<- data.frame(k, prob)  
  colnames(prob1) <- c("k", "prob")
  
  prob2 <- data.frame(k = as.numeric(colnames(prob.ls[[i]])),
                      prob = as.numeric(apply(prob.ls[[i]], 2, mean, na.rm = TRUE)))
  
  p[[i]] <- ggplot() +
    geom_point(data = prob1,
               aes(x = k, y = prob, col = "green"), size = 1.0) +
    geom_point(dat = na.omit(prob2),
               aes(x = k, y = prob, col = "red"), size = 0.5) +
    scale_color_manual(name = "",
                       values = c("red" = "red",
                                  "green" = "green"),
                       labels = c("Theoretical", "Empirical")) +
    ylab("P(X = k)") +
    ggtitle(paste("Marginal probabilities of variable ", i, ".", sep = "")) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
  
  prob <- NULL
}

## plot

pp <- gridExtra::grid.arrange(
  p[[1]], p[[2]],
  p[[3]], p[[4]],
  ncol = 2)

#################################################################
### Figure 4: Specified vs. Empirical : correlation structure ###
#################################################################

# step 1: specify a bivariate generalized Poisson distribution

theta.vec.fig4 <- c(3, 55)
lambda.vec.fig4 <- c(0.4, -0.25)

# step 2: determine the lower and upper correlation bounds 

corr.bounds.fig4 <- ComputeCorrGpois(theta.vec.fig4, lambda.vec.fig4, verbose = FALSE)
min.bound.fig4 <- corr.bounds.fig4$min[1, 2] # or equivalently use: min[2,1]
max.bound.fig4 <- corr.bounds.fig4$max[1, 2] # or equivalently use: max[2,1]

# step 3: set up the range of correlations to be drawn

corr.range.fig4 <- seq(round(min.bound.fig4, 2) + 0.05, 
                       round(max.bound.fig4, 2) - 0.05, 
                       by = 0.05)

# step 4: check which pairs are positive definite (should be all)
no.gpois.fig4 <- 2
pd.mat.fig4 <- numeric(length(corr.range.fig4))
for (k in 1:length(corr.range.fig4)) {
  M.temp <- corr.range.fig4[k]
  N.temp <- diag(no.gpois.fig4); N.temp[lower.tri(N.temp)] <- M.temp
  TV.temp <- N.temp + t(N.temp); diag(TV.temp) <- 1
  pd.mat.fig4[k] <- is.positive.definite(TV.temp)
}
which(pd.mat.fig4 == 0)
# step 5: simulate bivariate GPD according to the specified range of correlations

sample.size.fig4 <- 100
iter.fig4 <- 50
temp.emp.corrs <- numeric(iter.fig4)
emp.corrs <- numeric(length(corr.range.fig4))
set.seed(12345)

for (j in 1:length(corr.range.fig4)){
  # current correlation structure 
  M.temp <- corr.range.fig4[j]
  N.temp <- diag(no.gpois.fig4); N.temp[lower.tri(N.temp)] <- M.temp
  TV.temp <- N.temp + t(N.temp); diag(TV.temp) <- 1
  cstar.fig4.temp <- CmatStarGpois(TV.temp, theta.vec.fig4, 
                                   lambda.vec.fig4, verbose = FALSE)
  for (i in 1:iter.fig4){
    dat <- GenMVGpois(sample.size.fig4, no.gpois.fig4, cstar.fig4.temp, 
                      theta.vec.fig4, lambda.vec.fig4, details = FALSE)
    temp.emp.corrs[i] <- cor(dat)[1,2] # or equivalently use: cor(dat)[2,1]
  }
  emp.corrs[j] <- mean(temp.emp.corrs)
}

# step 5: draw figure 4

fig4.sim.res <- data.frame(emp.rhos = emp.corrs,
                           the.rhos = corr.range.fig4)

ggplot(fig4.sim.res, aes(emp.rhos, the.rhos)) +
  geom_line(col = "blue", size = 1) +
  geom_abline(intercept = 0, 
              slope = 1,
              linetype = "dashed",
              size = 1, col = "red") +
  xlab("Empirical Correlations") + ylab("Specified Correlations") + 
  scale_x_continuous(breaks = seq(round(emp.corrs[1], 2), 
                                  round(emp.corrs[36], 2), by = 0.1)) +
  scale_y_continuous(breaks = seq(round(corr.range.fig4[1], 2), 
                                  round(corr.range.fig4[36], 2), by = 0.1))



