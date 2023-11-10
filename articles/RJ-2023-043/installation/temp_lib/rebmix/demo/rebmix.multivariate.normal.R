##############################################
## R sources for reproducing the results in ##
##   Marko Nagode:                          ##
##   Finite Mixture Modeling via REBMIX     ##
##############################################

options(prompt = "> ", continue = "+ ", width = 70,
  useFancyQuotes = FALSE, digits = 3)

###################
## Preliminaries ##
###################

# load package.

library(rebmix)

##################################
## Multivariate normal datasets ##
##################################

# Generate multivariate normal datasets.

n <- c(75, 100, 125, 150, 175)

Theta <- new("RNGMIX.Theta", c = 5, pdf = rep("normal", 4))

a.theta1(Theta, 1) <- c(10, 12, 10, 12)
a.theta1(Theta, 2) <- c(8.5, 10.5, 8.5, 10.5)
a.theta1(Theta, 3) <- c(12, 14, 12, 14)
a.theta1(Theta, 4) <- c(13, 15, 7, 9)
a.theta1(Theta, 5) <- c(7, 9, 13, 15)

a.theta2(Theta, 1) <- c(1, 1, 1, 1)
a.theta2(Theta, 2) <- c(1, 1, 1, 1)
a.theta2(Theta, 3) <- c(1, 1, 1, 1)
a.theta2(Theta, 4) <- c(2, 2, 2, 2)
a.theta2(Theta, 5) <- c(3, 3, 3, 3)

normal <- RNGMIX(Dataset = paste("normal_", 1:100, sep = ""), n = n, Theta = a.Theta(Theta))

# Estimate number of components, component weights and component parameters.

Sturges <- as.integer(1 + log2(sum(n))) # Minimum v follows the Sturges rule.
Log10 <- as.integer(10 * log10(sum(n))) # Maximum v follows the Log10 rule.

normalest <- REBMIX(Dataset = a.Dataset(normal), Preprocessing = "histogram",
  K = Sturges:Log10, Criterion = "BIC", pdf = rep("normal", 4))

c <- a.summary(normalest, "c")
IC <- a.summary(normalest, "IC")

summary(c)
summary(IC, digits = 5)

format(length(c[c == 5]) / length(c))
