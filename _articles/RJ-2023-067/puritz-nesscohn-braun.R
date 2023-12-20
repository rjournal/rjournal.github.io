## This file runs the code in the 'Example' section of our manuscript. ##
## Estimated time: <5 seconds                                          ##

library(fasano.franceschini.test)
library(MASS)
set.seed(0)

S1 <- mvrnorm(n = 50, mu = c(0, 0), Sigma = diag(2))
S2 <- mvrnorm(n = 75, mu = c(0, 0), Sigma = diag(2))
fasano.franceschini.test(S1, S2, seed = 1, verbose = FALSE)

S3 <- mvrnorm(n = 40, mu = c(0, 0), Sigma = diag(2))
S4 <- mvrnorm(n = 42, mu = c(1, 1), Sigma = diag(2))
fasano.franceschini.test(S3, S4, seed = 2, verbose = FALSE)

S5 <- mvrnorm(n = 1000, mu = c(1, 3, 5), Sigma = diag(3) + 1)
S6 <- mvrnorm(n = 600, mu = c(1, 3, 5), Sigma = diag(3))
fasano.franceschini.test(S5, S6, seed = 3, threads = 4)

fasano.franceschini.test(S5, S6, seed = 3, threads = 1, verbose = FALSE)
