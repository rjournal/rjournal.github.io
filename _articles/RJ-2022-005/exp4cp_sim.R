## Exponential with four change-points
library(cpsurvsim)
load("simstudyfns.RData")

param4cp.exp <- cbind(n = c(50, 100, 500),
                      endtime = as.numeric(100),
                      theta1 = as.numeric(0.005),
                      theta2 = rep(c(0.01, 0.01 ,0.02), each = 3),
                      theta3 = rep(c(0.02, 0.005, 0.005), each = 3),
                      theta4 = rep(c(0.01, 0.01, 0.02), each = 3),
                      theta5 = as.numeric(0.005),
                      tau1 = as.numeric(20),
                      tau2 = as.numeric(40),
                      tau3 = as.numeric(60),
                      tau4 = as.numeric(80))
sim <- 10000
set.seed(27)

exp4cpout <- apply(X = param4cp.exp, MARGIN = 1, FUN = test_cp,
                   cp = 4, dist = "exp", nsim = sim)

save(exp4cpout, file="exp4cpoutput.RData")