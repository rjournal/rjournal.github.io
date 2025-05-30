## Exponential with three change-points
library(cpsurvsim)
load("simstudyfns.RData")

param3cp.exp <- cbind(n = c(50, 100, 500),
                      endtime = as.numeric(100),
                      theta1 = as.numeric(0.005),
                      theta2 = rep(c(0.01,0.02), each = 3),
                      theta3 = rep(c(0.005, 0.005, 0.02, 0.01), each = 3),
                      theta4 = rep(c(0.01, 0.02, 0.005, 0.005), each = 3),
                      tau1 = as.numeric(25),
                      tau2 = as.numeric(50),
                      tau3 = as.numeric(75))
sim <- 10000
set.seed(103)

exp3cpout <- apply(X = param3cp.exp, MARGIN = 1, FUN = test_cp,
                   cp = 3, dist = "exp", nsim = sim)

save(exp3cpout, file="exp3cpoutput.RData")