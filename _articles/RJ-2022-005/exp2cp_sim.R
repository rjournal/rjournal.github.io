## Exponential with two change-points
library(cpsurvsim)
load("simstudyfns.RData")

param2cp.exp <- cbind(n = c(rep(50, 4), rep(100, 4), rep(500,4)),
                      endtime = as.numeric(100),
                      theta1 = as.numeric(0.005),
                      theta2 = c(rep(0.01, 12), rep(0.02, 12)),
                      theta3 = as.numeric(0.005), 
                      tau1 = c(33, 20, 20, 50),
                      tau2 = c(66, 50, 80, 80))
sim <- 10000
set.seed(523)

exp2cpout <- apply(X = param2cp.exp, MARGIN = 1, FUN = test_cp,
                   cp = 2, dist = "exp", nsim = sim)

save(exp2cpout, file="exp2cpoutput.RData")
