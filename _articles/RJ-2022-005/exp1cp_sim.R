## Exponential with one change-point
library(cpsurvsim)
load("simstudyfns.RData")

param1cp.exp <- cbind(n = c(rep(50, 3), rep(100, 3), rep(500,3)),
                      endtime = as.numeric(100),
                      theta1 = c(rep(0.001, 9), rep(0.01, 9)),
                      theta2 = c(rep(0.01, 9), rep(0.001, 9)),
                      tau = c(20, 50, 80))

sim <- 10000
set.seed(4546)

exp1cpout <- apply(X = param1cp.exp, MARGIN = 1, FUN = test_cp,
                   cp = 1, dist = "exp", nsim = sim)

save(exp1cpout, file="exp1cpoutput.RData")