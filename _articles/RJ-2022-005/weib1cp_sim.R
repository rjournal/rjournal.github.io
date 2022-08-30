## Weibull with one change-point
library(cpsurvsim)
load("simstudyfns.RData")

param1cp.weib <- cbind(n = c(rep(50, 3), rep(100, 3), rep(500,3)),
                       endtime = as.numeric(100),
                       gamma = as.numeric(2),
                       theta1 = c(rep(0.0001, 9), rep(0.001, 9)),
                       theta2 = c(rep(0.001, 9), rep(0.0001, 9)),
                       tau = c(20, 50, 80))
sim <- 10000
set.seed(2010)

weib1cpout <- apply(X = param1cp.weib, MARGIN = 1, FUN = test_cp,
                    cp = 1, dist = "weib", nsim = sim)

save(weib1cpout, file="weib1cpoutput.RData")