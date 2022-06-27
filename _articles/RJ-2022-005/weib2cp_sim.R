## Weibull with two change-points
library(cpsurvsim)
load("simstudyfns.RData")

param2cp.weib <- cbind(n = c(rep(50, 4), rep(100, 4), rep(500,4)),
                       endtime = as.numeric(100),
                       gamma = 2,
                       theta1 = as.numeric(0.0001),
                       theta2 = c(rep(0.0002, 12), rep(0.0004, 12)),
                       theta3 = as.numeric(0.0001), 
                       tau1 = c(33, 20, 20, 50),
                       tau2 = c(66, 50, 80, 80))
sim <- 10000
set.seed(1003)

weib2cpout <- apply(X = param2cp.weib, MARGIN = 1, FUN = test_cp, 
                    cp = 2, dist = "weib", nsim = sim)

save(weib2cpout, file="weib2cpoutput.RData")