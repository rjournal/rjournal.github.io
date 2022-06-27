## Weibull with four change-points
library(cpsurvsim)
load("simstudyfns.RData")

param4cp.weib <- cbind(n = c(50, 100, 500),
                       endtime = as.numeric(100),
                       gamma = 2,
                       theta1 = as.numeric(0.0001),
                       theta2 = rep(c(0.0002, 0.0002, 0.0004), each = 3),
                       theta3 = rep(c(0.0004, 0.0001, 0.0001), each = 3),
                       theta4 = rep(c(0.0002, 0.0002, 0.0004), each = 3),
                       theta5 = as.numeric(0.0001),
                       tau1 = as.numeric(20),
                       tau2 = as.numeric(40),
                       tau3 = as.numeric(60),
                       tau4 = as.numeric(80))
sim <- 10000
set.seed(427)

weib4cpout <- apply(X = param4cp.weib, MARGIN = 1, FUN = test_cp, 
                    cp = 4, dist = "weib", nsim = sim)

save(weib4cpout, file="weib4cpoutput.RData")