## Weibull with three change-points
library(cpsurvsim)
load("simstudyfns.RData")

param3cp.weib <- cbind(n = c(50, 100, 500),
                       endtime = as.numeric(100),
                       gamma = 2,
                       theta1 = as.numeric(0.0001),
                       theta2 = rep(c(0.0002,0.0004), each = 3),
                       theta3 = rep(c(0.0001, 0.0001, 0.0004, 0.0002), each = 3),
                       theta4 = rep(c(0.0002, 0.0004, 0.0001, 0.0001), each = 3),
                       tau1 = as.numeric(25),
                       tau2 = as.numeric(50),
                       tau3 = as.numeric(75))
sim <- 10000
set.seed(10320)

weib3cpout <- apply(X = param3cp.weib, MARGIN = 1, FUN = test_cp, 
                    cp = 3, dist = "weib", nsim = sim)

save(weib3cpout, file="weib3cpoutput.RData")
