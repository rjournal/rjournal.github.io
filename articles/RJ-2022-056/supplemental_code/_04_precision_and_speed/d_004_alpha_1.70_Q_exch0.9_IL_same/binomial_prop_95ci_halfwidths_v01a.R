###############################################################################
#How many n for MC method to get 95% confidence interval half width 0.001?    #
###############################################################################
n <- 8.0e5 

ns <- n * 0.7075104
nf <- n - ns

ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf)
diff(ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf))
diff(ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf))/2


###############################################################################
#How many n for MC method to get 95% confidence interval half width 0.0001?   #
###############################################################################
n <- 8.0e7 

ns <- n * 0.7075104
nf <- n - ns

ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf)
diff(ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf))
diff(ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf))/2

###############################################################################
#How many n for MC method to get 95% confidence interval half width 0.00001?  #
###############################################################################
n <- 8.0e9 

ns <- n * 0.7075104
nf <- n - ns

ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf)
diff(ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf))
diff(ns/n + 1.96*c(-1,1)/(n*sqrt(n)) * sqrt(ns*nf))/2

