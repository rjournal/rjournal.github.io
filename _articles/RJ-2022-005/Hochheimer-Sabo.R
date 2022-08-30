library(cpsurvsim)

set.seed(2015)
dta1 <- exp_cdfsim(n = 50, endtime = 100, theta = c(0.005, 0.01, 0.05), tau = c(33,66))
head(dta1)
dta2 <- weib_cdfsim(n = 50, endtime = 100, gamma = 2, theta = c(0.0001, 0.0002, 0.0001), tau = c(33,66))
head(dta2)

dta3 <- exp_memsim(n = 50, endtime = 100, theta = c(0.005, 0.01, 0.05), 
                      tau = c(33,66))
head(dta3)
dta4 <- weib_memsim(n = 50, endtime = 100, gamma = 2, 
                    theta = c(0.0001, 0.0002, 0.0001), tau = c(33,66))
head(dta4)
