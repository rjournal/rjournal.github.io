#################################################################
# Simulations and Data Analysis
#################################################################

library(nortsTest)

## subsection: Numerical experiments

lobato100 = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "lobato")
epps100   = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "epps")
rp100     = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "rp",k = 1)
vavra100  = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "vavra")
elbouch100 = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "elbouch")
l_sb100   = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "lobato_bootstrap")
e_sb100   = nortsTest:::rejection_table(reps = 1000,n = 100,htest = "epps_bootstrap")

rr100 = rbind(lobato100,epps100)
rr100 = rbind(rr100,rp100)
rr100 = rbind(rr100,vavra100)
rr100 = rbind(rr100, l_sb100)
rr100 = rbind(rr100, e_sb100)
rr100 = rbind(rr100, elbouch100)

row.names(rr100) = NULL
colnames(rr100) = c("-0.4","-0.25","0.0","0.25","0.4","max time")
rr100 = data.frame(rr100)

rr100$distribution = rep(c("N","logN","t3","chisq10","Gamma(7,1)"),7)
rr100$test = c(rep("Lobato and Velasco",5),rep("Epps",5),rep("Random projection k = 1",5),
               rep("Psaradakis and Vavra",5),rep("Bootstrap Lobato",5),rep("Bootstrap Epps",5),
               rep("El bouch",5))

rrr100 = rr100[,c(8,7,1:6)]

################################ Rejection Rates m = 250 #################################

lobato100 = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "lobato")
epps100   = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "epps")
rp100     = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "rp",k = 1)
vavra100  = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "vavra")
elbouch100 = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "elbouch")
l_sb100   = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "lobato_bootstrap")
e_sb100   = nortsTest:::rejection_table(reps = 1000,n = 250,htest = "epps_bootstrap")

rr100 = rbind(lobato100,epps100)
rr100 = rbind(rr100,rp100)
rr100 = rbind(rr100,vavra100)
rr100 = rbind(rr100, l_sb100)
rr100 = rbind(rr100, e_sb100)
rr100 = rbind(rr100, elbouch100)

row.names(rr100) = NULL
colnames(rr100) = c("-0.4","-0.25","0.0","0.25","0.4","max time")
rr100 = data.frame(rr100)

rr100$distribution = rep(c("N","logN","t3","chisq10","Gamma(7,1)"),7)
rr100$test = c(rep("Lobato and Velasco",5),rep("Epps",5),rep("Random projection k = 1",5),
               rep("Psaradakis and Vavra",5),rep("Bootstrap Lobato",5),rep("Bootstrap Epps",5),
               rep("El bouch",5))

rrr250 = rr100[,c(8,7,1:6)]


################################ Rejection Rates m = 500 ################################

lobato100 = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "lobato")
epps100   = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "epps")
rp100     = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "rp",k = 1)
vavra100  = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "vavra")
elbouch100 = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "elbouch")
l_sb100   = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "lobato_bootstrap")
e_sb100   = nortsTest:::rejection_table(reps = 1000,n = 500,htest = "epps_bootstrap")

rr100 = rbind(lobato100,epps100)
rr100 = rbind(rr100,rp100)
rr100 = rbind(rr100,vavra100)
rr100 = rbind(rr100, l_sb100)
rr100 = rbind(rr100, e_sb100)
rr100 = rbind(rr100, elbouch100)

row.names(rr100) = NULL
colnames(rr100) = c("-0.4","-0.25","0.0","0.25","0.4","max time")
rr100 = data.frame(rr100)

rr100$distribution = rep(c("N","logN","t3","chisq10","Gamma(7,1)"),7)
rr100$test = c(rep("Lobato and Velasco",5),rep("Epps",5),rep("Random projection k = 1",5),
               rep("Psaradakis and Vavra",5),rep("Bootstrap Lobato",5),rep("Bootstrap Epps",5),
               rep("El bouch",5))

rrr500 = rr100[,c(8,7,1:6)]

################################ Rejection Rates m = 1000 ##################################

lobato100 = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "lobato")
epps100   = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "epps")
rp100     = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "rp",k = 1)
vavra100  = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "vavra")
elbouch100 = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "elbouch")
l_sb100   = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "lobato_bootstrap")
e_sb100   = nortsTest:::rejection_table(reps = 1000,n = 1000,htest = "epps_bootstrap")

rr100 = rbind(lobato100,epps100)
rr100 = rbind(rr100,rp100)
rr100 = rbind(rr100,vavra100)
rr100 = rbind(rr100, l_sb100)
rr100 = rbind(rr100, e_sb100)
rr100 = rbind(rr100, elbouch100)

row.names(rr100) = NULL
colnames(rr100) = c("-0.4","-0.25","0.0","0.25","0.4","max time")
rr100 = data.frame(rr100)

rr100$distribution = rep(c("N","logN","t3","chisq10","Gamma(7,1)"),7)
rr100$test = c(rep("Lobato and Velasco",5),rep("Epps",5),rep("Random projection k = 1",5),
               rep("Psaradakis and Vavra",5),rep("Bootstrap Lobato",5),rep("Bootstrap Epps",5),
               rep("El bouch",5))

rrr1000 = rr100[,c(8,7,1:6)]

results1 = cbind(rrr100,rrr250[,3:8])
results2 = cbind(rrr500,rrr1000[,3:8])

rm(lobato100, epps100, rp100, vavra100, elbouch100, l_sb100, e_sb100)
save.image("~/Documents/nortsTest_paper/data/r_sim.Rdata")
