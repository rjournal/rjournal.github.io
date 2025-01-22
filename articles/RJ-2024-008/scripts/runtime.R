#################################################################
# runtime simulations
#################################################################

library(nortsTest)

lobato = c(nortsTest:::rejection_rate(n = 1000,  htest = "lobato", seed = 1975)[2],
           nortsTest:::rejection_rate(n = 2000,  htest = "lobato", seed = 1975)[2],
           nortsTest:::rejection_rate(n = 3000,  htest = "lobato", seed = 1975)[2],
           nortsTest:::rejection_rate(n = 4000,  htest = "lobato", seed = 1975)[2],
           nortsTest:::rejection_rate(n = 5000,  htest = "lobato", seed = 1975)[2])/1000

epps  = c(nortsTest:::rejection_rate(n = 1000,  htest = "epps", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 2000,  htest = "epps", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 3000,  htest = "epps", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 4000,  htest = "epps", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 5000,  htest = "epps", seed = 1975)[2])/1000

rp = c(nortsTest:::rejection_rate(n = 1000,  htest = "rp", seed = 1975)[2],
       nortsTest:::rejection_rate(n = 2000,  htest = "rp", seed = 1975)[2],
       nortsTest:::rejection_rate(n = 3000,  htest = "rp", seed = 1975)[2],
       nortsTest:::rejection_rate(n = 4000,  htest = "rp", seed = 1975)[2],
       nortsTest:::rejection_rate(n = 5000,  htest = "rp", seed = 1975)[2])/1000

elbouch = c(nortsTest:::rejection_rate(n = 1000,  htest = "elbouch", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 2000,  htest = "elbouch", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 3000,  htest = "elbouch", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 4000,  htest = "elbouch", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 5000,  htest = "elbouch", seed = 1975)[2])/1000

vavra = c(nortsTest:::rejection_rate(n = 1000,  htest = "vavra", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 2000,  htest = "vavra", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 3000,  htest = "vavra", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 4000,  htest = "vavra", seed = 1975)[2],
          nortsTest:::rejection_rate(n = 5000,  htest = "vavra", seed = 1975)[2])/1000

lobato_sb = c(nortsTest:::rejection_rate(n = 1000,  htest = "lobato_bootstrap", seed = 1975)[2],
              nortsTest:::rejection_rate(n = 2000,  htest = "lobato_bootstrap", seed = 1975)[2],
              nortsTest:::rejection_rate(n = 3000,  htest = "lobato_bootstrap", seed = 1975)[2],
              nortsTest:::rejection_rate(n = 4000,  htest = "lobato_bootstrap", seed = 1975)[2],
              nortsTest:::rejection_rate(n = 5000,  htest = "lobato_bootstrap", seed = 1975)[2])/1000

epps_sb = c(nortsTest:::rejection_rate(n = 1000,  htest = "epps_bootstrap", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 2000,  htest = "epps_bootstrap", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 3000,  htest = "epps_bootstrap", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 4000,  htest = "epps_bootstrap", seed = 1975)[2],
            nortsTest:::rejection_rate(n = 5000,  htest = "epps_bootstrap", seed = 1975)[2])/1000

runtime = matrix(c(lobato, epps, rp, elbouch, vavra, lobato_sb, epps_sb),
                 nrow = 7, ncol = 5, byrow = TRUE)

tests = c("Lobato and Velasco", "Epps", "Random Projections","El Bouch",
                      "Psaradakis and Vavra", "Bootstrap Lobato", "Bootstrap Epps")

runtime = as.data.frame(runtime)
colnames(runtime) = paste("n =",c(1000,2000,3000,4000,5000))
runtime = cbind(tests, runtime)

rm(lobato, epps, rp, elbouch, vavra, lobato_sb, epps_sb,tests)
save.image("~/Documents/nortsTest_paper/data/runtime.Rdata")
