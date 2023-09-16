############## Codes for "Using the package combinIT" section #####
# install.packages("combinIT")
library(combinIT)
data(CNV)
interaction_plot(CNV)
CI_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE, nc0 = 10000, opvalue = 0.6187, Elapsed_time = FALSE)
Franck_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE, plot = TRUE, Elapsed_time = FALSE)
Boik_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE)
Piepho_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE)
KKSA_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE, plot = FALSE, Elapsed_time = FALSE)
Malik_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE, Elapsed_time = FALSE)
KKM_test(CNV, nsim = 10000, alpha = 0.05, report = TRUE, nc0 = 10000)
############################# End #########################

######### Codes for comparing the speed execution of Malik.test with MalikPavlue  #######
N <- 10000
# install.packages("hiddenf")
library(hiddenf)
library(ggplot2)
CNV.mtx <- HiddenF(CNV)
system.time(
  Malik_test(CNV, nsim = N, Elapsed_time = FALSE)
)
system.time( 
  MalikPvalue(CNV.mtx, N = N) # MalikPvalue comes from hiddenf package and take long time to run.
)
# The dataset file "elapsed.csv" is available in the attached folder.
elapsed <- read.csv(file = "elapsed.csv")
ggplot(elapsed, aes(N, Time, colour = Function)) +
  geom_line(size = 1, aes(linetype = Function)) +
  labs(x = "Number of Monte Carlo Samples", y = "Elapsed Time(Seconds)") +
  geom_point(size = 3) +
  theme(
    legend.position = "top",
    axis.line = element_line(size = 1, linetype = "solid")
  )


############################# End ################################

############## Codes for "Simulation study" section ###########
##
## This code is only for a=4 and b=3.
## To reproduce the other values, simpliy the values of a and b should be changed.
## Note that this code is run in paralell.
##
# rm(list = ls())
# library(foreach)
# library(doParallel)
# library(doSNOW)
# library(tcltk)
# library(iterators)
# library(plyr)
# library(hiddenf)
# library(MASS)
# library(mvtnorm)
# library(combinIT)
# #-----initial values -------------
# N2 <- 100 # change to 10000 for final result
# N1 <- 1000 # change to 100000 for final result
# pre <- ceiling(log10(N1)) + 1
# a <- 4
# b <- 3
# k <- 6 + 2 # the number of methods(Boik, Piepho, ..., Tukey,Mande1)
# alpha <- c(0.1, 0.05, 0.01)
# #---------------------- Simulation is started here ----------------------
# Bon <- rep(0, 0)
# Sidak <- rep(0, 0)
# jacobi <- rep(0, 0)
# GC <- rep(0, 0)
# 
# first <- proc.time()
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(max = N2, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# parout <- foreach(ee = 1:N2, .combine = rbind, .packages = c("MASS", "combinIT", "hiddenf", "mvtnorm", "tcltk"), .verbose = FALSE, .options.snow = opts) %dopar% {
#   x <- matrix(rnorm(a * b), nrow = a)
#   #----------------------------- Calculating p-values -----------------------
#   out <- HiddenF(x)
#   tukeypvalue <- TukeyPvalue(out)$pvalue
#   mandelPvalue <- MandelPvalue(out)$pvalue
#   pval6 <- CI_test(x, nsim = N1, alpha = 0.05, report = FALSE, nc0 = 10000, opvalue = c(tukeypvalue, mandelPvalue), Elapsed_time = FALSE)
#   KKSApvalue <- pval6$KKSA.pvalue
#   Boikpvalue <- pval6$Boik.pvalue
#   PICpvalue <- pval6$KKM.pvalue
#   Piephopvalue <- pval6$Piepho.pvalue
#   hiddenpvalue <- pval6$Franck.pvalue
#   Malikpvalue <- pval6$Malik.pvalue
#   pvalues <- c(KKSApvalue, PICpvalue, Boikpvalue, Malikpvalue, hiddenpvalue, Piephopvalue, tukeypvalue, mandelPvalue)
#   Bon <- pval6$Bonferroni
#   Sidak <- pval6$Sidak
#   Jacobi <- pval6$Jacobi
#   GC <- pval6$GC
#   attr(GC, which = "error") <- NULL
#   attr(GC, which = "msg") <- NULL
#   c(Bon, Sidak, Jacobi, GC)
# }
# close(pb)
# stopCluster(cl)
# Bon.p <- matrix(0, 2, length(alpha))
# rownames(Bon.p) <- c("pvalue", "EAE")
# colnames(Bon.p) <- c("Bon0.1", "Bon0.05", "Bon0.01")
# sidak.p <- matrix(0, 2, length(alpha))
# rownames(sidak.p) <- c("pvalue", "EAE")
# colnames(sidak.p) <- c("Sid0.1", "Sid0.05", "Sid0.01")
# jacobi.p <- matrix(0, 2, length(alpha))
# rownames(jacobi.p) <- c("pvalue", "EAE")
# colnames(jacobi.p) <- c("Jac0.1", "Jac0.05", "Jac0.01")
# GC.p <- matrix(0, 2, length(alpha))
# rownames(GC.p) <- c("pvalue", "EAE")
# colnames(GC.p) <- c("GC0.1", "GC0.05", "GC0.01")
# 
# for (i in 1:length(alpha)) {
#   Bon.p[1, i] <- mean(parout[, 1] < alpha[i])
#   Bon.p[2, i] <- 1.96 * sqrt(Bon.p[1, i] * (1 - Bon.p[1, i]) / N2)
#   sidak.p[1, i] <- mean(parout[, 2] < alpha[i])
#   sidak.p[2, i] <- 1.96 * sqrt(sidak.p[1, i] * (1 - sidak.p[1, i]) / N2)
#   jacobi.p[1, i] <- mean(parout[, 3] < alpha[i])
#   jacobi.p[2, i] <- 1.96 * sqrt(jacobi.p[1, i] * (1 - jacobi.p[1, i]) / N2)
#   GC.p[1, i] <- mean(parout[, 4] < alpha[i])
#   GC.p[2, i] <- 1.96 * sqrt(GC.p[1, i] * (1 - GC.p[1, i]) / N2)
# }
# last <- date()
# finalout <- cbind(Bon.p, sidak.p, jacobi.p, GC.p)
# write.table(round(finalout, 4), file = "C:\\Users\\SaFa\\Downloads\\Desktop\\finalout.txt", quote = FALSE, sep = "\t", append = FALSE, row.names = TRUE, col.names = TRUE)
# finalout
# last <- proc.time()
# last - first