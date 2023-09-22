#### Need the following packages to run the examples in the paper
install.packages("cmprsk")
install.packages("crrp")
install.packages("doParallel")
install.packages("fastcmprsk")
###

library(fastcmprsk)
set.seed(2019)
N <- 500

beta1 <- c(0.40, -0.40, 0, -0.50, 0, 0.60, 0.75, 0, 0, -0.80)
beta2 <- -beta1

Z <- matrix(rnorm(N * length(beta1)), nrow = N)

dat <- simulateTwoCauseFineGrayModel(N, beta1, beta2, + Z, u.min = 0, u.max = 1, p = 0.5)

# Event counts (0 = censored; 1 = event of interest; 2 = competing event)
table(dat$fstatus)


# First 6 observed survival times
head(dat$ftime)

# cmprsk package
library(cmprsk)
fit1 <- crr(dat$ftime, dat$fstatus, Z, failcode = 1, cencode = 0,
            variance = FALSE)

fit2 <- fastCrr(Crisk(dat$ftime, dat$fstatus, cencode = 0, failcode = 1) ~ Z,
                variance = FALSE)
max(abs(fit1$coef - fit2$coef)) # Compare the coefficient estimates for both methods [1] 8.534242e-08

# Estimate variance via 100 bootstrap samples using seed 2019.
vc <- varianceControl(B = 100, seed = 2019)
fit3 <- fastcmprsk::fastCrr(Crisk(dat$ftime, dat$fstatus) ~ Z, variance = TRUE,
                            var.control = vc,
                            returnDataFrame = TRUE)

# returnDataFrame = TRUE is necessary for CIF estimation (next section)
round(sqrt(diag(fit3$var)), 3) # Standard error estimates rounded to 3rd decimal place [1] 0.108 0.123 0.085 0.104 0.106 0.126 0.097 0.097 0.104 0.129

coef(fit3)

logLik(fit3)

AIC(fit3, k = 2)

-2 * logLik(fit3) + 2 * length(coef(fit3))

vcov(fit3)[1:3, 1:3] 
confint(fit3, level = 0.95)

summary(fit3, conf.int = TRUE)

library(doParallel)
n.cores <- 2 # No. of cores
myClust <- makeCluster(n.cores)
# Set useMultipleCores = TRUE to enable parallelization
vc = varianceControl(B = 1000, useMultipleCores = TRUE)
registerDoParallel(myClust)
fit3 <- fastCrr(Crisk(dat$ftime, dat$fstatus) ~ Z, variance = TRUE, 
                var.control = vc)
stopCluster(myClust)


set.seed(2019)
# Make sure getBreslowJumps and returnDataFrame are set to TRUE
fit4 <- fastCrr(Crisk(dat$ftime, dat$fstatus, cencode = 0, failcode = 1) ~ Z,
                                               variance = FALSE,
                                                getBreslowJumps = TRUE, # Default = TRUE
                                                returnDataFrame = TRUE) # Default is FALSE for storage purposes
z0 <- rnorm(10) # New covariate entries to predict
cif.point <- predict(fit4, newdata = z0, getBootstrapVariance = TRUE,
                        type = "interval", tL = 0.2, tU = 0.9,
                        var.control = varianceControl(B = 100, seed = 2019))
plot(cif.point) # Figure 1 (Plot of CIF and 95% C.I.)



library(crrp)
lam.path <- 10^seq(log10(0.1), log10(0.001), length = 25)

# crrp package
fit.crrp <- crrp(dat$ftime, dat$fstatus, Z, penalty = "LASSO", 
                 lambda = lam.path, eps = 1E-6)
 # fastcmprsk package
fit.fcrrp <- fastCrrp(Crisk(dat$ftime, dat$fstatus) ~ Z, penalty = "LASSO", 
                      lambda = lam.path)
# Check to see the two methods produce the same estimates. 
max(abs(fit.fcrrp$coef - fit.crrp$beta))
plot(fit.fcrrp) # Figure 2 (Solution path)
