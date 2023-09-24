NUM <- 100

delta <- delta1 <- numeric(NUM)

#Store results
cmprsk_time_no_var <- matrix(NA, nrow = NUM, ncol = 3)
fast_time_no_var   <- matrix(NA, nrow = NUM, ncol = 3)
cmprsk_time_var    <- matrix(NA, nrow = NUM, ncol = 3)
fast_time_var      <- matrix(NA, nrow = NUM, ncol = 3)

cmprsk_beta <- matrix(NA, nrow = NUM, ncol = length(beta1))
fast_beta   <- matrix(NA, nrow = NUM, ncol = length(beta1))

cmprsk_var <- list()
fast_var   <- list()

V <- makeDesignMatrix(nobs, length(beta1), method = "ar", rho = rho)$Sigma
R <- chol(V)
for(num in 1:NUM) {
  set.seed(seed + num)
  cat(paste(num))
  X <- matrix(rnorm(nobs * ncovs), nrow = nobs) %*% R
  dat <- simulateTwoCauseFineGrayModel(nobs, beta1, beta2, X = X, p = .5, u.max = u.max, returnX = FALSE)
  delta[num] <- sum(dat$fstatus == 0) / nobs
  delta1[num] <- sum(dat$fstatus == 1) / nobs

  cmprsk_time_no_var[num, ] <- system.time(fit.cmprsk <- crr(dat$ftime, dat$fstatus, X, variance = FALSE))[1:3]
  fast_time_no_var[num, ]   <- system.time(fit.fast   <- fastCrr(Crisk(dat$ftime, dat$fstatus) ~ X, variance = FALSE))[1:3]

  cmprsk_beta[num, ] <- fit.cmprsk$coef
  fast_beta[num, ]   <- fit.fast$coef

  cmprsk_time_var[num, ] <- system.time(fit.cmprsk <- crr(dat$ftime, dat$fstatus, X, variance = TRUE))[1:3]
  fast_time_var[num, ]   <- system.time(fit.boot1   <- fastCrr(Crisk(dat$ftime, dat$fstatus) ~ X, variance = TRUE,
                                                                var.control = varianceControl(B = 100, seed = (seed + num))))[1:3]

  cmprsk_var[[num]] <- sqrt(diag(fit.cmprsk$var))
  fast_var[[num]]   <- sqrt(diag(fit.boot1$var))
}
