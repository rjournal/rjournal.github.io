NUM <- 100

delta <- delta1 <- numeric(NUM)

#Store results
crrp_lasso_time <- matrix(NA, nrow = NUM, ncol = 3)
fast_lasso_time <- matrix(NA, nrow = NUM, ncol = 3)

crrp_scad_time <- matrix(NA, nrow = NUM, ncol = 3)
fast_scad_time <- matrix(NA, nrow = NUM, ncol = 3)

crrp_mcp_time <- matrix(NA, nrow = NUM, ncol = 3)
fast_mcp_time <- matrix(NA, nrow = NUM, ncol = 3)


V <- makeDesignMatrix(nobs, length(beta1), method = "ar", rho = rho)$Sigma
R <- chol(V)
for(num in 1:NUM) {
  set.seed(seed + num)
  cat(paste(num))
  X <- matrix(rnorm(nobs * ncovs), nrow = nobs) %*% R
  dat <- simulateTwoCauseFineGrayModel(nobs, beta1, beta2, X = X, p = .5, u.max = u.max, returnX = FALSE)

  lambda.path <- getLambdaPath(dat$ftime, dat$fstatus, X, lambda.min = 0.1)
  delta[num] <- sum(dat$fstatus == 0) / nobs
  delta1[num] <- sum(dat$fstatus == 1) / nobs

  #LASSO
  crrp_lasso_time[num, ] <- system.time(fit.cmprsk.lasso <- crrp_BIC(dat$ftime, dat$fstatus, X,
                                                           penalty = "LASSO", lambda = lambda.path))[1:3]
  fast_lasso_time[num, ] <- system.time(fit.fast.lasso   <- fastCrrp(Crisk(dat$ftime, dat$fstatus) ~ X,
                                                              penalty = "LASSO", lambda = lambda.path))[1:3]

  #SCAD
  crrp_scad_time[num, ] <- system.time(fit.cmprsk.scad <- crrp_BIC(dat$ftime, dat$fstatus, X,
                                                         penalty = "SCAD", lambda = lambda.path))[1:3]
  fast_scad_time[num, ] <- system.time(fit.fast.scad   <- fastCrrp(Crisk(dat$ftime, dat$fstatus) ~ X,
                                                         penalty = "SCAD", lambda = lambda.path))[1:3]

  #MCP
  crrp_mcp_time[num, ] <- system.time(fit.cmprsk.mcp <- crrp_BIC(dat$ftime, dat$fstatus, X,
                                                         penalty = "MCP", lambda = lambda.path))[1:3]
  fast_mcp_time[num, ] <- system.time(fit.fast.mcp   <- fastCrrp(Crisk(dat$ftime, dat$fstatus) ~ X,
                                                         penalty = "MCP", lambda = lambda.path))[1:3]
}
