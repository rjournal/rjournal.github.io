library(fnets)
library(doParallel)
library(lpSolve)

getROC <- function(TPR, FPR, grid){
  
  out <- grid * 0
  mg <- min(which(grid >= max(FPR)))
  for(ii in 1:mg) out[ii] <- max(TPR[which(abs(FPR - grid[ii]) == min(abs(FPR - grid[ii])))])
  return(list(out = out, mg = mg))
  
}


sim.adaptive <- function(n, p, N_sims = 100, q = 2, common.setting = c(1, 2),
                trunc.lags = 20, n.perm = 30,
                idio.var.order = 1,
                cv.args = list(n.folds = 1, path.length = 10, do.plot = TRUE),
                heavy = FALSE, nonid = NULL, thresh = FALSE) {
  # library(fnets)
  
  n1 <- n + 1
  burnin <- 100
  
  nm <- c('las.tpr', 'las.f', 'las.2', 'las.1',
          'ds.tpr', 'ds.f', 'ds.2', 'ds.1')
  out_est <- matrix(0, nrow = N_sims, ncol = length(nm))
  colnames(out_est) <- nm
  
  nm <- c('las.tpr', 'las.f', 'las.2', 'las.1', 'ds.tpr', 'ds.f', 'ds.2', 'ds.1')
  out_omega <- matrix(0, nrow = N_sims, ncol = length(nm))
  colnames(out_omega) <- nm
  out_omega_adaptive <- out_omega
  
  out_delta <- out_delta_adaptive <- out_omega
  
  ROC_len <- 200
  grid <- seq(0, 1, length.out = ROC_len)
  out_roc <- array(0, dim = c(ROC_len, 4, 2))
  dimnames(out_roc)[[2]] <- c("las.delta", "ds.delta", "las.omega", "ds.omega")
  out_roc_adaptive <- out_roc
  
  constant <- array(0, c(N_sims, 4, 3))
  dimnames(constant)[[2]] <- c("las.delta", "ds.delta", "las.omega", "ds.omega")
  constant_adaptive <- constant
  
  for(ii in 1:N_sims){
    if((ii - 1) %% 10 == 0) print(ii)
    
    set.seed(ii)
    
    Gamma <- Delta <- diag(1, p)
    if(!is.null(nonid) ){
      if(nonid=="random"){
        while(TRUE){
          index <- sample(c(0, 1), p^2, TRUE, prob = c(1 - 1/p, 1/p))
          Delta[which(index == 1)] <- 1
          cc <- colSums(Delta != 0)
          CC <- cc %*% t(cc)
          CC <- -1/sqrt(CC)
          Delta[which(index == 1)] <- CC[which(index == 1)]
          diag(Delta) <- 1.5
          eig <- eigen(Delta, symmetric = TRUE)
          if(all(eig$values > 0)) break
          Delta <- diag(1, p)
        }
        Gamma <- eig$vectors %*% diag(1/(eig$values)) %*% t(eig$vectors)
        # sv <- sqrt(diag(Gamma))
        # Gamma <- t(t(Gamma)/sv)/sv
        # Delta <- t(t(Delta)*sv)*sv
      } else {
      if(nonid=="band"){ 
        Delta <- stats::toeplitz(c(1, .6, .3, rep(0, p-3)))
        eig <- eigen(Delta, symmetric = TRUE)
        Gamma <- eig$vectors %*% diag(1/(eig$values)) %*% t(eig$vectors) 
      }
      if(nonid=="AR"){ 
        Delta <- stats::toeplitz(.6^(1:p - 1))
      }
      eig <- eigen(Delta, symmetric = TRUE)
      Gamma <- eig$vectors %*% diag(1/(eig$values)) %*% t(eig$vectors) 
      }
    }
    
    if(common.setting == 1){
      r <- NULL
      chi <- matrix(0, p, n1)
      chi.target <- rep(0, p)
      
      if(!heavy){
        uu <- matrix(rnorm((n1 + trunc.lags) * q), ncol = q)
      } else{
        uu <- matrix(rt((n1 + trunc.lags) * q, df = 5), ncol = q) * sqrt(3/5)
      }
      a <- matrix(runif(p * q, -1, 1), ncol = q)
      alpha <- matrix(runif(p * q, -.8, .8), ncol = q)
      for(ll in 1:p){
        for(jj in 1:q){
          coeffs <- alpha[ll, jj] * as.numeric(var.to.vma(as.matrix(a[ll, jj]), trunc.lags)) #fnets:::
          for(tt in 1:n1) chi[ll, tt] <- chi[ll, tt] + coeffs %*% uu[(tt + trunc.lags):tt, jj]
          chi.target[ll] <- chi.target[ll] + coeffs[-1] %*% uu[(n + trunc.lags):(n + 1), jj]
        }
      }
      
      idio <- sim.var(n1, p, Gamma = Gamma, heavy = heavy)
      xi <- idio$data
    }
    if(common.setting == 2){
      r <- 2 * q
      if(!heavy){
        uu <- matrix(rnorm((n1 + burnin) * q), nrow = q)
      } else{
        uu <- matrix(rt((n1 + burnin) * q, df = 5), nrow = q) * sqrt(3/5)
      }
      D0 <- matrix(runif(q^2, 0, .3), nrow = q)
      diag(D0) <- runif(q, .5, .8)
      D <- 0.7 * D0/norm(D0, type = '2')
      f <- matrix(0, nrow = q, ncol = n1 + burnin)
      f[, 1] <-  uu[, 1]
      for(tt in 2:(n1 + burnin)) f[, tt] <- D %*% f[, tt - 1] +  uu[, tt]
      f <- f[, -(1:(burnin - 1))]
      loadings <- matrix(rnorm(p * r, 0, 1), nrow = p)
      chi <- matrix(0, p, n1)
      for(ll in 0:1) chi <- chi + loadings[, ll * q + 1:q] %*% f[, 1:n1 + 1 - ll]
      
      # dp <- fnets:::dyn.pca(chi, q = 2)
      # eig <- eigen(dp$acv$Gamma_c[,, 1], symmetric = TRUE)
      # plot(t(dp$acv$Gamma_x[,, 2]) %*% eig$vectors[, 1:r] %*% diag(1/eig$values[1:r]) %*% t(eig$vectors[, 1:r]) %*% chi[, n])
      # points(chi[, n1], col = 2)
      # points(loadings[, 1:q] %*% D %*% f[, n + 1] + loadings[, q + 1:q] %*% f[, n + 1], col = 3)
      
      chi.target <- loadings[, 1:q] %*% D %*% f[, n + 1] + loadings[, q + 1:q] %*% f[, n + 1]
      
      idio <- sim.var(n + 1, p, Gamma = Gamma, heavy = heavy)
      xi <- idio$data
      
      sc <- apply(xi, 1, sd)/apply(chi, 1, sd)
      chi <- chi * sc
      chi.target <- chi.target * sc
    }
    
    A <- idio$A
    Omega <- 2 * pi * t(diag(1, p) - A) %*% Delta %*% (diag(1, p) - A)
    
    x <- chi + xi
    xi.target <- A %*% xi[, n]
    x.target <- chi.target + xi.target
    
    # lasso, ds
    for(mm in 1:2){
      suppressWarnings(obj <- fnets(x = x[, 1:n], q = q, ic.op = 5, kern.const = 4,
                                    common.args = list(var.order = NULL, max.var.order = NULL, trunc.lags = trunc.lags, n.perm = n.perm),
                                    idio.var.order = 1, idio.method = c('lasso', 'ds')[mm],
                                    lrpc.method = c('par', 'npar', 'none')[1],
                                    cv.args = cv.args))
      # beta
      fpr <- tpr <- rep(0, ROC_len) 
      A_hat0 <- t(obj$idio.var$beta)
      thr <- seq(min(abs(A_hat0)) * .999, max(abs(A_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        A_hat <- A_hat0
        A_hat[abs(A_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(A_hat[A == 0] != 0)/sum(A == 0)
        tpr[jj] <- sum(A_hat[A != 0] != 0)/sum(A != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      # plot(fpr, tpr, type = 'l'); lines(grid, roc, col = 2) 
      ind <- which(abs(fpr - .05) == min(abs(fpr - .05)))
      out_est[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_est[ii, 4 * (mm - 1) + 2] <- norm(A_hat0 - A, "F")/norm(A, "F")
      out_est[ii, 4 * (mm - 1) + 3] <- norm(A_hat0 - A, "2")/norm(A, "2")
      out_est[ii, 4 * (mm - 1) + 4] <- norm(A_hat0 - A, "1")/norm(A, "1")
      
      out_roc[, mm, 1] <- out_roc[, mm, 1] + roc$out
      out_roc[1:roc$mg, mm, 2] <- out_roc[1:roc$mg, mm, 2] + 1
      constant[ii, mm, ] <- max(abs(A_hat0[A == 0])) / c(1, obj$idio.var$lambda, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      # th beta
      out_est_th <- out_est
      out_roc_th <- out_roc
      if(thresh) {
        obj$idio.var$beta <- threshold(obj$idio.var$beta, do.plot = T)$network
        fpr <- tpr <- rep(0, ROC_len) 
        A_hat0 <- t(obj$idio.var$beta)
        thr <- seq(min(abs(A_hat0)) * .999, max(abs(A_hat0)) * .999, length.out = ROC_len)
        for(jj in 1:ROC_len){
          A_hat <- A_hat0
          A_hat[abs(A_hat) < thr[jj]] <- 0
          fpr[jj] <- sum(A_hat[A == 0] != 0)/sum(A == 0)
          tpr[jj] <- sum(A_hat[A != 0] != 0)/sum(A != 0)
        }
        roc <- getROC(tpr, fpr, grid)
        # plot(fpr, tpr, type = 'l'); lines(grid, roc, col = 2)
         
        ind <- which(abs(fpr - .05) == min(abs(fpr - .05)))
        out_est_th[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
        out_est_th[ii, 4 * (mm - 1) + 2] <- norm(A_hat0 - A, "F")/norm(A, "F")
        out_est_th[ii, 4 * (mm - 1) + 3] <- norm(A_hat0 - A, "2")/norm(A, "2")
        out_est_th[ii, 4 * (mm - 1) + 4] <- norm(A_hat0 - A, "1")/norm(A, "1")
        
        out_roc_th[, mm, 1] <- out_roc_th[, mm, 1] + roc$out
        out_roc_th[1:roc$mg, mm, 2] <- out_roc_th[1:roc$mg, mm, 2] + 1
        # constant[ii, mm, ] <- max(abs(A_hat0[A == 0])) / c(1, obj$idio.var$lambda, max((log(n)/n)^(1/3), 1/sqrt(p)))
      }
      adap <- adaptive.par.lrpc(object = obj, x = x[, 1:n], cv.args = cv.args, eta= NULL) ##
      
      ## non-adaptive
      # omega
      fpr <- tpr <- rep(0, ROC_len)
      O_hat0 <- obj$lrpc$Omega
      thr <- seq(min(abs(O_hat0)) * .999, max(abs(O_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        O_hat <- O_hat0
        O_hat[abs(O_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(O_hat[Omega == 0] != 0)/sum(Omega == 0)
        tpr[jj] <- sum(O_hat[Omega != 0] != 0)/sum(Omega != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      ind <- which(abs(fpr - .05) == min(abs(fpr - .05)))
      out_omega[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_omega[ii, 4 * (mm - 1) + 2] <- norm(O_hat0 - Omega, "F")/norm(Omega, "F")
      out_omega[ii, 4 * (mm - 1) + 3] <- norm(O_hat0 - Omega, "2")/norm(Omega, "2")
      out_omega[ii, 4 * (mm - 1) + 4] <- norm(O_hat0 - Omega, "1")/norm(Omega, "1")
      
      out_roc[, mm + 2, 1] <- out_roc[, mm + 2, 1] + roc$out
      out_roc[1:roc$mg, mm + 2, 2] <- out_roc[1:roc$mg, mm + 2, 2] + 1
      constant[ii, mm + 2, ] <- max(abs(O_hat0[Omega == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      # delta
      fpr <- tpr <- rep(0, ROC_len)
      D_hat0 <- obj$lrpc$Delta
      thr <- seq(min(abs(D_hat0)) * .999, max(abs(D_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        D_hat <- D_hat0
        D_hat[abs(D_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(D_hat[Delta == 0] != 0)/sum(Delta == 0)
        tpr[jj] <- sum(D_hat[Delta != 0] != 0)/sum(Delta != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      out_delta[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_delta[ii, 4 * (mm - 1) + 2] <- norm(D_hat0 - Delta, "F")/norm(Delta, "F")
      out_delta[ii, 4 * (mm - 1) + 3] <- norm(D_hat0 - Delta, "2")/norm(Delta, "2")
      out_delta[ii, 4 * (mm - 1) + 4] <- norm(D_hat0 - Delta, "1")/norm(Delta, "1")
      
      out_roc[, mm, 1] <- out_roc[, mm , 1] + roc$out
      out_roc[1:roc$mg, mm , 2] <- out_roc[1:roc$mg, mm , 2] + 1
      constant[ii, mm, ] <- max(abs(D_hat0[Delta == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      
      ## adaptive
      # omega
      fpr <- tpr <- rep(0, ROC_len)
      O_hat0 <- adap$Omega
      thr <- seq(min(abs(O_hat0)) * .999, max(abs(O_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        O_hat <- O_hat0
        O_hat[abs(O_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(O_hat[Omega == 0] != 0)/sum(Omega == 0)
        tpr[jj] <- sum(O_hat[Omega != 0] != 0)/sum(Omega != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      ind <- which(abs(fpr - .05) == min(abs(fpr - .05)))
      out_omega_adaptive[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_omega_adaptive[ii, 4 * (mm - 1) + 2] <- norm(O_hat0 - Omega, "F")/norm(Omega, "F")
      out_omega_adaptive[ii, 4 * (mm - 1) + 3] <- norm(O_hat0 - Omega, "2")/norm(Omega, "2")
      out_omega_adaptive[ii, 4 * (mm - 1) + 4] <- norm(O_hat0 - Omega, "1")/norm(Omega, "1")
      
      out_roc_adaptive[, mm + 2, 1] <- out_roc_adaptive[, mm + 2, 1] + roc$out
      out_roc_adaptive[1:roc$mg, mm + 2, 2] <- out_roc_adaptive[1:roc$mg, mm + 2, 2] + 1
      constant_adaptive[ii, mm + 2, ] <- max(abs(O_hat0[Omega == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      # delta
      fpr <- tpr <- rep(0, ROC_len)
      D_hat0 <- adap$Delta
      thr <- seq(min(abs(D_hat0)) * .999, max(abs(D_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        D_hat <- D_hat0
        D_hat[abs(D_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(D_hat[Delta == 0] != 0)/sum(Delta == 0)
        tpr[jj] <- sum(D_hat[Delta != 0] != 0)/sum(Delta != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      out_delta_adaptive[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_delta_adaptive[ii, 4 * (mm - 1) + 2] <- norm(D_hat0 - Delta, "F")/norm(Delta, "F")
      out_delta_adaptive[ii, 4 * (mm - 1) + 3] <- norm(D_hat0 - Delta, "2")/norm(Delta, "2")
      out_delta_adaptive[ii, 4 * (mm - 1) + 4] <- norm(D_hat0 - Delta, "1")/norm(Delta, "1")
      
      out_roc_adaptive[, mm, 1] <- out_roc_adaptive[, mm, 1] + roc$out
      out_roc_adaptive[1:roc$mg, mm , 2] <- out_roc_adaptive[1:roc$mg, mm , 2] + 1
      constant_adaptive[ii, mm, ] <- max(abs(D_hat0[Delta == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      }
    }
  
  
  ls <- list(omega = out_omega, omega_adap = out_omega_adaptive, delta = out_delta, delta_adap = out_delta_adaptive,
             roc = out_roc, roc_adap = out_roc_adaptive, 
             constant = constant, constant_adap = constant_adaptive,
             est = out_est, est_th = out_est_th, roc_th = out_roc_th,
             n = n, p = p, common.setting = common.setting, heavy = heavy, nonid = nonid)
  return(ls)
}
 





sim_oracle.adaptive <- function(n, p, N_sims = 100, idio.var.order = 1,
                       cv.args = list(n.folds = 1, path.length = 10, do.plot = FALSE),
                       heavy = FALSE, nonid = NULL) {
  # library(fnets)
  
  n1 <- n + 1
  burnin <- 100
  
  nm <- c('las.tpr', 'las.f', 'las.2', 'las.1', 'ds.tpr', 'ds.f', 'ds.2', 'ds.1')
  out_omega <- matrix(0, nrow = N_sims, ncol = length(nm))
  colnames(out_omega) <- nm
  out_omega_adaptive <- out_omega
  
  out_delta <- out_delta_adaptive <- out_omega
  
  ROC_len <- 200
  grid <- seq(0, 1, length.out = ROC_len)
  out_roc <- array(0, dim = c(ROC_len, 4, 2))
  dimnames(out_roc)[[2]] <- c("las.delta", "ds.delta", "las.omega", "ds.omega")
  out_roc_adaptive <- out_roc
  
  constant <- array(0, c(N_sims, 4, 3))
  dimnames(constant)[[2]] <- c("las.delta", "ds.delta", "las.omega", "ds.omega")
  constant_adaptive <- constant
  
  for(ii in 1:N_sims){
    if((ii - 1) %% 10 == 0) print(ii)
    
    set.seed(ii)
    
    Gamma <- Delta <- diag(1, p)
    if(!is.null(nonid) ){
      if(nonid=="random"){
        while(TRUE){
          index <- sample(c(0, 1), p^2, TRUE, prob = c(1 - 1/p, 1/p))
          Delta[which(index == 1)] <- 1
          cc <- colSums(Delta != 0)
          CC <- cc %*% t(cc)
          CC <- -1/sqrt(CC)
          Delta[which(index == 1)] <- CC[which(index == 1)]
          diag(Delta) <- 1.5
          eig <- eigen(Delta, symmetric = TRUE)
          if(all(eig$values > 0)) break
          Delta <- diag(1, p)
        }
        Gamma <- eig$vectors %*% diag(1/(eig$values)) %*% t(eig$vectors)
        # sv <- sqrt(diag(Gamma))
        # Gamma <- t(t(Gamma)/sv)/sv
        # Delta <- t(t(Delta)*sv)*sv
      } else {
        if(nonid=="band"){ 
          Delta <- stats::toeplitz(c(1, .6, .3, rep(0, p-3)))
          eig <- eigen(Delta, symmetric = TRUE)
          Gamma <- eig$vectors %*% diag(1/(eig$values)) %*% t(eig$vectors) 
        }
        if(nonid=="AR"){ 
          Delta <- stats::toeplitz(.6^(1:p - 1))
        }
        eig <- eigen(Delta, symmetric = TRUE)
        Gamma <- eig$vectors %*% diag(1/(eig$values)) %*% t(eig$vectors) 
      }
    }
    
    idio <- sim.var(n1, p, Gamma = Gamma, heavy = heavy)
    x <- idio$data
    
    A <- idio$A
    Omega <- 2 * pi * t(diag(1, p) - A) %*% Delta %*% (diag(1, p) - A)
    
    x.target <- A %*% x[, n]
    
    # lasso, ds
    for(mm in 1:2){
      suppressWarnings(obj <- fnets(x = x[, 1:n], q = 0, ic.op = 5, kern.const = 4, 
                                    idio.var.order = 1, idio.method = c('lasso', 'ds')[mm],
                                    lrpc.method = c('par', 'npar', 'none')[1],
                                    cv.args = cv.args))
      adap <- adaptive.par.lrpc(object = obj, x = x[, 1:n], cv.args = cv.args, eta= NULL) ##
      
      ## non-adaptive
      # omega
      fpr <- tpr <- rep(0, ROC_len)
      O_hat0 <- obj$lrpc$Omega
      thr <- seq(min(abs(O_hat0)) * .999, max(abs(O_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        O_hat <- O_hat0
        O_hat[abs(O_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(O_hat[Omega == 0] != 0)/sum(Omega == 0)
        tpr[jj] <- sum(O_hat[Omega != 0] != 0)/sum(Omega != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      ind <- which(abs(fpr - .05) == min(abs(fpr - .05)))
      out_omega[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_omega[ii, 4 * (mm - 1) + 2] <- norm(O_hat0 - Omega, "F")/norm(Omega, "F")
      out_omega[ii, 4 * (mm - 1) + 3] <- norm(O_hat0 - Omega, "2")/norm(Omega, "2")
      out_omega[ii, 4 * (mm - 1) + 4] <- norm(O_hat0 - Omega, "1")/norm(Omega, "1")
      
      out_roc[, mm + 2, 1] <- out_roc[, mm + 2, 1] + roc$out
      out_roc[1:roc$mg, mm + 2, 2] <- out_roc[1:roc$mg, mm + 2, 2] + 1
      constant[ii, mm + 2, ] <- max(abs(O_hat0[Omega == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      # delta
      fpr <- tpr <- rep(0, ROC_len)
      D_hat0 <- obj$lrpc$Delta
      thr <- seq(min(abs(D_hat0)) * .999, max(abs(D_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        D_hat <- D_hat0
        D_hat[abs(D_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(D_hat[Delta == 0] != 0)/sum(Delta == 0)
        tpr[jj] <- sum(D_hat[Delta != 0] != 0)/sum(Delta != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      out_delta[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_delta[ii, 4 * (mm - 1) + 2] <- norm(D_hat0 - Delta, "F")/norm(Delta, "F")
      out_delta[ii, 4 * (mm - 1) + 3] <- norm(D_hat0 - Delta, "2")/norm(Delta, "2")
      out_delta[ii, 4 * (mm - 1) + 4] <- norm(D_hat0 - Delta, "1")/norm(Delta, "1")
      
      out_roc[, mm, 1] <- out_roc[, mm + 2, 1] + roc$out
      out_roc[1:roc$mg, mm , 2] <- out_roc[1:roc$mg, mm + 2, 2] + 1
      constant[ii, mm, ] <- max(abs(D_hat0[Delta == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      
      ## adaptive
      # omega
      fpr <- tpr <- rep(0, ROC_len)
      O_hat0 <- adap$Omega
      thr <- seq(min(abs(O_hat0)) * .999, max(abs(O_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        O_hat <- O_hat0
        O_hat[abs(O_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(O_hat[Omega == 0] != 0)/sum(Omega == 0)
        tpr[jj] <- sum(O_hat[Omega != 0] != 0)/sum(Omega != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      ind <- which(abs(fpr - .05) == min(abs(fpr - .05)))
      out_omega_adaptive[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_omega_adaptive[ii, 4 * (mm - 1) + 2] <- norm(O_hat0 - Omega, "F")/norm(Omega, "F")
      out_omega_adaptive[ii, 4 * (mm - 1) + 3] <- norm(O_hat0 - Omega, "2")/norm(Omega, "2")
      out_omega_adaptive[ii, 4 * (mm - 1) + 4] <- norm(O_hat0 - Omega, "1")/norm(Omega, "1")
      
      out_roc_adaptive[, mm + 2, 1] <- out_roc_adaptive[, mm + 2, 1] + roc$out
      out_roc_adaptive[1:roc$mg, mm + 2, 2] <- out_roc_adaptive[1:roc$mg, mm + 2, 2] + 1
      constant_adaptive[ii, mm + 2, ] <- max(abs(O_hat0[Omega == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
      
      # delta
      fpr <- tpr <- rep(0, ROC_len)
      D_hat0 <- adap$Delta
      thr <- seq(min(abs(D_hat0)) * .999, max(abs(D_hat0)) * .999, length.out = ROC_len)
      for(jj in 1:ROC_len){
        D_hat <- D_hat0
        D_hat[abs(D_hat) < thr[jj]] <- 0
        fpr[jj] <- sum(D_hat[Delta == 0] != 0)/sum(Delta == 0)
        tpr[jj] <- sum(D_hat[Delta != 0] != 0)/sum(Delta != 0)
      }
      roc <- getROC(tpr, fpr, grid)
      
      out_delta_adaptive[ii, 4 * (mm - 1) + 1] <- max(tpr[ind])
      out_delta_adaptive[ii, 4 * (mm - 1) + 2] <- norm(D_hat0 - Delta, "F")/norm(Delta, "F")
      out_delta_adaptive[ii, 4 * (mm - 1) + 3] <- norm(D_hat0 - Delta, "2")/norm(Delta, "2")
      out_delta_adaptive[ii, 4 * (mm - 1) + 4] <- norm(D_hat0 - Delta, "1")/norm(Delta, "1")
      
      out_roc_adaptive[, mm, 1] <- out_roc_adaptive[, mm + 2, 1] + roc$out
      out_roc_adaptive[1:roc$mg, mm , 2] <- out_roc_adaptive[1:roc$mg, mm + 2, 2] + 1
      constant_adaptive[ii, mm, ] <- max(abs(D_hat0[Delta == 0])) / c(1, obj$lrpc$eta, max((log(n)/n)^(1/3), 1/sqrt(p)))
    }
  }
  
  
  ls <- list(omega = out_omega, omega_adap = out_omega_adaptive, delta = out_delta, delta_adap = out_delta_adaptive,
             roc = out_roc, roc_adap = out_roc_adaptive, 
             constant = constant, constant_adap = constant_adaptive,
             n = n, p = p, heavy = heavy, nonid = nonid)
  return(ls)
}


## run code

adap_th_20050 <- sim.adaptive(n = 200,p = 50, 100, common.setting = 1, thresh = TRUE)
save.image("adap_th.Rdata") 
adap_th_200100 <- sim.adaptive(n = 200,p = 100, 100, common.setting = 1, thresh = TRUE)
save.image("adap_th.Rdata") 
adap_th_500100 <- sim.adaptive(n = 500,p = 100, 100, common.setting = 1, thresh = TRUE)
save.image("adap_th.Rdata") 
adap_th_500200 <- sim.adaptive(n = 500,p = 200, 100, common.setting = 1, thresh = TRUE)
save.image("adap_th.Rdata") 

adap_th_20050_random <- sim.adaptive(n = 200,p = 50, 100, common.setting = 1, nonid = "random", thresh=TRUE)
save.image("adap_th_random.Rdata") 
adap_th_200100_random <- sim.adaptive(n = 200,p = 100, 100, common.setting = 1, nonid = "random", thresh=TRUE)
save.image("adap_th_random.Rdata") 
adap_th_500100_random <- sim.adaptive(n = 500,p = 100, 100, common.setting = 1, nonid = "random", thresh=TRUE)
save.image("adap_th_random.Rdata") 
adap_th_500200_random <- sim.adaptive(n = 500,p = 200, 100, common.setting = 1, nonid = "random", thresh=TRUE)
save.image("adap_th_random.Rdata") 


adap_th_20050_band <- sim.adaptive(n = 200,p = 50, 100, common.setting = 1, nonid = "band", thresh = TRUE)
save.image("adap_th_band.Rdata") 
adap_th_200100_band <- sim.adaptive(n = 200,p = 100, 100, common.setting = 1, nonid = "band", thresh = TRUE)
save.image("adap_th_band.Rdata") 
adap_th_500100_band <- sim.adaptive(n = 500,p = 100, 100, common.setting = 1, nonid = "band", thresh = TRUE)
save.image("adap_th_band.Rdata") 
adap_th_500200_band <- sim.adaptive(n = 500,p = 200, 100, common.setting = 1, nonid = "band", thresh = TRUE)
save.image("adap_th_band.Rdata") 


## get results

rbind(colMeans(adap_th_20050$delta),
      apply(adap_th_20050$delta, 2, sd),
      colMeans(adap_th_200100$delta),
      apply(adap_th_200100$delta, 2, sd),
      colMeans(adap_th_500100$delta),
      apply(adap_th_500100$delta, 2, sd),
      colMeans(adap_th_500200$delta),
      apply(adap_th_500200$delta, 2, sd)
      )
rbind(colMeans(adap_th_20050$delta_adap),
      apply(adap_th_20050$delta_adap, 2, sd),
      colMeans(adap_th_200100$delta_adap),
      apply(adap_th_200100$delta_adap, 2, sd),
      colMeans(adap_th_500100$delta_adap),
      apply(adap_th_500100$delta_adap, 2, sd),
      colMeans(adap_th_500200$delta_adap),
      apply(adap_th_500200$delta_adap, 2, sd)
)



rbind(colMeans(adap_th_20050$omega),
      apply(adap_th_20050$omega, 2, sd),
      colMeans(adap_th_200100$omega),
      apply(adap_th_200100$omega, 2, sd),
      colMeans(adap_th_500100$omega),
      apply(adap_th_500100$omega, 2, sd),
      colMeans(adap_th_500200$omega),
      apply(adap_th_500200$omega, 2, sd)
)
rbind(colMeans(adap_th_20050$omega_adap),
      apply(adap_th_20050$omega_adap, 2, sd),
      colMeans(adap_th_200100$omega_adap),
      apply(adap_th_200100$omega_adap, 2, sd),
      colMeans(adap_th_500100$omega_adap),
      apply(adap_th_500100$omega_adap, 2, sd),
      colMeans(adap_th_500200$omega_adap),
      apply(adap_th_500200$omega_adap, 2, sd)
)
