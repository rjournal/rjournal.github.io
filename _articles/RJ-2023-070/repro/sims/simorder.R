library(fnets)
sim.order <- function(n, p, N_sims = 100, q = 2, idio.var.order = 1,
                      trunc.lags = 20, n.perm = 30,
                      cv.args = list(n.folds = 1, path.length = 10, do.plot = F),
                      ic.args = list(penalty = NULL, path.length = 10, do.plot = F),
                      common.setting = 0, heavy = FALSE, nonid = NULL) {
  # library(fnets)
  n1 <- n + 1
  burnin <- 100
  
  nm <- c('las.order', 'ds.order')
  order_cv <- matrix(0, nrow = N_sims, ncol = length(nm))
  colnames(order_cv) <- nm
  order_bic <- array(0, dim = c(N_sims,  length(nm), 3))
  colnames(order_bic) <- nm   
  
  nm <- c('static.idio.las', 'static.idio.ds')
  avg_xcast_bic <- array(0, dim = c(N_sims,  length(nm), 3)) 
  colnames(avg_xcast_bic) <- nm 
  max_xcast_bic <- maxcast_bic <- avgcast_bic <- avg_xcast_bic
  
  avg_xcast_cv <- matrix(0, nrow = N_sims, ncol = length(nm))
  colnames(avg_xcast_cv) <- nm
  max_xcast_cv <- maxcast_cv <- avgcast_cv <- avg_xcast_cv
  
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
    
    idio <- sim.var.d(n1, p, d= idio.var.order, Gamma = Gamma, heavy = heavy)
    xi <- idio$data
    
    if(common.setting == 0){
      chi.target <- chi <- 0;
      q <- 0;
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
      
      sc <- apply(xi, 1, sd)/apply(chi, 1, sd)
      chi <- chi * sc
      chi.target <- chi.target * sc
    }
    
    A <- idio$A 
    x <- chi + xi
    xi.target <- A[,,1] %*% xi[, n]
    if(idio.var.order>1){
      for (jj in 2:idio.var.order) {
        xi.target <- A[,,jj] %*% xi[, n-jj+1]
      }
    }
    x.target <- chi.target + xi.target
    
    # lasso, ds
    bic0 <- matrix(0, 3, 4)
    #bic0[1] <- log(abs(det(cov(t(x)))))
    obj_list <- as.list(1:4)
    par(mfrow = c(2,3))
    for(mm in 1:2){
      # ebic 
      # for (order.hat in 1:4) {
      suppressWarnings(obj <- fnets.ic(x = x[, 1:n], q = q, ic.op = 5, kern.const = 4, 
                                       idio.var.order = 1:4, idio.method = c('lasso', 'ds')[mm],
                                       lrpc.method = c('par', 'npar', 'none')[3],
                                       idio.tuning = "ic",
                                       ic.args = ic.args)) 
      # order.hat <- nrow(obj$idio.var$beta)/ncol(obj$idio.var$beta) 
      # order.hat <- obj$idio.var$var.order
      # obj_list[[order.hat]] <- obj
      # bic0[1,order.hat] <- ebic(obj,n,0)
      # bic0[2,order.hat] <- ebic(obj,n,0.5)
      # bic0[3,order.hat] <- ebic(obj,n,1)
      # }
      
      # plot(bic0[1,]); plot(bic0[2,], col="red"); plot(bic0[3,], col="blue");
      
      for (alpha in 1) {
        # order.hat <- #which.min(bic0[alpha,]) #-1 
        # obj <- obj_list[[max(order.hat,1)]]
        order_bic[ii,mm,alpha] <- obj$idio.var$var.order - idio.var.order
        
        ip <- predict.fnets(obj, x[, 1:n])$idio.pred$fc
        avgcast_bic[ii, mm,alpha] <- sum((ip - x.target)^2)/sum(x.target^2)
        maxcast_bic[ii, mm,alpha] <- max(abs(ip - x.target))/max(abs(x.target))
        avg_xcast_bic[ii, mm,alpha] <- sum((ip - x[, n1])^2)/sum(x[, n1]^2)
        max_xcast_bic[ii, mm,alpha] <- max(abs(ip - x[, n1]))/max(abs(x[, n1]))
      }
      
      
      #cv
      suppressWarnings(obj <- fnets(x = x[, 1:n], q = q, ic.op = 5, kern.const = 4,
                                    idio.var.order = 1:4, idio.method = c('lasso', 'ds')[mm],
                                    lrpc.method = c('par', 'npar', 'none')[3],
                                    cv.args = cv.args))
      order.hat <- nrow(obj$idio.var$beta)/ncol(obj$idio.var$beta)
      order_cv[ii,mm] <- order.hat - idio.var.order
      ip <- predict.fnets(obj, x[, 1:n])$idio.pred$fc
      avgcast_cv[ii, mm] <- sum((ip - x.target)^2)/sum(x.target^2)
      maxcast_cv[ii, mm] <- max(abs(ip - x.target))/max(abs(x.target))
      avg_xcast_cv[ii, mm] <- sum((ip - x[, n1])^2)/sum(x[, n1]^2)
      max_xcast_cv[ii, mm] <- max(abs(ip - x[, n1]))/max(abs(x[, n1]))
      
    }
  }
  
  pl <- recordPlot()
  par(mfrow = c(1,1))
  
  ls <- list(avgcast_bic = avgcast_bic, avg_xcast_bic = avg_xcast_bic, maxcast_bic = maxcast_bic, max_xcast_bic = max_xcast_bic,
             avgcast_cv = avgcast_cv, avg_xcast_cv = avg_xcast_cv, maxcast_cv = maxcast_cv, max_xcast_cv = max_xcast_cv,
             bic = order_bic, cv = order_cv, plot = pl,
             n = n, p = p, heavy = heavy, nonid = nonid)
  return(ls)
}





ic.args = list(penalty = 0, path.length = 10, do.plot = F) 

simorder_200_10_1 <- sim.order(200,10, 100, common.setting = 0, idio.var.order = 1, ic.args =ic.args)
saveRDS(simorder_200_10_1, "simorderbic0_200_10_1.RDS")
simorder_200_20_1 <- sim.order(200,20, 100, common.setting = 0, idio.var.order = 1, ic.args =ic.args)
saveRDS(simorder_200_20_1, "simorderbic0_200_20_1.RDS")

simorder_500_10_1 <- sim.order(500,10, 100, common.setting = 0, idio.var.order = 1, ic.args =ic.args)
saveRDS(simorder_500_10_1, "simorderbic0_500_10_1.RDS")
simorder_500_20_1 <- sim.order(500,20, 100, common.setting = 0, idio.var.order = 1, ic.args =ic.args)
saveRDS(simorder_500_20_1, "simorderbic0_500_20_1.RDS")

simorder_200_10_3 <- sim.order(200,10, 100, common.setting = 0, idio.var.order = 3, ic.args =ic.args)
saveRDS(simorder_200_10_3, "simorderbic0_200_10_3.RDS")
simorder_200_20_3 <- sim.order(200,20, 100, common.setting = 0, idio.var.order = 3, ic.args =ic.args)
saveRDS(simorder_200_20_3, "simorderbic0_200_20_3.RDS")

simorder_500_10_3 <- sim.order(500,10, 100, common.setting = 0, idio.var.order = 3, ic.args =ic.args)
saveRDS(simorder_500_10_3, "simorderbic0_500_10_3.RDS")
simorder_500_20_3 <- sim.order(500,20, 100, common.setting = 0, idio.var.order = 3, ic.args =ic.args)
saveRDS(simorder_500_20_3, "simorderbic0_500_20_3.RDS")






orderlist <- list(simorderbic0_200_10_1, simorderbic0_200_20_1, 
                  simorderbic0_500_10_1,simorderbic0_500_20_1,
                  simorderbic0_200_10_3, simorderbic0_200_20_3,
                  simorderbic0_500_10_3,simorderbic0_500_20_3) 

order_nvec <- rep(c(200,200, 500, 500),2) #c(200,200,500,200,200,500)
order_pvec <- rep(c(10, 20),4) #c(50,100,100,50,100,100)
order_dvec <- c(1,1,1,1,
                3,3,3,3) # c(1,1,1,3,3,3)
# setwd("~/Documents/GDFM/gdfmvar/thesis_sims")
library(ggplot2)
library(gridExtra)
par(mfrow = c(1, 1))
par(mar = c(3, 3, 2, .5))
pl <- list()
for (ii in 1:4) {
  df<- data.frame(freq = tabulate(orderlist[[ii]]$cv[,1] +1, 4), q = 0:3, method = "CV, Lasso" )
  df<- rbind.data.frame(df, data.frame(freq = tabulate(orderlist[[ii]]$cv[,2] +1, 4), q = 0:3, method = "CV, DS" ) )
 df<- rbind.data.frame(df, data.frame(freq = tabulate(orderlist[[ii]]$bic[,1,1]+1, 4),  q = 0:3,method = "BIC, Lasso" ))
 df<- rbind.data.frame(df, data.frame(freq = tabulate(orderlist[[ii]]$bic[,2,1]+1, 4),  q = 0:3,method = "BIC, DS" ))
  pl[[ii ]] <-
    ggplot(df, aes(x = q, y = freq, fill = method )) + #, linetype = model
    geom_bar(stat = "identity", width = 0.6, position = "dodge2", show.legend = 0) +
    coord_cartesian( ylim = c(0,100)) +
    labs(title = paste("n = ", order_nvec[ii],", p = ", order_pvec[ii],", d = ", order_dvec[ii], sep = "")) +
    theme_classic() +
    #scale_x_continuous(breaks = 2:6, labels = as.character(0:4 ))  +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    theme(legend.position = "none")
}
for (ii in 5:8) {
  df<- data.frame(freq = tabulate(orderlist[[ii]]$cv[,1]+3, 4), q = (-2):1, method = "CV, Lasso" )
  df<- rbind.data.frame(df, data.frame(freq = tabulate(orderlist[[ii]]$cv[,2] +3, 4), q = (-2):1, method = "CV, DS") )
  df<- rbind.data.frame(df, data.frame(freq = tabulate(orderlist[[ii]]$bic[,1,1]+3, 4), q = (-2):1, method = "BIC, Lasso"))
  df<- rbind.data.frame(df, data.frame(freq = tabulate(orderlist[[ii]]$bic[,2,1]+3, 4), q = (-2):1, method = "BIC, DS" ))
  pl[[ii ]] <-
    ggplot(df, aes(x = q, y = freq,  fill = method )) + #, linetype = model
    geom_bar(stat = "identity", width = 0.6, position = "dodge2", show.legend = (ii==5) ) +
    coord_cartesian( ylim = c(0,100)) +
    labs(title = paste("n = ", order_nvec[ii],", p = ", order_pvec[ii],", d = ", order_dvec[ii], sep = "")) +
    theme_classic() +
    #scale_x_continuous(breaks = 2:6, labels = as.character(0:4 ))  +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}
pl[[5 ]] <- pl[[5 ]] + theme(legend.position = c(.85,.8), legend.text = element_text(size=8),
                             legend.key.size = unit(.3, 'cm'),
                             legend.background = element_rect(fill = "white", color = "black"))
#grid.arrange(grobs = pl , nrow = 4)
grid.arrange(grobs = list(pl[[1]],pl[[5]],pl[[2]],pl[[6]],pl[[3]],pl[[7]],pl[[4]],pl[[8]]) , nrow = 4)

#
ordertab1 <- matrix(0, 8, 16)
for (ii in 1:8) {
  ordertab1[ii,] <- pl[[ii]]$data$freq
}
