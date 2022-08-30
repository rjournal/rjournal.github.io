#######################################################################################################
#  R-code for reproducing the analysis of the article (and associated supplementary material)
#
#  ROCnReg: An R Package for Receiver Operating Characteristic Curve Inference with and without Covariate Information
#
#  by MX Rodriguez-Alvarez and V Inacio
#
#  Contact: V Inacio             - Vanda.Inacio@ed.ac.uk
#           MX Rodriguez-Alvarez - mxrodriguez@bcamath.org
#
#####################################################################################################
# All packages, including ROCnReg are avaliable in CRAN (https://CRAN.R-project.org/)
library(ROCnReg)
library(gridExtra)
library(grid)
library(ggplot2)
library(coda)
library(nor1mix)

rm(list = ls())

# Summary of the dataset
data("endosyn")
summary(endosyn)

############################################################################################################################################################
############################################################################################################################################################
# Package presentation and illustration: Pooled ROC curve
############################################################################################################################################################
############################################################################################################################################################ 
  
  ################################################################### 
  # DPM pooled ROC curve: L = 10
  ###################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    pROC_dpm <- pooledROC.dpm(marker = "bmi", group = "cvd_idf", 
      tag.h = 0, data = endosyn,
      standardise = TRUE, p = seq(0, 1, l = 101), ci.level = 0.95, 
      compute.lpml = TRUE, compute.WAIC = TRUE, compute.DIC = TRUE,
      pauc = pauccontrol(compute = TRUE, focus = "FPF", value = 0.1), 
      density = densitycontrol(compute = TRUE),
      prior.h = priorcontrol.dpm(L = 10), 
      prior.d = priorcontrol.dpm(L = 10),
      mcmc = mcmccontrol(nsave = 8000, nburn = 2000, nskip = 1),
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_pROC_dpm <- end - start
    summary(pROC_dpm)

    plot(pROC_dpm, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
    ##################################################################
    # Densities: using ggplot2
    ##################################################################
      # Nondiseased
        df_den_h <- data.frame(x = rep(pROC_dpm$dens$h$grid, each = pROC_dpm$mcmc$nsave), y = c(pROC_dpm$dens$h$dens))
        p1 <- ggplot() + geom_histogram(aes(x = pROC_dpm$marker$h, y = ..density..), fill = "grey", color = "black") +
        stat_summary(data = df_den_h, aes(x = x, y = y), geom = "line", fun = "mean", col = "red", size = 1) +
        stat_summary(data = df_den_h, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.025), col = "red", linetype = "dashed") + 
        stat_summary(data = df_den_h, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.975), col = "red", linetype = "dashed") +
        labs(x = "BMI", y = "Density", title = "Nondiseased population") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 20), 
              axis.text = element_text(size = 20), 
              axis.title = element_text(size = 20),)
        
        print(p1)
        
      # Diseased
        df_den_d <- data.frame(x = rep(pROC_dpm$dens$d$grid, each = pROC_dpm$mcmc$nsave), y = c(pROC_dpm$dens$d$dens))
        p2 <-ggplot() + geom_histogram(aes(x = pROC_dpm$marker$d, y = ..density..), fill = "grey", color = "black") +
        stat_summary(data = df_den_d, aes(x = x, y = y), geom = "line", fun = "mean", col = "red", size = 1) +
        stat_summary(data = df_den_d, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.025), col = "red", linetype = "dashed") + 
        stat_summary(data = df_den_d, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.975), col = "red", linetype = "dashed") +
        labs(x = "BMI", y = "Density", title = "Diseased population")+
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 20), 
              axis.text = element_text(size=20), 
              axis.title = element_text(size = 20))
        
        
        print(p2)
        
    ################################################################### 
    # Trace plots, effective sample sizes and Geweke statistics
    ###################################################################
      # Trace plots for several BMI values (randomly selected), for healthy and diseased population
        # Nondiseased
          set.seed(123, "Mersenne-Twister") # for reproducibility
          # BMI values (randomly selected)
          pos_h_den <- sort(sample(1:length(pROC_dpm$dens$h$grid), 3))
          bmi_h_den <- round(pROC_dpm$dens$h$grid[pos_h_den])

          op <- par(mfrow = c(1,3))
          for(i in 1:3) {
            plot(1:pROC_dpm$mcmc$nsave, pROC_dpm$dens$h$dens[,pos_h_den[i]], 
              type = "l", xlab = "Iteration", ylab = "Density", 
              main = paste0("Trace plot \n Nondiseased population - BMI = ", round(bmi_h_den[i])), 
              cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          }
          par(op)
          
        # Diseased          
          set.seed(123, "Mersenne-Twister") # for reproducibility
          # BMI values (randomly selected)
          pos_d_den <- sort(sample(1:length(pROC_dpm$dens$d$grid), 3))
          bmi_d_den <- round(pROC_dpm$dens$d$grid[pos_d_den])

          op <- par(mfrow = c(1,3))
          for(i in 1:3) {
            plot(1:pROC_dpm$mcmc$nsave, pROC_dpm$dens$d$dens[,pos_d_den[i]], 
              type = "l", xlab = "Iteration", ylab = "Density", 
              main = paste0("Trace plot \n Diseased population - BMI = ", bmi_d_den[i]), 
              cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          }
          par(op)
          
      # Effective sample size in healthy and diseased populations
        # Nondiseased
          plot(pROC_dpm$dens$h$grid, effectiveSize(pROC_dpm$dens$h$dens), 
            type = "l", xlab = "BMI", ylab = "Effective Sample Size (density)", 
            main = "Nondiseased population", 
            cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          
        # Diseased
          plot(pROC_dpm$dens$d$grid, effectiveSize(pROC_dpm$dens$d$dens), 
            type = "l", xlab = "BMI", ylab = "Effective Sample Size (density)", 
            main = "Diseased population", 
            cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          
        
      # Geweke statistic in healthy and diseased populations
        # Nondiseased
          plot(pROC_dpm$dens$h$grid, geweke.diag(pROC_dpm$dens$h$dens)$z, 
             type = "l", xlab = "BMI", ylab = "Geweke Statistic (density)", 
             main = "Nondiseased population", 
             cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
        
        # Diseased
          plot(pROC_dpm$dens$d$grid, geweke.diag(pROC_dpm$dens$d$dens)$z, 
             type = "l", xlab = "BMI", ylab = "Geweke Statistic (density)", 
             main = "Diseased population", 
             cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
        
    
    ####################################################
    # Quantile residuals (Supplementary Material B.1)
    ####################################################
    # Nondiseased
      start <- proc.time()  
      traj <- matrix(0, nrow = pROC_dpm$mcmc$nsave, ncol = length(pROC_dpm$marker$h))
      lgrid <- length(pROC_dpm$marker$h)
      grid <- qnorm(ppoints(lgrid))
      for (l in 1:pROC_dpm$mcmc$nsave){
        aux <- norMix(mu = pROC_dpm$fit$h$mu[l,], 
                    sigma = pROC_dpm$fit$h$sd[l,], 
                    w = pROC_dpm$fit$h$probs[l,])
        traj[l, ] <- quantile(qnorm(pnorMix(pROC_dpm$marker$h, aux)), ppoints(lgrid), type = 2)
      }
      end <- proc.time()  
      end - start

      l.band_h <- apply(traj, 2, quantile, prob = 0.025)
      trajhat_h <- apply(traj, 2, mean)
      u.band_h <- apply(traj, 2, quantile, prob = 0.975)

      op <- par(pty = "s")
      plot(grid, trajhat_h, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", 
        main = "Nondiseased population", cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
      lines(grid, l.band_h, lty = 2, lwd = 2)
      lines(grid, u.band_h, lty = 2, lwd = 2)
      abline(a = 0, b = 1, col ="red", lwd = 2)
      par(op)
      
    
    # Diseased
      traj <- matrix(0, nrow = pROC_dpm$mcmc$nsave, ncol = length(pROC_dpm$marker$d))
      lgrid <- length(pROC_dpm$marker$d)
      grid <- qnorm(ppoints(lgrid))
      for (l in 1:pROC_dpm$mcmc$nsave){
        aux <- norMix(mu = pROC_dpm$fit$d$mu[l,], 
                  sigma = pROC_dpm$fit$d$sd[l,], 
                  w = pROC_dpm$fit$d$probs[l,])
        traj[l, ] <- quantile(qnorm(pnorMix(pROC_dpm$marker$d, aux)), ppoints(lgrid), type = 2)
      }
      l.band_d <- apply(traj, 2, quantile, prob = 0.025)
      trajhat_d <- apply(traj, 2, mean)
      u.band_d <- apply(traj, 2, quantile, prob = 0.975)

      op <- par(pty = "s")
      plot(grid, trajhat_d, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", 
        main = "Diseased population", cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
      lines(grid, l.band_d, lty = 2, lwd = 2)
      lines(grid, u.band_d, lty = 2, lwd = 2)
      abline(a = 0, b = 1, col ="red", lwd = 2)
      par(op)
      

  ################################################################### 
  # DPM pooled ROC curve: L = 1 (Binormal model)
  ###################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    pROC_normal <- pooledROC.dpm(marker = "bmi", group = "cvd_idf", 
      tag.h = 0, data = endosyn,
      standardise = TRUE, p = seq(0, 1, l = 101), ci.level = 0.95,
      compute.lpml = TRUE, compute.WAIC = TRUE, compute.DIC = TRUE,
      pauc = pauccontrol(compute = TRUE, focus = "FPF", value = 0.1), 
      density = densitycontrol(compute = TRUE),
      prior.h = priorcontrol.dpm(L = 1), 
      prior.d = priorcontrol.dpm(L = 1),
      mcmc = mcmccontrol(nsave = 8000, nburn = 2000, nskip = 1),
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_pROC_normal <- end - start
    summary(pROC_normal)

    plot(pROC_normal, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
    
    ##################################################################
    # Densities: using ggplot2
    ##################################################################
      # Nondiseased
        df_den_h <- data.frame(x = rep(pROC_normal$dens$h$grid, each = pROC_normal$mcmc$nsave), y = c(pROC_normal$dens$h$dens))
        p1 <- ggplot() + geom_histogram(aes(x = pROC_normal$marker$h, y = ..density..), fill = "grey", color = "black") +
        stat_summary(data = df_den_h, aes(x = x, y = y), geom = "line", fun = "mean", col = "red", size = 1) +
        stat_summary(data = df_den_h, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.025), col = "red", linetype = "dashed") + 
        stat_summary(data = df_den_h, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.975), col = "red", linetype = "dashed") +
        labs(x = "BMI", y = "Density", title = "Nondiseased population") +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, size = 20), 
              axis.text = element_text(size = 20), 
              axis.title = element_text(size = 20))
        
        print(p1)
        
      # Diseased     
        df_den_d <- data.frame(x = rep(pROC_normal$dens$d$grid, each = pROC_normal$mcmc$nsave), y = c(pROC_normal$dens$d$dens))
        p2 <-ggplot() + geom_histogram(aes(x = pROC_normal$marker$d, y = ..density..), fill = "grey", color = "black") +
        stat_summary(data = df_den_d, aes(x = x, y = y), geom = "line", fun = "mean", col = "red", size = 1) +
        stat_summary(data = df_den_d, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.025), col = "red", linetype = "dashed") + 
        stat_summary(data = df_den_d, aes(x = x, y = y), geom = "line", fun = "quantile", fun.args = list(0.975), col = "red", linetype = "dashed") +
        labs(x = "BMI", y = "Density", title = "Diseased population") +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, size = 20), 
              axis.text = element_text(size=20), 
              axis.title = element_text(size = 20))
        
        print(p2)
  
    ############################################################
    # Quantile residuals (Supplementary Material B.1)
    ############################################################
      # Nondiseased population
        traj_normal <- matrix(0, nrow = pROC_normal$mcmc$nsave, ncol = length(pROC_normal$marker$h))
        lgrid <- length(pROC_normal$marker$h)
        grid <- qnorm(ppoints(lgrid))
        for (l in 1:pROC_normal$mcmc$nsave){
          traj_normal[l, ] <- quantile(qnorm(pnorm(pROC_normal$marker$h,
                                        mean = pROC_normal$fit$h$mu[l],
                                        sd = pROC_normal$fit$h$sd[l])), ppoints(lgrid), type = 2)
        }
        l.band_normal_h <- apply(traj_normal, 2, quantile, prob = 0.025)
        trajhat_normal_h <- apply(traj_normal, 2, mean)
        u.band_normal_h <- apply(traj_normal, 2, quantile, prob = 0.975)

        op <- par(pty = "s")
        plot(grid, trajhat_normal_h, xlab = "Theoretical Quantiles", 
              ylab = "Sample Quantiles", main = "Nondiseased population",
              cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
        lines(grid, l.band_normal_h, lty = 2, lwd = 2)
        lines(grid, u.band_normal_h, lty = 2, lwd = 2)
        abline(a = 0, b = 1, col ="red", lwd = 2)
        par(op)
        
      
      # Diseased population
        traj_normal <- matrix(0, nrow = pROC_normal$mcmc$nsave, ncol = length(pROC_normal$marker$d))
        lgrid <- length(pROC_normal$marker$d)
        grid <- qnorm(ppoints(lgrid))
        for (l in 1:pROC_normal$mcmc$nsave){
          traj_normal[l, ] <- quantile(qnorm(pnorm(pROC_normal$marker$d, 
                                        mean = pROC_normal$fit$d$mu[l], 
                                        sd = pROC_normal$fit$d$sd[l])), ppoints(lgrid), type = 2)
        }
        l.band_normal_d <- apply(traj_normal, 2, quantile, prob = 0.025)
        trajhat_normal_d <- apply(traj_normal, 2, mean)
        u.band_normal_d <- apply(traj_normal, 2, quantile, prob = 0.975)

        op <- par(pty = "s")
        plot(grid, trajhat_normal_d, xlab = "Theoretical Quantiles", 
              ylab = "Sample Quantiles", main = "Diseased population", 
              cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
        lines(grid, l.band_normal_d, lty = 2, lwd = 2)
        lines(grid, u.band_normal_d, lty = 2, lwd = 2)
        abline(a = 0, b = 1, col ="red", lwd = 2)
        par(op)
        

  #################################################################################
  # Bayesian bootstrap pooled ROC curve
  #################################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    pROC_BB <- pooledROC.BB(marker = "bmi", group = "cvd_idf", 
      tag.h = 0, data = endosyn,
      p = seq(0, 1, l = 101),
      pauc = pauccontrol(compute = TRUE, focus = "FPF", value = 0.1), 
      B = 5000, ci.level = 0.95, 
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_pROC_BB <- end - start
    summary(pROC_BB)

    plot(pROC_BB, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
    
  ###################################################################
  # Comparisons between DPM approach and BB
  ###################################################################
    plot(pROC_BB$p, pROC_BB$ROC[,1], type = "l", xlim = c(0,1), ylim = c(0,1),
      xlab = "FPF", ylab = "TPF", 
      main = "Pooled ROC curve \n Bayesian DPM and Bayesian Bootstrap",
      cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    lines(pROC_BB$p, pROC_BB$ROC[,2], lty = 2)
    lines(pROC_BB$p, pROC_BB$ROC[,3], lty = 2)

    lines(pROC_dpm$p, pROC_dpm$ROC[,1], col = 2)
    lines(pROC_dpm$p, pROC_dpm$ROC[,2], col = 2, lty = 2)
    lines(pROC_dpm$p, pROC_dpm$ROC[,3], col = 2, lty = 2)
    abline(0, 1, col = "grey", lty = 2)
    
  #################################################################################
  # Cutpoints based on the Youden Index (using DPM)
  #################################################################################
    start <- proc.time()[3]
    th_pROC_dmp <- compute.threshold.pooledROC(pROC_dpm, criterion = "YI", ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_th_pROC_dpm <- end - start

    th_pROC_dmp

  ################################################################### 
  # Empirical pooled ROC curve (Supplementary Material B.2)
  ###################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    pROC_emp <- pooledROC.emp(marker = "bmi", group = "cvd_idf", 
      tag.h = 0, data = endosyn, 
      p = seq(0, 1, l = 101),
      pauc = pauccontrol(compute = TRUE, focus = "FPF", value = 0.1), B = 500, 
      ci.level = 0.95, parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_pROC_emp <- end - start
    summary(pROC_emp)

    plot(pROC_emp, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
    
  #################################################################################
  # Kernel-based pooled ROC curve (Supplementary Material B.2)
  #################################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    pROC_kernel <- pooledROC.kernel(marker = "bmi", group = "cvd_idf", 
      tag.h = 0, data = endosyn,
      p = seq(0, 1, l = 101),
      bw = "SRT",
      B = 500, ci.level = 0.95, 
      method = "coutcome",
      pauc = pauccontrol(compute = TRUE, focus = "TPF", value = 0.8),
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_pROC_kernel <- end - start
    summary(pROC_kernel)

    plot(pROC_kernel, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
    
  ###################################################################
  # Comparisons between all approaches (Supplementary Material B.3)
  ###################################################################
    plot(pROC_emp$p, pROC_emp$ROC[,1], type = "s", xlim = c(0,1), ylim = c(0,1),
      xlab = "FPF", ylab = "TPF", 
      main = "Pooled ROC curve - Different approaches",
      cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    lines(pROC_dpm$p, pROC_dpm$ROC[,1], col = 2)
    lines(pROC_BB$p, pROC_BB$ROC[,1], col = 3)
    lines(pROC_kernel$p, pROC_kernel$ROC[,1], col = 4)
    abline(0, 1, col = "grey", lty = 2)
    legend("topleft", legend = c("Empirical", "DPM", "BB", "Kernel"), lty = 1, col = 1:4, bty = "n", lwd = 2, cex = 1.5)
    
  #################################################################################
  # Save objects (if desired)
  #################################################################################
  # save(pROC_dpm, pROC_normal, pROC_emp, th_pROC_dmp, pROC_BB, pROC_kernel, file = "pooledROC.RData")

############################################################################################################################################################
############################################################################################################################################################
# Package presentation and illustration: Covariate-specific ROC curve
############################################################################################################################################################
############################################################################################################################################################
  
  # Data frame for predictions
  agep <- seq(22, 80, l = 30)
  endopred <- data.frame(age = rep(agep,2), gender = factor(rep(c("Women", "Men"), each = length(agep))))

  ###################################################################
  # Bayesian approach with L = 1 and parametric/linear effects
  ###################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    cROC_bp <- cROC.bnp(formula.h = bmi ~ gender*age,
      formula.d = bmi ~ gender*age,
      group = "cvd_idf", 
      tag.h = 0,
      data = endosyn,
      newdata = endopred,
      standardise = TRUE, 
      p = seq(0, 1, l = 101),
      ci.level = 0.95,
      compute.lpml = TRUE, compute.WAIC = TRUE, compute.DIC = TRUE, 
      pauc = pauccontrol(compute = FALSE),
      prior.h = priorcontrol.bnp(L = 1),
      prior.d = priorcontrol.bnp(L = 1),
      density = densitycontrol(compute = TRUE),
      mcmc = mcmccontrol(nsave = 8000, nburn = 2000, nskip = 1),
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_cROC_bp <- end - start
    summary(cROC_bp)

    op <- par(mfrow = c(2,2))
    plot(cROC_bp, ask = FALSE)
    par(op)
    
  ###################################################################
  # Bayesian approach with L = 10 and nonlinear effects of age
  # Note: Time consuming
  ###################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    levels(endosyn$gender)
    start <- proc.time()[3]
    cROC_bnp <- cROC.bnp(formula.h = bmi ~ gender + f(age, by = gender, K = c(0,0)),
      formula.d = bmi ~ gender + f(age, by = gender, K = c(4,4)),
      group = "cvd_idf", 
      tag.h = 0,
      data = endosyn,
      newdata = endopred,
      standardise = TRUE, 
      p = seq(0, 1, l = 101),
      ci.level = 0.95,
      compute.lpml = TRUE, compute.WAIC = TRUE, compute.DIC = TRUE, 
      pauc = pauccontrol(compute = FALSE),
      prior.h = priorcontrol.bnp(L = 10),
      prior.d = priorcontrol.bnp(L = 10),
      density = densitycontrol(compute = TRUE),
      mcmc = mcmccontrol(nsave = 8000, nburn = 2000, nskip = 1),
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_cROC_bnp <- end - start
    summary(cROC_bnp)

    op <- par(mfrow = c(2,2))
    plot(cROC_bnp, ask = FALSE)
    par(op)
    
  ###################################################################
  # Predictive checks
  ###################################################################
    op <- par(mfrow = c(2,3))
    pc_cROC_bp <- predictive.checks(cROC_bp, statistics = c("kurtosis", "skewness"), devnew = FALSE)
    par(op)
    
    op <- par(mfrow = c(2,3))
    pc_cROC_bnp <- predictive.checks(cROC_bnp, statistics = c("kurtosis", "skewness"), devnew = FALSE)
    par(op)
    
  ###################################################################### 
  # Trace plots, effective sample sizes and Geweke statistics 
  # (Supplementary Material C.1)
  # cROC_bnp object 
  ######################################################################    
    # Trace plots for several BMI values and covariates (randomly selected), for healthy and diseased population
      # Nondiseased
        set.seed(54321, "Mersenne-Twister") # for reproducibility
        # BMI values
        pos_h_den <- sort(sample(1:length(cROC_bnp$dens$h$grid), 2))
        bmi_h_den <- round(cROC_bnp$dens$h$grid[pos_h_den])
        # Covariate values
        cov_h_den <- sort(sample(1:nrow(cROC_bnp$newdata), 3))
        
        op <- par(mfrow = c(2,3))
        for(i in 1:2) {
          for(j in 1:3) {
            plot(1:cROC_bnp$mcmc$nsave, cROC_bnp$dens$h$dens[pos_h_den[i],,cov_h_den[j]], type = "l", 
              xlab = "Iteration", 
              ylab = "Density", 
              main = paste0("Trace plot - Nondiseased population \n  BMI = ", round(bmi_h_den[i]), " - Age = ", cROC_bnp$newdata$age[cov_h_den[j]], " - Gender = ", cROC_bnp$newdata$gender[cov_h_den[j]]), 
              cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
          }
        }
        par(op)
        
      # Diseased
        set.seed(123456, "Mersenne-Twister")
        # BMI values
        pos_d_den <- sort(sample(1:length(cROC_bnp$dens$d$grid), 2))
        bmi_d_den <- round(cROC_bnp$dens$d$grid[pos_d_den])

        # Covariate values
        cov_d_den <- sort(sample(1:nrow(cROC_bnp$newdata), 3))
        
        op <- par(mfrow = c(2,3))
        for(i in 1:2) {
          for(j in 1:3) {
            plot(1:cROC_bnp$mcmc$nsave, cROC_bnp$dens$d$dens[pos_d_den[i],,cov_d_den[j]], type = "l", 
              xlab = "Iteration", 
              ylab = "Density", 
              main = paste0("Trace plot - Diseased population \n  BMI = ", round(bmi_d_den[i]), " - Age = ", cROC_bnp$newdata$age[cov_d_den[j]], " - Gender = ", cROC_bnp$newdata$gender[cov_d_den[j]]), 
              cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
          } 
        }
        par(op)
        
    # Effective sample size for several covariate values (randomly selected) in healthy and diseased populations 
      # Nondiseased
        op <- par(mfrow = c(1,3))
        for(i in 1:3) {
          plot(cROC_bnp$dens$h$grid, effectiveSize(t(cROC_bnp$dens$h$dens[,,cov_h_den[i]])), 
            type = "l", 
            xlab = "BMI", 
            ylab = "Effective Sample Size (density)", 
            main = paste0("Nondiseased population \n  Age = ", cROC_bnp$newdata$age[cov_h_den[i]], " - Gender = ", cROC_bnp$newdata$gender[cov_h_den[i]]),
            cex.main = 1.2, cex.lab = 1.5, cex.axis = 1.5)
        }
        par(op)
        
      # Diseased
        op <- par(mfrow = c(1,3))
        for(i in 1:3) {
          plot(cROC_bnp$dens$d$grid, effectiveSize(t(cROC_bnp$dens$d$dens[,,cov_d_den[i]])), 
            type = "l", 
            xlab = "BMI", 
            ylab = "Effective Sample Size (density)", 
            main = paste0("Diseased population \n  Age = ", cROC_bnp$newdata$age[cov_d_den[i]], " - Gender = ", cROC_bnp$newdata$gender[cov_d_den[i]]),
            cex.main = 1.2, cex.lab = 1.5, cex.axis = 1.5)
        }
        par(op)
        
      
    # Geweke statistics for the same covariate values as the ESS
      # Nondiseased
        op <- par(mfrow = c(1,3))
        for(i in 1:3) {
          plot(cROC_bnp$dens$h$grid, geweke.diag(t(cROC_bnp$dens$h$dens[,,cov_h_den[i]]))$z, 
            type = "l", 
            xlab = "BMI", 
            ylab = "Geweke Statistic (density)", 
            main = paste0("Nondiseased population \n  Age = ", cROC_bnp$newdata$age[cov_h_den[i]], " - Gender = ", cROC_bnp$newdata$gender[cov_h_den[i]]),
            cex.main = 1.2, cex.lab = 1.5, cex.axis = 1.5)
        }
        par(op)
              
      # Diseased
        op <- par(mfrow = c(1,3))
        for(i in 1:3) {
          plot(cROC_bnp$dens$d$grid, geweke.diag(t(cROC_bnp$dens$d$dens[,,cov_d_den[i]]))$z, 
            type = "l", 
            xlab = "BMI", 
            ylab = "Geweke Statistic (density)", 
            main = paste0("Diseased population \n  Age = ", cROC_bnp$newdata$age[cov_d_den[i]], " - Gender = ", cROC_bnp$newdata$gender[cov_d_den[i]]),
            cex.main = 1.2, cex.lab = 1.5, cex.axis = 1.5)
        }
        par(op)
        

  ###################################################################
  # Covariate-specific threshold values for a fixed FPF
  # cROC_bnp object
  # Note: Time consuming
  ###################################################################
    start <- proc.time()[3]
    th_fpf_cROC_bnp <- compute.threshold.cROC(cROC_bnp, criterion = "FPF", FPF = 0.3, newdata = endopred,
      ci.level = 0.95, parallel = "snow", ncpus = 2)
    names(th_fpf_cROC_bnp)
    end <- proc.time()[3]
    c_time_th_fpf_cROC_bnp  <- end - start
    
    # FPF-based threshold values
      df <- data.frame(age = th_fpf_cROC_bnp$newdata$age,
                       gender = th_fpf_cROC_bnp$newdata$gender,  
                        y = th_fpf_cROC_bnp$thresholds[[1]][,"est"], 
                       ql = th_fpf_cROC_bnp$thresholds[[1]][,"ql"],  
                       qh = th_fpf_cROC_bnp$thresholds[[1]][,"qh"]) 
          
      g0 <- ggplot(df, aes(x = age, y = y, ymin = ql, ymax = qh)) +
      geom_line() + 
      geom_ribbon(alpha = 0.2) + 
      labs(title = "Covariate-specific thresholds for a FPF = 0.3", x = "Age (years)", y = "BMI") +
      theme_bw() + 
      theme(strip.text.x = element_text(size = 20), 
            plot.title = element_text(hjust = 0.5, size = 20), 
            axis.text = element_text(size = 20), 
            axis.title = element_text(size = 20)) +
      facet_wrap(~gender)
      g0
    
    # Associated TPF
      df <- data.frame(age = th_fpf_cROC_bnp$newdata$age,
                       gender = th_fpf_cROC_bnp$newdata$gender,  
                        y = th_fpf_cROC_bnp$TPF[[1]][,"est"], 
                       ql = th_fpf_cROC_bnp$TPF[[1]][,"ql"],  
                       qh = th_fpf_cROC_bnp$TPF[[1]][,"qh"]) 
          
      g1 <- ggplot(df, aes(x = age, y = y, ymin = ql, ymax = qh)) +
      geom_line() + 
      geom_ribbon(alpha = 0.2) + 
      labs(title = "TPF attached to the thresholds for a FPF = 0.3", x = "Age (years)", y = "TPF") +
      theme_bw() + 
      theme(strip.text.x = element_text(size = 20), 
            plot.title = element_text(hjust = 0.5, size = 20), 
            axis.text = element_text(size = 20), 
            axis.title = element_text(size = 20)) +
      facet_wrap(~gender)
      g1
      
  ############################################################
  # Quantile residuals (Supplementary Material C.2)
  # Note: Very time consuming
  ############################################################        
      ######################################################################    
      # cROC_bp object 
      ######################################################################
        # Nondiseased
          cdf_h_bp <- quantile_residuals_h_bp <- matrix(0, nrow = length(cROC_bp$data_model$y$h), ncol = cROC_bp$mcmc$nsave)
          for(l in 1:cROC_bp$mcmc$nsave){
            mu <- cROC_bp$data_model$X$h%*%cROC_bp$fit$h$beta[l,]
            cdf_h_bp[,l] <- pnorm(cROC_bp$data_model$y$h, mean = mu, sd = cROC_bp$fit$h$sd[l])            
            quantile_residuals_h_bp[, l] <- quantile(qnorm(cdf_h_bp[,l]), ppoints(length(cROC_bp$data_model$y$h)), type = 2)
          }
          
          trajhat.h.m_bp <- apply(quantile_residuals_h_bp, 1, mean)
          trajhat.h.l_bp <- apply(quantile_residuals_h_bp, 1, quantile, prob = 0.025)
          trajhat.h.u_bp <- apply(quantile_residuals_h_bp, 1, quantile, prob = 0.975)
          
          grid <- qnorm(ppoints(length(cROC_bp$data_model$y$h)))
          
          op <- par(pty = "s")
          plot(grid, trajhat.h.m_bp, 
               xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
               main = "Nondiseased population",
               cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          lines(grid, trajhat.h.l_bp, lty = 2 , lwd = 2)
          lines(grid, trajhat.h.u_bp, lty = 2 , lwd = 2)
          abline(a = 0, b = 1, col = "red", lwd = 2)
          par(op)
          
      
      # Diseased
        cdf_d_bp <- quantile_residuals_d_bp <- matrix(0, nrow = length(cROC_bp$data_model$y$d), ncol = cROC_bp$mcmc$nsave)
        for(l in 1:cROC_bp$mcmc$nsave){
          mu <- cROC_bp$data_model$X$d%*%cROC_bp$fit$d$beta[l,]
          cdf_d_bp[,l] <- pnorm(cROC_bp$data_model$y$d, mean = mu, sd = cROC_bp$fit$d$sd[l])
          quantile_residuals_d_bp[,l] <- quantile(qnorm(cdf_d_bp[,l]), ppoints(length(cROC_bp$data_model$y$d)), type = 2)
        }
        
        trajhat.d.m_bp <- apply(quantile_residuals_d_bp, 1, mean)
        trajhat.d.l_bp <- apply(quantile_residuals_d_bp, 1, quantile, prob = 0.025)
        trajhat.d.u_bp <- apply(quantile_residuals_d_bp, 1, quantile, prob = 0.975)
        
        grid <- qnorm(ppoints(length(cROC_bp$data_model$y$d)))
        
        op <- par(pty = "s")
        plot(grid, trajhat.d.m_bp, 
             xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
             main = "Diseased population",
             cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
        lines(grid, trajhat.d.l_bp, lty = 2 , lwd = 2)
        lines(grid, trajhat.d.u_bp, lty = 2 , lwd = 2)
        abline(a = 0, b = 1, col = "red", lwd = 2)
        par(op)

      ######################################################################    
      # cROC_bnp object 
      ######################################################################    
        # Nondiseased
          cdf_h <- quantile_residuals_h <- matrix(0, nrow = length(cROC_bnp$data_model$y$h), ncol = cROC_bnp$mcmc$nsave)
          for(l in 1:cROC_bnp$mcmc$nsave){
            mu <- cROC_bnp$data_model$X$h%*%t(cROC_bnp$fit$h$beta[l,,])
            for(i in 1:length(cROC_bnp$data_model$y$h)){
              aux <- norMix(mu = mu[i,], sigma =  cROC_bnp$fit$h$sd[l,], w = cROC_bnp$fit$h$probs[l,])
              cdf_h[i, l] <- pnorMix(cROC_bnp$data_model$y$h[i], aux)
            }
            quantile_residuals_h[, l] <- quantile(qnorm(cdf_h[,l]), ppoints(length(cROC_bnp$data_model$y$h)), type = 2)
          }
          
          trajhat.h.m <- apply(quantile_residuals_h, 1, mean)
          trajhat.h.l <- apply(quantile_residuals_h, 1, quantile, prob = 0.025)
          trajhat.h.u <- apply(quantile_residuals_h, 1, quantile, prob = 0.975)
          
          grid <- qnorm(ppoints(length(cROC_bnp$data_model$y$h)))
          
          op <- par(pty = "s")
          plot(grid, trajhat.h.m, 
               xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
               main = "Nondiseased population",
               cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          lines(grid, trajhat.h.l, lty = 2 , lwd = 2)
          lines(grid, trajhat.h.u, lty = 2 , lwd = 2)
          abline(a = 0, b = 1, col = "red", lwd = 2)
          par(op)
          
        # Diseased
          cdf_d <- quantile_residuals_d <- matrix(0, nrow = length(cROC_bnp$data_model$y$d), ncol = cROC_bnp$mcmc$nsave)
          for(l in 1:cROC_bnp$mcmc$nsave){
            mu <- cROC_bnp$data_model$X$d%*%t(cROC_bnp$fit$d$beta[l,,])
            for(i in 1:length(cROC_bnp$data_model$y$d)){
              aux <- norMix(mu = mu[i,], sigma =  cROC_bnp$fit$d$sd[l,], w = cROC_bnp$fit$d$probs[l,])
              cdf_d[i,l] <- pnorMix(cROC_bnp$data_model$y$d[i], aux)
            }
            quantile_residuals_d[, l] <- quantile(qnorm(cdf_d[,l]), ppoints(length(cROC_bnp$data_model$y$d)), type = 2)
          }
          
          trajhat.d.m <- apply(quantile_residuals_d, 1, mean)
          trajhat.d.l <- apply(quantile_residuals_d, 1, quantile, prob = 0.025)
          trajhat.d.u <- apply(quantile_residuals_d, 1, quantile, prob = 0.975)
          
          grid <- qnorm(ppoints(length(cROC_bnp$data_model$y$d)))
          
          op <- par(pty = "s")
          plot(grid, trajhat.d.m,
               xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
               main = "Diseased population",
               cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
          lines(grid, trajhat.d.l, lty = 2 , lwd = 2)
          lines(grid, trajhat.d.u, lty = 2 , lwd = 2)
          abline(a = 0, b = 1, col = "red", lwd = 2)
          par(op)
        
  ######################################################################
  # Conditional histograms and densities (Supplementary Material C.2)
  # cROC_bnp object
  # NOTE: the code is specific for our application but it gives the idea on how
  #       conditional histograms and densities can be obtained
  ######################################################################
    plot.cden.hist <- function(object, group = c("Nondiseased", "Diseased"), age, gender, range_hist, xlab = NULL, title = NULL) {
      group <- match.arg(group)
      if(group == "Nondiseased") {
          denm <- apply(object$dens$h$dens, c(1,3), mean)
          denl <- apply(object$dens$h$dens, c(1,3), quantile, 0.025)
          denh <- apply(object$dens$h$dens, c(1,3), quantile, 0.975)
          grid <- object$dens$h$grid
          group.ind <- 0
          title.aux <- "Nondiseased Population"
      } else {
          denm <- apply(object$dens$d$dens, c(1,3), mean)
          denl <- apply(object$dens$d$dens, c(1,3), quantile, 0.025)
          denh <- apply(object$dens$d$dens, c(1,3), quantile, 0.975)
          grid <- object$dens$d$grid
          group.ind <- 1
          title.aux <- "Diseased Population"
      }
      pos <- object$newdata$age == age & object$newdata$gender == gender
      if(length(range_hist) == 2) {
         pos_data <- object$data$age >= range_hist[1] & object$data$age < range_hist[2] & object$data$gender == gender & object$data[,object$group] == group.ind
         xlab.aux <- paste0("BMI (", range_hist[1], " <= Age < ",  range_hist[2], ")")
      } else {
        if(range_hist > age) {
           pos_data <- object$data$age < range_hist & object$data$gender == gender & object$data[,object$group] == group.ind
           xlab.aux <- paste0("BMI (Age < ",  range_hist, ")")
        } else {
           pos_data <- object$data$age > range_hist & object$data$gender == gender & object$data[,object$group] == group.ind
           xlab.aux <- paste0("BMI (Age > ",  range_hist, ")")
        }
      }
     
      df_dens <- data.frame(dm = denm[, pos], dl = denl[, pos], dh = denh[, pos], seqbmi = grid)
      df_hist <- data.frame(var1 = object$data[pos_data, object$marker])

      if(is.null(title)) {
        title <- paste0(title.aux, ", Gender: ", gender, ", Age: ", age)
      }
      if(is.null(xlab)) {
        xlab <- xlab.aux
      }

      g <- ggplot(df_dens, aes(x = seqbmi, y = dm)) + 
          geom_line(size = 2) +
          geom_ribbon(data = df_dens, aes(x = seqbmi, ymin = dl, ymax = dh), alpha = 0.3, fill = "dodgerblue1") +
          theme_bw() +
          xlab(xlab) +
          ylab("Density") +
          geom_histogram(data = df_hist, aes(x = var1, y=..density..), alpha = 0.2, bins = 25, inherit.aes = FALSE) +
          theme(axis.text = element_text(size = 15), axis.title.x = element_text(size=20),  axis.title.y = element_text(size=20)) + 
          ggtitle(title) + 
          theme(plot.title = element_text(, size = 20, hjust = 0.5)) 

      g
    }

    g1 <- plot.cden.hist(cROC_bnp, group = "Nondiseased", age = 30, gender = "Women", range_hist = 35)
    g2 <- plot.cden.hist(cROC_bnp, group = "Nondiseased", age = 40, gender = "Women", range_hist = c(35,45))
    g3 <- plot.cden.hist(cROC_bnp, group = "Nondiseased", age = 50, gender = "Women", range_hist = 45)

    g4 <- plot.cden.hist(cROC_bnp, group = "Nondiseased", age = 30, gender = "Men", range_hist = 35)
    g5 <- plot.cden.hist(cROC_bnp, group = "Nondiseased", age = 40, gender = "Men", range_hist = c(35,45))
    g6 <- plot.cden.hist(cROC_bnp, group = "Nondiseased", age = 50, gender = "Men", range_hist = 45)

    g7 <- plot.cden.hist(cROC_bnp, group = "Diseased", age = 30, gender = "Women", range_hist = 35)
    g8 <- plot.cden.hist(cROC_bnp, group = "Diseased", age = 40, gender = "Women", range_hist = c(35,45))
    g9 <- plot.cden.hist(cROC_bnp, group = "Diseased", age = 50, gender = "Women", range_hist = 45)

    g10 <- plot.cden.hist(cROC_bnp, group = "Diseased", age = 30, gender = "Men", range_hist = 35)
    g11 <- plot.cden.hist(cROC_bnp, group = "Diseased", age = 40, gender = "Men", range_hist = c(35,45))
    g12 <- plot.cden.hist(cROC_bnp, group = "Diseased", age = 50, gender = "Men", range_hist = 45)

    # Nondiseased
      grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 3)

    # Diseased
      grid.arrange(g7, g8, g9, g10, g11, g12, ncol = 3)

  #####################################################################
  # Induced (semiparametric) linear model (Supplementary Material C.3)
  #####################################################################
    set.seed(123, "L'Ecuyer-CMRG")
    start <- proc.time()[3]
    cROC_sp <- cROC.sp(formula.h = bmi ~ gender*age,
      formula.d = bmi ~ gender*age,
      group = "cvd_idf", 
      tag.h = 0,
      data = endosyn,
      newdata = endopred, 
      est.cdf = "normal", 
      p = seq(0, 1, l = 101),
      B = 500,
      ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_cROC_sp  <- end - start
    summary(cROC_sp)
    
    op <- par(mfrow = c(2,2))
    plot(cROC_sp, ask = FALSE)
    par(op)
  
  ####################################################################################
  # Kernel-based approach: separately in men and women (Supplementary Material C.3)
  # Note: Time consuming
  ####################################################################################
    # Data frame for predictions
    agep <- seq(22, 80, l = 30)
    endopred_ker <- data.frame(age = agep)

    # Men
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    cROC_kernel_men <- cROC.kernel(marker = "bmi",
      covariate = "age",
      group = "cvd_idf", 
      tag.h = 0,
      data = subset(endosyn, gender == "Men"),
      newdata = endopred_ker, 
      p = seq(0, 1, l = 101),
      B = 500, # Bootstrap resamples
      ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_cROC_kernel_men  <- end - start
    summary(cROC_kernel_men)

    # Women
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    cROC_kernel_women <- cROC.kernel(marker = "bmi",
      covariate = "age",
      group = "cvd_idf", 
      tag.h = 0,
      data = subset(endosyn, gender == "Women"),
      newdata = endopred_ker, 
      p = seq(0, 1, l = 101),
      B = 500, # Bootstrap resamples
      ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_cROC_kernel_women  <- end - start
    summary(cROC_kernel_women)

    op <- par(mfcol = c(2,2))
    plot(cROC_kernel_women, ask = FALSE)
    plot(cROC_kernel_men, ask = FALSE)
    par(op)
    
  #################################################################################
  # Save objects (if desired)
  #################################################################################
  # save(th_fpf_cROC_bnp, quantile_residuals_h, quantile_residuals_d, cROC_bp, cROC_bnp, cROC_sp, cROC_kernel_men, cROC_kernel_women, file = "cROC.RData")
  
############################################################################################################################################################
############################################################################################################################################################
# # Package presentation and illustration: Covariate-adjusted ROC curve
############################################################################################################################################################
############################################################################################################################################################
  
  ###################################################################
  # Bayesian approach with L = 10 and nonlinear effect of age
  ###################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    AROC_bnp <- AROC.bnp(formula.h = bmi ~ gender + f(age, by = gender, K = c(0,0)),
      group = "cvd_idf", 
      tag.h = 0,
      data = endosyn,
      standardise = TRUE, 
      p = seq(0, 1, l = 101),
      ci.level = 0.95,
      compute.lpml = TRUE, compute.WAIC = TRUE, compute.DIC = TRUE, 
      pauc = pauccontrol(compute = FALSE),
      prior.h = priorcontrol.bnp(L = 10),
      density = densitycontrol.aroc(compute = FALSE),
      mcmc = mcmccontrol(nsave = 8000, nburn = 2000, nskip = 1),
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_AROC_bnp  <- end - start
    summary(AROC_bnp)

    plot(AROC_bnp, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.4)
  
  ###################################################################    
  # Compare pooled and AROC
  ###################################################################
    plot(AROC_bnp$p, AROC_bnp$ROC[,1], 
      type = "l", xlim = c(0,1), ylim = c(0,1),
      xlab = "FPF", ylab = "TPF", 
      main = "Pooled ROC curve vs AROC curve", 
      cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
      lines(AROC_bnp$p, AROC_bnp$ROC[,2], col = 1, lty = 2)
      lines(AROC_bnp$p, AROC_bnp$ROC[,3], col = 1, lty = 2)
      lines(pROC_dpm$p, pROC_dpm$ROC[,1], col = 2)
      lines(pROC_dpm$p, pROC_dpm$ROC[,2], col = 2, lty = 2)
      lines(pROC_dpm$p, pROC_dpm$ROC[,3], col = 2, lty = 2)
      abline(0, 1, col = "grey", lty = 2)
      

  ######################################################################
  # Induced (semiparametric) linear model (Supplementary Material D.1)
  ######################################################################
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    AROC_sp <- AROC.sp(formula.h = bmi ~ gender*age,
      group = "cvd_idf", 
      tag.h = 0,
      data = endosyn,
      est.cdf = "normal", 
      p = seq(0, 1, l = 101),
      B = 500,
      ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_AROC_sp  <- end - start
    summary(AROC_sp)
    
    plot(AROC_sp, cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.4)
    
  ##################################################################################
  # Kernel-based approach: separately in men and women (Supplementary Material D.1)
  # Note: Time consuming
  ##################################################################################
    # Men
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    AROC_kernel_men <- AROC.kernel(marker = "bmi",
      covariate = "age",
      group = "cvd_idf", 
      tag.h = 0,
      data = subset(endosyn, gender == "Men"),
      p = seq(0, 1, l = 101),
      B = 500, # Bootstrap resamples
      ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_AROC_kernel_men  <- end - start
    summary(AROC_kernel_men)

    # Women
    set.seed(123, "L'Ecuyer-CMRG") # for reproducibility
    start <- proc.time()[3]
    AROC_kernel_women <- AROC.kernel(marker = "bmi",
      covariate = "age",
      group = "cvd_idf", 
      tag.h = 0,
      data = subset(endosyn, gender == "Women"),
      p = seq(0, 1, l = 101),
      B = 500, # Bootstrap resamples
      ci.level = 0.95,
      parallel = "snow", ncpus = 2)
    end <- proc.time()[3]
    c_time_AROC_kernel_women  <- end - start
    summary(AROC_kernel_women)

    op <- par(mfcol = c(1,2))
    plot(AROC_kernel_women, main = "AROC kernel \n Women", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.7)
    plot(AROC_kernel_men, main = "AROC kernel \n Men", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.7)
    par(op)
    
  #################################################################################
  # Save objects (if desired)
  #################################################################################
  # save(AROC_bnp, AROC_sp, AROC_kernel_men, AROC_kernel_women, file = "AROC.RData")