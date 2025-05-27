rm(list=ls())
library(SNSeg)
library(changepoint)
library(ecp)
library(mosum)
library(phyclust)
library(foreach)


##########################
### Code for Example 1 ###
##########################

# Generate model (V1)
set.seed(7)
ts <- MAR_Variance(reptime = 1, type = "V1") # generate model (V1)
par(mfcol = c(2, 1), mar = c(4, 2.5, 2.5, 0.5))

# SNCP in the change of variance
result1 <- SNSeg_Uni(ts, paras_to_test = "variance", confidence = 0.9,
                     grid_size_scale = 0.05, grid_size = NULL, plot_SN = TRUE,
                     est_cp_loc = TRUE)

# Segmentation plot for SN-based test statistics
SNstat1 <- max_SNsweep(result1, plot_SN = TRUE, est_cp_loc = TRUE, critical_loc = TRUE)

# Output
result1$est_cp
result1$grid_size
result1$critical_value

# S3 method: print
print(result1)
# S3 method: summary
summary(result1)

# parameter estimates for each segment
SNSeg_estimate(SN_result = result1)

# SNCP in the change of variance with a different grid_size and confidence level
result2 <- SNSeg_Uni(ts, paras_to_test = "variance", confidence = 0.9,
                     grid_size_scale = 0.05, grid_size = 102, plot_SN = FALSE,
                     est_cp_loc = FALSE)
# Output
result2$est_cp
result2$grid_size
result2$critical_value

##########################
### Code for Example 2 ###
##########################

############# 1. Example of user-defined parameter ###############
# Generate model (V1)
set.seed(7)
ts <- MAR_Variance(reptime = 1, type = "V1") # generate model (V1)
# define a function for paras_to_test
# change in 2nd moment
second_moment <- function(ts){
  result <- mean(ts^2)
  return(result)
}
start.time <- Sys.time()
result.general <- SNSeg_Uni(ts, paras_to_test = second_moment, confidence = 0.9,
                            grid_size_scale = 0.05, grid_size = NULL,
                            plot_SN = FALSE, est_cp_loc = TRUE)
end.time <- Sys.time()

# Output
as.numeric(difftime(end.time,start.time)) # execution time (in minutes)
result.general$est_cp # change-point estimates

############ 2. Comparison of built-in variance and user-defined variance #######
# bulit-in variance
start.time <- Sys.time()
result1 <- SNSeg_Uni(ts, paras_to_test = "variance", confidence = 0.9,
                     grid_size_scale = 0.05, grid_size = NULL, plot_SN = TRUE,
                     est_cp_loc = TRUE)
end.time <- Sys.time()
difftime(end.time,start.time) # built-in parameter time

# user defined variance
paras_to_test <- function(ts){
  var(ts)
}
start.time <- Sys.time()
result.general <- SNSeg_Uni(ts, paras_to_test = paras_to_test, confidence = 0.9,
                            grid_size_scale = 0.05, grid_size = NULL,
                            plot_SN = FALSE, est_cp_loc = TRUE)
end.time <- Sys.time()
difftime(end.time,start.time) # general functional parameter time
c(result1$est_cp,result.general$est_cp)



##########################
### Code for Example 3 ###
##########################

# Generate model (MP1)
set.seed(7)
require(truncnorm)
require(evd)

mix_GauGPD <- function(u, p, trunc_r, gpd_scale, gpd_shape) {
  # function for generating a mixture of truncated normal + GPD
  indicator <- (u < p)
  rv <- rep(0, length(u))
  rv[indicator > 0] <- qtruncnorm(u[indicator > 0] / p, a = -Inf, b = trunc_r)
  rv[indicator <= 0] <- qgpd((u[indicator <= 0] - p) / (1 - p), loc = trunc_r,
                             scale = gpd_scale, shape = gpd_shape)
  return(rv)
}

n <- 1000
cp_sets <- c(0, 333, 667, 1000)
rho <- 0.2
ts <- MAR(n, 1, rho) * sqrt(1 - rho ^ 2) # generate AR(1)
trunc_r <- 0
p <- pnorm(trunc_r)
gpd_scale <- 2
gpd_shape <- 0.125

ts[(cp_sets[2] + 1):cp_sets[3]] <-
  mix_GauGPD(u = pnorm(ts[(cp_sets[2] + 1):cp_sets[3]]), p, trunc_r, gpd_scale, gpd_shape)

# SNCP in the change of 90% quantile
result_q9 <- SNSeg_Uni(ts, paras_to_test = c(0.9), confidence = 0.9,
                       grid_size_scale = 0.1, plot_SN = FALSE, est_cp_loc = FALSE)
# Output
result_q9$est_cp
result_q9$grid_size
result_q9$critical_value


# SNCP in the change of variance
result_v <- SNSeg_Uni(ts, paras_to_test = c('variance'), confidence = 0.9,
                      grid_size_scale = 0.1, plot_SN = FALSE, est_cp_loc = FALSE)
# Output
result_v$est_cp
result_v$grid_size
result_v$critical_value


# SNCP in the change of variance and 90% quantile
result_q9v <- SNSeg_Uni(ts, paras_to_test = c(0.9, 'variance'), confidence = 0.9,
                        grid_size_scale = 0.1, plot_SN = FALSE, est_cp_loc = FALSE)
# Output
result_q9v$est_cp
result_q9v$grid_size
result_q9v$critical_value



##########################
### Code for Example 4 ###
##########################
# Generate model (M2)
set.seed(7)
d <- 5
n <- 1000
cp_sets <- c(0, 75, 375, 425, 525, 575, 1000)
mean_shift <- c(-3, 0, 3, 0, -3, 0) / sqrt(d)
rho_sets <- 0.5
sigma_cross <- list(diag(d))
ts <- MAR_MTS_Covariance(n, 1, rho_sets, cp_sets = c(0, n), sigma_cross)[[1]] # generate VAR(1)
no_seg <- length(cp_sets) - 1

for (index in 1:no_seg) { # Mean shift
  tau1 <- cp_sets[index] + 1
  tau2 <- cp_sets[index + 1]
  ts[, tau1:tau2] <- ts[, tau1:tau2] + mean_shift[index]
}

# SNCP in the change of multivariate mean
result_multimean <- SNSeg_Multi(ts, paras_to_test = "mean", confidence = 0.9,
                                grid_size_scale = 0.05, plot_SN = TRUE,
                                est_cp_loc = TRUE)

# Output
result_multimean$est_cp
result_multimean$grid_size
result_multimean$critical_value

plot(ts[1, ], main = 'SN Segmentation Plot for the First Time Series')
abline(v = result_multimean$est_cp, col = 'red')
SNstat_multimean <- max_SNsweep(result_multimean, plot_SN = TRUE, est_cp_loc = TRUE, critical_loc = TRUE)



##########################
### Code for Example 5 ###
##########################

# Generate model (HD)
set.seed(7)
p <- 100
n <- 600
cp_sets <- c(0, 100, 200, 300, 400, 500, 600)
mean_shift <- c(0, sqrt(4 / 5), 0, sqrt(4 / 5), 0, sqrt(4 / 5))
ts <- matrix(rnorm(n * p, 0, 1), n, p)
no_seg <- length(cp_sets) - 1
for (index in 1:no_seg) { # Mean shift
  tau1 <- cp_sets[index] + 1
  tau2 <- cp_sets[index + 1]
  ts[tau1:tau2, 1:5] <- ts[tau1:tau2, 1:5] + mean_shift[index]
}

# SNHD for high-dimensional means
par(mfrow = c(2,2))
result_hd <- SNSeg_HD(ts, confidence = 0.9, grid_size_scale = 0.05,
                      plot_SN = TRUE, est_cp_loc = TRUE,
                      ts_index = c(1:4))

# Output
result_hd$est_cp


##########################
### Code for Section 4 ###
##########################
library(SNSeg)
library(changepoint)
library(ecp)
library(mosum)
library(phyclust)
library(foreach)

################################################
### Functions for Hausdorff distance and ARI ###
################################################

# calculate d1, d2, and (dH = max(d1, d2))
Haus_dist <- function(est_cp, true_cp, n){
  if(length(true_cp)==0){
    if(length(est_cp)==0||is.null(est_cp)){
      return(c(dist1=0, dist2=0))
    }
    return(c(dist1=n/2, dist2=0))
  }
  if(length(est_cp)==0||is.null(est_cp)){
    return(c(dist1=0, dist2=n/2))
  }
  dist1 <- dist2 <- 0
  for(cp in est_cp){ # measure error by over-estimation of change-point
    current_dist1 <- min(abs(true_cp-cp))
    dist1 <- max(dist1, current_dist1)
  }
  for(cp in true_cp){ # measure error by under-estimation of change-point
    current_dist2 <- min(abs(est_cp-cp))
    dist2 <- max(dist2, current_dist2)
  }
  return(c(dist1,dist2))
}

# Calculate the Rand index and Adjusted Rand Index (ARI)
rand_index <- function(est_cp, true_cp){
  no_segments <- length(true_cp)-1
  group1 <- rep(1:no_segments, times=diff(true_cp)) # true cluster
  group0 <- rep(1,n) # null cluster
  
  rand_index_null <- RRand(group0,group1)$Rand # when no change-point is detected
  adj_rand_index_null <- RRand(group0,group1)$adjRand # when no change-point is detected
  
  if(is.null(est_cp)){
    rand_index <- rand_index_null
    adj_rand_index <- adj_rand_index_null
  }else{
    est_cp <- c(0, est_cp, n)
    no_segments <- length(est_cp)-1
    group2 <- rep(1:no_segments, times=diff(est_cp)) # true cluster
    tmp_rand_index <- RRand(group1, group2)
    rand_index <- tmp_rand_index$Rand # when one change-point is detected
    adj_rand_index <- tmp_rand_index$adjRand # when one change-point is detected
  }
  return(list(rand_index=rand_index, adj_rand_index=adj_rand_index))
}

##########################
###  Code for Table 1  ###
##########################

# sensitivity analysis
reptime <- 100
confidence_list <- c(0.9,0.95)
q_list <- c(0.05,0.08,0.1,0.12,0.15)
delta_list <- c(sqrt(3),sqrt(6))

for(delta in delta_list){
  d1_list <- d2_list <- dh_list <- ari_list <- t_list <- list()
  n_list <- list()

  set.seed(7)
  n <- 1200
  nocp <- 7
  cp_sets <- round(seq(0,nocp+1,1)/(nocp+1)*n)
  cp_sets2 <- cp_sets[-c(1,length(cp_sets))] # true change points in the middle
  mean_shift <- rep(c(0,delta),100)[1:(length(cp_sets)-1)]
  rho <- 0.5
  # to run the code for 1000 repetition, please set reptime=1000
  ts <- MAR(n, reptime=reptime, rho) # AR time series with no change-point
  ts_all <- ts*sqrt(1-rho^2)
  no_seg <- length(cp_sets)-1
  for(index in 1:no_seg){ # Mean shift
    tau1 <- cp_sets[index]+1
    tau2 <- cp_sets[index+1]
    ts_all[tau1:tau2,] <- ts_all[tau1:tau2,] + mean_shift[index]
  }
  d1_table <- d2_table <- dh_table <- ari_table <- t_table <- matrix(NA,nrow=length(confidence_list),ncol=length(q_list))
  current_count <- list()
  for(i in 1:length(confidence_list)){
    confidence <- confidence_list[i]
    for(j in 1:length(q_list)){
      d1_vec <- d2_vec <- dh_vec <- ari_vec <- t_vec <- n_vec <- c()
      grid_size_scale <- q_list[j]
      for(t in 1:reptime){
        ts <- ts_all[,t]
        start.time <- Sys.time()
        result.SNCP <- SNSeg_Uni(ts, paras_to_test = "mean", confidence = confidence,
                                 grid_size_scale = grid_size_scale, grid_size = NULL,
                                 plot_SN = F, est_cp_loc = TRUE)
        end.time <- Sys.time()
        cpts.sncp <- result.SNCP$est_cp
        # time
        time.sncp <- as.numeric(end.time-start.time,units="secs")
        t_vec <- c(t_vec,time.sncp)
        # d1,d2
        b_dist <- Haus_dist(cpts.sncp,cp_sets2,n)
        d1_vec <- c(d1_vec,b_dist[1]); d2_vec <- c(d2_vec,b_dist[2])
        # dH
        dh_vec <- c(dh_vec, max(b_dist))
        # ARI
        ari_vec <- c(ari_vec, rand_index(cpts.sncp,cp_sets)$adj_rand_index)
        # number of change points
        n_vec <- c(n_vec,length(cpts.sncp))
      }
      t_table[i,j] <- mean(t_vec)
      d1_table[i,j] <- mean(d1_vec)
      d2_table[i,j] <- mean(d2_vec)
      dh_table[i,j] <- mean(dh_vec)
      ari_table[i,j] <- mean(ari_vec)
      current_count[[5*(i-1)+j]] <- table(n_vec)
    }
  }
  index <- which(delta_list==delta)
  t_list[[index]] <- t_table
  d1_list[[index]] <- d1_table
  d2_list[[index]] <- d2_table
  dh_list[[index]] <- dh_table
  ari_list[[index]] <- ari_table
  n_list[[index]] <- current_count
  if(delta == sqrt(3)){
    result_delta1 <- list(d1=d1_list, d2=d2_list, dh=dh_list, ari=ari_list, t=t_list, counts=n_list)
  }else{
    result_delta2 <- list(d1=d1_list, d2=d2_list, dh=dh_list, ari=ari_list, t=t_list, counts=n_list)
  }
}

result_delta1 # result for delta == sqrt(3)
result_delta2 # result for delta == sqrt(6)

##########################
###  Code for Table 2  ###
##########################

# Time comparison among different parameters
reptime <- 100

# model (V1)
set.seed(7)
ts1 <- MAR_Variance(reptime=reptime, "V1")

# model (MP1)
set.seed(7)
n <- 1000
cp_sets <- c(0,333,667,1000)
no_seg <- length(cp_sets)-1
rho <- 0
ts2 <- MAR(n, reptime=reptime, rho)*sqrt(1-rho^2) # AR time series with no change-point (mean, var)=(0,1)
no_seg <- length(cp_sets)-1
sd_shift <- c(1,1.6,1)
for(index in 1:no_seg){ # Mean shift
  tau1 <- cp_sets[index]+1
  tau2 <- cp_sets[index+1]
  ts2[tau1:tau2,] <- ts2[tau1:tau2,]*sd_shift[index]
}

parameter <- list('mean','variance','acf',0.9,c(0.9,'variance'))
t1 <- t2 <- matrix(NA, nrow=reptime, ncol=length(parameter))

for(i in 1:reptime){
  ts.1 <- ts1[,i]
  ts.2 <- ts2[,i]
  for(j in 1:length(parameter)){
    paras_to_test <- parameter[[j]]
    if(j ==5){
      start.time <- Sys.time()
      result <- SNSeg_Uni(ts.1, paras_to_test = paras_to_test,
                          confidence = 0.9, grid_size_scale = 0.05, grid_size = NULL,
                          plot_SN = FALSE)
      end.time <- Sys.time()
      t1[i,j] <- as.numeric(difftime(end.time,start.time,'secs'))
      start.time <- Sys.time()
      result <- SNSeg_Uni(ts.2, paras_to_test = paras_to_test,
                          confidence = 0.9, grid_size_scale = 0.05, grid_size = NULL,
                          plot_SN = FALSE)
      end.time <- Sys.time()
      t2[i,j] <- as.numeric(difftime(end.time,start.time,'secs'))
    }else{
      if(!(paras_to_test %in% c('BinSeg','PELT'))){
        start.time <- Sys.time()
        result <- SNSeg_Uni(ts.1, paras_to_test = paras_to_test,
                            confidence = 0.9, grid_size_scale = 0.05, grid_size = NULL,
                            plot_SN = FALSE)
        end.time <- Sys.time()
        t1[i,j] <- as.numeric(difftime(end.time,start.time,'secs'))
        start.time <- Sys.time()
        result <- SNSeg_Uni(ts.2, paras_to_test = paras_to_test,
                            confidence = 0.9, grid_size_scale = 0.05, grid_size = NULL,
                            plot_SN = FALSE)
        end.time <- Sys.time()
        t2[i,j] <- as.numeric(difftime(end.time,start.time,'secs'))
      }else{
        start.time <- Sys.time()
        result = cpt.var(ts.1,method=paras_to_test)
        end.time <- Sys.time()
        t1[i,j] <- as.numeric(difftime(end.time,start.time,'secs'))
        start.time <- Sys.time()
        result = cpt.var(ts.2,method=paras_to_test)
        end.time <- Sys.time()
        t2[i,j] <- as.numeric(difftime(end.time,start.time,'secs'))
      }
    }
  }
}
colMeans(t1) # time for model (V1)
colMeans(t2) # time for model (MP1)

##########################
###  Code for Table 3  ###
##########################

# number of change-points detected for the no-change-point scenario

set.seed(7)
reptime <- 100
n <- 1000
rho_list <- c(0,0.4,0.7)
t <- matrix(NA,5,length(rho_list))
count_result <- list()

for(rho in rho_list){
  set.seed(7)
  ts_all <- MAR(n, reptime=reptime, rho)
  res = foreach(i= 1:reptime, .packages = c("changepoint","SNSeg","ecp","mosum")) %dopar% {
    ts <- ts_all[,i]
    ts <- ts*sqrt(1-rho^2)
    # sncp
    start.time <- Sys.time()
    result.SNCP <- SNSeg_Uni(ts, paras_to_test = "mean", confidence = 0.9,
                             grid_size_scale = 0.05, grid_size = NULL, plot_SN = F,
                             est_cp_loc = TRUE)
    end.time <- Sys.time()
    time.sncp <- as.numeric(end.time-start.time,units="secs")
    cpts.sncp <- result.SNCP$est_cp;

    # pelt
    start.time <- Sys.time()
    result.pelt <- cpt.mean(ts,method="PELT")
    end.time <- Sys.time()
    time.pelt <-  as.numeric(end.time-start.time,units="secs")
    cpts.pelt <- result.pelt@cpts;
    cpts.pelt <- cpts.pelt[-length(cpts.pelt)]

    # bs
    start.time <- Sys.time()
    result.bs <- cpt.mean(ts,method="BinSeg")
    end.time <- Sys.time()
    time.bs <-  as.numeric(end.time-start.time,units="secs")
    cpts.bs <- result.bs@cpts;
    cpts.bs <- cpts.bs[-length(cpts.bs)]

    # mosum (G=100)
    start.time <- Sys.time()
    result.mosum <- mosum(ts,G=100)
    end.time <- Sys.time()
    time.mosum <-  as.numeric(end.time-start.time,units="secs")
    cpts.mosum <- result.mosum$cpts

    # ecp
    start.time <- Sys.time()
    result.ecp <- e.divisive(as.matrix(ts))
    end.time <- Sys.time()
    time.ecp <- as.numeric(end.time-start.time,units="secs")
    cpts.ecp <- result.ecp$estimates
    cpts.ecp <- cpts.ecp[-c(1,length(cpts.ecp))]

    list(time=c(time.sncp,time.pelt,time.bs,time.mosum,time.ecp),
         cpts.sncp=cpts.sncp,cpts.pelt=cpts.pelt,cpts.bs=cpts.bs,cpts.mosum=cpts.mosum,cpts.ecp=cpts.ecp)
  }
  n.sncp <- n.bs <- n.pelt <- n.mosum <- n.ecp <- rep(NA,reptime)
  d1.sncp <- d1.bs <- d1.pelt <- d1.mosum <- d1.ecp <- c()
  d2.sncp <- d2.bs <- d2.pelt <- d2.mosum <- d2.ecp <- c()
  dist.sncp <- dist.bs <- dist.pelt <- dist.mosum <- dist.ecp <- c()
  ari.sncp <- ari.bs <- ari.pelt <- ari.mosum <- ari.ecp <- c()
  time.sncp <- time.bs <- time.pelt <- time.mosum <-time.ecp <- c()
  method.time <- c()

  for (i in 1:reptime) {
    cpts.sncp = res[[i]]$cpts.sncp
    cpts.pelt = res[[i]]$cpts.pelt
    cpts.bs = res[[i]]$cpts.bs
    cpts.mosum = res[[i]]$cpts.mosum
    cpts.ecp = res[[i]]$cpts.ecp
    method.time = res[[i]]$time

    time.sncp <- c(time.sncp, method.time[1])
    n.sncp[i] <- length(cpts.sncp)

    time.pelt <- c(time.pelt, method.time[2])
    n.pelt[i] <- length(cpts.pelt)

    time.bs <- c(time.bs, method.time[3])
    n.bs[i] <- length(cpts.bs)

    time.mosum <- c(time.mosum, method.time[4])
    n.mosum[i] <- length(cpts.mosum)

    time.ecp <- c(time.ecp, method.time[5])
    n.ecp[i] <- length(cpts.ecp)
  }
  index <- which(rho_list==rho)
  t[,index] <- c(mean(time.sncp), mean(time.bs), mean(time.pelt), mean(time.mosum), mean(time.ecp))
  current_count <- list(n.sncp=table(n.sncp),n.bs=table(n.bs),n.pelt=table(n.pelt),n.mosum=table(n.mosum),
                        n.ecp=table(n.ecp))
  count_result[[index]] <- current_count
}

count_result

##########################
###  Code for Table 4  ###
##########################

# number of change-points detected for model (M) under rho = c(0,0.4,0.7)

set.seed(7)
reptime <- 100
n <- 1000
nocp <- 4
cp_sets <- round(seq(0,nocp+1,1)/(nocp+1)*n)
mean_shift <- rep(c(0,2),100)[1:(length(cp_sets)-1)]
no_seg <- length(cp_sets)-1
rho_list <- c(0,0.4,0.7)
cp_sets2 <- cp_sets[-c(1,length(cp_sets))] # true change points in the middle

d1 <- d2 <- dh <- ari <- t <- matrix(NA,5,length(rho_list))
count_result <- list()
for(rho in rho_list){
  set.seed(7)
  ts_all <- MAR(n, reptime=reptime, rho)
  ts_all <- ts_all*sqrt(1-rho^2)
  res = foreach(i= 1:reptime, .packages = c("changepoint","SNSeg","ecp","mosum")) %dopar% {
    ts <- ts_all[,i]
    for(index in 1:no_seg){ # Mean shift
      tau1 <- cp_sets[index]+1
      tau2 <- cp_sets[index+1]
      ts[tau1:tau2] <- ts[tau1:tau2] + mean_shift[index]
    }

    # sncp
    start.time <- Sys.time()
    result.SNCP <- SNSeg_Uni(ts, paras_to_test = "mean", confidence = 0.9,
                             grid_size_scale = 0.05, grid_size = NULL, plot_SN = F,
                             est_cp_loc = TRUE)
    end.time <- Sys.time()
    time.sncp <- as.numeric(end.time-start.time,units="secs")
    cpts.sncp <- result.SNCP$est_cp;

    # pelt
    start.time <- Sys.time()
    result.pelt <- cpt.mean(ts,method="PELT")
    end.time <- Sys.time()
    time.pelt <-  as.numeric(end.time-start.time,units="secs")
    cpts.pelt <- result.pelt@cpts;
    cpts.pelt <- cpts.pelt[-length(cpts.pelt)]

    # bs
    start.time <- Sys.time()
    result.bs <- cpt.mean(ts,method="BinSeg")
    # result.pelt <- cpt.var(ts,method="BinSeg")
    end.time <- Sys.time()
    time.bs <-  as.numeric(end.time-start.time,units="secs")
    cpts.bs <- result.bs@cpts;
    cpts.bs <- cpts.bs[-length(cpts.bs)]

    # mosum (G=100)
    start.time <- Sys.time()
    result.mosum <- mosum(ts,G=100)
    end.time <- Sys.time()
    time.mosum <-  as.numeric(end.time-start.time,units="secs")
    cpts.mosum <- result.mosum$cpts


    # ecp
    start.time <- Sys.time()
    result.ecp <- e.divisive(as.matrix(ts))
    end.time <- Sys.time()
    time.ecp <- as.numeric(end.time-start.time,units="secs")
    cpts.ecp <- result.ecp$estimates
    cpts.ecp <- cpts.ecp[-c(1,length(cpts.ecp))]

    list(time=c(time.sncp,time.pelt,time.bs,time.mosum,time.ecp),
         cpts.sncp=cpts.sncp,cpts.pelt=cpts.pelt,cpts.bs=cpts.bs,cpts.mosum=cpts.mosum,cpts.ecp=cpts.ecp)
  }
  n.sncp <- n.bs <- n.pelt <- n.mosum <- n.ecp <- rep(NA,reptime)
  d1.sncp <- d1.bs <- d1.pelt <- d1.mosum <- d1.ecp <- c()
  d2.sncp <- d2.bs <- d2.pelt <- d2.mosum <- d2.ecp <- c()
  dist.sncp <- dist.bs <- dist.pelt <- dist.mosum <- dist.ecp <- c()
  ari.sncp <- ari.bs <- ari.pelt <- ari.mosum <- ari.ecp <- c()
  time.sncp <- time.bs <- time.pelt <- time.mosum <-time.ecp <- c()
  method.time <- c()

  for (i in 1:reptime) {
    cpts.sncp = res[[i]]$cpts.sncp
    cpts.pelt = res[[i]]$cpts.pelt
    cpts.bs = res[[i]]$cpts.bs
    cpts.mosum = res[[i]]$cpts.mosum
    cpts.ecp = res[[i]]$cpts.ecp
    method.time = res[[i]]$time

    time.sncp <- c(time.sncp, method.time[1])
    n.sncp[i] <- length(cpts.sncp)
    b_dist <- Haus_dist(cpts.sncp,cp_sets2,n)
    d1.sncp <- c(d1.sncp,b_dist[1]); d2.sncp <- c(d2.sncp,b_dist[2])
    dist.sncp <- c(dist.sncp, max(b_dist))
    ari.sncp <- c(ari.sncp, rand_index(cpts.sncp,cp_sets)$adj_rand_index)

    time.pelt <- c(time.pelt, method.time[2])
    n.pelt[i] <- length(cpts.pelt)
    b_dist <- Haus_dist(cpts.pelt,cp_sets2,n)
    d1.pelt <- c(d1.pelt,b_dist[1]); d2.pelt <- c(d2.pelt,b_dist[2])
    dist.pelt <- c(dist.pelt, max(b_dist))
    ari.pelt <- c(ari.pelt, rand_index(cpts.pelt,cp_sets)$adj_rand_index)


    time.bs <- c(time.bs, method.time[3])
    n.bs[i] <- length(cpts.bs)
    b_dist <- Haus_dist(cpts.bs,cp_sets2,n)
    d1.bs <- c(d1.bs,b_dist[1]); d2.bs <- c(d2.bs,b_dist[2])
    dist.bs <- c(dist.bs, max(b_dist))
    ari.bs <- c(ari.bs, rand_index(cpts.bs,cp_sets)$adj_rand_index)


    time.mosum <- c(time.mosum, method.time[4])
    n.mosum[i] <- length(cpts.mosum)
    b_dist <- Haus_dist(cpts.mosum,cp_sets2,n)
    d1.mosum <- c(d1.mosum,b_dist[1]); d2.mosum <- c(d2.mosum,b_dist[2])
    dist.mosum <- c(dist.mosum, max(b_dist))
    ari.mosum <- c(ari.mosum, rand_index(cpts.mosum,cp_sets)$adj_rand_index)


    time.ecp <- c(time.ecp, method.time[5])
    n.ecp[i] <- length(cpts.ecp)
    b_dist <- Haus_dist(cpts.ecp,cp_sets2,n)
    d1.ecp <- c(d1.ecp,b_dist[1]); d2.ecp <- c(d2.ecp,b_dist[2])
    dist.ecp <- c(dist.ecp, max(b_dist))
    ari.ecp <- c(ari.ecp, rand_index(cpts.ecp,cp_sets)$adj_rand_index)

  }
  index <- which(rho_list==rho)
  d1[,index] <- c(mean(d1.sncp), mean(d1.bs) ,mean(d1.pelt) , mean(d1.mosum) , mean(d1.ecp))
  d2[,index] <- c(mean(d2.sncp), mean(d2.bs) ,mean(d2.pelt) , mean(d2.mosum) , mean(d2.ecp))
  dh[,index] <- c(mean(dist.sncp), mean(dist.bs), mean(dist.pelt), mean(dist.mosum), mean(dist.ecp))
  ari[,index] <- c(mean(ari.sncp), mean(ari.bs), mean(ari.pelt), mean(ari.mosum), mean(ari.ecp))
  t[,index] <- c(mean(time.sncp), mean(time.bs), mean(time.pelt), mean(time.mosum), mean(time.ecp))
  current_count <- list(n.sncp=table(n.sncp),n.bs=table(n.bs),n.pelt=table(n.pelt),n.mosum=table(n.mosum),
                        n.ecp=table(n.ecp))
  count_result[[index]] <- current_count
}

result.table4 <- list(d1=d1,d2=d2,dh=dh,ari=ari,t=t,count=count_result)
result.table4

##########################
###  Code for Table 5  ###
##########################

# compare single vs multiple parameters for model (M) using change in
# (1) single mean, (2) mean and variance (3) mean and 20% quantile

set.seed(7)
reptime <- 100
n <- 1000
rho <- 0.4
nocp <- 4
cp_sets <- round(seq(0,nocp+1,1)/(nocp+1)*n)
mean_shift <- rep(c(0,2),100)[1:(length(cp_sets)-1)]
no_seg <- length(cp_sets)-1
cp_sets2 <- cp_sets[-c(1,length(cp_sets))] # true change points in the middle

ts_all <- MAR(n, reptime, rho) # for change in mean

res = foreach(i= 1:reptime, .packages = c("SNSeg")) %dopar% {
  ts <- ts_all[,i]
  #ts <- MAR(n, reptime=1, rho) # for change in mean
  #ts <- ts*sqrt(1-rho^2)
  #
  for(index in 1:no_seg){ # Mean shift
    tau1 <- cp_sets[index]+1
    tau2 <- cp_sets[index+1]
    ts[tau1:tau2] <- ts[tau1:tau2] + mean_shift[index]
  }
  # sncp mean
  start.time <- Sys.time()
  result.mean <- SNSeg_Uni(ts, paras_to_test = "mean", confidence = 0.9,
                           grid_size_scale = 0.05, grid_size = NULL, plot_SN = F,
                           est_cp_loc = TRUE)
  end.time <- Sys.time()
  time.mean <- as.numeric(end.time-start.time,units="secs")
  cpts.mean <- result.mean$est_cp;

  # sncp mean and variance
  start.time <- Sys.time()
  result.multi <- SNSeg_Uni(ts, paras_to_test = c("mean",'variance'), confidence = 0.9,
                            grid_size_scale = 0.05, grid_size = NULL, plot_SN = F,
                            est_cp_loc = TRUE)
  end.time <- Sys.time()
  time.multi1 <- as.numeric(end.time-start.time,units="secs")
  cpts.multi1 <- result.multi$est_cp;

  start.time <- Sys.time()
  result.multi2 <- SNSeg_Uni(ts, paras_to_test = c("mean",0.2), confidence = 0.9,
                             grid_size_scale = 0.05, grid_size = NULL, plot_SN = F,
                             est_cp_loc = TRUE)
  end.time <- Sys.time()
  time.multi2 <- as.numeric(end.time-start.time,units="secs")
  cpts.multi2 <- result.multi2$est_cp;

  list(time=c(time.mean,time.multi1,time.multi2), cpts.mean=cpts.mean,cpts.multi1=cpts.multi1,cpts.multi2=cpts.multi2)
}
n.mean <- n.multi1 <- n.multi2 <- rep(NA,reptime)
d1.mean <- d1.multi1 <- d1.multi2 <- c()
d2.mean <- d2.multi1 <- d2.multi2 <- c()
dh.mean <- dh.multi1 <- dh.multi2 <- c()
ari.mean <- ari.multi1 <- ari.multi2 <- c()
time.mean <- time.multi1 <- time.multi2 <- c()
method.time <- c()

for (i in 1:reptime) {
  cpts.mean = res[[i]]$cpts.mean
  cpts.multi1 = res[[i]]$cpts.multi1
  cpts.multi2 = res[[i]]$cpts.multi2
  method.time = res[[i]]$time

  time.mean <- c(time.mean, method.time[1])
  n.mean[i] <- length(cpts.mean)
  b_dist <- Haus_dist(cpts.mean,cp_sets2,n)
  d1.mean <- c(d1.mean,b_dist[1]); d2.mean <- c(d2.mean,b_dist[2])
  dh.mean <- c(dh.mean, max(b_dist))
  ari.mean <- c(ari.mean, rand_index(cpts.mean,cp_sets)$adj_rand_index)

  time.multi1 <- c(time.multi1, method.time[2])
  n.multi1[i] <- length(cpts.multi1)
  b_dist <- Haus_dist(cpts.multi1,cp_sets2,n)
  d1.multi1 <- c(d1.multi1,b_dist[1]); d2.multi1 <- c(d2.multi1,b_dist[2])
  dh.multi1 <- c(dh.multi1, max(b_dist))
  ari.multi1 <- c(ari.multi1, rand_index(cpts.multi1,cp_sets)$adj_rand_index)

  time.multi2 <- c(time.multi2, method.time[3])
  n.multi2[i] <- length(cpts.multi2)
  b_dist <- Haus_dist(cpts.multi2,cp_sets2,n)
  d1.multi2 <- c(d1.multi2,b_dist[1]); d2.multi2 <- c(d2.multi2,b_dist[2])
  dh.multi2 <- c(dh.multi2, max(b_dist))
  ari.multi2 <- c(ari.multi2, rand_index(cpts.multi2,cp_sets)$adj_rand_index)
}
c(mean(d1.mean), mean(d1.multi1),mean(d1.multi2)) # d1
c(mean(d2.mean), mean(d2.multi1),mean(d2.multi2)) # d2
c(mean(dh.mean), mean(dh.multi1),mean(dh.multi2)) # dh
c(mean(ari.mean), mean(ari.multi1),mean(ari.multi2)) # ARI
c(mean(time.mean), mean(time.multi1),mean(time.multi2)) # time
# count result
table(n.mean)
table(n.multi1)
table(n.multi2)
