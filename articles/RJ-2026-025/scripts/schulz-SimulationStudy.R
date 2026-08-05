# Paper: "deseats: An R Package for Data-Driven Trend and Seasonality Estimation in Time Series"
# Script: Simulation study
# Version: December 24, 2025
# Author: Dominik Schulz

# Author's note: 
# This runs a simulation study that takes a few hours to finish. Therefore,
# this script is not included in the Rmd file of the submission and instead,
# this script saves an RData-file that is being loaded within the
# Rmd-file of the submission.

### 1. Load libraries
library(deseats)
library(magrittr)
library(tidyverse)

### 2. Number of observations (100, 200, 400)
fctor <- 2
n1 <- 100              # 100 obs., i.e. 25 years
n2 <- fctor * n1       # 200 obs., i.e. 50 years
n3 <- fctor * n2       # 400 obs., i.e. 100 years

### 3. Time points on interval [0, 1]

xt_n1 <- (1:n1) / n1
xt_n2 <- (1:n2) / n2
xt_n3 <- (1:n3) / n3

### 4. Trend functions

# Trend 1

t1_n1 <- (5 * xt_n1) %>%
  ts(frequency = 4)

t1_n2 <- (5 * xt_n2) %>%
  ts(frequency = 4)

t1_n3 <- (5 * xt_n3) %>%
  ts(frequency = 4)

trend1_t <- expression(5 * x)

# Trend 2

t2_n1 <- (2.5 * tanh(5 * (xt_n1 - 0.4)) + 2.5) %>%
  ts(frequency = 4)

t2_n2 <- (2.5 * tanh(5 * (xt_n2 - 0.4)) + 2.5) %>%
  ts(frequency = 4)

t2_n3 <- (2.5 * tanh(5 * (xt_n3 - 0.4)) + 2.5) %>%
  ts(frequency = 4)

trend2_t <- expression(2.5 * tanh(5 * (x - 0.4)) + 2.5)

# Trend 3

t3_n1 <- (3.2 * xt_n1 + 0.92 * (sin(3.2 * pi * (xt_n1 - 0.2))) + 0.85) %>%
  ts(frequency = 4)

t3_n2 <- (3.2 * xt_n2 + 0.92 * (sin(3.2 * pi * (xt_n2 - 0.2))) + 0.85) %>%
  ts(frequency = 4)

t3_n3 <- (3.2 * xt_n3 + 0.92 * (sin(3.2 * pi * (xt_n3 - 0.2))) + 0.85) %>%
  ts(frequency = 4)

trend3_t <- expression(3.2 * x + 0.92 * (sin(3.2 * pi * (x - 0.2))) + 0.85)

### 5. Seasonalities

# Exactly periodic and with frequency 4

s1 <- deseats(log(deseats::GDP), set_options(order_poly = 3)) %>%
  season() %>%
  head(4) %>%
  rep(length.out = n)

s1 <- (s1 - mean(s1)) * 15

s1_n1 <- rep(s1, length.out = n1) %>%
  ts(frequency = 4)
s1_n2 <- rep(s1, length.out = n2) %>%
  ts(frequency = 4)
s1_n3 <- rep(s1, length.out = n3) %>%
  ts(frequency = 4)


# Slowly changing and with frequency 4

s2_n1 <-  s1_n1 * ((0.4 * t2_n1 + 3) * 0.19)
s2_n2 <-  s1_n2 * ((0.4 * t2_n2 + 3) * 0.19)
s2_n3 <-  s1_n3 * ((0.4 * t2_n3 + 3) * 0.19)

### 6. Error settings

arma1 <- list(ar = 0.75, sd_e = 0.06)
arma2 <- list(ar = c(1.05, -0.3), sd_e = 0.03)

### 7. Tibble with all settings

# Combinations of settings for number of obs., trends, seasonalities and errors
# as data frame
sim <- expand.grid(
  c(n1, n2, n3),
  c("g1", "g2", "g3"),
  c("s1", "s2"),
  c("e1", "e2")
) %>%
  as.data.frame()

# Adjust names of data frame variables
names(sim) <- c("n", "Trend_f", "Season_f", "Error_p")

# Adjust data types of variables
sim$n <- as.numeric(sim$n)
sim$Trend_f <- as.character(sim$Trend_f)
sim$Season_f <- as.character(sim$Season_f)
sim$Error_p <- as.character(sim$Error_p)

# Transform into tibble
sim <- sim %>%
  as_tibble()

# Insert actual trend series into the tibble for the different settings
sim$Trend <-  vector(mode = "list", length = length(sim$n))
sim$Trend[sim$n == n1 & sim$Trend_f == "g1"] <- list(t1_n1)
sim$Trend[sim$n == n2 & sim$Trend_f == "g1"] <- list(t1_n2)
sim$Trend[sim$n == n3 & sim$Trend_f == "g1"] <- list(t1_n3)
sim$Trend[sim$n == n1 & sim$Trend_f == "g2"] <- list(t2_n1)
sim$Trend[sim$n == n2 & sim$Trend_f == "g2"] <- list(t2_n2)
sim$Trend[sim$n == n3 & sim$Trend_f == "g2"] <- list(t2_n3)
sim$Trend[sim$n == n1 & sim$Trend_f == "g3"] <- list(t3_n1)
sim$Trend[sim$n == n2 & sim$Trend_f == "g3"] <- list(t3_n2)
sim$Trend[sim$n == n3 & sim$Trend_f == "g3"] <- list(t3_n3)  

# Insert actual seasonality series into the tibble for the different settings
sim$Season <- vector(mode = "list", length = length(sim$n))
sim$Season[sim$n == n1 & sim$Season_f == "s1"] <- list(s1_n1)
sim$Season[sim$n == n2 & sim$Season_f == "s1"] <- list(s1_n2)
sim$Season[sim$n == n3 & sim$Season_f == "s1"] <- list(s1_n3)
sim$Season[sim$n == n1 & sim$Season_f == "s2"] <- list(s2_n1)
sim$Season[sim$n == n2 & sim$Season_f == "s2"] <- list(s2_n2)
sim$Season[sim$n == n3 & sim$Season_f == "s2"] <- list(s2_n3)

# Insert ARMA settings at the corresponding places in the tibble
sim$ARMA <- vector(mode = "list", length = length(sim$n))
sim$ARMA[sim$Error_p == "e1"] <- replicate(length(sim$n[sim$Error_p == "e1"]),
                                           expr = {arma1}, simplify = FALSE)
sim$ARMA[sim$Error_p == "e2"] <- replicate(length(sim$n[sim$Error_p == "e2"]),
                                           expr = {arma2}, simplify = FALSE)

# Space for true asymptotically optimal bandwidth in tibble
sim$hA_p1 <- sim$hA_p3 <- vector(mode = "list", length = length(sim$n))
sim$hA_p1[sim$Trend_f == "g1"] <- 0.49
sim$hA_p3[sim$Trend_f == "g1"] <- 0.49

# Grid with combinations of nobs, trends 2 and 3 and errors
grid <- expand.grid(
  c(n1, n2, n3),
  c("g2", "g3"),
  c("e1", "e2")
) %>%
  as.data.frame()

names(grid) <- c("n", "Trend", "Error")

# Run through the different combinations and compute the asymptotically
# optimal bandwidths and insert them into the tibble

for (i in seq_along(grid$n)) {
  
  # Get proper trend expression
  trend_t <- ifelse(grid$Trend[[i]] == "g2", trend2_t, trend3_t)

  # Get ARMA settings
  if (grid$Error[[i]] == "e1") {
    arma <- arma1
  } else {
    arma <- arma2
  }

  # Compute the theoretical bandwidth and insert it into the tibble
  # (for polynomial order 1)
  sim$hA_p1[sim$n == grid$n[[i]] & as.character(sim$Trend_f) == as.character(grid$Trend[[i]]) & sim$Error_p == grid$Error[[i]]] <- hA_calc(

      m = trend_t,
      arma = arma,
      p = 1,
      mu = 1,
      frequ = 4,
      n = as.numeric(grid$n[[i]]),
      cb = 0.05
    )$hA

  # Compute the theoretical bandwidth and insert it into the tibble
  # (for polynomial order 3)  
  sim$hA_p3[sim$n == grid$n[[i]] & as.character(sim$Trend_f) == as.character(grid$Trend[[i]]) & sim$Error_p == grid$Error[[i]]] <- hA_calc(

      m = trend_t,
      arma = arma,
      p = 3,
      mu = 1,
      frequ = 4,
      n = as.numeric(grid$n[[i]]),
      cb = 0.1
    )$hA  
  
}  
  


sim

### 8. Begin of the estimation step

# Load parallel programming packages
library(future)
library(furrr)

niter <- 1000         # Number of iterations
oldplan <- plan()     # Get current settings of plan()
ncores <- max(availableCores() - 1, 1)   # Get number of cores to use

plan(multisession, workers = ncores)  #  Set up multisession

# Run actual simulation
sim <- sim %>%
  mutate(
    Estimation = future_pmap(
      list(.c = sim$Trend, .d = sim$Season, .e = sim$ARMA, .f = sim$n),
      function(.c, .d, .e, .f, .g, niter) {
        
        # Set the seed
        set.seed(1)
        
        # Replicate "niter"-times
        replicate(niter, {
          err_series <- arima.sim(         # Simulate errors following specification
            model = list(ar = .e$ar, ma = .e$ma),
            n = .f, sd = .e$sd_e 
          ) %>%
          ts(start = c(1, 1), frequency = 4)  # Make it "ts" object
          
          obs_series <- err_series + .c + .d  # Add trend and seasonality
          
          # Estimate via DeSeaTS algorithm with polynomial order 1 and optimal infl. rate
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth
          est_deseats_opt_p1 <- deseats(obs_series, set_options(order_poly = 1), inflation_rate = "optimal", correction_factor = FALSE, drop = 0.05)
          trend_deseats_opt_p1 <- trend(est_deseats_opt_p1)
          season_deseats_opt_p1 <- season(est_deseats_opt_p1)
          MSE_trend_deseats_opt_p1 <- mean((.c - trend_deseats_opt_p1)^2)
          MSE_season_deseats_opt_p1 <- mean((.d - season_deseats_opt_p1)^2)
          MSE_m_deseats_opt_p1 <- mean((.c + .d - (trend_deseats_opt_p1 + season_deseats_opt_p1))^2)
          bwidth_deseats_opt_p1 <- bwidth(est_deseats_opt_p1)
          rm(est_deseats_opt_p1)
          rm(trend_deseats_opt_p1)
          rm(season_deseats_opt_p1)
          
          # Estimate via DeSeaTS algorithm with polynomial order 3 and optimal infl. rate
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth          
          est_deseats_opt_p3 <- deseats(obs_series, set_options(order_poly = 3), inflation_rate = "optimal", correction_factor = FALSE, drop = 0.1)
          trend_deseats_opt_p3 <- trend(est_deseats_opt_p3)
          season_deseats_opt_p3 <- season(est_deseats_opt_p3)
          MSE_trend_deseats_opt_p3 <- mean((.c - trend_deseats_opt_p3)^2)
          MSE_season_deseats_opt_p3 <- mean((.d - season_deseats_opt_p3)^2)
          MSE_m_deseats_opt_p3 <- mean((.c + .d - (trend_deseats_opt_p3 + season_deseats_opt_p3))^2)
          bwidth_deseats_opt_p3 <- bwidth(est_deseats_opt_p3)
          rm(est_deseats_opt_p3)
          rm(trend_deseats_opt_p3)
          rm(season_deseats_opt_p3)          
          
          # Estimate via DeSeaTS algorithm with polynomial order 1 and naive infl. rate
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth          
          est_deseats_nai_p1 <- deseats(obs_series, set_options(order_poly = 1), inflation_rate = "naive", correction_factor = FALSE, drop = 0.05)
          trend_deseats_nai_p1 <- trend(est_deseats_nai_p1)
          season_deseats_nai_p1 <- season(est_deseats_nai_p1)
          MSE_trend_deseats_nai_p1 <- mean((.c - trend_deseats_nai_p1)^2)
          MSE_season_deseats_nai_p1 <- mean((.d - season_deseats_nai_p1)^2)
          MSE_m_deseats_nai_p1 <- mean((.c + .d - (trend_deseats_nai_p1 + season_deseats_nai_p1))^2)
          bwidth_deseats_nai_p1 <- bwidth(est_deseats_nai_p1)
          rm(est_deseats_nai_p1)
          rm(trend_deseats_nai_p1)
          rm(season_deseats_nai_p1)          
          
          # Estimate via DeSeaTS algorithm with polynomial order 3 and naive infl. rate
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth             
          est_deseats_nai_p3 <- deseats(obs_series, set_options(order_poly = 3), inflation_rate = "naive", correction_factor = FALSE, drop = 0.1)
          trend_deseats_nai_p3 <- trend(est_deseats_nai_p3)
          season_deseats_nai_p3 <- season(est_deseats_nai_p3)
          MSE_trend_deseats_nai_p3 <- mean((.c - trend_deseats_nai_p3)^2)
          MSE_season_deseats_nai_p3 <- mean((.d - season_deseats_nai_p3)^2)
          MSE_m_deseats_nai_p3 <- mean((.c + .d - (trend_deseats_nai_p3 + season_deseats_nai_p3))^2)
          bwidth_deseats_nai_p3 <- bwidth(est_deseats_nai_p3)
          rm(est_deseats_nai_p3)
          rm(trend_deseats_nai_p3)
          rm(season_deseats_nai_p3)            
          
          # Estimate via BV4.1 base model
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth     
          est_bv <- BV4.1(obs_series)
          trend_bv <- trend(est_bv)
          season_bv <- season(est_bv)
          MSE_trend_bv <- mean((.c - trend_bv)^2)
          MSE_season_bv <- mean((.d - season_bv)^2)  
          MSE_m_bv <- mean((.c + .d - (trend_bv + season_bv))^2)
          rm(est_bv)
          rm(trend_bv)
          rm(season_bv)
          
          # Estimate via X13-ARIMA
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth            
          spec <- RJDemetra::x13_spec(spec = "RSA3", x11.mode = "Additive", outlier.enabled = FALSE)
          est_x12 <- suppressWarnings(RJDemetra::x13(obs_series, spec = spec))
          trend_x12 <- est_x12$final$series[, 3]
          season_x12 <- est_x12$final$series[, 4]
          MSE_trend_x12 <- mean((.c - trend_x12)^2)
          MSE_season_x12 <- mean((.d - season_x12)^2)  
          MSE_m_x12 <- mean((.c + .d - (trend_x12 + season_x12))^2)
          rm(est_x12)
          rm(trend_x12)
          rm(season_x12)  
          
          # Estimate via TRAMOSEATS (with previous regARIMA step)
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth  
          spec_t <- RJDemetra::tramoseats_spec(spec = "RSA3", outlier.enabled = FALSE)
          est_tramo <- suppressWarnings(RJDemetra::tramoseats(obs_series, spec = spec_t))
          trend_tramo <- est_tramo$final$series[, 3]
          season_tramo <- est_tramo$final$series[, 4]
          MSE_trend_tramo <- mean((.c - trend_tramo)^2)
          MSE_season_tramo <- mean((.d - season_tramo)^2)  
          MSE_m_tramo <- mean((.c + .d - (trend_tramo + season_tramo))^2)
          rm(est_tramo)
          rm(trend_tramo)
          rm(season_tramo)            
          
          # Estimate via STL (via forecast's mstl())
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth            
          est_stl <- forecast::mstl(obs_series)
          trend_stl <- est_stl[, 2]
          season_stl <- est_stl[, 3]
          MSE_trend_stl <- mean((.c - trend_stl)^2)
          MSE_season_stl <- mean((.d - season_stl)^2)            
          MSE_m_stl <- mean((.c + .d - (trend_stl + season_stl))^2)
          rm(est_stl)
          rm(trend_stl)
          rm(season_stl)
          
          # Estimate via stats' decompose() function
          # and compute trend, seasonality and trend+seasonality MSE,
          # and get bandwidth            
          est_ma <- stats::decompose(obs_series, type = "additive")
          trend_ma <- est_ma$trend
          season_ma <- est_ma$seasonal
          MSE_trend_ma <- mean((.c - trend_ma)^2, na.rm = TRUE)
          MSE_season_ma <- mean((.d - season_ma)^2, na.rm = TRUE)            
          MSE_m_ma <- mean((.c + .d - (trend_ma + season_ma))^2, na.rm = TRUE)
          rm(est_ma)
          rm(trend_ma)
          rm(season_ma)          
          
          list(bwidth_deseats_opt_p1 = bwidth_deseats_opt_p1, MSE_trend_deseats_opt_p1 = MSE_trend_deseats_opt_p1, MSE_season_deseats_opt_p1 = MSE_season_deseats_opt_p1, MSE_m_deseats_opt_p1 = MSE_m_deseats_opt_p1, bwidth_deseats_opt_p3 = bwidth_deseats_opt_p3, MSE_trend_deseats_opt_p3 = MSE_trend_deseats_opt_p3, MSE_season_deseats_opt_p3 = MSE_season_deseats_opt_p3, MSE_m_deseats_opt_p3 = MSE_m_deseats_opt_p3, bwidth_deseats_nai_p1 = bwidth_deseats_nai_p1, MSE_trend_deseats_nai_p1 = MSE_trend_deseats_nai_p1, MSE_season_deseats_nai_p1 = MSE_season_deseats_nai_p1, MSE_m_deseats_nai_p1 = MSE_m_deseats_nai_p1, bwidth_deseats_nai_p3 = bwidth_deseats_nai_p3, MSE_trend_deseats_nai_p3 = MSE_trend_deseats_nai_p3, MSE_season_deseats_nai_p3 = MSE_season_deseats_nai_p3, MSE_m_deseats_nai_p3 = MSE_m_deseats_nai_p3, MSE_trend_bv = MSE_trend_bv, MSE_season_bv = MSE_season_bv, MSE_m_bv = MSE_m_bv, MSE_trend_x12 = MSE_trend_x12, MSE_season_x12 = MSE_season_x12, MSE_m_x12 = MSE_m_x12, MSE_trend_tramo = MSE_trend_tramo, MSE_season_tramo = MSE_season_tramo, MSE_m_tramo = MSE_m_tramo, MSE_trend_stl = MSE_trend_stl, MSE_season_stl = MSE_season_stl, MSE_m_stl = MSE_m_stl, MSE_trend_ma = MSE_trend_ma, MSE_season_ma = MSE_season_ma, MSE_m_ma = MSE_m_ma)
          
        }, simplify = FALSE) 
        
      }, niter = niter,
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )
  )

# Reinstate previous settings of plan()
plan(oldplan)


### 9. Rearrange results into different shape

# Load further packages
library(purrr)

# Get a variables for MSE of each combination of component (trend, seasonal,
# trend+seasonal) and estimation method

sim$MSE_trend_deseats_opt_p1 <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_deseats_opt_p1 <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_deseats_opt_p1 <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_deseats_opt_p3 <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_deseats_opt_p3 <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_deseats_opt_p3 <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_deseats_nai_p1 <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_deseats_nai_p1 <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_deseats_nai_p1 <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_deseats_nai_p3 <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_deseats_nai_p3 <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_deseats_nai_p3 <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_bv <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_bv <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_bv <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_x12 <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_x12 <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_x12 <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_tramo <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_tramo <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_tramo <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_stl <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_stl <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_stl <- vector(mode = "list", length = length(sim$n))

sim$MSE_trend_ma <- vector(mode = "list", length = length(sim$n))
sim$MSE_season_ma <- vector(mode = "list", length = length(sim$n))
sim$MSE_m_ma <- vector(mode = "list", length = length(sim$n))

# Set variable for each DeSeaTS sub-algorithm for the bandwidth
# (also for MSE, mean and standard deviation)

sim$h_est_opt_p1 <- vector(mode = "list", length = length(sim$n))
sim$MSE_h_opt_p1 <- NA
sim$Mean_h_opt_p1 <- NA
sim$SD_h_opt_p1 <- NA

sim$h_est_opt_p3 <- vector(mode = "list", length = length(sim$n))
sim$MSE_h_opt_p3 <- NA
sim$Mean_h_opt_p3 <- NA
sim$SD_h_opt_p3 <- NA

sim$h_est_nai_p1 <- vector(mode = "list", length = length(sim$n))
sim$MSE_h_nai_p1 <- NA
sim$Mean_h_nai_p1 <- NA
sim$SD_h_nai_p1 <- NA

sim$h_est_nai_p3 <- vector(mode = "list", length = length(sim$n))
sim$MSE_h_nai_p3 <- NA
sim$Mean_h_nai_p3 <- NA
sim$SD_h_nai_p3 <- NA

# Run through tibble collect MSE values for methods

for (i in 1:length(sim$n)) {
  
  ## ...for trend, seasonality and trend+seasonality
  
  # DeSeaTS, p = 1, opt. infl. rate
  
  sim$MSE_trend_deseats_opt_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_deseats_opt_p1
    }
  )
  
  sim$MSE_season_deseats_opt_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_deseats_opt_p1
    }
  ) 
  
  sim$MSE_m_deseats_opt_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_deseats_opt_p1
    }
  )   
  
  # DeSeaTS, p = 3, opt. infl. rate
  
  sim$MSE_trend_deseats_opt_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_deseats_opt_p3
    }
  )
  
  sim$MSE_season_deseats_opt_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_deseats_opt_p3
    }
  ) 
  
  sim$MSE_m_deseats_opt_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_deseats_opt_p3
    }
  )   
  
  # DeSeaTS, p = 1, naive infl. rate  
  
  sim$MSE_trend_deseats_nai_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_deseats_nai_p1
    }
  )
  
  sim$MSE_season_deseats_nai_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_deseats_nai_p1
    }
  ) 
  
  sim$MSE_m_deseats_nai_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_deseats_nai_p1
    }
  )   
  
  # DeSeaTS, p = 3, naive infl. rate     
  
  sim$MSE_trend_deseats_nai_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_deseats_nai_p3
    }
  )
  
  sim$MSE_season_deseats_nai_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_deseats_nai_p3
    }
  ) 
  
  sim$MSE_m_deseats_nai_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_deseats_nai_p3
    }
  )     
  
  # BV4.1 
  
  sim$MSE_trend_bv[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_bv
    }
  )
  
  sim$MSE_season_bv[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_bv
    }
  )  
  
  sim$MSE_m_bv[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_bv
    }
  )     
  
  # X13
  
  sim$MSE_trend_x12[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_x12
    }
  )
  
  sim$MSE_season_x12[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_x12
    }
  )  
  
  sim$MSE_m_x12[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_x12
    }
  )       
  
  # TRAMOSEATS
  
  sim$MSE_trend_tramo[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_tramo
    }
  )
  
  sim$MSE_season_tramo[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_tramo
    }
  )  
  
  sim$MSE_m_tramo[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_tramo
    }
  )      
  
  # STL
  
  sim$MSE_trend_stl[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_stl
    }
  )
  
  sim$MSE_season_stl[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_stl
    }
  )  
  
  sim$MSE_m_stl[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_stl
    }
  )       
  
  # decompose()
  
  sim$MSE_trend_ma[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_trend_ma
    }
  )
  
  sim$MSE_season_ma[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_season_ma
    }
  )  
  
  sim$MSE_m_ma[[i]] <- map_dbl(
    sim$Estimation[[i]],
    function(.x) {
      .x$MSE_m_ma
    }
  )     
  
  # DeSeaTS, p = 1, opt. infl. rate
  ## Collect estimated bandwidths
  sim$h_est_opt_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    ~ .x$bwidth_deseats_opt_p1
  )
  
  ## Compute MSE of bandwidths, and mean and stand. dev.
  sim$MSE_h_opt_p1[[i]] <- sim$h_est_opt_p1[[i]] %>% 
    `-`(sim$hA_p1[[i]], .) %>%
    .^2 %>%
    mean()
  
  sim$Mean_h_opt_p1[[i]] <- sim$h_est_opt_p1[[i]] %>%
    mean()
  
  sim$SD_h_opt_p1[[i]] <- sim$h_est_opt_p1[[i]] %>%
    sd()  
  
  # Same for DeSeaTS, p = 3, opt. infl. rate
  
  sim$h_est_opt_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    ~ .x$bwidth_deseats_opt_p3
  )
  
  sim$MSE_h_opt_p3[[i]] <- sim$h_est_opt_p3[[i]] %>% 
    `-`(sim$hA_p3[[i]], .) %>%
    .^2 %>%
    mean()
  
  sim$Mean_h_opt_p3[[i]] <- sim$h_est_opt_p3[[i]] %>%
    mean()
  
  sim$SD_h_opt_p3[[i]] <- sim$h_est_opt_p3[[i]] %>%
    sd()  
  
  # Same for DeSeaTS, p = 1, nai. infl. rate
  
  sim$h_est_nai_p1[[i]] <- map_dbl(
    sim$Estimation[[i]],
    ~ .x$bwidth_deseats_nai_p1
  )
  
  sim$MSE_h_nai_p1[[i]] <- sim$h_est_nai_p1[[i]] %>% 
    `-`(sim$hA_p1[[i]], .) %>%
    .^2 %>%
    mean()
  
  sim$Mean_h_nai_p1[[i]] <- sim$h_est_nai_p1[[i]] %>%
    mean()
  
  sim$SD_h_nai_p1[[i]] <- sim$h_est_nai_p1[[i]] %>%
    sd()   
  


  # Same for DeSeaTS, p = 3, nai. infl. rate 
  
  sim$h_est_nai_p3[[i]] <- map_dbl(
    sim$Estimation[[i]],
    ~ .x$bwidth_deseats_nai_p3
  )
  
  sim$MSE_h_nai_p3[[i]] <- sim$h_est_nai_p3[[i]] %>% 
    `-`(sim$hA_p3[[i]], .) %>%
    .^2 %>%
    mean()
  
  sim$Mean_h_nai_p3[[i]] <- sim$h_est_nai_p3[[i]] %>%
    mean()
  
  sim$SD_h_nai_p3[[i]] <- sim$h_est_nai_p3[[i]] %>%
    sd()   
  
}

# Compute mean MSE values for trend

sim$mean_MSE_trend_deseats_opt_p1 <- map_dbl(
  sim$MSE_trend_deseats_opt_p1, ~ mean(.x)
)

sim$mean_MSE_trend_deseats_opt_p3 <- map_dbl(
  sim$MSE_trend_deseats_opt_p3, ~ mean(.x)
)

sim$mean_MSE_trend_deseats_nai_p1 <- map_dbl(
  sim$MSE_trend_deseats_nai_p1, ~ mean(.x)
)

sim$mean_MSE_trend_deseats_nai_p3 <- map_dbl(
  sim$MSE_trend_deseats_nai_p3, ~ mean(.x)
)

sim$mean_MSE_trend_bv <- map_dbl(
  sim$MSE_trend_bv, ~ mean(.x)
)

sim$mean_MSE_trend_x12 <- map_dbl(
  sim$MSE_trend_x12, ~ mean(.x)
)

sim$mean_MSE_trend_tramo <- map_dbl(
  sim$MSE_trend_tramo, ~ mean(.x)
)

sim$mean_MSE_trend_stl <- map_dbl(
  sim$MSE_trend_stl, ~ mean(.x)
)

sim$mean_MSE_trend_ma <- map_dbl(
  sim$MSE_trend_ma, ~ mean(.x)
)

# Compute mean MSE values for seasonality

sim$mean_MSE_season_deseats_opt_p1 <- map_dbl(
  sim$MSE_season_deseats_opt_p1, ~ mean(.x)
)

sim$mean_MSE_season_deseats_opt_p3 <- map_dbl(
  sim$MSE_season_deseats_opt_p3, ~ mean(.x)
)

sim$mean_MSE_season_deseats_nai_p1 <- map_dbl(
  sim$MSE_season_deseats_nai_p1, ~ mean(.x)
)

sim$mean_MSE_season_deseats_nai_p3 <- map_dbl(
  sim$MSE_season_deseats_nai_p3, ~ mean(.x)
)

sim$mean_MSE_season_bv <- map_dbl(
  sim$MSE_season_bv, ~ mean(.x)
)

sim$mean_MSE_season_x12 <- map_dbl(
  sim$MSE_season_x12, ~ mean(.x)
)

sim$mean_MSE_season_tramo <- map_dbl(
  sim$MSE_season_tramo, ~ mean(.x)
)

sim$mean_MSE_season_stl <- map_dbl(
  sim$MSE_season_stl, ~ mean(.x)
)

sim$mean_MSE_season_ma <- map_dbl(
  sim$MSE_season_ma, ~ mean(.x)
)

# Compute mean MSE values for trend+seasonality

sim$mean_MSE_m_deseats_opt_p1 <- map_dbl(
  sim$MSE_m_deseats_opt_p1, ~ mean(.x)
)

sim$mean_MSE_m_deseats_opt_p3 <- map_dbl(
  sim$MSE_m_deseats_opt_p3, ~ mean(.x)
)

sim$mean_MSE_m_deseats_nai_p1 <- map_dbl(
  sim$MSE_m_deseats_nai_p1, ~ mean(.x)
)

sim$mean_MSE_m_deseats_nai_p3 <- map_dbl(
  sim$MSE_m_deseats_nai_p3, ~ mean(.x)
)

sim$mean_MSE_m_bv <- map_dbl(
  sim$MSE_m_bv, ~ mean(.x)
)

sim$mean_MSE_m_x12 <- map_dbl(
  sim$MSE_m_x12, ~ mean(.x)
)

sim$mean_MSE_m_tramo <- map_dbl(
  sim$MSE_m_tramo, ~ mean(.x)
)

sim$mean_MSE_m_stl <- map_dbl(
  sim$MSE_m_stl, ~ mean(.x)
)

sim$mean_MSE_m_ma <- map_dbl(
  sim$MSE_m_ma, ~ mean(.x)
)


# Remove unnecessary elements for article tables and plots

sim$Trend <- NULL
sim$Season <- NULL
sim$Estimation <- NULL

### 10. Save results

save(sim, file = "SimResults20251223-final.RData")