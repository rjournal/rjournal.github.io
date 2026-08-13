# Paper: "deseats: An R Package for Data-Driven Trend and Seasonality Estimation in Time Series"
# Script: Simulation study 1 for bandwidth selection algorithm
# Version: March 28, 2026
# Author: Dominik Schulz

# Author's note: 
# This runs a simulation study that takes a few hours to finish. Therefore,
# this script is not included in the Rmd file of the submission and instead,
# this script saves CSV-files that are being loaded within the
# main R script of the submission.

### 1. Load libraries
library(deseats)       # For bandwidth algorithm
library(tidyverse)     # For data wrangling
library(future)        # For parallel computation
library(furrr)         # Also for parallel computation

### 2. General settings
s_periods <- c(4, 12)  # Seasonal periods to check (here: like quarterly and monthly)
n_year1 <- 15        # Smallest number of years to check
n_year2 <- 2 * n_year1  # Second number of years to check
n_year3 <- 2 * n_year2  # Largest number of years to check
n_year <- c(n_year1, n_year2, n_year3)   # Vector of numbers of years to check
trends <- paste0("t", 1:3)    # Indicator vector for three trends to check
seasonalities <- paste0("s", 1:2)  # Indicator for two seasonalities to check
error_processes <- paste0("e", 1:2) # Indicator for two error processes to consider
M <- 1000 # Number of series to simulate for each setting combination

### 3. Fix trend, seasonality and error settings

#### -> Trends

trend_funs <- list(
  "t1" = function(xt) {            # Linear trend
    5 * xt + 1
  },
  "t2" = function(xt) {            # Slightly more complex trend
    2.5 * tanh(5 * (xt - 0.4)) + 3.5
  },
  "t3" = function(xt) {            # Even more complex trend
    3.2 * xt + 0.92 * (sin(3.2 * pi * (xt - 0.2))) + 1.85
  }
)

#### -> Seasonalities

seas_funs <- list(
  "s1_sp4" = function(xt) {          # First seasonality for quarterly series
    
    t <- 1:length(xt)
    quart <- (t - 1) %% 4
    fx <- rep(-0.39, length(xt))
    fx[quart == 1] <- 0.01
    fx[quart == 2] <- -0.04
    fx[quart == 3] <- 0.42
    fx
  },
  "s1_sp12" = function(xt) {         # First seasonality for monthly series
    
    t <- 1:length(xt)
    quart <- (t - 1) %% 12
    fx <- rep(-0.39, length(xt))
    fx[quart == 1] <- 0.05
    fx[quart == 2] <- -0.01
    fx[quart == 3] <- -0.16
    fx[quart == 4] <- -0.31
    fx[quart == 5] <- -0.24
    fx[quart == 6] <- -0.25
    
    fx[quart == 7] <- -0.10
    fx[quart == 8] <- 0.50
    fx[quart == 9] <- 0.62
    fx[quart == 10] <- 0.34 
    fx[quart == 11] <- -0.05      
    
    
    fx
  },
  "s2_sp4" = function(xt) {             # Second seasonality for quarterly series
    
    t <- 1:length(xt)
    quart <- (t - 1) %% 4
    fx <- rep(-0.39, length(xt))
    fx[quart == 1] <- 0.01
    fx[quart == 2] <- -0.04
    fx[quart == 3] <- 0.42
    
   0.83 * (1 + 0.1 * sin(pi * xt * 50 / 9) + 0.2 * cos(3 * pi * xt)) * fx
    
  },
  "s2_sp12" = function(xt) {           # Second seasonality for monthly series
    
    t <- 1:length(xt)
    quart <- (t - 1) %% 12
    fx <- rep(-0.39, length(xt))
    fx[quart == 1] <- 0.05
    fx[quart == 2] <- -0.01
    fx[quart == 3] <- -0.16
    fx[quart == 4] <- -0.31
    fx[quart == 5] <- -0.24
    fx[quart == 6] <- -0.25
    
    fx[quart == 7] <- -0.10
    fx[quart == 8] <- 0.50
    fx[quart == 9] <- 0.62
    fx[quart == 10] <- 0.34 
    fx[quart == 11] <- -0.05      
    
    0.83 * (1 + 0.1 * sin(pi * xt * 50 / 9) + 0.2 * cos(3 * pi * xt)) * fx
    
  }
)

#### -> Errors

errors <- list(
  "e1" = list(ar = c(0.75), sd = 0.083),
  "e2" = list(ar = c(1.05, -0.3), sd = 0.06)
)

### 4. Data frame with combinations of settings

combinations <- expand.grid(
  N_Year = n_year,
  Seasonal_Period = s_periods,
  Trend = trends,
  Season = seasonalities,
  Error_Process = error_processes,
  stringsAsFactors = FALSE
) %>%
  mutate(n = N_Year * Seasonal_Period)

dim(combinations)      # 72 settings
print(combinations)

### 5. Write simulation function

run_sim1 <- function(combinations, M) {
  n_cores <- max(future::availableCores() - 1, 1)   # Number of cores to use

  oldplan <- future::plan()                         # Get old plan

  future::plan(future::multisession, workers = n_cores) # Set multisession
  on.exit({future::plan(oldplan)}, add = TRUE, after = TRUE)  # On exit back to initial plan
  
  sim1_out <- furrr::future_pmap(
    list(combinations$Trend, combinations$Season, combinations$Error_Process, combinations$Seasonal_Period, combinations$n),
    function(.trend, .season, .error, .speriod, .nobs, trend_funs, seas_funs, errors, M) {
      set.seed(1, kind = "Mersenne-Twister") # Set a seed to reuse every time for each set of 1000 series
    
      tp <- 1:.nobs
      xt <- tp / .nobs
    
      tfun <- trend_funs[[.trend]](xt)
      sfun <- seas_funs[[paste0(.season, "_sp", .speriod)]](xt)
      ts_fun <- tfun + sfun
    
      pars <- errors[[.error]]
      ar <- pars[["ar"]]  # Coefficients
      sd_v <- pars[["sd"]] # Innovation standard deviation
    
      bwidth_r1_opt <- rep(NA, M)
      bwidth_r1_nai <- rep(NA, M)
      bwidth_r3_opt <- rep(NA, M)
      bwidth_r3_nai <- rep(NA, M)
    
      MSE_trend_r1_opt <- rep(NA, M)
      MSE_trend_r1_nai <- rep(NA, M)
      MSE_trend_r3_opt <- rep(NA, M)
      MSE_trend_r3_nai <- rep(NA, M)
    
      MSE_seas_r1_opt <- rep(NA, M)
      MSE_seas_r1_nai <- rep(NA, M)
      MSE_seas_r3_opt <- rep(NA, M)
      MSE_seas_r3_nai <- rep(NA, M)   
    
      MSE_trendseas_r1_opt <- rep(NA, M)
      MSE_trendseas_r1_nai <- rep(NA, M)
      MSE_trendseas_r3_opt <- rep(NA, M)
      MSE_trendseas_r3_nai <- rep(NA, M)    
    
      for (i in 1:M) {
        error_series <- as.numeric(arima.sim(model = list(ar = ar), n = .nobs, sd = sd_v))
        yt <- ts(tfun + sfun + error_series, frequency = .speriod)
      
        # Check with local linear trend and optimal inflation factor
        tryCatch({
          est <- deseats(yt, set_options(order_poly = 1), inflation_rate = "optimal")
          bwidth_r1_opt[[i]] <- est@bwidth # Get estimated bandwidth
          MSE_trend_r1_opt[[i]] <- mean((trend(est) - tfun)^2)  # Compute trend MSE over all time points
          MSE_seas_r1_opt[[i]] <- mean((season(est) - sfun)^2)  # Compute seasonality MSE over all time points
          MSE_trendseas_r1_opt[[i]] <- mean((trend(est) + season(est) - ts_fun)^2)  # Compute trend-seasonality MSE over all time points
        }, error = function(e1) {
          bwidth_r1_opt[[i]] <- NA
          MSE_trend_r1_opt[[i]] <- NA
          MSE_seas_r1_opt[[i]] <- NA
          MSE_trendseas_r1_opt[[i]] <- NA 
        })

        # Check with local linear trend and naive inflation factor
        tryCatch({
          est <- deseats(yt, set_options(order_poly = 1), inflation_rate = "naive")
          bwidth_r1_nai[[i]] <- est@bwidth
          MSE_trend_r1_nai[[i]] <- mean((trend(est) - tfun)^2)
          MSE_seas_r1_nai[[i]] <- mean((season(est) - sfun)^2)
          MSE_trendseas_r1_nai[[i]] <- mean((trend(est) + season(est) - ts_fun)^2)   
        }, error = function(e1) {
          bwidth_r1_nai[[i]] <- NA
          MSE_trend_r1_nai[[i]] <- NA
          MSE_seas_r1_nai[[i]] <- NA
          MSE_trendseas_r1_nai[[i]] <- NA   
        })
        
        # Check with local cubic trend and optimal inflation factor
        tryCatch({
          est <- deseats(yt, set_options(order_poly = 3), inflation_rate = "optimal")
          bwidth_r3_opt[[i]] <- est@bwidth
          MSE_trend_r3_opt[[i]] <- mean((trend(est) - tfun)^2)
          MSE_seas_r3_opt[[i]] <- mean((season(est) - sfun)^2)
          MSE_trendseas_r3_opt[[i]] <- mean((trend(est) + season(est) - ts_fun)^2)
        }, error = function(e1) {
          bwidth_r3_opt[[i]] <- NA
          MSE_trend_r3_opt[[i]] <- NA
          MSE_seas_r3_opt[[i]] <- NA
          MSE_trendseas_r3_opt[[i]] <- NA
        })
      
        # Check with local cubic trend and naive inflation factor        
        tryCatch({
          est <- deseats(yt, set_options(order_poly = 3), inflation_rate = "naive")
          bwidth_r3_nai[[i]] <- est@bwidth
          MSE_trend_r3_nai[[i]] <- mean((trend(est) - tfun)^2)
          MSE_seas_r3_nai[[i]] <- mean((season(est) - sfun)^2)
          MSE_trendseas_r3_nai[[i]] <- mean((trend(est) + season(est) - ts_fun)^2)   
        }, error = function(e1) {
          bwidth_r3_nai[[i]] <- NA
          MSE_trend_r3_nai[[i]] <- NA
          MSE_seas_r3_nai[[i]] <- NA
          MSE_trendseas_r3_nai[[i]] <- NA
        })
      
      }
    
      # Return list with all results
      list(
        bwidth_r1_opt = bwidth_r1_opt,
        bwidth_r1_nai = bwidth_r1_nai,
        bwidth_r3_opt = bwidth_r3_opt,
        bwidth_r3_nai = bwidth_r3_nai,
      
        MSE_trend_r1_opt = MSE_trend_r1_opt,
        MSE_trend_r1_nai = MSE_trend_r1_nai,
        MSE_trend_r3_opt = MSE_trend_r3_opt,
        MSE_trend_r3_nai = MSE_trend_r3_nai,
      
        MSE_seas_r1_opt = MSE_seas_r1_opt,
        MSE_seas_r1_nai = MSE_seas_r1_nai,
        MSE_seas_r3_opt = MSE_seas_r3_opt,
        MSE_seas_r3_nai = MSE_seas_r3_nai,
      
        MSE_trendseas_r1_opt = MSE_trendseas_r1_opt,
        MSE_trendseas_r1_nai = MSE_trendseas_r1_nai,
        MSE_trendseas_r3_opt = MSE_trendseas_r3_opt,
        MSE_trendseas_r3_nai = MSE_trendseas_r3_nai      
      )
    
    
    }, trend_funs = trend_funs, seas_funs = seas_funs, errors = errors, M = M,
       .options = furrr::furrr_options(seed = NULL), .progress = TRUE
  )
  
  plan(oldplan)
  
  sim1_out
  
}

### 6. Run the simulation

sim <- run_sim1(combinations = combinations, M = M)
# save (sim, file = "schulz-sim1.Rdata")    # Option to save intermediate results

### 7. Compute true bandwidths (assuming exactly periodic seasonal component)

true_bwidths <- pmap(
  list(combinations$Trend, combinations$Season, combinations$Error_Process, combinations$n, combinations$Seasonal_Period),
  function(.tf, .sf, .ep, .nobs, .seasp) {
    r1 <- 1
    r3 <- 3
    cb1 <- 0.05      # Boundary cut-off under local linear
    cb3 <- 0.1       # Boundary cut-off under local cubic
    
    bwidths <- if (.tf == "t1") {       # For simple linear trend use maximum possible bandwidth selectable by the algorithm
      list("r1" = 0.49, "r3" = 0.49)
    } else if (.tf %in% c("t2", "t3")) {
      m <- switch(
        .tf,
        "t2" = expression(2.5 * tanh(5 * (x - 0.4)) + 3.5),
        "t3" = expression(3.2 * x + 0.92 * (sin(3.2 * pi * (x - 0.2))) + 1.85)
      )
      error_settings <- if (.ep == "e1") {
        list(ar = c(0.75), sd = 0.083)
      } else if (.ep == "e2") {
        list(ar = c(1.05, -0.3), sd = 0.06)
      }
      ar <- error_settings$ar
      ma <- error_settings$ma
      sd_e <- error_settings$sd
      
      # Compute optimal bandwidths under local linear and local cubic
      b_r1 <- hA_calc(m = m, arma = list(ar = ar, ma = ma, sd_e = sd_e),
                      p = r1, mu = 1, frequ = .seasp, n = .nobs, cb = cb1)$hA
      b_r3 <- hA_calc(m = m, arma = list(ar = ar, ma = ma, sd_e = sd_e),
                      p = r3, mu = 1, frequ = .seasp, n = .nobs, cb = cb3)$hA
      
      list("r1" = b_r1, "r3" = b_r3)
      
    }
    
    bwidths
  }
)

# 8. Compute (simulated) bandwidth bias, SD, and MSE

# For each setting combination, compute sample standard deviation over 
# the estimated bandwidths for the M series (for each DeSeaTS algorithm
# specification separately)
bwidth_SD <- map(
  1:length(true_bwidths),
  function(.x, true_bwidths, sim) {
    fac <- 1000      # Arbitrary multiplication factor
    
    r1_opt <- sd(sim[[.x]]$bwidth_r1_opt) * fac   
    r1_nai <- sd(sim[[.x]]$bwidth_r1_nai) * fac
    r3_opt <- sd(sim[[.x]]$bwidth_r3_opt) * fac
    r3_nai <- sd(sim[[.x]]$bwidth_r3_nai) * fac
    
    list(r1_opt = r1_opt, r1_nai = r1_nai, r3_opt = r3_opt, r3_nai = r3_nai)
    
  },
  sim = sim
)

# For each setting combination, compute sample mean over 
# the estimated bandwidths for the M series (for each DeSeaTS algorithm
# specification separately)
bwidth_mean <- map(
  1:length(true_bwidths),
  function(.x, sim) {
    
    r1_opt <- mean(sim[[.x]]$bwidth_r1_opt)
    r1_nai <- mean(sim[[.x]]$bwidth_r1_nai)
    r3_opt <- mean(sim[[.x]]$bwidth_r3_opt)
    r3_nai <- mean(sim[[.x]]$bwidth_r3_nai)
    
    list(r1_opt = r1_opt, r1_nai = r1_nai, r3_opt = r3_opt, r3_nai = r3_nai)
    
  },
  sim = sim
)

# For each setting combination, compute sample MSE over 
# the estimated bandwidths for the M series (for each DeSeaTS algorithm
# specification separately)
bwidth_MSE <- map(
  1:length(true_bwidths),
  function(.x, true_bwidths, sim) {
    fac <- 1000
    b_r1 <- true_bwidths[[.x]][["r1"]]
    b_r3 <- true_bwidths[[.x]][["r3"]]
    
    r1_opt <- mean((sim[[.x]]$bwidth_r1_opt - b_r1)^2) * fac
    r1_nai <- mean((sim[[.x]]$bwidth_r1_nai - b_r1)^2) * fac
    r3_opt <- mean((sim[[.x]]$bwidth_r3_opt - b_r3)^2) * fac
    r3_nai <- mean((sim[[.x]]$bwidth_r3_nai - b_r3)^2) * fac
    
    list(r1_opt = r1_opt, r1_nai = r1_nai, r3_opt = r3_opt, r3_nai = r3_nai)
    
  },
  true_bwidths = true_bwidths, sim = sim
)

# 9. Compute (simulated) average MSE for estimators of deterministic components

mean_det_MSE <- map(1:length(sim),
  function(.x) {
    fac <- 1000   # Arbitrary factor
    list(
      "trend_r1_opt" = mean(sim[[.x]]$MSE_trend_r1_opt) * fac,
      "trend_r1_nai" = mean(sim[[.x]]$MSE_trend_r1_nai) * fac,
      "trend_r3_opt" = mean(sim[[.x]]$MSE_trend_r3_opt) * fac,
      "trend_r3_nai" = mean(sim[[.x]]$MSE_trend_r3_nai) * fac,  
      "seas_r1_opt" = mean(sim[[.x]]$MSE_seas_r1_opt) * fac,
      "seas_r1_nai" = mean(sim[[.x]]$MSE_seas_r1_nai) * fac,
      "seas_r3_opt" = mean(sim[[.x]]$MSE_seas_r3_opt) * fac,
      "seas_r3_nai" = mean(sim[[.x]]$MSE_seas_r3_nai) * fac,  
      "trendseas_r1_opt" = mean(sim[[.x]]$MSE_trendseas_r1_opt) * fac,
      "trendseas_r1_nai" = mean(sim[[.x]]$MSE_trendseas_r1_nai) * fac,
      "trendseas_r3_opt" = mean(sim[[.x]]$MSE_trendseas_r3_opt) * fac,
      "trendseas_r3_nai" = mean(sim[[.x]]$MSE_trendseas_r3_nai) * fac
    )    
  }
)

# 10. Collect component MSE values by setting and DeSeaTS selection

mse_names <- names(mean_det_MSE[[1]])
MSE_det <- vector(mode = "list", length = length(mse_names))
names(MSE_det) <- mse_names

for (nam in mse_names) {
  MSE_det[[nam]] <- map_dbl(
    mean_det_MSE,
    function(.x, nam) {
      .x[[nam]]
    },
    nam = nam
  )
}

MSE_r1_opt <- vector(mode = "list", length = length(MSE_det$trend_r1_opt))
MSE_r1_nai <- vector(mode = "list", length = length(MSE_det$trend_r1_nai))
MSE_r3_opt <- vector(mode = "list", length = length(MSE_det$trend_r3_opt))
MSE_r3_nai <- vector(mode = "list", length = length(MSE_det$trend_r3_nai))
for (i in 1:length(MSE_det$trend_r1_opt)) {
  MSE_r1_opt[[i]] <- c("trend" = MSE_det$trend_r1_opt[[i]],
                       "seas" = MSE_det$seas_r1_opt[[i]],
                       "trendseas" = MSE_det$trendseas_r1_opt[[i]])
  MSE_r1_nai[[i]] <- c("trend" = MSE_det$trend_r1_nai[[i]],
                       "seas" = MSE_det$seas_r1_nai[[i]],
                       "trendseas" = MSE_det$trendseas_r1_nai[[i]])
  MSE_r3_opt[[i]] <- c("trend" = MSE_det$trend_r3_opt[[i]],
                       "seas" = MSE_det$seas_r3_opt[[i]],
                       "trendseas" = MSE_det$trendseas_r3_opt[[i]])
  MSE_r3_nai[[i]] <- c("trend" = MSE_det$trend_r3_nai[[i]],
                       "seas" = MSE_det$seas_r3_nai[[i]],
                       "trendseas" = MSE_det$trendseas_r3_nai[[i]])    
}

# 11. Get combinations data frame for each seasonal period - error combination

#### Enrich combinations data frame by unique index variable
combinations <- combinations %>%
  mutate(IDX = 1:72)

#### Split by seasonal period and error type (four groups total)
combinations2 <- combinations %>%
  group_by(Seasonal_Period, Error_Process) %>%
  group_split() %>%
  map(function(.x) {.x %>% arrange(Error_Process, Season, Trend, N_Year)})
names(combinations2) <- c("quarterly_e1", "quarterly_e2", "monthly_e1", "monthly_e2")

#### Collect the component MSEs for the four groups
results_sp_err <- combinations2 %>%
  map(function(.x, MSE_r1_opt, MSE_r1_nai, MSE_r3_opt, MSE_r3_nai) {
    idx <- .x[["IDX"]]
    list(
      "r1_opt" = Reduce(cbind, MSE_r1_opt[idx]),
      "r1_nai" = Reduce(cbind, MSE_r1_nai[idx]),
      "r3_opt" = Reduce(cbind, MSE_r3_opt[idx]),
      "r3_nai" = Reduce(cbind, MSE_r3_nai[idx])
    )
  }, 
  MSE_r1_opt = MSE_r1_opt, MSE_r1_nai = MSE_r1_nai, 
  MSE_r3_opt = MSE_r3_opt, MSE_r3_nai = MSE_r3_nai
  )

# 12. Collect bandwidth means, SDs and MSEs by DeSeaTS setting

sets <- c("r1_opt", "r1_nai", "r3_opt", "r3_nai")
names(sets) <- sets
bwidth_mean_2 <- map(
    sets,
    function(.x, bwidth_mean) {
      map_dbl(
        bwidth_mean,
        function(.y, idx) {
          .y[[idx]]
        },
        idx = .x
      )
    },
    bwidth_mean = bwidth_mean
  )

bwidth_SD_2 <- map(
    sets,
    function(.x, bwidth_SD) {
      map_dbl(
        bwidth_SD,
        function(.y, idx) {
          .y[[idx]]
        },
        idx = .x
      )
    },
    bwidth_SD = bwidth_SD
  )

bwidth_MSE_2 <- map(
    sets,
    function(.x, bwidth_MSE) {
      map_dbl(
        bwidth_MSE,
        function(.y, idx) {
          .y[[idx]]
        },
        idx = .x
      )
    },
    bwidth_MSE = bwidth_MSE
  )

bwidth_res <- combinations2 %>%
  map(function(.x, bwidth_mean_2, bwidth_SD_2, bwidth_MSE_2) {
    idx <- .x[["IDX"]]
    list(
      "r1_opt" = rbind(bwidth_mean_2[["r1_opt"]][idx], bwidth_SD_2[["r1_opt"]][idx], bwidth_MSE_2[["r1_opt"]][idx]),
      "r1_nai" = rbind(bwidth_mean_2[["r1_nai"]][idx], bwidth_SD_2[["r1_nai"]][idx], bwidth_MSE_2[["r1_nai"]][idx]),
      "r3_opt" = rbind(bwidth_mean_2[["r3_opt"]][idx], bwidth_SD_2[["r3_opt"]][idx], bwidth_MSE_2[["r3_opt"]][idx]),
      "r3_nai" = rbind(bwidth_mean_2[["r3_nai"]][idx], bwidth_SD_2[["r3_nai"]][idx], bwidth_MSE_2[["r3_nai"]][idx])
    )
  },
  bwidth_mean_2 = bwidth_mean_2, bwidth_SD_2 = bwidth_SD_2, bwidth_MSE_2 = bwidth_MSE_2
  )

# 13. Collect true bandwidths by local linear or local cubic setting

true_bwidths_r1_s <- map_dbl(
  true_bwidths, function(.x) {
    .x$r1
  }
)
true_bwidths_r1 <- combinations2 %>%
  map(function(.x, true_bwidths_r1_s) {
    idx <- .x[["IDX"]]
    true_bwidths_r1_s[idx]
  }, true_bwidths_r1_s = true_bwidths_r1_s)
true_bwidths_r3_s <- map_dbl(
  true_bwidths, function(.x) {
    .x$r3
  }
)
true_bwidths_r3 <- combinations2 %>%
  map(function(.x, true_bwidths_r3_s) {
    idx <- .x[["IDX"]]
    true_bwidths_r3_s[idx]
  }, true_bwidths_r3_s = true_bwidths_r3_s)

# 14. Create final output tables

dfs_out <- pmap(
  list(true_bwidths_r1, true_bwidths_r3, bwidth_res, results_sp_err),
  function(.tbwidths_r1, .tbwidths_r3, .bwidths, .mse_results) {
    rbind(
      .tbwidths_r1,
      .bwidths[["r1_opt"]],
      .mse_results[["r1_opt"]],
      .bwidths[["r1_nai"]],
      .mse_results[["r1_nai"]],
      .tbwidths_r3,
      .bwidths[["r3_opt"]],
      .mse_results[["r3_opt"]],
      .bwidths[["r3_nai"]],
      .mse_results[["r3_nai"]]
    )
  }
)

#### Get column names
nams <- combinations2[[1]] %>%
  select(Season, Trend, N_Year) %>%
  arrange(Season, Trend, N_Year) %>%
  mutate(Name = paste0(Season, "_", Trend, "_", N_Year)) %>%
  pull(Name)

### Apply column names to output data frames and adjust the output data frames
### even further (row names etc.)

dfs_out <- dfs_out %>%
  map(function(.x, nams) {
    dimnames(.x) <- NULL
    tab_out <- .x %>%
      as.data.frame() %>%
      map_dfc(function(.y) {sprintf("%.2f", .y)}) %>%
      mutate("Quantity" = rep(c("$h_A$", "$\\text{Mean}\\left(\\hat{h}\\right)$", "$\\text{SD}\\left(\\hat{h}\\right)$", "$\\text{MSE}\\left(\\hat{h}\\right)$", "$\\text{MSE}\\left(\\hat{g}\\right)$", "$\\text{MSE}\\left(\\hat{s}\\right)$", "$\\text{MSE}\\left(\\hat{m}\\right)$", "$\\text{Mean}\\left(\\hat{h}\\right)$", "$\\text{SD}\\left(\\hat{h}\\right)$", "$\\text{MSE}\\left(\\hat{h}\\right)$", "$\\text{MSE}\\left(\\hat{g}\\right)$", "$\\text{MSE}\\left(\\hat{s}\\right)$", "$\\text{MSE}\\left(\\hat{m}\\right)$"), 2)) %>%
      relocate(Quantity)
    names(tab_out) <- c("Quantity", nams) 
    tab_out %>%
      mutate("$r$" = c("1", rep("", 12), "3", rep("", 12))) %>%
      mutate("Inflation" = c("", "optimal", rep("", 5), "naive", rep("", 5), "", "optimal", rep("", 5), "naive", rep("", 5))) %>%
      relocate(`$r$`, Inflation)
  }, nams = nams)
  

# 15. Save tables locally
dfs_out %>% iwalk(
  function(.x, .y) {
    nam_split <- str_split_1(.y, pattern = "_")
    sp <- str_to_title(nam_split[[1]])
    err <- switch(
      nam_split[[2]],
      "e1" = "Error1",
      "e2" = "Error2"
    )
    
    write.table(.x, file = paste0("Sim1Table-", sp, "-", err, ".csv"),
            quote = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
    
  }
)

#############################################################################
#############################################################################

##################### Creation of tables for boxplot figures ################

comb_r_infr <- expand.grid("r" = c(1, 3), "infr" = c("opt", "nai"), stringsAsFactors = FALSE)
r_vals <- comb_r_infr$r  
infr_vals <- comb_r_infr$infr  
names(r_vals) <- paste0(comb_r_infr$r, "_", comb_r_infr$infr)
names(infr_vals) <- paste0(comb_r_infr$r, "_", comb_r_infr$infr)

combinations3 <- map2(
  r_vals, infr_vals,
  function(.x, .y, combinations, sim) {
    selector <- paste0("bwidth_r", .x, "_", .y)
    combs_sep <- combinations %>%
      mutate(IDX = factor(IDX)) %>%
      group_by(IDX) %>%
      group_split()
    
    (1:72) %>% map(
      function(.z, sim, combs_sep, selector, rv, ifac) {
        combs_sep[[.z]][rep(1, 1000), ] %>%
          mutate(
            bwidth = sim[[.z]][[selector]],
            r = rv,
            infr = ifac
          )
      },
      sim = sim, combs_sep = combs_sep, selector = selector, rv = .x, ifac = .y
    )
  },
  combinations = combinations, sim = sim
) %>%
  Reduce(f = c, x = .) %>%
  do.call("rbind", args = .) %>%
  select(-n, -IDX) %>%
  mutate(N_Year = factor(N_Year)) %>%
  group_by(N_Year, Seasonal_Period, Trend, Season, r, Error_Process, infr) %>%
  group_split()

comb3_names <- rep(NA, length(combinations3))
for(i in 1:length(combinations3)) {
  comb3_names[[i]] <- paste0(
    combinations3[[i]][["N_Year"]][[1]], "_",
    combinations3[[i]][["Seasonal_Period"]][[1]], "_",
    combinations3[[i]][["Trend"]][[1]], "_",
    combinations3[[i]][["Season"]][[1]], "_",
    combinations3[[i]][["Error_Process"]][[1]], "_",
    combinations3[[i]][["r"]][[1]], "_",
    combinations3[[i]][["infr"]][[1]]
  )
}

names(combinations3) <- comb3_names
combinations3 <- combinations3 %>%
  map(function(.x) {
    .x %>%
      select(-N_Year, -Seasonal_Period, -Error_Process, -r, -infr, -Trend, -Season)
  })
# Note: since we have a space limitation for submission to the R Journal,
#       we save one file with only bandwidths and setting indication by
#       column name
all_bwidths <- combinations3 %>%
  do.call("cbind", args = .)
names(all_bwidths) <- names(combinations3)
write.table(
  all_bwidths, file = "sim1_all_bwidths.csv", quote = FALSE, sep = ",",
  row.names = FALSE, col.names = TRUE
)

