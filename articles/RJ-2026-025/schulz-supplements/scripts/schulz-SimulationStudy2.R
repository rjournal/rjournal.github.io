# Paper: "deseats: An R Package for Data-Driven Trend and Seasonality Estimation in Time Series"
# Script: Simulation study 2 for method comparison
# Version: March 28, 2026
# Author: Dominik Schulz

# Author's note: 
# This runs a simulation study that takes a few hours to finish. Therefore,
# this script is not included in the Rmd file of the submission and instead,
# this script saves CSV-files that are being loaded within the
# main R script of the submission.

trends <- c("t1", "t2")
seasons <- c("s1", "s2", "s3")
errors <- c("e1", "e2", "e3")

t1_fun <- function(xt) {
  g_out <- 3.2 * xt + 0.92 * (sin(3.2 * pi * (xt - 0.2)))
  (g_out - mean(g_out)) / sd(g_out)
}

t2_fun <- function(xt) {
  n <- length(xt)
  et <- rnorm(n = n + 200) + 0.05
  
  g_out <- tail(cumsum(et), n)
  (g_out - mean(g_out)) / sd(g_out)
  
}

sfun_s_creator <- function(pse) {
  if (pse == 4) {
    function(t, ps) {
      sx <- rep(-0.39, length(t))
      tm1 <- t - 1
      sx[(tm1 %% ps) == 1] <- 0.01
      sx[(tm1 %% ps) == 2] <- -0.04
      sx[(tm1 %% ps) == 3] <- 0.42
      sx
    }
  } else if (pse == 12) {
    function(t, ps) {
      sx <- rep(-0.39, length(t))
      tm1 <- t - 1
      sx[(tm1 %% ps) == 1] <- 0.05
      sx[(tm1 %% ps) == 2] <- -0.01
      sx[(tm1 %% ps) == 3] <- -0.16
      sx[(tm1 %% ps) == 4] <- -0.31
      sx[(tm1 %% ps) == 5] <- -0.24
      sx[(tm1 %% ps) == 6] <- -0.25
      sx[(tm1 %% ps) == 7] <- -0.1
      sx[(tm1 %% ps) == 8] <- 0.5
      sx[(tm1 %% ps) == 9] <- 0.62
      sx[(tm1 %% ps) == 10] <- 0.34
      sx[(tm1 %% ps) == 11] <- -0.05
      sx
    }
  }
}

s1_fun <- function(t, sig2, ps, sfun_s) {
  sx <- sfun_s(t, ps)
  
  (sx - mean(sx)) / sd(sx)
}

s2_fun <- function(t, sig2, ps, sfun_s) {
  sx <- sfun_s(t, ps)
  
  n <- length(t)
  xt <- (1:n) / n
  sx <- sx * 0.83 * (1 + 0.1 * sin(50 * pi * xt / 9) + 0.2 * cos(3 * pi * xt))
  
  (sx - mean(sx)) / sd(sx)
}

s3_fun <- function(t, sig2, ps, sfun_s) {
  sig <- sqrt(sig2)
  
  n <- length(t)
  m <- 300 * ps
  
  sx <- rep(0, n + m)
  sx[1:(ps - 1)] <- rnorm(n = ps - 1, mean = 0, sd = sig)
  
  for (i in ps:(n + m)) {
    sx[[i]] <- -sum(sx[(i - (ps - 1)):(i - 1)]) + rnorm(n = 1, mean = 0, sd = sig)
  }
  sx <- tail(sx, n)
  
  (sx - mean(sx)) / sd(sx)
  
  
}

e1_fun <- function(n) {
  rnorm(n)
}

e2_fun <- function(n) {
  as.numeric(arima.sim(model = list(ar = 0.75), n = n, sd = sqrt(1 - 0.75^2)))
}

e3_fun <- function(n) {
  b1 <- 1.05
  b2 <- -0.3
  as.numeric(arima.sim(model = list(ar = c(1.05, -0.3)), n = n, sd = sqrt((1 + b2) * ((1 - b2)^2 - b1^2) / (1 - b2))))
}

sim_fun <- function(tf, sf, ep, n_y, ps, alpha, beta, gamma, sig2, seas_fun_s) {
  
  trend_f <- switch(
    tf,
    "t1" = t1_fun,
    "t2" = t2_fun
  )
  
  seas_f <- switch(
    sf,
    "s1" = s1_fun,
    "s2" = s2_fun,
    "s3" = s3_fun
  )
  
  err_f <- switch(
    ep,
    "e1" = e1_fun,
    "e2" = e2_fun,
    "e3" = e3_fun
  )
  n <- n_y * ps
  t <- 1:n
  xt <- t / n
  
  seas_c <- ts(beta * seas_f(t, sig2, ps, seas_fun_s), frequency = ps)
  
  yt <- ts(
    alpha * trend_f(xt) + as.numeric(seas_c) + gamma * err_f(n), 
    frequency = ps
  )
  
  list(obs = yt, seas_comp = seas_c)
  
  
  
}

season_mse_all <- function(yt, s_true, ps) {
  
  `%>%` <- magrittr::`%>%`
  
  season_ds_r1_opt <- function(yt) {
    deseats::season(deseats::deseats(yt, deseats::set_options(order_poly = 1), inflation_rate = "optimal"))
  }
  season_ds_r1_nai <- function(yt) {
    deseats::season(deseats::deseats(yt, deseats::set_options(order_poly = 1), inflation_rate = "naive"))
  }
  season_ds_r3_opt <- function(yt) {
    deseats::season(deseats::deseats(yt, deseats::set_options(order_poly = 3), inflation_rate = "optimal"))
  }
  season_ds_r3_nai <- function(yt) {
    deseats::season(deseats::deseats(yt, deseats::set_options(order_poly = 3), inflation_rate = "naive"))
  }  
  season_x13 <- function(yt) {
    RJDemetra::x13(yt, spec = "RSA3")$final$series[, "s"]   # Omit calendar effects
  }
  season_tramoseats <- function(yt) {
    RJDemetra::tramoseats(yt, spec = "RSA3")$final$series[, "s"]   # Omit calendar effects
  }
  season_stl <- function(yt) {
    forecast::mstl(yt)[, 3]
  }
  season_tbats <- function(yt) {
    forecast::tbats.components(forecast::tbats(yt))[, "season"]
  }
  
  season_ma <- function(yt) {
    stats::decompose(yt, type = "additive")$seasonal
  }
  
  all_funs <- list(
    "DS_r1_opt" = season_ds_r1_opt,
    "DS_r1_nai" = season_ds_r1_nai,
    "DS_r3_opt" = season_ds_r3_opt,
    "DS_r3_nai" = season_ds_r3_nai,
    "X13" = season_x13,
    "TS" = season_tramoseats,
    "STL" = season_stl,
    "TBATS" = season_tbats,
    "MA" = season_ma
  )
  
  all_funs %>%
    purrr::map(
      function(.x, yt, ps) {
        tryCatch({suppressWarnings(.x(yt))},
          error = function(e1) {
            ts(rep(NA, length(yt)), frequency = ps)
          }
        )
      },
      yt = yt, ps = ps
    ) %>%
    purrr::map_dbl(
      function(.x, s_true) {
        mean((.x - s_true)^2)
      },
      s_true = s_true
    )
  
}

collect_season_mse <- function(n_y, ps, trend, season, error, alpha, beta, gamma, sig2) {
  
  M <- 1000  # Number of repititions
    
  mse_vals <- list(
    "DS_r1_opt" = rep(0, M),
    "DS_r1_nai" = rep(0, M),
    "DS_r3_opt" = rep(0, M),
    "DS_r3_nai" = rep(0, M),
    "X13" = rep(0, M),
    "TS" = rep(0, M),
    "STL" = rep(0, M),
    "TBATS" = rep(0, M),
    "MA" = rep(0, M)
  )
  
  seas_fun_s <- sfun_s_creator(ps)
  
  set.seed(1, kind = "Mersenne-Twister")
    
  for (i in 1:M) {
    sim <- sim_fun(
      tf = trend,
      sf = season,
      ep = error,
      n_y = n_y,
      ps = ps,
      sig2 = sig2,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      seas_fun_s = seas_fun_s
    )
    
    results <- season_mse_all(yt = sim$obs, s_true = sim$seas_comp, ps)
    
    mse_vals[["DS_r1_opt"]][[i]] <- results[["DS_r1_opt"]]
    mse_vals[["DS_r1_nai"]][[i]] <- results[["DS_r1_nai"]]
    mse_vals[["DS_r3_opt"]][[i]] <- results[["DS_r3_opt"]]
    mse_vals[["DS_r3_nai"]][[i]] <- results[["DS_r3_nai"]]
    mse_vals[["X13"]][[i]] <- results[["X13"]]
    mse_vals[["TS"]][[i]] <- results[["TS"]]
    mse_vals[["STL"]][[i]] <- results[["STL"]]
    mse_vals[["TBATS"]][[i]] <- results[["TBATS"]]
    mse_vals[["MA"]][[i]] <- results[["MA"]]
    
  }
  
  mse_vals
  
}

library(tidyverse)
library(future)
library(furrr)

n_y <- c(15, 30, 60)
ps <- 12
setup <- expand.grid(
  N_Years = n_y,
  Seasonal_Period = ps,
  Trend = trends,
  Season = seasons,
  Error = errors,
  sig2 = 0.025,
  alpha = 1,
  beta = c(0.4),
  gamma = c(0.1),
  stringsAsFactors = FALSE
)

run_sim <- function(setup) {
  ncores <- max(1, future::availableCores() - 1)
  oldplan <- future::plan()
  
  plan(future::multisession, workers = ncores)
  on.exit({future::plan(oldplan)}, add = TRUE, after = TRUE)
  
  setup <- as.list(setup)
  attributes(setup) <- NULL
  
  out <- furrr::future_pmap(
    setup, function(.ny, .sp, .t, .s, .e, .sig2, .alpha, .beta, .gamma) {
      collect_season_mse(n_y = .ny, ps = .sp, trend = .t, season = .s, error = .e, alpha = .alpha, beta = .beta, gamma = .gamma, sig2 = .sig2)
    },
    .options = furrr::furrr_options(seed = NULL), .progress = TRUE
  )
  
  future::plan(oldplan)
  
  out
  
}

sim_results_s04e01monthly <- run_sim(setup)


#save(sim_results_s04e01monthly, file = "sim2V6s04e01monthly.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Collect NAs

collect_NAs <- function(setup, sim_results) {

  na_collect <- replicate(9, expr = {rep(NA, dim(setup)[[1]])}, simplify = FALSE)
  names(na_collect) <- names(sim_results[[1]])
  na_nams <- names(na_collect)
  for (i in 1:dim(setup)[[1]]) {
    na_collect[[na_nams[[1]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[1]]]]))
    na_collect[[na_nams[[2]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[2]]]]))
    na_collect[[na_nams[[3]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[3]]]]))
    na_collect[[na_nams[[4]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[4]]]]))
    na_collect[[na_nams[[5]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[5]]]]))
    na_collect[[na_nams[[6]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[6]]]]))
    na_collect[[na_nams[[7]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[7]]]]))
    na_collect[[na_nams[[8]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[8]]]]))
    na_collect[[na_nams[[9]]]][[i]] <- sum(is.na(sim_results[[i]][[na_nams[[9]]]]))
  }

  na_collect
  
}

collect_NAs(setup = setup, sim_results = sim_results_s04e01monthly)

write_sim2_tables <- function(sim_results, setup, gamma, seasp) {

  fac <- 1000

  means <- map(
    sim_results,
    function(.x, fac) {
      map_chr(
        .x,
        function(.y, fac) {
          sprintf("%.2f", fac * mean(.y, na.rm = TRUE))
        },
        fac = fac
      )
    },
    fac = fac
  ) %>%
    Reduce(f = rbind, x = .) %>%
    as.data.frame()

  row.names(means) <- NULL
  names(means) <- paste0("Mean_MSE/", names(means))

  SDs <- map(
    sim_results,
    function(.x, fac) {
      map_chr(
        .x,
        function(.y, fac) {
          sprintf("%.2f", fac * sd(.y, na.rm = TRUE))
        },
        fac = fac
      )
    },
    fac = fac
  ) %>%
    Reduce(f = rbind, x = .) %>%
    as.data.frame()

  row.names(SDs) <- NULL
  names(SDs) <- paste0("SD_MSE/", names(SDs))

  df_results <- as.data.frame(
    cbind(setup, means, SDs)
  )

  # Error 1

  df_e1 <- df_results %>%
    filter(Error == "e1") %>%
    select(-Seasonal_Period, -Error, -sig2, -alpha, -beta, -gamma) %>%
    mutate(Trend = factor(Trend), Season = factor(Season)) %>%
    arrange(Season, Trend, N_Years) %>%
    pivot_longer(cols = 4:21, names_to = "Method1", values_to = "Value") %>%
    pivot_wider(id_cols = c("Method1"), names_from = c("Season", "Trend", "N_Years"), names_sep = "-",
              values_from = "Value")

  idx <- rep(NA, 18)
  k <- 0
  l <- 0
  for (i in 1:9) {
    k <- 2 * (i - 1) + 1
    l <- k + 1
    idx[[k]] <- i
    idx[[l]] <- i + 9
  }

  df_e1 <- df_e1[idx, ]
  row.names(df_e1) <- NULL

  df_e1 <- df_e1 %>%
    mutate(Method = map_chr(Method1, function(.x) {str_split_1(.x, "/")[[2]]})) %>%
    mutate(Quantity = map_chr(Method1, function(.x) {str_split_1(.x, "/")[[1]]})) %>%
    relocate(Method, Quantity) %>%
    select(-Method1)%>%
    mutate(Quantity = case_when(
      Quantity == "Mean_MSE" ~ "Mean",
      Quantity == "SD_MSE" ~ "SD"
    )) %>%
    mutate(
      Method = case_when(
        Method == "DS_r1_opt" ~ "$\\text{DS}_{1, \\text{opt}}$",
        Method == "DS_r1_nai" ~ "$\\text{DS}_{1, \\text{nai}}$",
        Method == "DS_r3_opt" ~ "$\\text{DS}_{3, \\text{opt}}$",
        Method == "DS_r3_nai" ~ "$\\text{DS}_{3, \\text{nai}}$",
        .default = Method
      )
    )
  
  df_e1[seq(from = 2, to = 18, by = 2), "Method"] <- ""
  row.names(df_e1) <- NULL

  # Error 2

  df_e2 <- df_results %>%
    filter(Error == "e2") %>%
    select(-Seasonal_Period, -Error, -sig2, -alpha, -beta, -gamma) %>%
    mutate(Trend = factor(Trend), Season = factor(Season)) %>%
    arrange(Season, Trend, N_Years) %>%
    pivot_longer(cols = 4:21, names_to = "Method1", values_to = "Value") %>%
    pivot_wider(id_cols = c("Method1"), names_from = c("Season", "Trend", "N_Years"), names_sep = "-",
              values_from = "Value")

  idx <- rep(NA, 18)
  k <- 0
  l <- 0
  for (i in 1:9) {
    k <- 2 * (i - 1) + 1
    l <- k + 1
    idx[[k]] <- i
    idx[[l]] <- i + 9
  }

  df_e2 <- df_e2[idx, ]
  row.names(df_e2) <- NULL


  df_e2 <- df_e2 %>%
    mutate(Method = map_chr(Method1, function(.x) {str_split_1(.x, "/")[[2]]})) %>%
    mutate(Quantity = map_chr(Method1, function(.x) {str_split_1(.x, "/")[[1]]})) %>%
    relocate(Method, Quantity) %>%
    select(-Method1)%>%
    mutate(Quantity = case_when(
      Quantity == "Mean_MSE" ~ "Mean",
      Quantity == "SD_MSE" ~ "SD"
    )) %>%
    mutate(
      Method = case_when(
        Method == "DS_r1_opt" ~ "$\\text{DS}_{1, \\text{opt}}$",
        Method == "DS_r1_nai" ~ "$\\text{DS}_{1, \\text{nai}}$",
        Method == "DS_r3_opt" ~ "$\\text{DS}_{3, \\text{opt}}$",
        Method == "DS_r3_nai" ~ "$\\text{DS}_{3, \\text{nai}}$",
        .default = Method
      )
    )
  
  df_e2[seq(from = 2, to = 18, by = 2), "Method"] <- ""
  row.names(df_e2) <- NULL

  # Error 3

  df_e3 <- df_results %>%
    filter(Error == "e3") %>%
    select(-Seasonal_Period, -Error, -sig2, -alpha, -beta, -gamma) %>%
    mutate(Trend = factor(Trend), Season = factor(Season)) %>%
    arrange(Season, Trend, N_Years) %>%
    pivot_longer(cols = 4:21, names_to = "Method1", values_to = "Value") %>%
    pivot_wider(id_cols = c("Method1"), names_from = c("Season", "Trend", "N_Years"), names_sep = "-",
              values_from = "Value")

  idx <- rep(NA, 18)
  k <- 0
  l <- 0
  for (i in 1:9) {
    k <- 2 * (i - 1) + 1
    l <- k + 1
    idx[[k]] <- i
    idx[[l]] <- i + 9
  }

  df_e3 <- df_e3[idx, ]
  row.names(df_e3) <- NULL


  df_e3 <- df_e3 %>%
    mutate(Method = map_chr(Method1, function(.x) {str_split_1(.x, "/")[[2]]})) %>%
    mutate(Quantity = map_chr(Method1, function(.x) {str_split_1(.x, "/")[[1]]})) %>%
    relocate(Method, Quantity) %>%
    select(-Method1)%>%
    mutate(Quantity = case_when(
      Quantity == "Mean_MSE" ~ "Mean",
      Quantity == "SD_MSE" ~ "SD"
    )) %>%
    mutate(
      Method = case_when(
        Method == "DS_r1_opt" ~ "$\\text{DS}_{1, \\text{opt}}$",
        Method == "DS_r1_nai" ~ "$\\text{DS}_{1, \\text{nai}}$",
        Method == "DS_r3_opt" ~ "$\\text{DS}_{3, \\text{opt}}$",
        Method == "DS_r3_nai" ~ "$\\text{DS}_{3, \\text{nai}}$",
        .default = Method
      )
    )
  
  df_e3[seq(from = 2, to = 18, by = 2), "Method"] <- ""
  row.names(df_e3) <- NULL

  # Highlight smallest MSEs

  for (i in 1:(dim(df_e1)[[2]] - 2)) {
    mse_sel <- seq(from = 1, to = 17, by = 2)
    wm <- which.min(as.numeric(df_e1[mse_sel, i + 2, drop = TRUE]))
    min_v <- df_e1[mse_sel[[wm]], i + 2][[1]]
    w_mse_sel <- mse_sel[which(df_e1[mse_sel, i + 2, drop = TRUE] == min_v)]
    df_e1[w_mse_sel, i + 2] <- paste0("\\bftab ", min_v)
  }

  for (i in 1:(dim(df_e2)[[2]] - 2)) {
    mse_sel <- seq(from = 1, to = 17, by = 2)
    wm <- which.min(as.numeric(df_e2[mse_sel, i + 2, drop = TRUE]))
    min_v <- df_e2[mse_sel[[wm]], i + 2][[1]]
    w_mse_sel <- mse_sel[which(df_e2[mse_sel, i + 2, drop = TRUE] == min_v)]
    df_e2[w_mse_sel, i + 2] <- paste0("\\bftab ", min_v)
  }

  for (i in 1:(dim(df_e3)[[2]] - 2)) {
    mse_sel <- seq(from = 1, to = 17, by = 2)
    wm <- which.min(as.numeric(df_e3[mse_sel, i + 2, drop = TRUE]))
    min_v <- df_e3[mse_sel[[wm]], i + 2][[1]]
    w_mse_sel <- mse_sel[which(df_e3[mse_sel, i + 2, drop = TRUE] == min_v)]
    df_e3[w_mse_sel, i + 2] <- paste0("\\bftab ", min_v)
  }

  sp_s <- switch(
    as.character(seasp),
    "4" = "quarterly",
    "12" = "monthly"
  )
  gamm_s <- switch(
    as.character(gamma),
    "0.1" = "01",
    "0.2" = "02"
  )
  
  
  write.table(df_e1, file = paste0("sim2_error1_gamma", gamm_s, "_", sp_s, ".csv"), quote = TRUE,
            sep = ",", col.names = TRUE, row.names = FALSE)
  write.table(df_e2, file = paste0("sim2_error2_gamma", gamm_s, "_", sp_s, ".csv"), quote = TRUE,
            sep = ",", col.names = TRUE, row.names = FALSE)
  write.table(df_e3, file = paste0("sim2_error3_gamma", gamm_s, "_", sp_s, ".csv"), quote = TRUE,
            sep = ",", col.names = TRUE, row.names = FALSE)

}

write_sim2_tables(sim_results = sim_results_s04e01monthly, setup = setup, 0.1, 12)

rm(sim_results_s04e01monthly)

#######################################

n_y <- c(15, 30, 60)
ps <- 4
setup <- expand.grid(
  N_Years = n_y,
  Seasonal_Period = ps,
  Trend = trends,
  Season = seasons,
  Error = errors,
  sig2 = 0.025,
  alpha = 1,
  beta = c(0.4),
  gamma = c(0.1),
  stringsAsFactors = FALSE
)

sim_results_s04e01quarterly <- run_sim(setup)

collect_NAs(setup = setup, sim_results = sim_results_s04e01quarterly)

write_sim2_tables(sim_results = sim_results_s04e01quarterly, setup = setup, 0.1, 4)

rm(sim_results_s04e01quarterly)

#######################################

n_y <- c(15, 30, 60)
ps <- 12
setup <- expand.grid(
  N_Years = n_y,
  Seasonal_Period = ps,
  Trend = trends,
  Season = seasons,
  Error = errors,
  sig2 = 0.025,
  alpha = 1,
  beta = c(0.4),
  gamma = c(0.2),
  stringsAsFactors = FALSE
)

sim_results_s04e02monthly <- run_sim(setup)

collect_NAs(setup = setup, sim_results = sim_results_s04e02monthly)

write_sim2_tables(sim_results = sim_results_s04e02monthly, setup = setup, 0.2, 12)

rm(sim_results_s04e02monthly)

#######################################

n_y <- c(15, 30, 60)
ps <- 4
setup <- expand.grid(
  N_Years = n_y,
  Seasonal_Period = ps,
  Trend = trends,
  Season = seasons,
  Error = errors,
  sig2 = 0.025,
  alpha = 1,
  beta = c(0.4),
  gamma = c(0.2),
  stringsAsFactors = FALSE
)

sim_results_s04e02quarterly <- run_sim(setup)

collect_NAs(setup = setup, sim_results = sim_results_s04e02quarterly)

write_sim2_tables(sim_results = sim_results_s04e02quarterly, setup = setup, 0.2, 4)

rm(sim_results_s04e02quarterly)
