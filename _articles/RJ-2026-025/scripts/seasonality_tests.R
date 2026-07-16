# QS-Test

qs_test_internal <- function(yt, k = 2) {
  seas_period <- stats::frequency(yt)
  rho <- as.numeric(stats::acf(as.numeric(yt), type = "correlation", plot = FALSE,
                    lag.max = k * seas_period)$acf)[-1]
  
  n <- length(yt)
  
  taus <- seq(from = seas_period, to = k * seas_period, by = seas_period)
  
  statistic <- n * (n + 2) * sum(max(0, rho[taus])^2 / (n - taus))
  
  p_val <- 1 - pchisq(statistic, df = k)
  
  list(test_statistic = statistic, p_val = p_val, k = k)
  
}

qs_test <- function(yt, k_max) {
  lapply(1:k_max, FUN = function(.x, yt) {
    qs_test_internal(yt = yt, k = .x)
  }, yt = yt)
}

# Friedman test

fried_test <- function(yt) {
  
  frequ <- frequency(yt)
  start_point <- start(yt)
  end_point <- end(yt)
  
  first_year <- start_point[[1]]
  last_year <- end_point[[1]]
  n_years <- last_year - (first_year - 1)
  
  y_mat <- matrix(rep(NA, n_years * frequ), ncol = frequ, nrow = n_years)
  
  first_entry <- start_point[[2]]
  
  l_first <- length(first_entry:frequ)
  
  yt <- as.numeric(yt)
  y_mat[1, first_entry:frequ] <- yt[1:l_first]
  for (i in 2:(n_years - 1)) {
    y_mat[i, 1:frequ] <- yt[(l_first + (i - 2) * frequ + 1):(l_first + (i - 2) * frequ + frequ)]
  }
  
  start_last <- (l_first + (i - 2) * frequ + frequ) + 1
  n <- length(yt)
  
  len_last <- length(start_last:n)
  y_mat[n_years, 1:len_last] <- yt[start_last:n]
  
  r_mat <- y_mat
  
  for (i in 1:n_years) {
    r_mat[i, ] <- rank(y_mat[i, ])
  }
  
  if (any(is.na(r_mat[n_years, ]))) {r_mat <- r_mat[-n_years, ]}
  if (any(is.na(r_mat[1, ]))) {r_mat <- r_mat[-1, ]}
  
  # Update number of full years
  n_years <- length(r_mat[, 1])
  
  mean_r <- mean(r_mat)
  
  SS_t <- 0
  
  for (i in 1:frequ) {
    SS_t <- SS_t + (mean(r_mat[, i]) - mean_r)^2
  }
  SS_t <- n_years * SS_t
  
  SS_e <- sum((r_mat - mean_r)^2) / (n_years * (frequ - 1))
  
  Q <- SS_t / SS_e
  
  list(test_statistic = Q, p_val = 1 - pchisq(q = Q, df = frequ - 1))
  
  
}

