
# Synthetic time series data

# Load libraries --------------------------------------------------------------
library(tidyverse)
library(echos)

# Parameters ------------------------------------------------------------------
# Number of observation (total)
n_obs <- 200
# Forecast horizon
n_ahead <- 25
# Number of training data
n_train <- n_obs - n_ahead
# Number of columns for faceting
fig_ncol <- 2
# Number of variables
n_vars <- length(unique(synthetic_data[["variable"]]))

# Prepare dataset -------------------------------------------------------------
synthetic_actual <- synthetic_data %>%
  mutate(type = "ACTUAL") %>%
  mutate(variable = paste0("(", rep(letters[1:n_vars], each = n_obs), ") ", variable))

# Extract adjusted variable names
xvariable <- unique(synthetic_actual[["variable"]])

# Prepare train and test data
synthetic_train <- synthetic_actual %>%
  group_by(variable) %>%
  filter(between(row_number(), 1, n() - n_ahead)) %>%
  ungroup()

synthetic_test <- synthetic_actual %>%
  group_by(variable) %>%
  filter(between(row_number(), n() - n_ahead + 1, n())) %>%
  ungroup()


# Loop over variables
synthetic_frame <- map_dfr(
  .x = 1:n_vars,
  .f = ~{
    xtrain <- synthetic_train %>%
      filter(variable == xvariable[.x]) %>%
      pull(value)
    
    # Train and forecast ESN model
    xmodel <- train_esn(y = xtrain)
    xfcst <- forecast_esn(xmodel, n_ahead = n_ahead)
    
    xfitted <- tibble(
      variable = xvariable[.x],
      type = "FITTED",
      index = 1:n_train,
      value = xmodel$fitted
    )
    
    xfcst <- tibble(
      variable = xvariable[.x],
      type = "FORECAST",
      index = ((n_train + 1):n_obs),
      value = xfcst$point
    )
    
    xoutput <- bind_rows(
      xfitted,
      xfcst
    )
  }
)

# Collect all data row-wise
signal_frame <- bind_rows(
  synthetic_actual,
  synthetic_frame
)

# Plot data -------------------------------------------------------------------

p <- ggplot()

p <- p + geom_line(
  data = signal_frame,
  aes(
    x = index,
    y = value,
    color = type,
    size = type
  )
)

p <- p + scale_color_manual(values = c("grey35", "orange", "steelblue"))
p <- p + scale_size_manual(values = c(0.5, 0.25, 0.5))

p <- p + geom_vline(
  xintercept = n_train,
  linetype = "dotted",
  color = "grey35",
  linewidth = 0.5
)

p <- p + facet_wrap(
  ~factor(
    variable, 
    levels = c(
      "(a) Square Wave",
      "(b) Sawtooth Wave",
      "(c) Harmonic Wave",
      "(d) Harmonic Wave w/ Trend",
      "(e) Amplitude Modulated Wave",
      "(f) Frequency Modulated Wave",
      "(g) AR(1) Process",
      "(h) MA(2) Process",
      "(i) White Noise Process",
      "(j) Random Walk Process"
    )),
  ncol = fig_ncol,
  scales = "free")

p <- p + labs(x = "Time")
p <- p + labs(y = "Value")



# # Configuration ---------------------------------------------------------------
# 
# amplitude <- 1                      # Amplitude
# period <- 20                        # Period
# n_obs <- 200                        # Number of observations
# index <- 1:n_obs                    # Discrete time index
# n_ahead <- 25                       # forecast horizon
# n_train <- n_obs - n_ahead          # Number of obs. for training
# ar_coef <- c(0.6)                   # AR coefficient
# ma_coef <- c(0.5, 0.4)              # MA coefficients
# n_seed <- 123                       # For reproducibility
# 
# # Number of columns for faceting
# fig_ncol <- 2
# 
# # Square Wave -----------------------------------------------------------------
# 
# # Signal name
# signal_name <- "(a) Square Wave"
# 
# # Create data
# square_signal <- amplitude * sign(sin(2 * pi * index / period))
# 
# # Prepare train and test data
# square_train <- square_signal[(1:n_train)]
# square_test <- square_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# square_model <- train_esn(y = square_train)
# square_fcst <- forecast_esn(square_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(square_fcst, test = square_test)
# 
# square_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = square_signal
# )
# 
# square_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = square_model$fitted
# )
# 
# square_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = square_fcst$point
# )
# 
# square_data <- bind_rows(
#   square_actual,
#   square_fitted,
#   square_fcst
# )
# 
# 
# # Sawtooth Wave ---------------------------------------------------------------
# 
# # Signal name
# signal_name <- "(b) Sawtooth Wave"
# 
# # Create data
# sawtooth_signal <- 2 * amplitude * (index / period - floor(index / period)) - amplitude
# 
# # Prepare train and test data
# sawtooth_train <- sawtooth_signal[(1:n_train)]
# sawtooth_test <- sawtooth_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# sawtooth_model <- train_esn(y = sawtooth_train)
# sawtooth_fcst <- forecast_esn(sawtooth_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(sawtooth_fcst, test = sawtooth_test)
# 
# 
# sawtooth_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = sawtooth_signal
# )
# 
# sawtooth_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = sawtooth_model$fitted
# )
# 
# sawtooth_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = sawtooth_fcst$point
# )
# 
# sawtooth_data <- bind_rows(
#   sawtooth_actual,
#   sawtooth_fitted,
#   sawtooth_fcst
# )
# 
# 
# # Harmonic Wave (Combination of Sine and Cosine Waves) ------------------------
# 
# # Signal name
# signal_name <- "(c) Harmonic Wave"
# 
# # Create data
# harmonic_signal <- sin(0.5 * index) + cos(0.75 * index)
# 
# # Prepare train and test data
# harmonic_train <- harmonic_signal[(1:n_train)]
# harmonic_test <- harmonic_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# harmonic_model <- train_esn(y = harmonic_train)
# harmonic_fcst <- forecast_esn(harmonic_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(harmonic_fcst, test = harmonic_test)
# 
# 
# harmonic_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = harmonic_signal
# )
# 
# harmonic_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = harmonic_model$fitted
# )
# 
# harmonic_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = harmonic_fcst$point
# )
# 
# harmonic_data <- bind_rows(
#   harmonic_actual,
#   harmonic_fitted,
#   harmonic_fcst
# )
# 
# 
# # Harmonic Wave plus Trend (Combination of Sine and Cosine Waves) -------------
# 
# # Signal name
# signal_name <- "(d) Harmonic Wave w/ Trend"
# 
# # Create data
# trend_signal <- (index/100) + sin(0.5 * index) + cos(0.75 * index)
# 
# # Prepare train and test data
# trend_train <- trend_signal[(1:n_train)]
# trend_test <- trend_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# trend_model <- train_esn(y = trend_train)
# trend_fcst <- forecast_esn(trend_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(trend_fcst, test = trend_test)
# 
# 
# trend_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = trend_signal
# )
# 
# trend_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = trend_model$fitted
# )
# 
# trend_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = trend_fcst$point
# )
# 
# trend_data <- bind_rows(
#   trend_actual,
#   trend_fitted,
#   trend_fcst
# )
# 
# # Amplitude Modulated (AM) Wave -----------------------------------------------
# 
# # Signal name
# signal_name <- "(e) Amplitude Modulated Wave"
# 
# # Create data
# # amplitude_signal <- (1 + 0.02 * index) * sin(2 * pi * index / period)
# amplitude_signal <- (1 + 0.02 * index) * (sin(0.5 * index) + cos(0.75 * index))
# 
# # Prepare train and test data
# amplitude_train <- amplitude_signal[(1:n_train)]
# amplitude_test <- amplitude_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# amplitude_model <- train_esn(y = amplitude_train)
# amplitude_fcst <- forecast_esn(amplitude_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(amplitude_fcst, test = amplitude_test)
# 
# 
# amplitude_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = amplitude_signal
# )
# 
# amplitude_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = amplitude_model$fitted
# )
# 
# amplitude_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = amplitude_fcst$point
# )
# 
# amplitude_data <- bind_rows(
#   amplitude_actual,
#   amplitude_fitted,
#   amplitude_fcst
# )
# 
# 
# # Frequency Modulated (FM) Wave -----------------------------------------------
# 
# # Signal name
# signal_name <- "(f) Frequency Modulated Wave"
# 
# # Create data
# frequency_signal <- numeric(n_obs)
# 
# for (i in 1:n_obs) {
#   if (i <= n_obs / 2) {
#     frequency_signal[i] <- sin(0.25 * (i - 1)) + cos(0.375 * (i - 1))
#   } else {
#     frequency_signal[i] <- sin(0.5 * (i - 1)) + cos(0.75 * (i - 1))
#   }
# }
# 
# 
# # Prepare train and test data
# frequency_train <- frequency_signal[(1:n_train)]
# frequency_test <- frequency_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# frequency_model <- train_esn(y = frequency_train)
# frequency_fcst <- forecast_esn(frequency_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(frequency_fcst, test = frequency_test)
# 
# 
# frequency_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = frequency_signal
# )
# 
# frequency_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = frequency_model$fitted
# )
# 
# frequency_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = frequency_fcst$point
# )
# 
# frequency_data <- bind_rows(
#   frequency_actual,
#   frequency_fitted,
#   frequency_fcst
# )
# 
# 
# # AR(1) Process ---------------------------------------------------------------
# 
# # Signal name
# signal_name <- "(g) AR(1) Process"
# 
# # Create data
# set.seed(n_seed)
# ar_signal <- arima.sim(model = list(ar = ar_coef), n = n_obs)
# 
# # Prepare train and test data
# ar_train <- ar_signal[(1:n_train)]
# ar_test <- ar_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# ar_model <- train_esn(y = ar_train)
# ar_fcst <- forecast_esn(ar_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(ar_fcst, test = ar_test)
# 
# ar_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = ar_signal
# )
# 
# ar_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = ar_model$fitted
# )
# 
# ar_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = ar_fcst$point
# )
# 
# ar_data <- bind_rows(
#   ar_actual,
#   ar_fitted,
#   ar_fcst
# )
# 
# # MA(2) Process ---------------------------------------------------------------
# 
# # Signal name
# signal_name <- "(h) MA(2) Process"
# 
# # Create data
# set.seed(n_seed)
# ma_signal <- arima.sim(model = list(ma = ma_coef), n = n_obs)
# 
# # Prepare train and test data
# ma_train <- ma_signal[(1:n_train)]
# ma_test <- ma_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# ma_model <- train_esn(y = ma_train)
# ma_fcst <- forecast_esn(ma_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(ma_fcst, test = ma_test)
# 
# ma_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = ma_signal
# )
# 
# ma_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = ma_model$fitted
# )
# 
# ma_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = ma_fcst$point
# )
# 
# ma_data <- bind_rows(
#   ma_actual,
#   ma_fitted,
#   ma_fcst
# )
# 
# 
# # White noise -----------------------------------------------------------------
# 
# # Signal name
# signal_name <- "(i) White Noise Process"
# 
# # Create data
# set.seed(n_seed)
# noise_signal <- rnorm(n = n_obs, mean = 0, sd = 1)
# 
# # Prepare train and test data
# noise_train <- noise_signal[(1:n_train)]
# noise_test <- noise_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# noise_model <- train_esn(y = noise_train)
# noise_fcst <- forecast_esn(noise_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(noise_fcst, test = noise_test)
# 
# 
# noise_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = noise_signal
# )
# 
# noise_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = noise_model$fitted
# )
# 
# noise_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = noise_fcst$point
# )
# 
# noise_data <- bind_rows(
#   noise_actual,
#   noise_fitted,
#   noise_fcst
# )
# 
# 
# # Random Walk Process ---------------------------------------------------------
# 
# # Signal name
# signal_name <- "(j) Random Walk Process"
# 
# # Create data
# set.seed(n_seed)
# eps <- rnorm(n_obs)   # White noise
# rw_signal <- cumsum(eps)  # X_t = X_{t-1} + eps_t
# 
# # Prepare train and test data
# rw_train <- rw_signal[(1:n_train)]
# rw_test <- rw_signal[((n_train+1):n_obs)]
# 
# # Train and forecast ESN model
# rw_model <- train_esn(y = rw_train)
# rw_fcst <- forecast_esn(rw_model, n_ahead = n_ahead)
# 
# # Plot result
# # plot(rw_fcst, test = rw_test)
# 
# 
# rw_actual <- tibble(
#   variable = signal_name,
#   type = "ACTUAL",
#   index = index,
#   value = rw_signal
# )
# 
# rw_fitted <- tibble(
#   variable = signal_name,
#   type = "FITTED",
#   index = 1:n_train,
#   value = rw_model$fitted
# )
# 
# rw_fcst <- tibble(
#   variable = signal_name,
#   type = "FORECAST",
#   index = ((n_train + 1):n_obs),
#   value = rw_fcst$point
# )
# 
# rw_data <- bind_rows(
#   rw_actual,
#   rw_fitted,
#   rw_fcst
# )
# 
# 
# # Collect all data row-wise
# signal_frame <- bind_rows(
#   square_data,
#   sawtooth_data,
#   harmonic_data,
#   trend_data,
#   amplitude_data,
#   frequency_data,
#   ar_data,
#   ma_data,
#   noise_data,
#   rw_data
# )
# 
# # Plot data -------------------------------------------------------------------
# 
# p <- ggplot()
# 
# p <- p + geom_line(
#   data = signal_frame,
#   aes(
#     x = index,
#     y = value,
#     color = type,
#     size = type
#   )
# )
# 
# p <- p + scale_color_manual(values = c("grey35", "#F8766D", "#00BFC4"))
# p <- p + scale_size_manual(values = c(0.5, 0.5, 1.0))
# 
# p <- p + geom_vline(
#   xintercept = n_train,
#   linetype = "dotted",
#   color = "grey35",
#   linewidth = 0.5
# )
# 
# p <- p + facet_wrap(
#   ~factor(
#     variable, 
#     levels = c(
#       "(a) Square Wave",
#       "(b) Sawtooth Wave",
#       "(c) Harmonic Wave",
#       "(d) Harmonic Wave w/ Trend",
#       "(e) Amplitude Modulated Wave",
#       "(f) Frequency Modulated Wave",
#       "(g) AR(1) Process",
#       "(h) MA(2) Process",
#       "(i) White Noise Process",
#       "(j) Random Walk Process"
#     )),
#   ncol = fig_ncol,
#   scales = "free")
# 
# p <- p + labs(x = "Time")
# p <- p + labs(y = "Value")

