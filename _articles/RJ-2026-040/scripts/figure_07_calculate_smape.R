
# Case Study (1) - Base functions
# Calculate sMAPE for model (a) and (b)

# Load libraries --------------------------------------------------------------
library(echos)

# Symmetric Mean Absolute Percentage Error (sMAPE)
smape_vec <- function(truth,
                      estimate,
                      na_rm = TRUE) {
  percent_scale <- 100
  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  mean(numer / denom, na.rm = na_rm) * percent_scale
}

# Forecast horizon
n_ahead <- 12
# Number of observations (total)
n_total <- length(AirPassengers)
# Number of observations (training data)
n_train <- n_total - n_ahead

# Prepare train and test data as numeric vectors
xtrain <- AirPassengers[(1:n_train)]
xtest <- AirPassengers[((n_train+1):n_total)]

# Train ESN models
model_a <- train_esn(y = xtrain)                   # (a) Default setting
model_b <- train_esn(y = xtrain, lambda = c(1, 2)) # (b) High regularization

# Forecast ESN models
fcst_a <- forecast_esn(model_a, n_ahead = n_ahead)
fcst_b <- forecast_esn(model_b, n_ahead = n_ahead)

smape_a <- smape_vec(truth = xtest, estimate = fcst_a$point)
smape_b <- smape_vec(truth = xtest, estimate = fcst_b$point)

round(smape_a, 2)
round(smape_b, 2)
