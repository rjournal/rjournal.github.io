
# Visualize reservoir (internal states)

# Load libraries --------------------------------------------------------------
library(tidyverse)
library(echos)
library(tsibble)
library(fabletools)

# Symmetric Mean Absolute Percentage Error (sMAPE)
smape_vec <- function(truth,
                      estimate,
                      na_rm = TRUE) {
  percent_scale <- 100
  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  mean(numer / denom, na.rm = na_rm) * percent_scale
}

# Configuration ---------------------------------------------------------------

# Forecast horizon
n_ahead <- 18
# Number of columns for faceting
fig_ncol <- 2

# Table with hyperparameters
pars <- tibble(
  id = c("(a) alpha = 1.0", "(b) alpha = 0.25", "(c) rho = 0.25", "(d) rho = 2.0"),
  alpha = c(1.0, 0.25, 1.0, 1.0),
  rho = c(1.0, 1.0, 0.25, 2.0)
)

# Prepare data and train model ------------------------------------------------

# Prepare train and test data
main_frame <- m4_monthly_subset %>%
  filter(series %in% c("M21655"))

train_frame <- main_frame %>%
  group_by_key() %>%
  filter(row_number() <= n() - n_ahead) %>%
  ungroup()

test_frame <- main_frame %>%
  group_by_key() %>%
  filter(row_number() > n() - n_ahead) %>%
  ungroup()

# Train ESN models
mable_frame <- map(
  .x = 1:nrow(pars),
  .f = ~{
    train_frame %>%
      model(
        !!(pars[["id"]][.x]) := ESN(
          value, 
          alpha = pars[["alpha"]][.x], 
          rho = pars[["rho"]][.x]
        )
      )
  }
)

# Forecast ESN models
fable_frame <- map(
  .x = 1:nrow(pars),
  .f = ~{
    mable_frame[[.x]] %>%
      forecast(h = n_ahead)
  }
)

# Extract fitted values and bind row-wise
fitted <- map_dfr(
  .x = 1:nrow(pars),
  .f = ~{
    mable_frame[[.x]] %>%
      fitted() %>%
      as_tibble() %>%
      rename(value = .fitted) %>%
      mutate(type = "FITTED") %>%
      select(series, .model, type, index, value)
  }
)

# Create training data and bind row-wise
train <- map_dfr(
  .x = 1:nrow(pars),
  .f = ~{
    train_frame %>%
      as_tibble() %>%
      mutate(type = "ACTUAL") %>%
      mutate(.model = pars[["id"]][.x]) %>%
      select(series, .model, type, index, value)
  }
)

train <- bind_rows(train, fitted)

# Extract forecasts and bind row-wise
fcst <- map_dfr(
  .x = 1:nrow(pars),
  .f = ~{
    fable_frame[[.x]] %>%
      as_tibble() %>%
      mutate(type = "FORECAST") %>%
      select(-value) %>%
      rename(value = .mean) %>%
      select(series, .model, type, index, value)
  }
)

# Create test data and bind row-wise
test <- map_dfr(
  .x = 1:nrow(pars),
  .f = ~{
    test_frame %>%
      as_tibble() %>%
      mutate(type = "ACTUAL") %>%
      mutate(.model = pars[["id"]][.x]) %>%
      select(series, .model, type, index, value)
  }
)

# Bind all data row-wise for plotting
model_frame <- bind_rows(train, fitted, test, fcst) %>%
  mutate(index = as.Date(index))

# Prepare start data of forecasts
start_date <- model_frame %>%
  filter(type == "FORECAST") %>%
  group_by(series) %>%
  summarise(index = min(index)) %>%
  ungroup() %>%
  mutate(index = as.Date(index))

# Forecast accuracy -----------------------------------------------------------
fcst <- fcst %>%
  select(-type) %>%
  rename("estimate" = "value")

test <- test %>%
  select(-type) %>%
  rename("truth" = "value")

accuracy_frame <- bind_cols(fcst, select(test, truth))

accuracy_frame <- accuracy_frame %>%
  group_by(.model) %>%
  summarise(
    value = smape_vec(
      truth = truth, 
      estimate = estimate)) %>%
  ungroup() %>%
  mutate(model = paste0(.model, " (sMAPE = ", round(value, 2), ")")) %>%
  select(-value)

model_frame <- left_join(
  x = model_frame,
  y = accuracy_frame,
  by = ".model")


# Plot data -------------------------------------------------------------------
p <- ggplot()

p <- p + geom_line(
  data = model_frame,
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
  data = start_date,
  aes(xintercept = index), 
  linetype = "dotted", 
  color = "grey35", 
  linewidth = 0.5
)

p <- p + facet_wrap(
  vars(model),
  ncol = fig_ncol,
  scales = "free")

p <- p + labs(x = "Time")
p <- p + labs(y = "Value")
p <- p + scale_x_date(labels = scales::label_date_short())
