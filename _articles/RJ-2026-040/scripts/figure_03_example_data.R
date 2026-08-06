
# Example time series data

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

# Train ESN model -------------------------------------------------------------

main_frame <- m4_monthly_subset %>%
  filter(series %in% c("M21655", "M21683", "M2717", "M28597"))

# Prepare train and test data
train_frame <- main_frame %>%
  group_by_key() %>%
  filter(row_number() <= n() - n_ahead) %>%
  ungroup()

test_frame <- main_frame %>%
  group_by_key() %>%
  filter(row_number() > n() - n_ahead) %>%
  ungroup()

# Model and forecast
mable_frame <- train_frame %>%
  model("ESN" = ESN(value))

fable_frame <- mable_frame %>%
  forecast(h = n_ahead)

# Prepare data for plotting
fitted <- mable_frame %>%
  fitted() %>%
  as_tibble() %>%
  rename(value = .fitted) %>%
  mutate(type = "FITTED") %>%
  select(series, type, index, value)

train <- train_frame %>%
  as_tibble() %>%
  mutate(type = "ACTUAL") %>%
  select(series, type, index, value)

train <- bind_rows(train, fitted)

fcst <- fable_frame %>%
  as_tibble() %>%
  mutate(type = "FORECAST") %>%
  select(-value) %>%
  rename(value = .mean) %>%
  select(series, type, index, value)

test <- test_frame %>%
  as_tibble() %>%
  mutate(type = "ACTUAL") %>%
  select(series, type, index, value)

model_frame <- bind_rows(train, fitted, test, fcst) %>%
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
  group_by(series) %>%
  summarise(
    value = smape_vec(
      truth = truth, 
      estimate = estimate)) %>%
  ungroup() %>%
  mutate(.series = paste0(series, " (sMAPE = ", round(value, 2), ")")) %>%
  select(-value)

model_frame <- left_join(
  x = model_frame,
  y = accuracy_frame,
  by = "series")


start_date <- model_frame %>%
  filter(type == "FORECAST") %>%
  group_by(.series) %>%
  summarise(index = min(index)) %>%
  ungroup() %>%
  mutate(index = as.Date(index))

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
  vars(.series),
  ncol = fig_ncol,
  scales = "free")

p <- p + labs(x = "Time")
p <- p + labs(y = "Value")
p <- p + scale_x_date(labels = scales::label_date_short())
