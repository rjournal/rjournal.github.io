
# Varying sample size

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

# Number of columns for faceting
fig_ncol <- 2

# Forecast horizon
n_ahead <- 18
n_init <- 12
n_step <- 12
n_split <- 10

split_frame <- tibble(
  .id = 1:n_split,
  split = paste0("(", letters[1:n_split], ") ", "T = ", .id*12)
)

# Train ESN model -------------------------------------------------------------

# Slice training data according to split
train_frame <- m4_monthly_subset %>%
  filter(series == "M21655") %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = n_init, .step = n_step) %>%
  filter(.id %in% c(1:n_split))

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
  select(series, .id, type, index, value)

train <- train_frame %>%
  as_tibble() %>%
  mutate(type = "ACTUAL") %>%
  select(series, .id, type, index, value)

train <- bind_rows(train, fitted)

fcst <- fable_frame %>%
  as_tibble() %>%
  mutate(type = "FORECAST") %>%
  select(-value) %>%
  rename(value = .mean) %>%
  select(series, .id, type, index, value)

test <- fcst %>%
  mutate(type = "ACTUAL") %>%
  rename(fcst = value) %>%
  select(series, .id, type, index, fcst)

test <- left_join(
  x = test,
  y = m4_monthly_subset,
  by = c("series", "index")) %>%
  select(series, .id, type, index, value)

model_frame <- bind_rows(train, fitted, test, fcst) %>%
  mutate(index = as.Date(index))

model_frame <- left_join(
  x = model_frame,
  y = split_frame,
  by = ".id"
)

# Forecast accuracy -----------------------------------------------------------
fcst <- left_join(
  x = fcst,
  y = split_frame,
  by = ".id") %>%
  select(-c(type, .id)) %>%
  rename("estimate" = "value")

test <- left_join(
  x = test,
  y = split_frame,
  by = ".id") %>%
  select(-c(type, .id)) %>%
  rename("truth" = "value")

accuracy_frame <- bind_cols(fcst, select(test, truth))

accuracy_frame <- accuracy_frame %>%
  group_by(split) %>%
  summarise(
    value = smape_vec(
      truth = truth, 
      estimate = estimate)) %>%
  ungroup() %>%
  mutate(.split = paste0(split, " (sMAPE = ", round(value, 2), ")")) %>%
  select(-value)

model_frame <- left_join(
  x = model_frame,
  y = accuracy_frame,
  by = "split")


start_date <- model_frame %>%
  filter(type == "FORECAST") %>%
  group_by(.split) %>%
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
  vars(.split),
  ncol = fig_ncol,
  scales = "free")

p <- p + labs(x = "Time")
p <- p + labs(y = "Value")
p <- p + scale_x_date(labels = scales::label_date_short())
