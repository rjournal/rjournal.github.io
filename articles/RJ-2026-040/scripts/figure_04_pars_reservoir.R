
# Visualize reservoir (internal states)

# Load libraries --------------------------------------------------------------
library(tidyverse)
library(echos)
library(tsibble)
library(fabletools)

# Number of states
n_states <- 10
# Number of observations
n_obs <- 100
# Number of columns for faceting
fig_ncol <- 2

# Prepare data and train model ------------------------------------------------

# Prepare train data
train_frame <- m4_monthly_subset %>%
  filter(series %in% c("M21655"))

# Train and forecast ESN model
mable_frame <- train_frame %>%
  model(
    "(a) alpha = 1.0" = ESN(value, alpha = 1.0, rho = 1.0),
    "(b) alpha = 0.25" = ESN(value, alpha = 0.25, rho = 1.0),
    "(c) rho = 0.25" = ESN(value, alpha = 1.0, rho = 0.25),
    "(d) rho = 2.0" = ESN(value, alpha = 1.0, rho = 2)
    )

# Extract reservoir and filter
reservoir_frame <- mable_frame %>%
  reservoir() %>%
  filter(index >= 1 & index <= n_obs) %>%
  group_by(series) %>%
  filter(state %in% unique(state)[1:n_states]) %>%
  ungroup()

# Plot data -------------------------------------------------------------------

p <- ggplot()

p <- p + geom_line(
  data = reservoir_frame,
  aes(
    x = index,
    y = value,
    group = state,
    color = state
  ),
  linewidth = 0.5, 
  alpha = 0.5
)

p <- p + facet_wrap(
  vars(model),
  ncol = fig_ncol,
  scales = "free")

p <- p + labs(x = "Index")
p <- p + labs(y = "Value")
p <- p + theme(legend.position = "none")
