### SIMULATIONS ######

#### CLASSIC #######

# -------------------------------
# 1. Load Necessary Libraries
# -------------------------------

library(DisaggregateTS)
library(xts)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(moments)

# -------------------------------
# 2. Data Preparation
# -------------------------------

set.seed(27)

# Define parameters
n_l <- 17    # Number of low-frequency data points
n <- 68      # Number of high-frequency (quarterly) data points
p_sim <- 5   # Number of high-frequency exogenous variables
rho_sim <- 0.8 # Autocorrelation parameter

# Generate simulated data using TempDisaggDGP
Sim_data <- TempDisaggDGP(n_l, n, aggRatio = 4, p = p_sim, rho = rho_sim)

# Extract simulated observations
Y_sim <- matrix(Sim_data$Y_Gen)         # Low-frequency simulated observations
Y_sim_HF_obs <- matrix(Sim_data$y_Gen)  # High-frequency simulated observations
X_sim <- Sim_data$X_Gen                  # Exogenous high-frequency variables

# Perform temporal disaggregation using Chow-Lin method
C_sparse_SIM <- disaggregate(Y_sim, X_sim, aggMat = "sum", aggRatio = 4, method = "Chow-Lin")
Y_HF_SIM <- C_sparse_SIM$y_Est[,1]      # Temporal disaggregated observations

# -------------------------------
# 3. Extract Dates from Excel Files
# -------------------------------

# Read a single CSV file
Data_X <- read.csv("../data/Exogenous_variables_IBM.csv", stringsAsFactors = FALSE)
Data_Y <- read.csv("../data/Total_emissions.csv", stringsAsFactors = FALSE)

# Extract relevant date ranges
Dates <- Data_Y$Dates[7:23]              # Low-frequency dates
Dates_Q <- Data_X$Dates[24:91]           # High-frequency dates

Dates <- as.Date(Dates, format = "%m/%d/%Y")  # convert character to Date
Dates_Q <- as.Date(Dates_Q, format = "%m/%d/%Y")
# -------------------------------
# 4. Proportional (Linear) Interpolation
# -------------------------------

# Create xts objects for interpolation
Y_temp <- xts(Y_sim / 4, order.by = Dates) # Divide by 4 as per aggregation ratio

# Merge low-frequency and high-frequency dates
Y_temp2 <- merge(Y_temp, xts(rep(NA, length(Dates_Q)), order.by = Dates_Q))

# Perform linear interpolation to fill NA values
Y_HF_approx <- as.numeric(na.approx(Y_temp2[,1], rule = 2)) # Ensure it's a numeric vector

# -------------------------------
# 5. Prepare Data for ggplot2
# -------------------------------

# Create a data frame combining all series
plot_data <- data.frame(
  Date = Dates_Q,
  Temporal_Disaggregated = Y_HF_SIM,
  High_Frequency = Y_sim_HF_obs,
  Interpolated = Y_HF_approx
)

# Convert to long format suitable for ggplot2
plot_long <- pivot_longer(
  plot_data,
  cols = c("Temporal_Disaggregated", "High_Frequency", "Interpolated"),
  names_to = "Series",
  values_to = "Value"
)

# -------------------------------
# 6. Rename Series Labels to Remove Underscores
# -------------------------------

# Use dplyr's mutate and recode to rename Series labels
plot_long <- plot_long %>%
  mutate(Series = recode(Series,
                         "Temporal_Disaggregated" = "Temporal Disaggregated",
                         "High_Frequency" = "High-Frequency",
                         "Interpolated" = "Interpolated"))

# -------------------------------
# 7. Create the ggplot2 Plot
# -------------------------------

ggplot(plot_long, aes(x = Date, y = Value, color = Series, linetype = Series)) +
  geom_line(size = 0.5) +       # Adjust line thickness for better visibility
  geom_point(data = subset(plot_long, Series == "Temporal Disaggregated"), size = 1.5) +      # Adjust point size
  labs(
    title = "Classical Setting",
    x = "Time",
    y = ""  # Empty y-axis label as per original code
  ) +
  theme_minimal(base_size = 14) +  # Apply a clean minimal theme with larger base font size
  theme(
    panel.grid.major = element_blank(),    # Remove major gridlines
    panel.grid.minor = element_blank(),    # Remove minor gridlines
    legend.position = "top",               # Position legend at the top
    legend.title = element_blank(),        # Remove legend title
    legend.text = element_text(size = 12), # Increase legend text size for readability
    plot.title = element_text(
      hjust = 0.5, size = 16, face = "bold" # Center and style the plot title
    ),
    axis.text = element_text(size = 12),   # Increase axis text size
    axis.title = element_text(size = 14)   # Increase axis title size
  ) +
  scale_color_manual(
    values = c(
      "High-Frequency" = "red",
      "Interpolated" = "blue",
      "Temporal Disaggregated" = "black"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "High-Frequency" = "solid",
      "Interpolated" = "dashed",
      "Temporal Disaggregated" = "solid"
    )
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),    # Arrange legend items in one row
    linetype = guide_legend(nrow = 1, byrow = TRUE) # Ensure linetype legend aligns with color
  )


############################################################################


#### HIGH-DIMENSIONAL

# -------------------------------
# 1. Load Necessary Libraries
# -------------------------------

# -------------------------------
# 2. Data Preparation
# -------------------------------

set.seed(27)

# Define parameters
n_l <- 17    # Number of low-frequency data points (annual)
n <- 68      # Number of high-frequency data points (quarterly)
p_sim <- 100 # Number of high-frequency exogenous variables
rho_sim <- 0.8 # Autocorrelation parameter

# Generate simulated data using TempDisaggDGP
Sim_data <- TempDisaggDGP(n_l, n, aggRatio = 4, p = p_sim, rho = rho_sim)

# Extract simulated observations
Y_sim <- matrix(Sim_data$Y_Gen)            # Low-frequency simulated observations
Y_sim_HF_obs <- matrix(Sim_data$y_Gen)     # High-frequency simulated observations
X_sim <- Sim_data$X_Gen                     # Exogenous high-frequency variables

# Perform temporal disaggregation using adaptive-spTD method
C_sparse_SIM <- disaggregate(Y_sim, X_sim, aggMat = "sum", aggRatio = 4, method = "adaptive-spTD")
Y_HF_SIM <- C_sparse_SIM$y_Est[,1]         # Temporal disaggregated observations

# -------------------------------
# 3. Extract Dates from Excel Files
# -------------------------------

# Read CSV files
Data_X <- read.csv("../data/Exogenous_variables_IBM.csv", stringsAsFactors = FALSE)
Data_Y <- read.csv("../data/Total_emissions.csv", stringsAsFactors = FALSE)

# Extract relevant date ranges
Dates <- Data_Y$Dates[7:23]                # Low-frequency dates
Dates_Q <- Data_X$Dates[24:91]             # High-frequency dates

Dates <- as.Date(Dates, format = "%m/%d/%Y")  # convert character to Date
Dates_Q <- as.Date(Dates_Q, format = "%m/%d/%Y")

# -------------------------------
# 4. Proportional (Linear) Interpolation
# -------------------------------

# Create xts objects for interpolation
Y_temp <- xts(Y_sim / 4, order.by = Dates) # Divide by 4 as per aggregation ratio

# Merge low-frequency and high-frequency dates
Y_temp2 <- merge(Y_temp, xts(rep(NA, length(Dates_Q)), order.by = Dates_Q))

# Perform linear interpolation to fill NA values
Y_HF_approx <- as.numeric(na.approx(Y_temp2[,1], rule = 2)) # Ensure it's a numeric vector

# -------------------------------
# 5. Prepare Data for ggplot2
# -------------------------------

# Create a data frame combining all series
plot_data <- data.frame(
  Date = Dates_Q,
  Temporal_Disaggregated = Y_HF_SIM,
  High_Frequency = Y_sim_HF_obs,
  Interpolated = Y_HF_approx
)

# Convert to long format suitable for ggplot2
plot_long <- pivot_longer(
  plot_data,
  cols = c("Temporal_Disaggregated", "High_Frequency", "Interpolated"),
  names_to = "Series",
  values_to = "Value"
)

# -------------------------------
# 6. Rename Series Labels to Remove Underscores
# -------------------------------

# Use dplyr's mutate and recode to rename Series labels
plot_long <- plot_long %>%
  mutate(Series = recode(Series,
                         "Temporal_Disaggregated" = "Temporal Disaggregated",
                         "High_Frequency" = "High-Frequency",
                         "Interpolated" = "Interpolated"))

# -------------------------------
# 7. Create the ggplot2 Plot
# -------------------------------

ggplot(plot_long, aes(x = Date, y = Value, color = Series, linetype = Series)) +
  geom_line(size = 0.5) +       # Adjust line thickness for better visibility
  geom_point(data = subset(plot_long, Series == "Temporal Disaggregated"), size = 1.5) +      # Adjust point size
  labs(
    title = "High-Dimensional Setting",
    x = "Time",
    y = ""  # Empty y-axis label as per original code
  ) +
  theme_minimal(base_size = 14) +  # Apply a clean minimal theme with larger base font size
  theme(
    panel.grid.major = element_blank(),    # Remove major gridlines
    panel.grid.minor = element_blank(),    # Remove minor gridlines
    legend.position = "top",               # Position legend at the top
    legend.title = element_blank(),        # Remove legend title
    legend.text = element_text(size = 12), # Increase legend text size for readability
    plot.title = element_text(
      hjust = 0.5, size = 16, face = "bold" # Center and style the plot title
    ),
    axis.text = element_text(size = 12),   # Increase axis text size
    axis.title = element_text(size = 14)   # Increase axis title size
  ) +
  scale_color_manual(
    values = c(
      "High-Frequency" = "red",
      "Interpolated" = "blue",
      "Temporal Disaggregated" = "black"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "High-Frequency" = "solid",
      "Interpolated" = "dashed",
      "Temporal Disaggregated" = "solid"
    )
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),    # Arrange legend items in one row
    linetype = guide_legend(nrow = 1, byrow = TRUE) # Ensure linetype legend aligns with color
  )

####################### MC analysis ###########################

type <- "Classic" # "Classic" or "High-Dimensional"
n_l <- 17  # Number of low-frequency data points (annual)
n <- 68  # Number of high-frequency data points (quarterly)
rho_sim <- 0.8  # Autocorrelation parameter

# Function to calculate MSE
mse <- function(predicted, actual) {
  mean((predicted - actual)^2, na.rm = TRUE)
}

# Function to calculate MAE
mae <- function(predicted, actual) {
  mean(abs(predicted - actual), na.rm = TRUE)
}

# Function to calculate summary statistics (std dev, kurtosis, MSE, MAE)
summary_stats <- function(data, reference) {
  std_dev <- sd(data, na.rm = TRUE)
  kurt <- kurtosis(data, na.rm = TRUE)
  mse_val <- mse(data, reference)
  mae_val <- mae(data, reference)
  
  return(c(std_dev, kurt, mse_val, mae_val))
}

# Monte Carlo Simulation setup
n_sim <- 1000  # Number of simulations

# Initialize result matrices with 4 columns (for std dev, kurtosis, MSE, MAE)
means_HF_SIM <- matrix(NA, nrow=n_sim, ncol=4)
means_HF_approx <- matrix(NA, nrow=n_sim, ncol=4)
means_HF_obs <- matrix(NA, nrow=n_sim, ncol=4)

# Extract dates for the interpolation process only
Data_X <- read.csv("../data/Exogenous_variables_IBM.csv", stringsAsFactors = FALSE)
Data_Y <- read.csv("../data/Total_emissions.csv", stringsAsFactors = FALSE)

Dates <- Data_Y$Dates[c(7:23)]
Dates_Q <- Data_X$Dates[c(24:91)]

Dates <- as.Date(Dates, format = "%m/%d/%Y")  # convert character to Date
Dates_Q <- as.Date(Dates_Q, format = "%m/%d/%Y")

# Monte Carlo Simulation loop
set.seed(27)  # For reproducibility
for (i in 1:n_sim) {
  # Check the type of simulation case
  if (type == "Classic") {
    p_sim <- 5  # Number of high-frequency exogenous variables
    Sim_data <- TempDisaggDGP(n_l, n, aggRatio = 4, p = p_sim, rho = rho_sim)
    method <- "Chow-Lin"
  } else {
    p_sim <- 100  # Number of high-frequency exogenous variables
    Sim_data <- TempDisaggDGP(n_l, n, aggRatio = 4, p = p_sim, rho = rho_sim)
    method <- "adaptive-spTD"
  }
  
  # Generate new simulation data for each iteration
  Y_sim_HF_obs <- matrix(Sim_data$y_Gen)  # HF simulated observations
  X_sim <- Sim_data$X_Gen  # HF exogenous variables
  Y_sim <- matrix(Sim_data$Y_Gen)  # LF simulated observations
  
  # Disaggregation based on method
  C_sparse_SIM <- disaggregate(Y_sim, X_sim, aggMat = "sum", aggRatio = 4, method = method)
  Y_HF_SIM <- C_sparse_SIM$y_Est[, 1]  # Temporal disaggregated estimates
  Y_HF_approx <- as.matrix(na.approx(merge(xts(Y_sim, Dates), xts(, Dates_Q)), rule = 2))
  
  # Store the results of summary statistics for each iteration
  means_HF_SIM[i, ] <- summary_stats(Y_HF_SIM, Y_sim_HF_obs)
  means_HF_approx[i, ] <- summary_stats(Y_HF_approx / 4, Y_sim_HF_obs)
  means_HF_obs[i, ] <- summary_stats(Y_sim_HF_obs, Y_sim_HF_obs)
}

# Calculate mean and standard deviation of the statistics across simulations
mean_HF_SIM <- apply(means_HF_SIM, 2, mean)
mean_HF_approx <- apply(means_HF_approx, 2, mean)
mean_HF_obs <- apply(means_HF_obs, 2, mean)

std_HF_SIM <- apply(means_HF_SIM, 2, sd)
std_HF_approx <- apply(means_HF_approx, 2, sd)
std_HF_obs <- apply(means_HF_obs, 2, sd)

# Create the mean summary table
mean_summary_table <- data.frame(
  Statistic = c("Standard Deviation", "Kurtosis", "MSE", "MAE"),
  `High Frequency Obs` = round(mean_HF_obs, 3),
  `Temporal Disaggregated Obs` = round(mean_HF_SIM, 3),
  `Interpolated Obs` = round(mean_HF_approx, 3)
)

# Create the standard deviation summary table
std_summary_table <- data.frame(
  Statistic = c("Standard Deviation", "Kurtosis", "MSE", "MAE"),
  `High Frequency Obs` = round(std_HF_obs, 3),
  `Temporal Disaggregated Obs` = round(std_HF_SIM, 3),
  `Interpolated Obs` = round(std_HF_approx, 3)
)

# Display the mean summary table
print("Mean Summary Table:")
print(mean_summary_table)

# Display the standard deviation summary table
print("Standard Deviation (Error) Summary Table:")
print(std_summary_table)

