# -------------------------------
# 1. Load Necessary Libraries
# -------------------------------

library(DisaggregateTS)
library(xts)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

# -------------------------------
# 2. Data Preparation
# -------------------------------

# Read xlsx files (Assuming the R Markdown file is in the parent directory of 'data')
Data_Y <- read.csv("../kaveh-nobari-5/data/Total_emissions.csv", stringsAsFactors = FALSE)
Data_X <- read.csv("../kaveh-nobari-5/data/Exogenous_variables_IBM.csv", stringsAsFactors = FALSE)

# -------------------------------
# 3. Subset Data
# -------------------------------

### Low-Frequency (LF) Data: 09-2005 (observation 7) to 09-2021 (observation 23)
Dates_LF <- Data_Y$Dates[7:23]
Dates_LF <- as.Date(Dates_LF, format = "%m/%d/%Y")

Y_LF <- as.matrix(as.numeric(Data_Y$IBM[7:23]))

### High-Frequency (HF) Data: 12-2004 (observation 21) to 09-2021 (observation 88)
Dates_HF <- Data_X$Dates[21:88]
Dates_HF <- as.Date(Dates_HF, format = "%m/%d/%Y")
X_HF <- Data_X[21:88, ]

# Convert all X_HF columns to numeric
X_HF <- sapply(X_HF, as.numeric)

# Remove columns containing NAs
X_HF <- X_HF[, colSums(is.na(X_HF)) == 0]

# -------------------------------
# 4. Filter Variables with High Correlations (>0.99)
# -------------------------------

# Compute correlation matrix
cor_matrix <- cor(X_HF)

# Zero out the upper triangle and diagonal
cor_matrix[upper.tri(cor_matrix)] <- 0
diag(cor_matrix) <- 0

# Identify columns with any correlation >= 0.99
high_corr_cols <- apply(cor_matrix, 2, function(x) any(abs(x) >= 0.99, na.rm = TRUE))

# Keep only columns without high correlations
X_filtered <- X_HF[, !high_corr_cols]

# -------------------------------
# 5. Perform Temporal Disaggregation
# -------------------------------

# Set seed for reproducibility
set.seed(27)

# Define parameters
n_l <- 17     # Number of low-frequency data points (annual)
n <- 68       # Number of high-frequency data points (quarterly)
p_sim <- ncol(X_filtered)  # Number of high-frequency exogenous variables after filtering
rho_sim <- 0.8 # Autocorrelation parameter

# Generate simulated data using TempDisaggDGP (if needed)
# Note: If you're using real data, you might skip this simulation step
# Sim_data <- TempDisaggDGP(n_l, n, aggRatio = 4, p = p_sim, rho = rho_sim)
# Y_LF <- as.matrix(as.numeric(Sim_data$Y_Gen))
# X_filtered <- Sim_data$X_Gen
# Y_HF_obs <- as.matrix(as.numeric(Sim_data$y_Gen))

# Perform temporal disaggregation using adaptive-spTD method
C_sparse <- disaggregate(Y_LF, X_filtered, aggMat = "sum", aggRatio = 4, method = "adaptive-spTD")
Y_HF <- C_sparse$y_Est[,1]  # Temporal disaggregated observations

# -------------------------------
# 6. Proportional (Linear) Interpolation
# -------------------------------

# Create xts objects for interpolation
Y_temp <- xts(Y_LF / 4, order.by = Dates_LF) # Divide by 4 as per aggregation ratio

# Merge low-frequency and high-frequency dates
Y_temp2 <- merge(Y_temp, xts(rep(NA, length(Dates_HF)), order.by = Dates_HF))

# Perform linear interpolation to fill NA values
Y_HF_approx <- as.numeric(na.approx(Y_temp2[,1], rule = 2)) # Ensure it's a numeric vector

# -------------------------------
# 7. Prepare Data for ggplot2
# -------------------------------

# Create a data frame combining all series
plot_data <- data.frame(
  Date = Dates_HF,
  Temporal_Disaggregated = Y_HF,
  Interpolated = Y_HF_approx
)

# Convert to long format suitable for ggplot2
plot_long <- pivot_longer(
  plot_data,
  cols = c("Temporal_Disaggregated", "Interpolated"),
  names_to = "Series",
  values_to = "Value"
)

# -------------------------------
# 8. Rename Series Labels to Remove Underscores
# -------------------------------

# Use dplyr's mutate and recode to rename Series labels
plot_long <- plot_long %>%
  mutate(Series = recode(Series,
                         "Temporal_Disaggregated" = "Temporal Disaggregated",
                         "Interpolated" = "Interpolated"))

# -------------------------------
# 9. Create the ggplot2 Plot
# -------------------------------

ggplot(plot_long, aes(x = Date, y = Value, color = Series, linetype = Series)) +
  geom_line(size = 0.5) +       # Plot lines for all series
  geom_point(data = filter(plot_long, Series == "Temporal Disaggregated"), size = 1.5) + # Plot points only for Temporal Disaggregated
  labs(
    title = "",
    x = "Time",
    y = "GHG Emissions"  # Added y-axis label as per original code
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
      "Temporal Disaggregated" = "black",
      "Interpolated" = "blue"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Temporal Disaggregated" = "solid",
      "Interpolated" = "dashed"
    )
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),    # Arrange legend items in one row
    linetype = guide_legend(nrow = 1, byrow = TRUE) # Ensure linetype legend aligns with color
  )

