# Load necessary libraries
require(Rfssa)
require(fda)

# Load Callcenter data
Call_data <- loadCallcenterData()

# Prepare the data
D <- matrix(sqrt(Call_data$calls), nrow = 240)
bs1 <- create.bspline.basis(c(0, 23), 22)

# Create a 'funts' object
Call_funts <- funts(D, bs1,
  start = as.Date("1999-1-1"),
  vnames = "Sqrt of Call Numbers",
  dnames = "Time (6 minutes aggregated)",
  tname = "Date"
)

xtlab <- list(c("00:00", "06:00", "12:00", "18:00", "24:00"))
xtloc <- list(c(1, 60, 120, 180, 240))

# Generate a line plot using Plotly
plotly_funts(Call_funts,
  main = "Call Center Data Line Plot",
  xticklabels = xtlab, xticklocs = xtloc
)

# Generate a heatmap plot using Plotly
plotly_funts(Call_funts,
  type = "heatmap", main = "Call Center Data Heatmap",
  xticklabels = xtlab, xticklocs = xtloc
)

# Generate a 3D line plot using Plotly
plotly_funts(Call_funts,
  type = "3Dline", main = "Call Center Data 3Dline plot",
  xticklabels = xtlab, xticklocs = xtloc
)

# Generate a 3D surface plot using Plotly
plotly_funts(Call_funts,
  type = "3Dsurface", main = "Call Center Data 3Dsurface plot",
  xticklabels = xtlab, xticklocs = xtloc
)



# ___________________________________________________________________________


# Load the Jambi dataset
Jambi <- loadJambiData()

# Extract NDVI and EVI array data
NDVI <- Jambi$NDVI
EVI <- (Jambi$EVI)

# Create a list containing NDVI and EVI array data
Jambi_Data <- list(NDVI, EVI)

# Create a multivariate B-spline basis
require(fda)
bs2 <- create.bspline.basis(c(0, 1), 13)
bs2d <- list(bs2, bs2)
bsmv <- list(bs2d, bs2d)

# Create a funts object
Y_J <- funts(
  X = Jambi_Data,
  basisobj = bsmv,
  start = as.Date("2000-02-18"), end = as.Date("2019-07-28"),
  vnames = c("NDVI", "EVI"), tname = "Date",
  dnames = list(c("Longitude", "Latitude"), c("Longitude", "Latitude"))
)

# Create a Plotly-based visualization of the NDVI Image
plotly_funts(Y_J[, 1],
  main = "NDVI Image (Jambi)",
  xticklabels = list(c("103.61\u00B0 E", "103.68\u00B0 E")),
  yticklabels = list(c("1.67\u00B0 S", "1.60\u00B0 S")),
  xticklocs = list(c(0, 1)),
  yticklocs = list(c(0, 1)),
  color_palette = "RdYlGn"
)

# ___________________________________________________________________________



# Load the Callcenter dataset
data("Callcenter")

# Perform FSSA:
fssa_results <- fssa(Callcenter, L = 28)

# FSSA plots:
plot(fssa_results,
  d = 9, type = "lcurves",
  main = "(A) Left Singular Functions (within days)"
)
plot(fssa_results,
  d = 9, type = "lheats",
  main = "(B) Left Singular Functions (between days)"
)
plot(fssa_results, d = 13, main = "(C) Singular Values")
plot(fssa_results,
  d = 9, type = "vectors",
  main = "(D) Right Singular Vectors"
)
plot(fssa_results,
  d = 10, type = "paired",
  main = "(E) Paired Plots of Right Singular Vectors"
)
plot(fssa_results,
  d = 9, type = "wcor",
  main = "(F) W-Correlation Matrix"
)

# ___________________________________________________________________________


# Define groups and their labels
groups <- list(1:7, 1, 2:3, 4:5, 6:7)
group_labels <- c(
  "(B) Extracted Signal",
  "(C) First Group",
  "(D) Second Group",
  "(E) Third Group",
  "(F) Fourth Group"
)

# Perform FSSA‌ reconstruction
reconstructed_data <- freconstruct(fssa_results, groups)

# Create and visualize plots
for (i in 1:length(groups)) {
  print(plotly_funts(reconstructed_data[[i]],
    main = group_labels[i],
    xticklocs = xtlab, xticklabels = xtloc
  ))
}


# ___________________________________________________________________________


# Perform FSSA R-forecasting
pr_R <- fforecast(U = fssa_results, groups = 1:7, len = 14, method = "recurrent")

# Perform FSSA V-forecasting
pr_V <- fforecast(U = fssa_results, groups = 1:7, len = 14, method = "vector",
                  only.new = FALSE)
plot(pr_R, main = "R-Forecast (only.new = TRUE) - ", ylim=c(0,6))
plot(pr_V, main = "V-Forecast (only.new = FALSE) - ", ylim=c(0,6))
print(pr_V)

# FSSA Forecast (fforecast) class:
# Groups: List of 2
# : num 1
# : int [1:7] 1 2 3 4 5 6 7
# Prediction method:  vector
# Predicted series length:  14
# Predicted time:  Date[1:14], format: "2000-01-01" "2000-01-02" ...

# ---------The original series-----------
# Functional time series (funts) object:
# Number of variables:  1
# Lenght:  365
# Start:  10592
# End:  10956
# Time:  Date[1:365], format: "1999-01-01" "1999-01-02" "1999-01-03"  ...


# ___________________________________________________________________________


# Define the data length
N <- Callcenter$N
U1 <- fssa(Callcenter[1:(N - 7)], 28)

# Perform recurrent forecasting using FSSA
fore_R <- fforecast(U1, groups = list(1:7), method = "recurrent", len = 7)[[1]]
# Perform vector forecasting using FSSA
fore_V <- fforecast(U1, groups = list(1:7), method = "vector", len = 7)[[1]]
# Extract the true call data
true_call <- Callcenter[(N - 7 + 1):N]
# Define weekdays and colors
wd <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
clrs <- c("black", "darkorange", "turquoise4")
argvals <- seq(0, 23, length.out = 100)
par(mfrow = c(1, 7), mar = c(0.2, 0.2, 2, 0.2))
# Iterate over the days of the week
for (i in 1:7) {
  plot(true_call[i], col = clrs[1], lwd = 2, ylim = c(0, 5.3),
       lty = 3, yaxt = "n", xaxt = "n", main = wd[i])
  plot(fore_R[i], col = clrs[2], add = TRUE, lwd = 2, lty=2)
  plot(fore_V[i], col = clrs[3], add = TRUE, lwd = 2, lty=5)
}
legend("top", c("Original", "R-forecating", "V-forecasting"), col = clrs, lty = c(3, 2, 5), lwd = 2)
