
# Installation of the smoots package from CRAN:
if (!require("smoots")) {
  install.packages("smoots")
  require(smoots)
}

#===================================================================#

### Numerical results ###

# Code for Example 1:
tempChange <- smoots::tempNH$Change
est_temp <- smoots::tsmooth(tempChange, p = 1, mu = 2, Mcf = "NP", 
  InfR = "Opt", bStart = 0.1, bvc = "Y", method = "lpr")
d1_temp <- smoots::dsmooth(tempChange, d = 1, mu = 2, pp = 3, bStart.p = 0.2, 
  bStart = 0.15)
d2_temp <- smoots::dsmooth(tempChange, d = 2, mu = 3, pp = 1, bStart.p = 0.1, 
  bStart = 0.2)
arma1 <- stats::arima(est_temp$res, order = c(1, 0, 1), include.mean = FALSE)  

#..............................................................................

# Code for Example 2:
l_gdp <- log(smoots::gdpUS$GDP)  
gdp_t1 <- smoots::msmooth(l_gdp, p = 1, mu = 1, bStart = 0.1, alg = "A", 
  method = "lpr")
gdp_t2 <- smoots::msmooth(l_gdp, p = 1, mu = 1, bStart = 0.1, alg = "A", 
  method = "kr")
gdp_d1 <- smoots::dsmooth(l_gdp, d = 1, mu = 1, pp = 1, bStart.p = 0.1, 
  bStart = 0.15)
gdp_d2 <- smoots::dsmooth(l_gdp, d = 2, mu = 1, pp = 1, bStart.p = 0.1, 
  bStart = 0.2) 
arma2 <- stats::arima(gdp_t1$res, order = c(1, 0, 1), include.mean = FALSE)

#..............................................................................

# Code for Example 3:

# Calculate the centralized log-returns
dax_close <- smoots::dax$Close; dax <- diff(log(dax_close))
rt <- dax - mean(dax); yt <- log(rt ^ 2)

# Step 1: Estimate the trend in the log-transformed data using 'smoots'
estim3 <- smoots::msmooth(yt, p = 3, alg = "A")
m_xt <- estim3$ye

# Step 2: Fit an ARMA model to the residuals
xi <- estim3$res
arma3 <- arima(xi, order = c(1, 0, 1), include.mean = FALSE)

# Step 3: Estimate further quantities and the total volatilities
mu_le <- -log(mean(exp(arma3$residuals)))
vol <- exp((xi - arma3$residuals + m_xt - mu_le) / 2)

# Conditional volatilities (without scale; needed for the graphic)
mu_lz <- -log(mean(exp(xi)))
vol_cond <- exp((xi - arma3$residuals + mu_lz - mu_le) / 2)

# For reference (different settings; needed for the graphic):
estim3.2 <- smoots::msmooth(yt)

# For reference (estimated parameters of the Log-GARCH representation):
par.lgarch <- c(
  alpha1 = sum(arma3$coef), 
  beta1 = -arma3$coef[["ma1"]],
  omega = (1 - arma3$coef[["ar1"]]) * mu_lz - (1 + arma3$coef[["ma1"]]) * mu_le
)

#..............................................................................

# Code for Example 4:

# Calculate the logarithm of the index
vix <- smoots::vix$Close; lnV <- log(vix)

# Step 1: Estimate the trend in the log-transformed data using 'smoots'
estim4 <- smoots::msmooth(lnV)
m_xt <- estim4$ye

# Step 2: Fit an ARMA model to the residuals
xi <- estim4$res
arma4 <- arima(xi, order = c(1, 0, 1), include.mean = FALSE)

# Step 3: Estimate further quantities and the total means
mu_le <- -log(mean(exp(arma4$residuals)))
means <- exp(xi - arma4$residuals + m_xt - mu_le)

# Conditional means (without scale; needed for the graphic)
mu_lz <- -log(mean(exp(xi)))
means_cond <- exp(xi - arma4$residuals + mu_lz - mu_le)

# For reference (different settings; needed for the graphic):
estim4.2 <- smoots::msmooth(lnV, p = 3, alg = "B")

# For reference (estimated parameters of the Log-ACD representation):
par.lacd <- c(
  alpha1 = sum(arma4$coef), 
  beta1 = -arma4$coef[["ma1"]],
  omega = (1 - arma4$coef[["ar1"]]) * mu_lz - (1 + arma4$coef[["ma1"]]) * mu_le
)

#===================================================================#
#===================================================================#

### Graphical results ###

# Note: Reworked for the first resubmission in base R

#..............................................................................

oldpar <- par(no.readonly = TRUE)
col.alpha <- function(col, alpha = 1) {
  col.rgb <- col2rgb(col) / 255
  rgb(red = col.rgb[[1]], green = col.rgb[[2]], blue = col.rgb[[3]],
      alpha = alpha)
}
line.col <- "lightgray"

## Code for Example 1: Semi-ARMA applied to NH temperature changes
n_temp <- length(tempChange)
t_temp <- (0:(n_temp - 1)) / n_temp * (2019 - 1880) + 1880 

pdf("Plot1_TempChange_final.pdf", height = 8, width = 12)
par(mfcol = c(2, 2), cex = 1.2, mar = c(0.5, 4, 2, 0.5) + 0.1,
  oma = c(3.5, 0, 0, 0))

# Plot 1
par(xpd = NA)
plot(t_temp, tempChange, type = "n", 
  main = "(a) Temperature changes & estimated trend",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab="Temp. change & trend (in \u00B0C)", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1880, 2020, 20), col = line.col, lty = 3)
abline(h = seq(-1.5, 2, 0.5), col = line.col, lty = 3)
lines(t_temp, tempChange, col = col.alpha("black", 0.3))
lines(t_temp, est_temp$ye, col = "red")
legend("topleft", 
  legend = c("Temp. changes", "Trend (local linear)"),
  lty = c(1, 1), col = c(col.alpha("black", 0.3), "red"),
  cex = 0.85, title = "Lines:", title.adj = 0.1,
  bg = col.alpha("white", 0.5))

# Plot 2
par(xpd = NA)
plot(t_temp, est_temp$res, type = "n",
  main = "(b) Residual series",
  ylab = "",
  xlab = "")
title(xlab = "Year", line = 2.5)
title(ylab = "Residuals", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1880, 2020, 20), col = line.col, lty = 3)
abline(h = seq(-1.5, 1, 0.5), col = line.col, lty = 3)
abline(h = 0, col = col.alpha("black", 0.5))
lines(t_temp, est_temp$res)

# Plot 3
par(xpd = NA)
plot(t_temp, d1_temp$ye, type = "n",
  main = "(c) Estimated first derivative",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "1st derivative", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1880, 2020, 20), col = line.col, lty = 3)
abline(h = seq(-0.5, 5, 0.5), col = line.col, lty = 3)
abline(h = 0, col = col.alpha("black", 0.5))
lines(t_temp, d1_temp$ye, col = "black")

# Plot 4
par(xpd = NA)
plot(t_temp, d2_temp$ye, type = "n",
  main = "(d) Estimated second derivative",
  ylab = "",
  xlab = "")
title(xlab = "Year", line = 2.5)
title(ylab = "2nd derivative", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1880, 2020, 20), col = line.col, lty = 3)
abline(h = seq(-20, 20, 5), col = line.col, lty = 3)
abline(h = 0, col = col.alpha("black", 0.5))
lines(t_temp, d2_temp$ye, col = "black")

dev.off()

#..............................................................................

## Code for Example 2: Semi-ARMA applied to Log-GDP of US
n_gdp <- length(l_gdp)
t_gdp <- seq(from = 1947, to = 2019.25, by = 0.25)

pdf("Plot2_GDP_final.pdf", height = 8, width = 12)
par(mfcol = c(2, 2), cex = 1.2, mar = c(0.5, 4, 2, 0.5) + 0.1,
  oma = c(3.5, 0, 0, 0))

# Plot 1
par(xpd = NA)
plot(t_gdp, l_gdp, type = "n", 
  main = "(a) Logarithm of US-GDP & estimated trends",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab="Log-GDP & trends", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1945, 2020, 5), col = line.col, lty = 3)
abline(h = seq(7.5, 10, 0.5), col = line.col, lty = 3)
lines(t_gdp, l_gdp, col = col.alpha("black", 0.3))
lines(t_gdp, gdp_t1$ye, col = "red")
lines(t_gdp, gdp_t2$ye, col = "blue", lty = 2)
legend("topleft", 
  legend = c("Logarithm of GDP", "Trend 1 (local linear)", "Trend 2 (kernel)"),
  lty = c(1, 1, 2), col = c(col.alpha("black", 0.3), "red", "blue"),
  cex = 0.85, title = "Lines:", title.adj = 0.1,
  bg = col.alpha("white", 0.5))

# Plot 2
par(xpd = NA)
plot(t_gdp, gdp_t1$res, type = "n",
  main = "(b) Residual series (based on trend 1)",
  ylab = "",
  xlab = "", 
  xaxt = "n")
axis(side = 1, at = seq(1950, 2020, 10))
title(xlab = "Year", line = 2.5)
title(ylab = "Residuals", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1945, 2020, 5), col = line.col, lty = 3)
abline(h = seq(-0.06, 0.06, 0.02), col = line.col, lty = 3)
abline(h = 0, col = col.alpha("black", 0.5))
lines(t_gdp, gdp_t1$res)

# Plot 3
par(xpd = NA)
plot(t_gdp, gdp_d1$ye, type = "n",
  main = "(c) Estimated first derivative",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "1st derivative", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1945, 2020, 5), col = line.col, lty = 3)
abline(h = seq(0.5, 3, 0.5), col = line.col, lty = 3)
abline(h = 0, col = col.alpha("black", 0.5))
lines(t_gdp, gdp_d1$ye, col = "black")

# Plot 4
par(xpd = NA)
plot(t_gdp, gdp_d2$ye, type = "n",
  main = "(d) Estimated second derivative",
  ylab = "",
  xlab = "",
  xaxt = "n")
axis(side = 1, at = seq(1950, 2020, 10))
title(xlab = "Year", line = 2.5)
title(ylab = "2nd derivative", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1945, 2020, 5), col = line.col, lty = 3)
abline(h = seq(-5, 5, 1.25), col = line.col, lty = 3)
abline(h = 0, col = col.alpha("black", 0.5))
lines(t_gdp, gdp_d2$ye, col = "black")

dev.off()

#..............................................................................

## Code for Example 3:
n_dax <- length(dax_close)
n_ret <- n_dax - 1
t_dax <- ((0:(n_dax - 1)) / n_dax) * (2019 + 7 / 12 - 1990) + 1990
t_dax2 <- t_dax[-1]

pdf("Plot3_DAX_final.pdf", height = 12, width = 12)
par(mfcol = c(4, 1), cex = 1.2, mar = c(0.5, 4, 2, 0.5) + 0.1,
  oma = c(3.5, 0, 0, 0))

# Plot 1
par(xpd = NA)
plot(t_dax2, rt, type = "n", 
  main = "(a) Centralized DAX return series",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "Returns", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(-0.1, 0.1, 0.025), col = line.col, lty = 3)
lines(t_dax2, rt, col = "black")

# Plot 2
par(xpd = NA)
plot(t_dax2, yt, type = "n",
  main = "(b) Log-transformed squared returns & estimated trends",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "Log-data & trends", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(-30, -5, 5), col = line.col, lty = 3)
lines(t_dax2, yt, col = col.alpha("black", 0.3))
lines(t_dax2, estim3$ye, col = "red")
lines(t_dax2, estim3.2$ye, col = "blue", lty = 2)
legend("bottomleft", 
  legend = c("Trend 1 (local cubic, optimal inflation factor)", 
    "Trend 2 (local linear, optimal inflation factor)", "Log-data"),
  lty = c(1, 2, 1), col = c("red", "blue", col.alpha("black", 0.3)),
  cex = 0.85, title = "Lines:", title.adj = 0.013,
  bg = col.alpha("white", 0.5), ncol = 3)

# Plot 3
par(xpd = NA)
plot(t_dax2, vol_cond, type = "n",
  main = "(c) Conditional volatility (based on trend 1)",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "Conditional volatility", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(0.5, 2, 0.5), col = line.col, lty = 3)
lines(t_dax2, vol_cond, col = "black")

# Plot 4
par(xpd = NA)
plot(t_dax2, vol, type = "n",
  main = "(d) Total volatility (based on trend 1)",
  ylab = "",
  xlab = "")
title(xlab = "Year", line = 2.5)
title(ylab = "Total volatility", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(0.005, 0.04, 0.005), col = line.col, lty = 3)
lines(t_dax2, vol, col = "black")

dev.off()





#..............................................................................

## Code for Example 4: Semi-Log-ACD applied to CBOE Volatility Index (VIX)
n_vix <- length(vix)
t_vix <- ((0:(n_vix - 1)) / n_vix) * (2019 + 7 / 12 - 1990) + 1990

pdf("Plot4_VIX_final.pdf", height = 12, width = 12)
par(mfcol = c(4, 1), cex = 1.2, mar = c(0.5, 4, 2, 0.5) + 0.1,
  oma = c(3.5, 0, 0, 0))

# Plot 1
par(xpd = NA)
plot(t_vix, vix, type = "n", 
  main = "(a) CBOE Volatility Index series",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "Index", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(10, 80, 10), col = line.col, lty = 3)
lines(t_vix, vix, col = "black")

# Plot 2
par(xpd = NA)
plot(t_vix, lnV, type = "n",
  main = "(b) Log-transformed series & estimated trends",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "Log-data & trends", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(2.25, 4.25, 0.25), col = line.col, lty = 3)
lines(t_vix, lnV, col = col.alpha("black", 0.3))
lines(t_vix, estim4$ye, col = "red")
lines(t_vix, estim4.2$ye, col = "blue", lty = 2)
legend("topleft", 
  legend = c("Trend 1 (local linear)", 
    "Trend 2 (local cubic)", "Log-data"),
  lty = c(1, 2, 1), col = c("red", "blue", col.alpha("black", 0.3)),
  cex = 0.85, bg = col.alpha("white", 0.5), title = "Lines:", 
  title.adj = 0.025, ncol = 3)

# Plot 3
par(xpd = NA)
plot(t_vix, means_cond, type = "n",
  main = "(c) Conditional means (based on trend 1)",
  ylab = "",
  xlab = "",
  xaxt = "n")
title(ylab = "Conditional means", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(0.75, 3, 0.25), col = line.col, lty = 3)
lines(t_vix, means_cond, col = "black")

# Plot 4
par(xpd = NA)
plot(t_vix, means, type = "n",
  main = "(d) Total means (based on trend 1)",
  ylab = "",
  xlab = "")
title(xlab = "Year", line = 2.5)
title(ylab = "Total means", line = 2.5)
par(xpd = FALSE)
abline(v = seq(1988, 2022, 1), col = line.col, lty = 3)
abline(h = seq(10, 70, 10), col = line.col, lty = 3)
lines(t_vix, means, col = "black")

dev.off()