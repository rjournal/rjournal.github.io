################################################################################
###################### Standalone replication script ###########################
################################################################################

################################################################################
## Set up

library(SEI)
library(ggplot2)
library(dplyr)
library(xts)
library(zoo)
library(gridExtra)

set.seed(10714)


################################################################################
## Section 4

### Figure 1 (histograms of values before and after standardisation)
x <- rgamma(1000, shape = 2, scale = 1)
x_std <- std_index(x)

plot_raw <- plot_sei(x, type = "hist", title = "Raw")
plot_std <- plot_sei(x_std, type = "hist", title = "Standardised")
grid.arrange(plot_raw, plot_std, nrow = 1)


################################################################################
## Section 5.1

### Code chunk 1 (load supply data)
data("data_supply", package = "SEI")
head(data_supply)


### Code chunk 2 (convert time series to xts object)
de_supply_h <- subset(data_supply, country == "Germany")
de_supply_h <- xts::xts(de_supply_h$PWS, de_supply_h$date) # convert to xts


### Code chunk 3 (rescale time series to get daily and weekly data)
de_supply_d <- xts::apply.daily(de_supply_h, "sum")    # daily data
de_supply_w <- xts::apply.weekly(de_supply_h, "sum")   # weekly data


### Figure 2 (time series of hourly, daily, and weekly renewable energy production)
lab <- "Renewable Energy Production (GWh)"
plot_h <- plot_sei(de_supply_h, lab = lab, title = "Hourly")
plot_d <- plot_sei(de_supply_d, lab = lab, title = "Daily")
plot_w <- plot_sei(de_supply_w, lab = lab, title = "Weekly")
grid.arrange(plot_h, plot_d, plot_w, nrow = 1)

### Code chunk 4 (calculate standardised indices)
srepi_h <- std_index(de_supply_h)
srepi_d <- std_index(de_supply_d)
srepi_w <- std_index(de_supply_w)


### Code chunk 5 (check rescaling in std_index)
z <- std_index(de_supply_h, rescale = "days")
all.equal(srepi_d, z)


### Figure 3 (time series of hourly, daily, and weekly standardised indices)
lab <- "SREPI"
ylims <- c(-3, 3)
plot_h <- plot_sei(srepi_h, lab = lab, ylims = ylims, title = "Hourly")
plot_d <- plot_sei(srepi_d, lab = lab, ylims = ylims, title = "Daily")
plot_w <- plot_sei(srepi_w, lab = lab, ylims = ylims, title = "Weekly")
grid.arrange(plot_h, plot_d, plot_w, nrow = 1)


### Figure 4 (histogram of renewable energy production before and after standardisation)
plot_raw <- plot_sei(de_supply_d, type = "hist", lab = "Renewable Energy Production (GWh)")
plot_ind <- plot_sei(srepi_d, type = "hist", lab = "SREPI")
grid.arrange(plot_raw, plot_ind, nrow = 1)


### Figure 5 (as in Figure 4 but for probability and bounded indices)
srepi_d_prob <- std_index(de_supply_d, index_type = "probability")
plot_prob <- plot_sei(srepi_d_prob, type = "hist", lab = "SREPI", ylims = c(0, 2), title = "Probability")
srepi_d_bnd <- std_index(de_supply_d, index_type = "bounded")
plot_bnd <- plot_sei(srepi_d_bnd, type = "hist", lab = "SREPI", ylims = c(0, 1), title = "Bounded")
grid.arrange(plot_prob, plot_bnd, nrow = 1)


### Code chunk 6 (get drought example)
thresholds <- -qnorm(c(0.9, 0.95, 0.975)) # -1.28, -1.64, -1.96
drought_df <- get_drought(srepi_d, thresholds, exceed = F)

### Code chunk 7 (calculate drought frequency)
num_ev <- table(drought_df$ins)
names(num_ev) <- c("None", "Moderate", "Severe", "Extreme")
print(num_ev)


### Code chunk 8 (calculate drought duration)
table(drought_df$dur[drought_df$dur > 0])


### Code chunk 9 (calculate drought magnitude)
mean(drought_df$mag[drought_df$mag != 0])


################################################################################
## Section 5.2

### Code chunk 10 (load wind speed data)
data("data_wind_de", package = "SEI")
data_wind_de <- subset(data_wind_de, format(date, "%Y") >= 2017)
head(data_wind_de)


### Code chunk 11 (convert wind speeds to xts time series)
de_wind_d <- xts::xts(data_wind_de$wsmean, data_wind_de$date) # convert to xts
de_wind_w <- xts::apply.weekly(de_wind_d, FUN = "mean")       # weekly data
de_wind_m <- xts::apply.monthly(de_wind_d, FUN = "mean")      # monthly data


### Figure 6 (plot time series of daily, weekly, and monthly wind speed)
lab <- "Wind speed (m/s)"
ylims <- c(0, 8)
plot_ws_d <- plot_sei(de_wind_d, lab = lab, ylims = ylims, title = "Daily")
plot_ws_w <- plot_sei(de_wind_w, lab = lab, ylims = ylims, title = "Weekly")
plot_ws_m <- plot_sei(de_wind_m, lab = lab, ylims = ylims, title = "Monthly")
grid.arrange(plot_ws_d, plot_ws_w, plot_ws_m, nrow = 1)


### Code chunk 12 (load wind speed data)
out_gamma <- fit_dist(data_wind_de$wsmean, dist = "gamma")
out_lnorm <- fit_dist(data_wind_de$wsmean, dist = "lnorm")
out_weibull <- fit_dist(data_wind_de$wsmean, dist = "weibull")


### Code chunk 13 (get aic values)
aic_vec <- c(out_gamma$fit_props['aic'], out_lnorm$fit_props['aic'], out_weibull$fit_props['aic'])
names(aic_vec) <- c("Gamma", "Log-normal", "Weibull")
print(aic_vec)


### Code chunk 14 (get kolmogorov-smirnov test p-values)
ksp_vec <- c(out_gamma$fit_props['ks_pval'], out_lnorm$fit_props['ks_pval'], out_weibull$fit_props['ks_pval'])
names(ksp_vec) <- c("Gamma", "Log-normal", "Weibull")
print(round(ksp_vec, 4))


### Figure 7 (superimpose parametric densities onto histogram of wind speeds)
x <- seq(0, 9, length.out = length(de_wind_d))
xlab <- "Wind speed (m/s)"
pars_gam <- out_gamma$params
plt_gam <- plot_sei(de_wind_d, type = "hist", lab = xlab, title = "Gamma") +
  geom_line(aes(x = x, y = dgamma(x, pars_gam[1], pars_gam[2])), col = "blue")
plt_lnorm <- plot_sei(de_wind_d, type = "hist", lab = xlab, title = "Log-normal")
plt_lnorm <- plt_lnorm + geom_line(aes(x = x, y = dlnorm(x, out_lnorm$params[1], out_lnorm$params[2])), col = "blue")
plt_weib <- plot_sei(de_wind_d, type = "hist", lab = xlab, title = "Weibull")
plt_weib <- plt_weib + geom_line(aes(x = x, y = dweibull(x, out_weibull$params[1], out_weibull$params[2])), col = "blue")
grid.arrange(plt_gam, plt_lnorm, plt_weib, nrow = 1)


### Code chunk 15 (convert daily wind speeds to standardised indices)
sei_ws_gam <- std_index(de_wind_d, dist = "gamma")
sei_ws_lnorm <- std_index(de_wind_d, dist = "lnorm")
sei_ws_weib <- std_index(de_wind_d, dist = "weibull")


### Figure 8 (histograms of standardised indices with three distributions)
x <- seq(-3.5, 3.5, length.out = length(sei_ws_gam))
xlab <- "SWSI"
plt_gam <- plot_sei(sei_ws_gam, type = "hist", lab = xlab, title = "Gamma") +
  geom_line(aes(x = x, y = dnorm(x)), col = "blue")
plt_lnorm <- plot_sei(sei_ws_lnorm, type = "hist", lab = xlab, title = "Log-normal") +
  geom_line(aes(x = x, y = dnorm(x)), col = "blue")
plt_weib <- plot_sei(sei_ws_weib, type = "hist", lab = xlab, title = "Weibull") +
  geom_line(aes(x = x, y = dnorm(x)), col = "blue")
grid.arrange(plt_gam, plt_lnorm, plt_weib, nrow = 1)


sessionInfo()
