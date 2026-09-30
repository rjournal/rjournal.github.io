################################################################################
#     Packages
################################################################################

library(plotly)
library(ggplot2)
library(sf)
library(gstat)
library(tidyr)
library(viridis)
library(gridExtra)
library(kableExtra)
library(BMEmapping)


################################################################################
#
#     Example 1: Using the QBME Implementation
#
################################################################################

# load data
data("utsnowload", package = "BMEmapping")
head(utsnowload)
tail(utsnowload)


# hard data locations
ch <- utsnowload[1:67, c("latitude", "longitude")]

# hard data values
zh <- utsnowload[1:67, c("hard")]

# soft data locations
cs <- utsnowload[68:227, c("latitude", "longitude")]

# lower and upper bounds of soft data (intervals)
a <- utsnowload[68:227, c("lower")]
b <- utsnowload[68:227, c("upper")]


################################################################################
#     Create BMEmapping object
################################################################################

data_object <- bme_map(ch, cs, zh, a, b)

################################################################################
#     Spatial plot of data (Figure 1)
################################################################################

plot(data_object)

################################################################################
#     Estimation locations
################################################################################

xk <- utsnowload[228:232, c("latitude", "longitude")]
xk


################################################################################
#     Posterior density estimation and plots selected locations
#        uisng default zk_range
################################################################################

p_1 <- q_prob_zk(xk[1,], data_object)
p_2 <- q_prob_zk(xk[2,], data_object)
p_3 <- q_prob_zk(xk[3,], data_object)
p_4 <- q_prob_zk(xk[4,], data_object)
p_df <- cbind.data.frame(p_1, p_2[, 2], p_3[, 2], p_4[, 2])
names(p_df) <- c("zk_i", "p1", "p2", "p3", "p4")

# Function to generate ggplot for a given column name
plot_prob_curve <- function(df, pi_col) {
  ggplot(df, aes_string(x = "zk_i", y = pi_col)) +
    geom_line(color = "blue", linewidth = 0.5) +
    labs(x = "z", y = "f(z)") +
    theme_minimal(base_size = 10) +
    theme(
      panel.background = ggplot2::element_rect(fill = "white",color = "black")
    )
}

# Generate individual plots
p1 <- plot_prob_curve(p_df, "p1")
p2 <- plot_prob_curve(p_df, "p2")
p3 <- plot_prob_curve(p_df, "p3")
p4 <- plot_prob_curve(p_df, "p4")

# Arrange in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)


################################################################################
#     Posterior density estimation and plots selected locations
#        uisng updated zk_range
################################################################################

q_1 <- q_prob_zk(xk[1,], data_object, zk_range = c(-1.8, 1.5))
q_2 <- q_prob_zk(xk[2,], data_object, zk_range = c(-1.8, 1.5))
q_3 <- q_prob_zk(xk[3,], data_object, zk_range = c(-1.8, 1.5))
q_4 <- q_prob_zk(xk[4,], data_object, zk_range = c(-1.8, 1.5))
q_df <- cbind.data.frame(q_1, q_2[, 2], q_3[, 2], q_4[, 2])
names(q_df) <- c("zk_i", "q1", "q2", "q3", "q4")

# Generate individual plots
q1 <- plot_prob_curve(q_df, "q1")
q2 <- plot_prob_curve(q_df, "q2")
q3 <- plot_prob_curve(q_df, "q3")
q4 <- plot_prob_curve(q_df, "q4")

grid.arrange(q1, q2, q3, q4, ncol = 2)


################################################################################
#     BME predictions for estimation locations
################################################################################

# posterior mode predictions
q_bme_predict(xk, data_object, zk_range = c(-1.8, 1.5), type = "mode")

# posterior mean predictions
q_bme_predict(xk, data_object, zk_range = c(-1.8, 1.5), type = "mean")

# posterior mean predictions
q_bme_predict(xk, data_object, zk_range = c(-1.8, 1.5), type = "median")


################################################################################
#     BME credible interval predictions for estimation locations
################################################################################

# 90% credible interval
q_bme_predict_ci(xk, data_object, zk_range = c(-1.8, 1.5), level = 0.90)

# 95% credible interval
q_bme_predict_ci(xk, data_object, zk_range = c(-1.8, 1.5), level = 0.95)


################################################################################
#     K-fold CV for posterior mode
################################################################################

QBME_cv <- q_bme_cv(data_object, zk_range = c(-1.8, 1.5), type = "mode", k = 5)
QBME_cv

# Summary of LOOCV results
summary(QBME_cv)

# diagnostic plots of residuals
plot(QBME_cv)



################################################################################
#
#     Example 2: Using the CBME Implementation
#
################################################################################

# Data Generation and partition
# 1. Setup Simulation Space & Geostatistical Background
set.seed(123)
n <- 625

# Generate static spatial coordinates uniformly across the domain
coords <- data.frame(
  x = runif(n, 0, 100),
  y = runif(n, 0, 100)
)
grid_sf <- st_as_sf(coords, coords = c("x", "y"))

# Define background exponential variogram structure
vgm_model <- vgm(psill = 1.5, model = "Exp", range = 40, nugget = 0.5)

g.dummy <- gstat(
  formula = z ~ 1,
  dummy = TRUE,
  beta = 0,
  model = vgm_model,
  nmax = 40
)

sim <- predict(g.dummy, newdata = grid_sf, nsim = 1)

# Define explicit mutually exclusive partitioning limits
nh <- 1:125    # Hard data indices
ns <- 126:525  # Soft data indices
nk <- 526:625  # Validation tracking indices


# 2. Main Simulation Configuration (Single Realization Run)
l_val <- 3
q_val <- c(0.25, 0.75)

# Extract true continuous field values for realization j
z_t    <- sim[[paste0("sim", 1)]]
t_data <- cbind(coords, z = z_t)

# Split spatial subsets cleanly without inline semi-colons
ch <- t_data[nh, c("x", "y")]; zh <- t_data[nh, "z"]
cs <- t_data[ns, c("x", "y")]; zs <- t_data[ns, "z"]
ck <- t_data[nk, c("x", "y")]; zk <- t_data[nk, "z"]


# 4. Generate Realistic Soft-Interval Bounding Windows
n_soft <- length(zs)

# Step A: Determine total window length of individual intervals
ln <- runif(n_soft, min = l_val - 0.5, max = l_val + 0.5)

# Step B: Pick random relative positioning factors to introduce skewness
u <- runif(n_soft, min = q_val[1], max = q_val[2])

# Step C: Mathematically calculate lower (a) and upper (b) boundary envelopes
a <- zs - ln * u
b <- zs + ln * (1 - u)


################################################################################
#     Visualizing the data
################################################################################

data_object <- bme_map(ch, cs, zh, a, b)
plot(data_object)


################################################################################
#     Variogram fitting (Using "sf" and "gstat" packages)
################################################################################

# combine hard data and midpoints of soft data
df <- data.frame(rbind(ch, cs), c(zh, (a + b) / 2))
colnames(df) <- c("x", "y", "z")
df_data <- sf::st_as_sf(df, coords = c("x", "y"))

# Compute the empirical variogram using all available distances
vg <- variogram(z ~ 1, data = df_data)

# Define the cutoff distance as 1.75 times the distance at which the
# empirical variogram reaches its maximum semi-variance. This reduces the
# influence of poorly estimated long-distance pairs while retaining the
# dominant spatial dependence structure.
cut_off <- 1.4 * vg[which.max(vg$gamma), "dist"]

# Recompute the empirical variogram using the selected cutoff distance
# before fitting the theoretical variogram model
vg <- variogram(z ~ 1, cutoff = cut_off, data = df_data)

# Fit the theoretical variogram model to the empirical variogram
vg_model <- fit.variogram(vg, model = vgm(c("Sph")))

# variogram plot
plot(vg, vg_model)

# variogram model
vg_model


################################################################################
#     Posterior density estimation and plots selected locations
#        uisng updated zk_range
################################################################################

# set model parameters
model <- as.character(vg_model[2, 1])
nugget <- vg_model[1, 2]
sill <- vg_model[2, 2]
range <- vg_model[2, 3]

sim_p1 <- prob_zk(ck[1,], data_object, model, nugget, sill, range)
sim_p2 <- prob_zk(ck[2,], data_object, model, nugget, sill, range)
sim_p3 <- prob_zk(ck[3,], data_object, model, nugget, sill, range)
sim_p4 <- prob_zk(ck[4,], data_object, model, nugget, sill, range)
sim_pdf <- cbind.data.frame(sim_p1, sim_p2[, 2], sim_p3[, 2], sim_p4[, 2])
names(sim_pdf) <- c("zk_i", "p1", "p2", "p3", "p4")

# Generate individual plots
p1 <- plot_prob_curve(sim_pdf, "p1")
p2 <- plot_prob_curve(sim_pdf, "p2")
p3 <- plot_prob_curve(sim_pdf, "p3")
p4 <- plot_prob_curve(sim_pdf, "p4")

# Arrange in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)


################################################################################
#     BME predictions of estimation locations
################################################################################

# posterior mode predictions
BME_mode <- bme_predict(ck, data_object, model, nugget, sill, range,
                        zk_range = c(-3, 4), type = "mode")
head(BME_mode)

# posterior mode predictions
BME_mean <- bme_predict(ck, data_object, model, nugget, sill, range,
                        zk_range = c(-3, 4), type = "mean")
head(BME_mean)

# posterior mode predictions
BME_median <- bme_predict(ck, data_object, model, nugget, sill, range,
                          zk_range = c(-3, 4), type = "median")
head(BME_median)


################################################################################
#     Spatial plots from BME predictions
################################################################################

plot(BME_mean)


################################################################################
#     BME credible interval predictions for estimation locations
################################################################################

# 90% credible interval
bme_predict_ci(head(ck), data_object, model, nugget, sill, range,
               zk_range = c(-3, 5), level = 0.90)

# 95% credible interval
bme_predict_ci(head(ck), data_object, model, nugget, sill, range,
               zk_range = c(-3, 5), level = 0.95)


################################################################################
#     LOOCV CV for posterior mean
################################################################################

CBME_cv <- bme_cv(data_object, model, nugget, sill, range,
                  zk_range = c(-3, 5), type = "mean", k = nrow(ch))
head(CBME_cv, 10)


# Summary of LOOCV results
summary(CBME_cv)

# diagnostic plots of residuals
plot(CBME_cv)
