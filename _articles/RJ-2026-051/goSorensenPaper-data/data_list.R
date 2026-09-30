# This script contains the simulation routine to create the simulated data to 
# plot the bootstrap and true distribution associated with the Sorensen dissimilarity

# Figure 3 of the paper was plotted from the data obtained in this script.

configs <- list(
  "Low" = c(0.0125, 0.005, 0.005),  # p11, p01, p10
  "Moderate" = c(0.125, 0.05, 0.05),
  "High" = c(0.4, 0.2, 0.2)
)

stat <- function(tab, d) {
  return((dSorensen(tab) - d) / seSorensen(tab))
}

generate_simulations <- function(p_values) {
  p11 <- p_values[1]
  p01 <- p_values[2]
  p10 <- p_values[3]
  p00 <- 1 - sum(p11, p01, p10)
  prbs <- c(p11, p01, p10, p00)
  
  nSim <- 50000
  n <- 1000
  set.seed(1111)
  
  samples <- rmultinom(nSim, n, prbs)
  one_samp <- as.vector(samples[, sample(1:nSim, 1)])
  boots <- rmultinom(nSim, n, one_samp/n)
  
  trues.stats <- apply(samples, 2, stat, d = dSorensen(prbs))
  boot.stats <- apply(boots, 2, stat, d = dSorensen(one_samp))
  
  density_true <- density(trues.stats)
  density_boot <- density(boot.stats)
  sz <- length(density_true$x)
  x_norm <- seq(-6, 4, length.out = sz)
  y_norm <- dnorm(x_norm, mean = 0, sd = 1)
  
  data <- data.frame(
    x = c(density_true$x, density_boot$x, x_norm),
    y = c(density_true$y, density_boot$y, y_norm),
    Distribution = as.factor(rep(c("True Distribution", "Bootstrap", "Norm(0, 1)"), c(sz, sz, sz)))
  )
  
  return(data)
}

library(goSorensen)

data_list <- lapply(configs, generate_simulations)
names(data_list) <- names(configs)
save(data_list, file = "data_list.rda")

## The following code generates the plot in Figure 3 for the low enrichment
## This graph is depicted exclusively in the pdf version
data_low <- data_list[["Low"]]

graphboot <- ggplot(data_low, aes(x, y, colour = Distribution)) + 
  geom_line(aes(linetype = Distribution, linewidth = Distribution)) +
  scale_color_manual(
    values = c("True Distribution" = "black", "Bootstrap" = "deeppink", 
               "Norm(0, 1)" = "deepskyblue"),
    name = "Distribution:"
  ) +
  scale_linetype_manual(
    values = c("True Distribution" = "longdash", "Bootstrap" = "solid", 
               "Norm(0, 1)" = "solid"),
    name = "Distribution:"
  ) +
  scale_linewidth_manual(
    values = c("True Distribution" = 0.75, "Bootstrap" = 0.75, 
               "Norm(0, 1)" = 0.75),
    name = "Distribution:"
  ) +
  labs(x = " ", y = " ") +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.75),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)) +
  xlim(-6, 4)
graphboot
