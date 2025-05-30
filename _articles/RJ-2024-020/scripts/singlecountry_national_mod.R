# This is the R script that will reproduce Use Case 2: A single-country national-level model.

# Install the mcmsupply R package from github or CRAN.
# devtools::install_github("hannahcomiskey/mcmsupply")
install.packages(mcmsupply)
library(mcmsupply)

# Access the national multi-country data
cleaned_natdata <- get_data(national=TRUE, local=TRUE, mycountry="Nepal")

# Get the modelling inputs for the JAGS models
pkg_data <- get_modelinputs(startyear=1990, endyear=2025.5, nsegments=12,
                            raw_data = cleaned_natdata)

# Run the JAGS model for a multi-country national-level model
mod <- run_jags_model(jagsdata = pkg_data, jagsparams = NULL,
                      n_iter = 50000, n_burnin = 10000, n_thin = 20)

## Check the model diagnostics and traceplots using ggplot2 and tidybayes R packages
plot(mod$JAGS)
print(mod$JAGS)

sample_draws <- tidybayes::tidy_draws(mod$JAGS$BUGSoutput$sims.matrix)

var <- sample_draws %>% dplyr::select(.chain, .iteration, .draw,`P[1,2,1]`) %>%
  dplyr::mutate(chain = rep(1:2, each=mod$JAGS$BUGSoutput$n.keep)) %>%
  dplyr::mutate(iteration = rep(1:mod$JAGS$BUGSoutput$n.keep, 2))

ggplot2::ggplot(data=var) +
  ggplot2::geom_line(ggplot2::aes(x=iteration, y=`P[1,2,1]`, color=as.factor(chain)))

# Plot the posterior estimates
plots <- plot_estimates(jagsdata = pkg_data, model_output = mod)

# Review the plot for Nepal
plots[[1]]

# Pull the estimates for a. specific year, e.g.:2018.
estimates_2018 <- pull_estimates(model_output = mod, country = 'Nepal', year=2018)

head(estimates_2018)

# Pull 4 samples from the estimated national-level method supply shares posterior
post_samps <- get_posterior_P_samps(jagsdata = pkg_data, model_output = mod, nposterior=4)
head(post_samps)

