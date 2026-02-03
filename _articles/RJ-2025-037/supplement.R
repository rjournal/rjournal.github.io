# Install packages
# install.packages("AcceptReject")
# install.packages("tictoc")
# install.packages("SimDesign")
# install.packages("numDeriv")
# install.packages("bench")
# install.packages("ggplot2")

# Load packages
library(AcceptReject)
library(parallel)
library(tictoc)
library(SimDesign)
library(numDeriv)
library(bench)
library(parallel)
library(ggplot2)

# Example 1 ---------------------------------------------------------------

# Considering c = 1 (default)
inspect(
  f = dweibull,
  f_base = dunif,
  xlim = c(0, 5),
  args_f = list(shape = 2, scale = 1),
  args_f_base = list(min = 0, max = 5),
  c = 1
)

# Considering c = 4.3
inspect(
  f = dweibull,
  f_base = dunif,
  xlim = c(0, 5),
  args_f = list(shape = 2, scale = 1),
  args_f_base = list(min = 0, max = 5),
  c = 4.3
)

# Example 2 ---------------------------------------------------------------

set.seed(0)

# Generate 100 observations from a random variable X with
# f_X(x) = 2x, 0 <= x <= 1.
x <- accept_reject(
  n = 100L,
  f = function(x) 2 * x,
  args_f = list(),
  xlim = c(0, 1),
  warning = FALSE
)
print(x)

# Example 3 ---------------------------------------------------------------

set.seed(0)
x <- accept_reject(
  n = 2000L,
  f = dbinom,
  continuous = FALSE,
  args_f = list(size = 5, prob = 0.5),
  xlim = c(0, 10)
)

# Printing the first 10 (default) observations
print(x)

# Printing the first 20 observations
print(x, n_min = 20L)

# Summary
summary(x)

# Example 4 ---------------------------------------------------------------

# Generating and plotting the theoretical density with the
# observed density.

# setting a seed for reproducibility
set.seed(0)

# Continuous case
accept_reject(
  n = 2000L,
  continuous = TRUE,
  f = dweibull,
  args_f = list(shape = 2.1, scale = 2.2),
  xlim = c(0, 10)
) |>
  plot(
    hist = FALSE,
    color_true_density = "#2B8b99",
    color_observed_density = "#F4DDB3",
    alpha = 0.6
  ) # Changing some arguments in plot()

# Discrete case
accept_reject(
  n = 1000L,
  f = dbinom,
  continuous = FALSE,
  args_f = list(size = 5, prob = 0.5),
  xlim = c(0, 10)
) |> plot()

# Example 5 ---------------------------------------------------------------

# Ensuring reproducibility in parallel computing
RNGkind("L'Ecuyer-CMRG")
set.seed(0)
mc.reset.stream()

# Simulation
simulation <- function(n, lambda = 0.7)
  accept_reject(
    n = n,
    f = dpois,
    continuous = FALSE, # discrete case
    args_f = list(lambda = lambda),
    xlim = c(0, 20),
    parallel = TRUE # Parallelizing the code in Unix-based systems
  )

# Generating observations
# n = 25 observations
tic()
simulation(25) |> plot()
toc()

# n = 2500 observations
tic()
simulation(2500) |> plot()
toc()

# Example 6 ---------------------------------------------------------------

# Ensuring reproducibility in parallel computing
RNGkind("L'Ecuyer-CMRG")
set.seed(0)
mc.reset.stream()

# Generating observations
accept_reject(
  n = 50L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4),
  parallel = TRUE
) |> plot()

accept_reject(
  n = 500L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4),
  parallel = TRUE
) |> plot()

# Example 7 ---------------------------------------------------------------

pdf <- function(x, G, ...){
  numDeriv::grad(
    func = \(x) G(x, ...),
    x = x
  )
}

# Modified Beta Distributions
# Link: https://link.springer.com/article/10.1007/s13571-013-0077-0
generator <- function(x, G, a, b, beta, ...){
  g <- pdf(x = x, G = G, ...)
  numerator <- beta^a * g * G(x, ...)^(a - 1) * (1 - G(x, ...))^(b - 1)
  denominator <- beta(a, b) * (1 - (1 - beta) * G(x, ...))^(a + b)
  numerator/denominator
}

# Probability density function - Modified Beta Weibull
pdf_mbw <- function(x, a, b, beta, shape, scale)
  generator(
    x = x,
    G = pweibull,
    a = a,
    b = b,
    beta = beta,
    shape = shape,
    scale = scale
  )

# Checking the value of the integral
integrate(
  f = \(x) pdf_mbw(x, 1, 1, 1, 1, 1),
  lower = 0,
  upper = Inf
)

# Example 8 ---------------------------------------------------------------

# True parameters
a <- 10.5
b <- 4.2
beta <- 5.9
shape <- 1.5
scale <- 1.7

# c = 1 (default)
inspect(
  f = pdf_mbw,
  f_base = dweibull,
  xlim = c(0, 4),
  args_f = list(a = a, b = b, beta = beta, shape = shape, scale = scale),
  args_f_base = list(shape = 2, scale = 1.2),
  c = 1
)

# c = 2.2
inspect(
  f = pdf_mbw,
  f_base = dweibull,
  xlim = c(0, 4),
  args_f = list(a = a, b = b, beta = beta, shape = shape, scale = scale),
  args_f_base = list(shape = 2, scale = 1.2),
  c = 2.2
)

# Example 9 ---------------------------------------------------------------

simulation <- function(n, parallel = TRUE, base = TRUE){
  # True parameters
  a <- 10.5
  b <- 4.2
  beta <- 5.9
  shape <- 1.5
  scale <- 1.7
  c <- 2.2
  
  # Generate data with the true parameters using
  # the AcceptReject package.
  if(base){ # Using the Weibull distribution as the base distribution
    accept_reject(
      n = n,
      f = pdf_mbw,
      args_f = list(a = a, b = b, beta = beta, shape = shape, scale = scale),
      f_base = dweibull,
      args_f_base = list(shape = 2, scale = 1.2),
      random_base = rweibull,
      xlim = c(0, 4),
      c = c,
      parallel = parallel
    )
  } else { # Using the uniforme distribution as the base distribution
    accept_reject(
      n = n,
      f = pdf_mbw,
      args_f = list(a = a, b = b, beta = beta, shape = shape, scale = scale),
      xlim = c(0, 4),
      parallel = parallel
    )
  }
}

benchmark <- function(n_values, time_unit = 's', base = TRUE){
  # Initialize an empty data frame to store the results
  results_df <- data.frame()
  
  # Run benchmarks for each sample size and each type of code
  for(n in n_values) {
    for(parallel in c(TRUE, FALSE)) {
      results <- bench::mark(
        simulation(n = n, parallel = parallel, base = base),
        time_unit = time_unit,
        memory = FALSE,
        check = FALSE
      )
      
      # Convert results to data frame and add columns for the sample
      # size and type of code
      results_df_temp <- as.data.frame(results)
      results_df_temp$n <- n
      results_df_temp
      results_df_temp$Code <- ifelse(parallel, "Parallel", "Serial")
      
      # Append the results to the results data frame
      results_df <- rbind(results_df, results_df_temp)
    }
  }
  
  # Create a scatter plot of the median time vs the sample size,
  # colored by the type of code
  ggplot(results_df, aes(x = n, y = median, color = Code)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "Sample Size (n)", y = "Median Time (s)", color = "Code Type") +
    ggtitle("Benchmark Results") +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(face = "plain")
    )
}

# Sample sizes
n <- c(50, 250, 500, 1e3, 5e3, 10e3, 15e3, 25e3, 50e3, 100e3, 150e3, 250e3, 500e3, 1e6)

# Ensuring reproducibility in parallel computing
RNGkind("L'Ecuyer-CMRG")
set.seed(0)
mc.reset.stream()

# Run the benchmark function for multiple sample sizes
n |> benchmark(n_values = _, base = TRUE)
n |> benchmark(n_values = _, base = FALSE)

# Example 10 --------------------------------------------------------------

simulation_1 <- function(n, parallel = TRUE, base = TRUE){
  accept_reject(
    n = n,
    f = pdf_mbw,
    args_f = list(a = 10, b = 1, beta = 20.5, shape = 2, scale = 0.3),
    xlim = c(0, 1),
    parallel = parallel
  )
}

simulation_2 <- function(n){
  
  df = \(x) pdf_mbw(x = x, a = 10, b = 1, beta = 20.5, shape = 2, scale = 0.3)
  dg = \(x) dunif(x = x, min = 0, max = 1)
  rg = \(n) runif(n = n, min = 0, max = 1)
  
  # when df and dg both integrate to 1, acceptance probability = 1/M
  M <-
    rejectionSampling(
      df = df,
      dg = dg,
      rg = rg
    )
  rejectionSampling(n, df = df, dg = dg, rg = rg, M = M)
}

benchmark <- function(n_values, parallel = TRUE){
  # Initialize an empty data frame to store the results
  results_df <- data.frame()
  
  # Run benchmarks for each sample size and each type of code
  for(n in n_values) {
    results <- bench::mark(
      AcceptReject = simulation_1(n = n, parallel = parallel),
      SimDesign = simulation_2(n = n),
      time_unit = 's',
      memory = FALSE,
      check = FALSE
    )
    
    # Convert results to data frame and add columns for the sample
    # size and type of code
    results_df_temp <- results
    results_df_temp$n <- n
    
    # Append the results to the results data frame
    results_df <- rbind(results_df, results_df_temp)
  }
  
  # Create a scatter plot of the median time vs the sample size,
  # colored by the type of code
  ggplot(results_df, aes(x = n, y = median, color = expression)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "Sample Size (n)", y = "Median Time (s)", color = "Packages") +
    ggtitle("Benchmark Results") +
    theme(
      axis.title = element_text(face = "bold"),
      title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "plain")
    )
}

small_and_moderate_sample <- c(100, 150, 250, 500, 1e3, 1500, 2000, 2500, 3500, 4500, 5500, 7500, 10e3, 25e3)
big_sample <- c(50e3, 75e3, 100e3, 150e3, 250e3, 500e3, 750e3, 1e6)
# Ensuring reproducibility in parallel computing
RNGkind("L'Ecuyer-CMRG")
set.seed(0)
mc.reset.stream()

# Serial
benchmark(n_values = small_and_moderate_sample, parallel = FALSE)

# Parallel
benchmark(n_values = big_sample, parallel = TRUE)