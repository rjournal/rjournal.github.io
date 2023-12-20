####### bayesassurance Replication Script ########
## Examples shown in order of paper

## install and load the required packages
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("pbapply")
# install.packages("dplyr")
# install.packages("plot3D")
# install.packages("plotly")
# install.paackages("latex2exp")
# library(ggplot2)
# library(MASS)
# library(pbapply)
# library(dplyr)
# library(plot3D)
# library(plotly)
# library(latex2exp)


library(bayesassurance)
## Page 4
## Reproduces Figure 1
n <- seq(100, 250, 10)
n_a <- 10
n_d <- 10
theta_0 <- 0.15
theta_1 <- 0.25
sigsq <- 0.30

out <- assurance_nd_na(n = n, n_a = n_a,  n_d = n_d, theta_0 = theta_0, theta_1 = theta_1, sigsq = sigsq,
                       alt = "greater", alpha = 0.05)

head(out$assurance_table)
out$assurance_plot




## Page 6
## Reproduces Figure 2a
n <- seq(10, 250, 5)
out <- pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.25, sigsq = 0.104, alt = "greater", alpha = 0.05)

out$pwr_table
out$pwr_plot




## Page 6
## Reproduces Figure 2b
n <- seq(10, 250, 5)
n_a <- 1e-8 # large precision parameter in analysis stage signifies weak analysis prior
n_d <- 1e+8
theta_0 <- 0.15
theta_1 <- 0.25
sigsq <- 0.104

out <- assurance_nd_na(n = n, n_a = n_a,  n_d = n_d, theta_0 = theta_0, theta_1 = theta_1, sigsq = sigsq,
                       alt = "greater", alpha = 0.05)

head(out$assurance_table)
out$assurance_plot




## Page 8
## Example 1 of Paper
n <- seq(100, 300, 10)

assur_vals <- bayesassurance::bayes_sim(n, p = 1, u = 1,
                                        C = 0.15, Xn = NULL, Vbeta_d = 0, Vbeta_a_inv = 0,
                                        Vn = NULL, sigsq = 0.265, mu_beta_d = 0.25, mu_beta_a = 0,
                                        alt = "greater", alpha = 0.05, mc_iter = 5000)
assur_vals$assurance_table
assur_vals$assurance_plot




## Page 9
## Example 2 of Paper (Entries taken from O'Hagan/Stevens 2001)
## Note the progress bar doesn't show up for scalar entries of n, plan to update
## this in future edits

n <- 285
p <- 4
K <- 20000 # threshold unit cost
C <- 0
u <- as.matrix(c(-K, 1, K, -1))
sigsq <- 4.04^2

## Assign mean parameters to analysis and design stage priors
mu_beta_d <- as.matrix(c(5, 6000, 6.5, 7200))
mu_beta_a <- as.matrix(rep(0, p))

## Assign correlation matrices (specified in paper)
## to analysis and design stage priors
Vbeta_a_inv <- matrix(rep(0, p^2), nrow = p, ncol = p)
Vbeta_d <- (1 / sigsq) * matrix(c(4, 0, 3, 0, 0, 10^7, 0,
                                  0, 3, 0, 4, 0, 0, 0, 0, 10^7), nrow = 4, ncol = 4)

tau1 <- tau2 <- 8700
sig <- sqrt(sigsq)
Vn <- matrix(0, nrow = n*p, ncol = n*p)
Vn[1:n, 1:n] <- diag(n)
Vn[(2*n - (n-1)):(2*n), (2*n - (n-1)):(2*n)] <- (tau1 / sig)^2 * diag(n)
Vn[(3*n - (n-1)):(3*n), (3*n - (n-1)):(3*n)] <- diag(n)
Vn[(4*n - (n-1)):(4*n), (4*n - (n-1)):(4*n)] <- (tau2 / sig)^2 * diag(n)

set.seed(10)
assur_val <- bayes_sim(n = 285, p = 4,  u = as.matrix(c(-K, 1, K, -1)),
                       C = 0, Xn = NULL, Vbeta_d = Vbeta_d, Vbeta_a_inv = Vbeta_a_inv,
                       Vn = Vn, sigsq = 4.04^2, mu_beta_d = as.matrix(c(5, 6000, 6.5, 7200)),
                       mu_beta_a = as.matrix(rep(0, p)), alt = "greater", alpha = 0.05, mc_iter = 500)

assur_val




## Page 11
## Example 3 of Paper
## Reproduces Figure 3
n <- seq(10, 100, 5)
ids <- c(1,2)
Vbeta_a_inv <- matrix(rep(0, 16), nrow = 4, ncol = 4)
sigsq = 100
Vbeta_d <- (1 / sigsq) * matrix(c(4, 0, 3, 0, 0, 6, 0, 0, 3, 0, 4, 0, 0, 0, 0, 6),
                                nrow = 4, ncol = 4)

set.seed(12)
assur_out <- bayes_sim(n = n, p = NULL, u = c(1, -1, 1, -1), C = 0, Xn = NULL,
                       Vbeta_d = Vbeta_d, Vbeta_a_inv = Vbeta_a_inv,
                       Vn = NULL, sigsq = 100,
                       mu_beta_d = as.matrix(c(5, 6.5, 62, 84)),
                       mu_beta_a = as.matrix(rep(0, 4)), mc_iter = 5000,
                       alt = "two.sided", alpha = 0.05, longitudinal = TRUE, ids = ids,
                       from = 10, to = 120)

head(assur_out$assurance_table)
assur_out$assurance_plot




## Page 13
## Example 4 of Paper
## Reproduces Figure 4
## Note this code block takes some time to run as the code is checking across all
## combinations of n1 and n2 to produce contour plot.
n1 <- seq(20, 75, 5)
n2 <- seq(50, 160, 10)

set.seed(3)
assur_out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, u = c(1, -1),
                                  C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),nrow = 2, ncol = 2),
                                  Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
                                  Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
                                  mu_beta_a = c(0, 0), alt = "two.sided", alpha = 0.05, mc_iter = 5000,
                                  surface_plot = TRUE)

head(assur_out$assurance_table)
assur_out$contourplot





## Page 14
## Example 5 of Paper
## Reproduces Figure 5
n1 <- c(4, 5, 15, 25, 30, 100, 200)
n2 <- c(8, 10, 20, 40, 50, 200, 250)

mu_beta_d <- as.matrix(c(5, 6000, 6.5, 7200))
mu_beta_a <- as.matrix(rep(0, 4))
K = 20000 # threshold unit cost
C <- 0
u <- as.matrix(c(-K, 1, K, -1))
sigsq <- 4.04^2
Vbeta_a_inv <- matrix(rep(0, 16), nrow = 4, ncol = 4)
Vbeta_d <- (1 / sigsq) * matrix(c(4, 0, 3, 0, 0, 10^7, 0, 0,
                                  3, 0, 4, 0, 0, 0, 0, 10^7),nrow = 4, ncol = 4)

set.seed(3)
assur_out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 2,
                                  u = as.matrix(c(-K, 1, K, -1)), C = 0, Xn = NULL,
                                  Vbeta_d = Vbeta_d, Vbeta_a_inv = Vbeta_a_inv,
                                  Vn = NULL, sigsq = 4.04^2,
                                  mu_beta_d = as.matrix(c(5, 6000, 6.5, 7200)),
                                  mu_beta_a = as.matrix(rep(0, 4)),
                                  alt = "greater", alpha = 0.05, mc_iter = 5000,
                                  surface_plot = TRUE)

assur_out$assurance_table
assur_out$contourplot




## Page 15
## Reproduces Figure 6
library(bayesassurance)

out <- pwr_curve(n = seq(10, 200, 10), n_a = 1e-8, n_d = 1e+8,
                 sigsq = 0.104, theta_0 = 0.15,theta_1 = 0.25,
                 alt = "greater", alpha = 0.05,
                 bayes_sim = TRUE, mc_iter = 5000)

out$power_table
out$assurance_table
out$plot





## Page 16
## Reproduces Figure 7
library(bayesassurance)

out <- pwr_curve(n = seq(10, 200, 10), n_a = 1e-8, n_d = 1e-8,
                 sigsq = 0.104, theta_0 = 0.15,theta_1 = 0.25,
                 alt = "greater", alpha = 0.05,
                 bayes_sim = TRUE, mc_iter = 5000)

out$plot




## Page 17
## Reproduces standard design matrix
n <- c(1,3,5,8)
gen_Xn(n = n)





## Page 18
## Reproduces longitudinal design matrix
ids <- c(1,2,3,4)
gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4)
