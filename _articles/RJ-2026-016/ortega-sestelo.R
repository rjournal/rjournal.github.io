################################################################################
#  EXAMPLE CODE FOR RUNNING NEURALGAM ON SIMULATED AND REAL DATA
#  Core + advanced features (confidence intervals & diagnostics)
#  Comparisons: deepregression & black-box neural networks
#  Simulated data (gaussian) + application to flight delay prediction
################################################################################


################################################################################
# 1. SIMULATED DATA (Gaussian)
################################################################################

## 1.1 Generate simlated scenario ----------------------------------------------

seed <- 42
set.seed(seed)
n <- 30625
x1 <- runif(n, -2.5, 2.5)
x2 <- runif(n, -2.5, 2.5)
x3 <- runif(n, -2.5, 2.5)

f1 <- x1 ** 2
f2 <- 2 * x2
f3 <- sin(x3)
f1 <- f1 - mean(f1)
f2 <- f2 - mean(f2)
f3 <- f3 - mean(f3)
fs <- data.frame(f1, f2, f3)

eta0 <- 2 + f1 + f2 + f3
epsilon <- rnorm(n, 0.25)
y <- eta0 + epsilon
dat <- data.frame(x1, x2, x3, y)
sample <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.8, 0.2))

train <- dat[sample,]
test <- dat[!sample,]

names(fs) <- names(dat[,1:3])
fs_train <- fs[sample,]
fs_test <- fs[!sample,]

summary(dat)

## 1.2 Fit neuralGAM with epistemic confidence intervals ------------------------

if (!require("neuralGAM")) install.packages("neuralGAM", quiet = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", quiet = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", quiet = TRUE)

suppressMessages(library(neuralGAM))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))

ngam <- neuralGAM(
  y ~ s(x1) + s(x2) + s(x3), data = train,
  num_units = 1024, learning_rate = 0.001,
  bf_threshold = 0.001, uncertainty_method = "epistemic",
  forward_passes = 150, seed = seed, verbose = 1
)

ngam


### MSE (train) and Deviance explained are available at the print summary but also:
cat(sprintf("Deviance explained : %.2f%%\n",
            attr(neuralGAM:::.deviance_explained.neuralGAM(ngam), "percent")))
cat("Train MSE (neuralGAM): ", format(ngam$mse, digits = 6), "\n", sep = "")

## 1.3 Predictions and test performance ----------------------------------------
pred_resp <- predict(ngam, test, type = "response")
cat("MSE Test (neuralGAM):", mean((pred_resp - test$y)^2), "\n")

## 1.4 Compare learned smooths to true functions + CI  -------------------------

term_list <- c("x1","x2","x3")
plot_terms <- lapply(term_list, function(v) {
  autoplot(ngam, which = "terms", term = v, interval = "confidence") +
    ggplot2::geom_line(aes(x = train[[v]], y = fs_train[[v]]), linetype = 2, colour = "blue")
})
gridExtra::grid.arrange(grobs = plot_terms, ncol = 3)

# 1.5 Deepregression  -----------------------------------------------

## 1.5.1. Setup deepregression and dependencies -------------------------------
if (!require("deepregression")) remotes::install_version("deepregression", version = "2.3.1")
# wE use the same reticulate environment as for neuralGAM to avoid double-install
py <- normalizePath(reticulate::py_config()$python, winslash = "/")
system2(py, c("-m", "pip", "install", "--upgrade",
              "tensorflow-probability==0.23.0"))
suppressMessages(library(deepregression))
deepregression::check_and_install(engine = "tf")
suppressMessages(tensorflow::tf$config$list_physical_devices("CPU"))

### 1.5.2. Fit a GAM model using deepregression
list_of_formulas <- list(
  loc = ~ 1 + s(x1) + s(x2) + s(x3),
  scale = ~ 1
)

deepreg_mod <- deepregression::deepregression(
  y = train$y, data = train,
  list_of_formulas = list_of_formulas,
  list_of_deep_models = NULL
)

deepreg_mod %>% fit(
  epochs = 100, view_metrics = FALSE, early_stopping = TRUE, patience = 5
)

yhat_gam_train <- predict(deepreg_mod, newdata = train)
mse_train_dr <- mean((yhat_gam_train - train$y)^2)
cat("MSE Train (deepregression):", mse_train_dr, "\n")

yhat_gam <- predict(deepreg_mod, newdata = test)
mse_dr <- mean((yhat_gam - test$y)^2)
cat("MSE Test (deepregression):", mse_dr, "\n")

# Compute deviance explained vs a null model (intercept only)
# predicted mean
yhat <- as.numeric(predict(deepreg_mod, newdata = train))

# residual deviance (for Gaussian: RSS)
D_model <- sum((train$y - yhat)^2)

# null deviance (mean-only model)
ybar <- mean(train$y)
D_null <- sum((train$y - ybar)^2)

# deviance explained
dev_exp <- 1 - (D_model / D_null)
cat("Deviance explained:", round(100 * dev_exp, 2), "%\n")

op <- par(mfrow = c(1,3))
for (x in c("x1", "x2", "x3")) {
  d <- plot(deepreg_mod, which = paste0("s(", x, ")"), only_data = TRUE)
  pe <- if (!is.null(d$partial_effect)) drop(d$partial_effect) else drop(d[[1]]$partial_effect)
  df_fit <- data.frame(x = train[[x]], fit = pe)
  df_fit <- df_fit[order(df_fit$x), ]   # sort for smooth plotting
  df_true <- data.frame(x = train[[x]], truth = fs_train[[x]])
  df_true <- df_true[order(df_true$x), ]  # sort too
  matplot(df_fit$x, df_fit$fit, type = "l", lwd = 1, col = "black",
          lty = 1, xlab = x, ylab = "Partial Effect", main = paste0("s(", x, ")"))
  matlines(df_true$x, df_true$truth, col = "blue", lty = 2)
}

### 1.5.3. Obtain uncertainty with deepregression ensembles (See Section 2.10 of Rügamer et al (2023))
# Add ensemble for uncertainty
ens <- ensemble(deepreg_mod, n_ensemble = 10, epochs = 100, early_stopping = TRUE, patience = 5)

# Per-member distributions along x1
mems <- deepregression:::.call_for_all_members(ens, get_distribution, data = train)

# Ensemble mixture distribution
ensd <- get_ensemble_distribution(ens, data = train)

# Member means (matrix 1000 x M)
mem_preds <- do.call("cbind", lapply(mems, function(d) as.matrix(tfd_mean(d))))

# Ensemble predictive mean
ens_preds <- as.matrix(tfd_mean(ensd))

# Aleatoric (predictive) uncertainty from mixture stddev (not used in the band below)
alea <- as.matrix(tfd_stddev(ensd))

# Epistemic uncertainty = pointwise SD across member means
unc <- apply(mem_preds, 1, sd)

# ----- helper: build plot data for one covariate (with epistemic band) -----
make_panel_data <- function(var, nd_base, ens, n = 200) {
  stopifnot(var %in% names(nd_base))
  xseq <- seq(min(train[[var]]), max(train[[var]]), length.out = n)

  # replicate base row to n rows, then replace the "var" column by xseq
  nd <- nd_base[rep(1L, n), , drop = FALSE]
  rownames(nd) <- NULL
  nd[[var]] <- xseq

  # per-member distributions at nd
  mems <- deepregression:::.call_for_all_members(ens, get_distribution, data = nd)

  # per-member means (n x M)
  mem_means <- do.call("cbind", lapply(mems, function(d) as.matrix(tfd_mean(d))))

  ref_idx <- which.min(abs(xseq - mean(train[[var]])))

  # subtract baseline (removes intercept + other covariates)
  part <- sweep(mem_means, 2, mem_means[ref_idx, ], "-")

  # mean center:
  part <- sweep(part, 2, colMeans(part), "-")

  # ensemble mixture distribution & mean (n x 1)
  ensd <- get_ensemble_distribution(ens, data = nd)
  ens_mean <- as.numeric(tfd_mean(ensd))

  mu <- rowMeans(part)
  sd <- apply(part, 1, sd)

  # epistemic SD across member means
  epi_sd <- apply(mem_means, 1, sd)

  list(x = xseq, mem = mem_means, members = part, ens = ens_mean, sd = epi_sd, mu = mu, sd_center = sd)
}

# base row: fix the other covariates (use means here)
nd_base <- data.frame(
  x1 = mean(train$x1),
  x2 = mean(train$x2),
  x3 = mean(train$x3)
)

# build data for each panel
pd1 <- make_panel_data("x1", nd_base, ens, n = 500)
pd2 <- make_panel_data("x2", nd_base, ens, n = 500)
pd3 <- make_panel_data("x3", nd_base, ens, n = 500)

# ----- plotting helper -----
panel_plot <- function(pd, var, ylab = "Predicted conditional mean") {
  matplot(pd$x, pd$ens, type = "l", col = "black",
          xlab = var, ylab = ylab, lwd = 2)
  polygon(c(pd$x, rev(pd$x)),
          c(pd$ens - pd$sd, rev(pd$ens + pd$sd)),
          border = NA, col = adjustcolor("grey70", 0.5))
}

op <- par(mfrow = c(1,3))
panel_plot(pd1, "x1")
panel_plot(pd2, "x2")
panel_plot(pd3, "x3")
par(op)

################################################################################
# 2. REAL DATA APPLICATION — FLIGHT DELAYS (Binomial)
################################################################################

if (!require("magrittr")) install.packages("magrittr", quiet = TRUE)
if (!require("dplyr")) install.packages("dplyr", quiet = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", quiet = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", quiet = TRUE)
if (!require("nycflights13")) install.packages("nycflights13", quiet = TRUE)
if (!require("pROC")) install.packages("pROC", quiet = TRUE)
if (!require("dplyr")) install.packages("dplyr")

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(nycflights13))
suppressMessages(library(dplyr))

# ---------- set env BEFORE loading TF/Keras ----------
seed <- 1234
set.seed(seed)

dat <- flights %>%
  filter(origin == "EWR" & month %in% c(10, 11, 12)) %>%
  left_join(weather, by = c("origin", "time_hour")) %>%
  select(arr_delay, dep_delay, air_time, temp, humid) %>%
  data.frame()

dat$temp <- (dat$temp - 32) / 1.8
dat$delay <- ifelse(dat$arr_delay > 0, 1, 0)
dat <- dat[!rowSums(is.na(dat)),]

sample <- sample(nrow(dat), 0.8 * nrow(dat))
train <- dat[sample, ]; test <- dat[-sample, ]

print(dat %>% count(delay))
head(dat)

### 2.1 PERFORMANCE COMPARISON ##################################################

## 2.1.1 neuralGAM --------------------------------------------------------------
ngam_bin <- neuralGAM(
  delay ~ s(air_time, activation = "swish", num_units = c(256,128)) +
    s(dep_delay) +
    s(temp, activation = "swish") +
    s(humid, activation = "tanh" ),
  data = train,
  family = "binomial",
  num_units = 128,
  bf_threshold = 1e-2,
  ls_threshold = 0.01, alpha = 0.05,
  loss = "mse",
  uncertainty_method = "epistemic", forward_passes = 300,
  dropout_rate = 0.01,
  seed = seed, verbose = 1
)

cat(sprintf("Deviance explained : %.2f%%\n",
            attr(neuralGAM:::.deviance_explained.neuralGAM(ngam_bin), "percent")))

prob_ngam <- as.numeric(neuralGAM:::predict.neuralGAM(ngam_bin, newdata = test, type = "response"))
auc_ngam <- pROC::roc(test$delay, prob_ngam)$auc
cat("AUC-ROC (neuralGAM):", auc_ngam, "\n")

## 2.1.2 deepregression ---------------------------------------------------------

deepreg_mod_bin <- deepregression::deepregression(
  y = train$delay, data = train,
  list_of_formulas = list(
    loc = ~ 1 + s(air_time) + s(dep_delay) + s(temp) + s(humid)
  ),
  list_of_deep_models = NULL, family = "bernoulli"
)

deepreg_mod_bin %>% fit(
  epochs = 100, view_metrics = FALSE, early_stopping = TRUE, patience = 5
)

yhat_dr <- predict(deepreg_mod_bin, newdata = test)
auc_dr <- pROC::roc(test$delay, yhat_dr)$auc
cat("AUC-ROC (deepregression):", auc_dr, "\n")
# AUC-ROC (deepregression): 0.8381508

# Fitted probabilities on TRAIN (Bernoulli mean)
p_hat <- as.numeric(predict(deepreg_mod_bin, newdata = train))  # in (0,1)

# True labels in {0,1}
y <- as.numeric(train$delay)

# Clip to avoid log(0)
eps <- 1e-12
p_hat <- pmin(pmax(p_hat, eps), 1 - eps)

# Binomial deviance for the fitted model
# D = 2 * sum[ y*log(y/p) + (1-y)*log((1-y)/(1-p)) ]
# With y in {0,1}, this simplifies to:
D_model <- 2 * sum( y * log(1 / p_hat) + (1 - y) * log(1 / (1 - p_hat)) )

# Null model: intercept-only probability = mean(y)
p0 <- mean(y)
p0 <- min(max(p0, eps), 1 - eps)
D_null <- 2 * sum( y * log(1 / p0) + (1 - y) * log(1 / (1 - p0)) )

# Deviance explained
dev_exp <- 1 - (D_model / D_null)
cat("Deviance explained (Bernoulli, deepregression):", round(100 * dev_exp, 2), "%\n")

## 2.1.3 Black-box NN (keras) ---------------------------------------------------

Sys.setenv(
  PYTHONHASHSEED = "0",
  TF_DETERMINISTIC_OPS = "1"
)

library(keras)
tensorflow::tf$random$set_seed(seed)

# ---------- Python + NumPy RNGs ----------
reticulate::py_run_string(sprintf("
import os, random
os.environ['PYTHONHASHSEED'] = '0'
random.seed(%d)
import numpy as np
np.random.seed(%d)
", seed, seed))

if (!is.null(tf$config$experimental$enable_op_determinism))
  tf$config$experimental$enable_op_determinism()

x_train <- as.matrix(train[, c("air_time","dep_delay","temp","humid")])
y_train <- train$delay
x_test  <- as.matrix(test[, c("air_time","dep_delay","temp","humid")])
y_test  <- test$delay

keras_model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "swish", input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.01, seed = seed) %>%
  layer_dense(units = 128, activation = "swish") %>%
  layer_dropout(rate = 0.01, seed = seed) %>%
  layer_dense(units = 1, activation = "sigmoid")
keras_model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "binary_crossentropy", metrics = c("accuracy")
)
keras_model %>% fit(x_train, y_train, epochs = 100, batch_size = 128, verbose = 1, shuffle = FALSE,
                    view_metrics = FALSE, callbacks = list(callback_early_stopping(monitor = "accuracy", patience = 5)))

prob_nn <- predict(keras_model, x_test) %>% as.numeric()
auc_nn <- pROC::roc(y_test, prob_nn)$auc
cat("AUC-ROC (keras NN):", auc_nn, "\n")

# Fitted probabilities on TRAIN (Bernoulli mean)
p_hat <- as.numeric(predict(keras_model, x_train))  # in (0,1)

# True labels in {0,1}
y <- as.numeric(train$delay)

# Clip to avoid log(0)
eps <- 1e-12
p_hat <- pmin(pmax(p_hat, eps), 1 - eps)

# Binomial deviance for the fitted model
# D = 2 * sum[ y*log(y/p) + (1-y)*log((1-y)/(1-p)) ]
# With y in {0,1}, this simplifies to:
D_model <- 2 * sum( y * log(1 / p_hat) + (1 - y) * log(1 / (1 - p_hat)) )

# Null model: intercept-only probability = mean(y)
p0 <- mean(y)
p0 <- min(max(p0, eps), 1 - eps)
D_null <- 2 * sum( y * log(1 / p0) + (1 - y) * log(1 / (1 - p0)) )

# Deviance explained
dev_exp <- 1 - (D_model / D_null)
cat("Deviance explained (Bernoulli) Keras:", round(100 * dev_exp, 2), "%\n")


### 2.2 INTERPRETABILITY COMPARISON #############################################

## 2.2.1 neuralGAM partial effects + CIs ---------------------------------------

p1 <- autoplot(ngam_bin, which = "terms", term = "air_time", interval = "confidence")
p2 <- autoplot(ngam_bin, which = "terms", term = "dep_delay", interval = "confidence")
p3 <- autoplot(ngam_bin, which = "terms", term = "temp", interval = "confidence")
p4 <- autoplot(ngam_bin, which = "terms", term = "humid", interval = "confidence")
(p1 | p2) / (p3 | p4)


## 2.2.2 deepregression smooths -------------------------------------------------
op <- par(mfrow = c(2,2))
plot(deepreg_mod_bin)

################################################################################
# END
################################################################################
