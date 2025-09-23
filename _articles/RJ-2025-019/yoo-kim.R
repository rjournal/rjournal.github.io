#####
## Required Packages
#####
install.packages("bartcs", dependencies = TRUE)
install.packages("bacr", dependencies = TRUE)
install.packages("tableone")

#####
## CODE FOR SECTION 3. Simulated Example
#####

## fix seed and create data
set.seed(42)
N <- 300
P <- 100
cov <- list()
for (i in seq_len(P)) {
  cov[[i]] <- rnorm(N, 0, 1)
}
X <- do.call(cbind, cov)
h1 <- ifelse(X[, 1] < 0, 1, -1)
h2 <- ifelse(X[, 2] < 0, -1, 1)
prob <- pnorm(0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) +
                1.5 * X[, 4] * X[, 5])
Trt <- rbinom(N, 1, prob)
mu1 <- 1 * h1 + 1.5 * h2 - 1 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 1 * abs(X[, 6]) - 1 * 1 * abs(X[, 7] + 1)
mu0 <- 1 * h1 + 1.5 * h2 - 0 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 0 * abs(X[, 6]) - 1 * 0 * abs(X[, 7] + 1)
Y1 <- rnorm(N, mu1, 0.3)
Y0 <- rnorm(N, mu0, 0.3)
Y <- Trt * Y1 + (1 - Trt) * Y0

library("tableone")
Xdata <- as.data.frame(cbind(Trt,X))
names(Xdata) <- c("Trt", paste0(rep("X", 100),1:100))
Table <- CreateTableOne(vars = paste0(rep("X", 12),1:12), strata = "Trt", data = Xdata, test = FALSE)
print(Table, smd = TRUE)

## load library and fit the separate bart model
library("bartcs")
separate_fit <- separate_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
separate_fit
summary(separate_fit)

## PIP plots in Figure 4
## create sub-directory "fig"
if (!file.exists("fig")) {
  dir.create("fig")
}
library(ggplot2)
plot(separate_fit, method = "pip", top_n = 10)
ggplot2::ggsave("fig/PIP_top_n.pdf", height = 4, width = 6)

plot(separate_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/PIP_threshold.pdf", height = 4, width = 6)


## Trace plots in Figure 5
plot(separate_fit, method = "trace")
ggplot2::ggsave("fig/traceplot_ATE.pdf", height = 4, width = 6)
plot(separate_fit, method = "trace", parameter = "dir_alpha")
ggplot2::ggsave("fig/traceplot_alpha.pdf", height = 4, width = 6)


## Comparison to BACR (Wang et al. 2015)
library("bacr")

Z <- as.data.frame(cbind(Y, Trt, X))
fit.bac <- bac(
  data = Z, exposure = "Trt", outcome = "Y",
  confounders = paste("V", 3:(P + 2), sep = ""),
  interactors = NULL, familyX = "binomial", familyY = "gaussian",
  omega = Inf, num_its = 20000, burnM = 10000, burnB = 10000, thin = 5)
summary(fit.bac)

#####
## CODE FOR SECTION 3.1 Connection to coda package
#####
summary(separate_fit$mcmc_list)

## create plots in Figure 5
pdf("fig/sep_mcmc_list.pdf", height = 5, width = 8)
par(mfrow=c(3,4))
plot(separate_fit$mcmc_list, auto.layout = F)
dev.off()

library("coda")
gelman.diag(separate_fit$mcmc_list)



#####
## CODE FOR SECTION 4. Real data example
#####

## load the ihdp dataset
set.seed(42)
data("ihdp", package = "bartcs")

## produce Table 2
library(xtable)
mean1 <- apply(ihdp[ihdp$treatment==1, c(2,6:30)], 2, mean)
IQR1 <- apply(ihdp[ihdp$treatment==1, c(2,6:30)], 2, function(x) quantile(x, c(0.25, 0.75)))
mean0 <- apply(ihdp[ihdp$treatment==0, c(2,6:30)], 2, mean)
IQR0 <- apply(ihdp[ihdp$treatment==0, c(2,6:30)], 2, function(x) quantile(x, c(0.25, 0.75)))
xt <- cbind(round(mean1, 2), sprintf("(%.2f, %.2f)", t(IQR1)[,1], t(IQR1)[,2]), round(mean0, 2), sprintf("(%.2f, %.2f)", t(IQR0)[,1], t(IQR0)[,2]))
xt <- rbind(c("Mean", "IQR", "Mean", "IQR"), xt)
rownames(xt) <- c("Variable", "$Y$", paste0(rep("$X^{\\star}_{", 6),1:6,"}$"), paste0(rep("$X_{", 19),7:25,"}$"))
xt <- xtable(xt, align = "l|cc|cc", caption = "Summary statistics for the IHDP data set. $\\star$ denotes a continuous potential confounder.", label = "tab:iphd")
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0("& \\multicolumn{2}{c|}{\\code{Treatment} = 1 (n=139)}", "& \\multicolumn{2}{c}{\\code{Treatment} = 0 (n=608)} \\\\")
print(xt, add.to.row=addtorow, include.colnames=F, hline.after=c(0:1,27), sanitize.text.function=function(x){x})

## Overlap plot in Figure 7
pdf("fig/overlap.pdf", height=6, width=11)
par(mfrow=c(2, 2), mar = c(4, 5, 3, 4))
covariate <- c(8, 10, 23, 28)
for(i in 1:4){
  index <- covariate[i]
  plot(ihdp[ihdp$treatment==1, index], ihdp[ihdp$treatment==1, 2], col="blue", pch=2, ylim=range(ihdp[,2]), xlim=range(ihdp[, index]), xlab=names(ihdp)[covariate[i]], ylab = "Y")
  points(ihdp[ihdp$treatment==0, index], ihdp[ihdp$treatment==0, 2], col="red", pch=3)
}
dev.off()


# fit single bart
single_fit <- single_bart(
  Y               = ihdp$y_factual,
  trt             = ihdp$treatment,
  X               = ihdp[, 6:30],
  num_tree        = 50,
  num_chain       = 4,
  num_post_sample = 2000,
  num_thin        = 5,
  num_burn_in     = 10000
)

single_fit
summary(single_fit)

separate_fit <- separate_bart(
  Y               = ihdp$y_factual,
  trt             = ihdp$treatment,
  X               = ihdp[, 6:30],
  num_tree        = 50,
  num_chain       = 4,
  num_post_sample = 2000,
  num_thin        = 5,
  num_burn_in     = 10000
)
separate_fit

## True values
mean(ihdp$mu1) # E[Y(1)]: 6.44858
mean(ihdp$mu0) # E[Y(0)]: 2.432513
mean(ihdp$mu1 - ihdp$mu0) # ATE: 4.016067

## Traceplots in Figure 8
plot(single_fit, method = "trace")
ggplot2::ggsave("fig/ihdp_trace_ATE.pdf", width=6, height=4)

## PIP plots in Figure 9
plot(single_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/ihdp_PIP_threshold_single.pdf", width=6, height=4)

plot(separate_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/ihdp_PIP_threshold_separate.pdf", width=6, height=4)


#####
## CODE FOR SECTION 4.1. Computation speed
#####

library("bartcs")
library("microbenchmark")

## fix seed and create data
set.seed(42)

N_list <- c(100, 100, 100, 500, 500, 500, 1000, 1000, 1000)
P_list <- c(30, 50, 100, 150, 250, 500, 300, 500, 1000)

## simuated data from Section 4
data_list <- list()
for (i in seq_len(9)) {
  N <- N_list[i]
  P <- P_list[i]
  cov <- list()
  for(i in seq_len(P)) {
    cov[[i]] <- rnorm(N, 0, 1)
  }
  X <- do.call(cbind, cov)
  h1 <- ifelse(X[, 1] < 0, 1, -1)
  h2 <- ifelse(X[, 2] < 0, -1, 1)
  prob <- pnorm(0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) +
                  1.5 * X[, 4] * X[, 5])
  Trt <- rbinom(N, 1, prob) 
  mu1 <- 1 * h1 + 1.5 * h2 - 1 + 2 * abs(X[, 3] + 1) +
    2 * X[, 4] + exp(0.5 * X[, 5]) -
    0.5 * 1 * abs(X[, 6]) - 1 * 1 * abs(X[, 7] + 1)
  mu0 <- 1 * h1 + 1.5 * h2 - 0 + 2 * abs(X[, 3] + 1) +
    2 * X[, 4] + exp(0.5 * X[, 5]) -
    0.5 * 0 * abs(X[, 6]) - 1 * 0 * abs(X[, 7] + 1)
  Y1 <- rnorm(N, mu1, 0.3)
  Y0 <- rnorm(N, mu0, 0.3)
  Y <- Trt * Y1 + (1 - Trt) * Y0
  data <- list(N = N, P = P, Y = Y, Trt = Trt, X = X, Xtrt = cbind(X, Trt))
  data_list[[length(data_list) + 1]] <- data
}

## Number of trees: 100

result <- data.frame(model = character(0),
                     N = numeric(0),
                     P = numeric(0),
                     sec = numeric(0))
model_list <- c("separate", "single")

num_thin <- 5
num_tree <- 100
num_post_sample <- 500
num_burn_in <- 2500

for (model in model_list) {
  for (data in data_list) {
    cat(model, data$N, data$P, "\n")
    if (model == "separate") {
      benchmark <- microbenchmark::microbenchmark(
        separate_bart(
          Y = data$Y, trt = data$Trt, X = data$X,
          num_tree = num_tree, num_chain = 1,
          num_burn_in = num_burn_in, num_thin = num_thin,
          num_post_sample = num_post_sample, verbose = FALSE
        ),
        times = 1)
    } else if (model == "single") {
      benchmark <- microbenchmark::microbenchmark(
        single_bart(Y = data$Y, trt = data$Trt, X = data$X,
                    num_tree = num_tree, num_chain = 1,
                    num_burn_in = num_burn_in, num_thin = num_thin,
                    num_post_sample = num_post_sample, verbose = FALSE
        ),
        times = 1)
    }
    result <- rbind(result, data.frame(model = model, N = data$N,
                                       P = data$P, sec = benchmark$time / 1e9))
  }
}

## draw a plot for computation speed
library("data.table")
sep <- subset(result, model == "separate")
sin <- subset(result, model == "single")

pdf("fig/speed_trees100.pdf", height = 5, width = 6)
plot(1:3, (sep$sec[c(1, 4, 7)]), type = "b", ylim = c(1,200), xaxt = "n", yaxt = "n", xlab = "", pch=1, ylab = "Comp.Time (sec)", main="Number of Trees: 100")
points(1:3, (sep$sec[c(2, 5, 8)]), type = "b", pch = 2)
points(1:3, (sep$sec[c(3, 6, 9)]), type = "b", pch = 3)

points(1:3, (sin$sec[c(1, 4, 7)]), type = "b", pch = 1, col = 2)
points(1:3, (sin$sec[c(2, 5, 8)]), type = "b", pch = 2, col = 2)
points(1:3, (sin$sec[c(3, 6, 9)]), type = "b", pch = 3, col = 2)
axis(1, at = 1:3, labels = c("100", "500", "1000"),  las = 1)
axis(2, at = c(50, 100, 150, 200), labels = c("50", "100", "150", "200"), las = 1)
title(xlab = "N")
legend(1, 200, legend = c(expression(paste("Single (", P==N %*% 0.3, ")")), 
                          expression(paste("Single (", P==N %*% 0.5, ")")), 
                          expression(paste("Single (", P==N %*% 1, ")")), 
                          expression(paste("Separate (", P==N %*% 0.3, ")")), 
                          expression(paste("Separate (", P==N %*% 0.5, ")")), 
                          expression(paste("Separate (", P==N %*% 1, ")"))),
       col = c(rep("red",3), rep("black",3)), pch=c(1:3,1:3), lty = 1, cex = 1)
dev.off()


## Number of trees: 200

num_tree <- 200

result <- data.frame(model = character(0),
                     N = numeric(0),
                     P = numeric(0),
                     sec = numeric(0))

for (model in model_list) {
  for (data in data_list) {
    cat(model, data$N, data$P, "\n")
    if (model == "separate") {
      benchmark <- microbenchmark::microbenchmark(
        separate_bart(
          Y = data$Y, trt = data$Trt, X = data$X,
          num_tree = num_tree, num_chain = 1,
          num_burn_in = num_burn_in, num_thin = num_thin,
          num_post_sample = num_post_sample, verbose = FALSE
        ),
        times = 1)
    } else if (model == "single") {
      benchmark <- microbenchmark::microbenchmark(
        single_bart(Y = data$Y, trt = data$Trt, X = data$X,
                    num_tree = num_tree, num_chain = 1,
                    num_burn_in = num_burn_in, num_thin = num_thin,
                    num_post_sample = num_post_sample, verbose = FALSE
        ),
        times = 1)
    }
    result <- rbind(result, data.frame(model = model, N = data$N,
                                       P = data$P, sec = benchmark$time / 1e9))
  }
}

## draw a plot for computation speed
sep <- subset(result, model == "separate")
sin <- subset(result, model == "single")

pdf("fig/speed_trees200.pdf", height = 5, width = 6)
plot(1:3, (sep$sec[c(1, 4, 7)]), type = "b", ylim = c(1,200), xaxt = "n", yaxt = "n", xlab = "", pch=1, ylab = "Comp.Time (sec)", main="Number of Trees: 200")
points(1:3, (sep$sec[c(2, 5, 8)]), type = "b", pch = 2)
points(1:3, (sep$sec[c(3, 6, 9)]), type = "b", pch = 3)

points(1:3, (sin$sec[c(1, 4, 7)]), type = "b", pch = 1, col = 2)
points(1:3, (sin$sec[c(2, 5, 8)]), type = "b", pch = 2, col = 2)
points(1:3, (sin$sec[c(3, 6, 9)]), type = "b", pch = 3, col = 2)
axis(1, at = 1:3, labels = c("100", "500", "1000"),  las = 1)
axis(2, at = c(50, 100, 150, 200), labels = c("50", "100", "150", "200"), las = 1)
title(xlab = "N")
legend(1, 200, legend = c(expression(paste("Single (", P==N %*% 0.3, ")")), 
                          expression(paste("Single (", P==N %*% 0.5, ")")), 
                          expression(paste("Single (", P==N %*% 1, ")")), 
                          expression(paste("Separate (", P==N %*% 0.3, ")")), 
                          expression(paste("Separate (", P==N %*% 0.5, ")")), 
                          expression(paste("Separate (", P==N %*% 1, ")"))),
       col = c(rep("red",3), rep("black",3)), pch=c(1:3,1:3), lty = 1, cex = 1)
dev.off()



#####
## CODE FOR SECTION 5. Continuous exposure example
#####

set.seed(42)
N <- 300
P <- 100
cov <- list()
for (i in 1:P) {
  cov[[i]] <- rnorm(N, 0, 1)
}
X <- do.call(cbind, cov)
h1 <- ifelse(X[, 1] < 0, 1, -1)
h2 <- ifelse(X[, 2] < 0, -1, 1)
mu_trt <- 0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) + 0.5 * X[, 4] * X[, 5]
Trt <- rnorm(N, mu_trt, 0.3)
mu_y <- 1 * h1 + 1 * h2 - Trt + 1 * abs(X[, 3] + 1) +
  1 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * Trt * abs(X[, 6]) - 0.5 * Trt * abs(X[, 7] + 1)
Y <- rnorm(N, mu_y, 0.3)

treatment <- quantile(Trt, 0.75)
control <- quantile(Trt, 0.25)

## fit the single bart model
single_fit <- single_bart(
  Y = Y, trt = Trt, X = X,
  trt_treated = treatment, trt_control = control,
  num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
single_fit

## PIP plots in Figure 9
plot(single_fit, method = "pip", top_n = 10)
ggplot2::ggsave("fig/conti_PIP_top_n.pdf", width=6, height=4)

plot(single_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/conti_PIP_threshold.pdf", width=6, height=4)


#####
## CODE FOR SECTION 6. Heterogeneous effects
#####

## fix seed and create data
set.seed(42)
N <- 300
P <- 100
cov <- list()
for (i in seq_len(P)) {
  cov[[i]] <- rnorm(N, 0, 1)
}
X <- do.call(cbind, cov)
h1 <- ifelse(X[, 1] < 0, 1, -1)
h2 <- ifelse(X[, 2] < 0, -1, 1)
prob <- pnorm(0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) +
                1.5 * X[, 4] * X[, 5])
Trt <- rbinom(N, 1, prob)
mu1 <- 1 * h1 + 1.5 * h2 - 1 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 1 * abs(X[, 6]) - 1 * 1 * abs(X[, 7] + 1)
mu0 <- 1 * h1 + 1.5 * h2 - 0 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 0 * abs(X[, 6]) - 1 * 0 * abs(X[, 7] + 1)
Y1 <- rnorm(N, mu1, 0.3)
Y0 <- rnorm(N, mu0, 0.3)
Y <- Trt * Y1 + (1 - Trt) * Y0

separate_fit <- separate_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)

# Y(1) samples from the 1st MCMC chain
separate_fit$chains[[1]]$Y1_sample
# Y(0) samples from the 1st MCMC chain
separate_fit$chains[[1]]$Y0_sample

Y1_sample <- do.call(cbind, lapply(separate_fit$chains, function(x) x$Y1_sample))
rowMeans(Y1_sample)
Y0_sample <- do.call(cbind, lapply(separate_fit$chains, function(x) x$Y0_sample))
rowMeans(Y0_sample)


## versions
pkgs <- installed.packages()
vers <- pkgs[, "Version"]

vers["bartcs"]
vers["bacr"]



#####
## CODE FOR APPENDIX: additional predictors
#####

## fix seed and create data
set.seed(42)
N <- 300
P <- 100
cov <- list()
for (i in seq_len(P)) {
  cov[[i]] <- rnorm(N, 0, 1)
}
X <- do.call(cbind, cov)
h1 <- ifelse(X[, 1] < 0, 1, -1)
h2 <- ifelse(X[, 2] < 0, -1, 1)
prob <- pnorm(0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) +
                1.5 * X[, 4] * X[, 5])
Trt <- rbinom(N, 1, prob)
mu1 <- 1 * h1 + 1.5 * h2 - 1 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 1 * abs(X[, 6]) - 1 * 1 * abs(X[, 7] + 1)
mu0 <- 1 * h1 + 1.5 * h2 - 0 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 0 * abs(X[, 6]) - 1 * 0 * abs(X[, 7] + 1)
Y1 <- rnorm(N, mu1, 0.3)
Y0 <- rnorm(N, mu0, 0.3)
Y <- Trt * Y1 + (1 - Trt) * Y0

## load library and fit the single bart model
library("bartcs")
single_fit <- single_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
single_fit
summary(single_fit)

## PIP plots
## create sub-directory "fig"
if (!file.exists("fig")) {
  dir.create("fig")
}
library(ggplot2)
plot(separate_fit, method = "pip", top_n = 10)
ggplot2::ggsave("fig/PIP_top_n_s1.pdf", height = 4, width = 6)

plot(separate_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/PIP_threshold_s1.pdf", height = 4, width = 6)


## Trace plots
plot(separate_fit, method = "trace")
ggplot2::ggsave("fig/traceplot_ATE_s1.pdf", height = 4, width = 6)
plot(separate_fit, method = "trace", parameter = "dir_alpha")
ggplot2::ggsave("fig/traceplot_alpha_s1.pdf", height = 4, width = 6)



#####
## CODE FOR APPENDIX: more true confounders (20)
#####

## fix seed and create data
set.seed(42)
N <- 600
P <- 100
cov <- list()
for (i in seq_len(P)) {
  cov[[i]] <- rnorm(N, 0, 1)
}
X <- do.call(cbind, cov)
h1 <- ifelse(X[, 1] < 0, 1, -1)
h2 <- ifelse(X[, 2] < 0, -1, 1)
prob <- pnorm(0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) +
                1.5 * X[, 4] * X[, 5] + X[, 6:20] %*% rep(0.5, 15))
Trt <- rbinom(N, 1, prob)
mu1 <- 1 * h1 + 1.5 * h2 - 1 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 1 * abs(X[, 6]) - 1 * 1 * abs(X[, 7] + 1) + X[, 6:20] %*% rep(0.5, 15)
mu0 <- 1 * h1 + 1.5 * h2 - 0 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5]) -
  0.5 * 0 * abs(X[, 6]) - 1 * 0 * abs(X[, 7] + 1) + X[, 6:20] %*% rep(0.5, 15)
Y1 <- rnorm(N, mu1, 0.3)
Y0 <- rnorm(N, mu0, 0.3)
Y <- Trt * Y1 + (1 - Trt) * Y0


## load library and fit the separate/single bart model
library("bartcs")
separate_fit <- separate_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
separate_fit
summary(separate_fit)
single_fit <- single_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
single_fit
summary(single_fit)

## PIP plots
## create sub-directory "fig"
if (!file.exists("fig")) {
  dir.create("fig")
}
library(ggplot2)
plot(separate_fit, method = "pip", top_n = 30)
ggplot2::ggsave("fig/PIP_top_n_s2.pdf", height = 4, width = 6)

plot(separate_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/PIP_threshold_s2.pdf", height = 4, width = 6)

plot(single_fit, method = "pip", top_n = 10)
ggplot2::ggsave("fig/PIP_top_n_s3.pdf", height = 4, width = 6)

plot(single_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/PIP_threshold_s3.pdf", height = 4, width = 6)


## Trace plots
plot(separate_fit, method = "trace")
ggplot2::ggsave("fig/traceplot_ATE_s2.pdf", height = 4, width = 6)
plot(separate_fit, method = "trace", parameter = "dir_alpha")
ggplot2::ggsave("fig/traceplot_alpha_s2.pdf", height = 4, width = 6)

plot(single_fit, method = "trace")
ggplot2::ggsave("fig/traceplot_ATE_s3.pdf", height = 4, width = 6)
plot(single_fit, method = "trace", parameter = "dir_alpha")
ggplot2::ggsave("fig/traceplot_alpha_s3.pdf", height = 4, width = 6)



#####
## CODE FOR APPENDIX: additional instruments
#####

## fix seed and create data
set.seed(42)
N <- 300
P <- 100
cov <- list()
for (i in seq_len(P)) {
  cov[[i]] <- rnorm(N, 0, 1)
}
X <- do.call(cbind, cov)
h1 <- ifelse(X[, 1] < 0, 1, -1)
h2 <- ifelse(X[, 2] < 0, -1, 1)
prob <- pnorm(0.5 + h1 + h2 - 0.5 * abs(X[, 3] - 1) +
                1.5 * X[, 4] * X[, 5] + 1.5 * X[, 6] - 1 * X[, 7])
Trt <- rbinom(N, 1, prob)
mu1 <- 1 * h1 + 1.5 * h2 - 1 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5])
mu0 <- 1 * h1 + 1.5 * h2 - 0 + 2 * abs(X[, 3] + 1) +
  2 * X[, 4] + exp(0.5 * X[, 5])
Y1 <- rnorm(N, mu1, 0.3)
Y0 <- rnorm(N, mu0, 0.3)
Y <- Trt * Y1 + (1 - Trt) * Y0


## load library and fit the separate/single bart model
library("bartcs")
separate_fit <- separate_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
separate_fit
summary(separate_fit)
single_fit <- single_bart(
  Y = Y, trt = Trt, X = X, num_tree = 200, num_chain = 4,
  num_burn_in = 10000, num_thin = 5, num_post_sample = 2000
)
single_fit
summary(single_fit)

## PIP plots
## create sub-directory "fig"
if (!file.exists("fig")) {
  dir.create("fig")
}
library(ggplot2)
plot(separate_fit, method = "pip", top_n = 10)
ggplot2::ggsave("fig/PIP_top_n_s4.pdf", height = 4, width = 6)

plot(separate_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/PIP_threshold_s4.pdf", height = 4, width = 6)

plot(single_fit, method = "pip", top_n = 10)
ggplot2::ggsave("fig/PIP_top_n_s5.pdf", height = 4, width = 6)

plot(single_fit, method = "pip", threshold = 0.5)
ggplot2::ggsave("fig/PIP_threshold_s5.pdf", height = 4, width = 6)


## Trace plots
plot(separate_fit, method = "trace")
ggplot2::ggsave("fig/traceplot_ATE_s4.pdf", height = 4, width = 6)
plot(separate_fit, method = "trace", parameter = "dir_alpha")
ggplot2::ggsave("fig/traceplot_alpha_s4.pdf", height = 4, width = 6)

plot(single_fit, method = "trace")
ggplot2::ggsave("fig/traceplot_ATE_s5.pdf", height = 4, width = 6)
plot(single_fit, method = "trace", parameter = "dir_alpha")
ggplot2::ggsave("fig/traceplot_alpha_s5.pdf", height = 4, width = 6)





