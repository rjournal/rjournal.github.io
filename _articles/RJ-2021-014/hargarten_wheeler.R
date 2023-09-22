# Created from knitr::purl(Rmd, documentation = 1)
## ----libraries, echo = FALSE-----------------------------------------------------------------------------------------------------
# The GGally and sessioninfo packages are also used in this vignette.
if (!requireNamespace("GGally", quietly = TRUE)) {
  message("You need to install the package GGally to show correlation plot.")
}
if (!requireNamespace("knitr", quietly = TRUE)) {
  message("You need to install the package knitr.")
} else {
  library("knitr")
}
if (!requireNamespace("sessioninfo", quietly = TRUE)) {
  message("You need to install the package sessioninfo to show computational details.")
} else {
  library("sessioninfo")
}


## ----setup,  echo=FALSE, cache=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,  #TRUE for debugging
  collapse = TRUE, # If TRUE, all output would be in the code chunk.
  results = "markup",
  comment = NA,
  prompt = TRUE,
  strip.white = TRUE,
  tidy = "styler",
  fig.pos = "h",
  fig.show = "asis",
  fig.align = "center"
  # fig.height = 4,  #inches  #Default figure size for vingette is 3 x 3.
  #  fig.width = 4   #inches
  #   tidy.opts = list(width.cutoff = 60), # options for tidy to remove blank lines [blank = FALSE] and set the approximate line width to be 60 WHEN tidy = 'formatR'.
)
options(tinytex.verbose = TRUE)


## ----function.f, echo = FALSE----------------------------------------------------------------------------------------------------
#' Summary Statistics calculated in the imputed bootstrapped and Bayesian arrays using apply()
f <- function(x) {
  a <- c(min(x), quantile(x, 0.05), mean(x), max(x))
  names(a) <- c("min", "P.5", "mean", "max")
  # noquote(sprintf(a, fmt = '%#.4f'))
  a
}



## ----fig.mi, fig.scap = NULL, echo=FALSE,  fig.ext = "png", fig.height = 4, fig.width = 4,  fig.cap= "\\label{fig::mi} The three stages of multiple imputation with WQS. Consider a dataset consisting of covariates, and a mixture of correlated, interval-censored, and continuous components that may be associated with a common outcome, y. The dark circle in the figure represents the values in the mixture that are below the detection limit (BDL). Datasets are given by circles; parameter estimates are given by diamonds. K is the number of complete datasets created."----

knitr::include_graphics("figure-word/fig_mi_intro.pdf")


## ----data1-----------------------------------------------------------------------------------------------------------------------
library("miWQS")
data("simdata87")


## ----dataDL----------------------------------------------------------------------------------------------------------------------
simdata87$DL


## ----dataCorrA, results="hold", echo = FALSE, fig.pos = "ht", fig.width=5, fig.height=5, fig.cap="\\label{fig::dataCorr} Heat map of the correlations using the fourteen observed chemical logarithmic concentrations in dataset simdata87. The heat map was generated using the GGally package."----

GGally::ggcorr(
  log(simdata87$X.bdl),
  method = c("pairwise", "spearman"),
  geom = "tile", layout.exp = 2, hjust = 0.75, size = 3,
  legend.position = "bottom"
)


## --------------------------------------------------------------------------------------------------------------------------------
summary(simdata87$Z.sim[, "Age"])
apply(simdata87$Z.sim[, -1], 2, table)


## ----dataY, results = "hold"-----------------------------------------------------------------------------------------------------
cat("Counts")
table(simdata87$y.scenario)


## ----eg1_wqs, cache = TRUE-------------------------------------------------------------------------------------------------------
set.seed(50679)
wqs.eg1 <- estimate.wqs(
  y = simdata87$y.scenario, X = simdata87$X.true, Z = simdata87$Z.sim,
  proportion.train = 0.5, n.quantiles = 4,
  place.bdls.in.Q1 = FALSE,
  B = 100, b1.pos = TRUE, signal.fn = "signal.converge.only",
  family = "binomial",
  verbose = FALSE
)
wqs.eg1


## ----eg1-plots-------------------------------------------------------------------------------------------------------------------
eg1.plots <- plot(wqs.eg1)
names(eg1.plots)


## ----histWeightA, warning=FALSE, fig.width=5, fig.height=5, echo=FALSE, fig.cap="\\label{fig::histWeight} Histograms of chemical weight estimates across 100 bootstraps for Example 1. Weight estimates are constrained to be between zero and one."----
eg1.plots$hist.weights


## ----histB1A, warning=FALSE, echo=FALSE,  fig.pos="ht", fig.width=3.5, fig.height=3.5, fig.cap="\\label{fig::histB1} Histogram of overall chemical effect in the training dataset across 100 bootstraps for Example 1. Its constraint is governed by the b1.pos argument in the estimate.wqs() function. In the simdata87 dataset, the overall mixture is constrained to have a positive association with cancer."----
eg1.plots$hist.beta1


## ----histWQSA, warning=FALSE, echo=FALSE, fig.pos ="ht", fig.width=3.5, fig.height=3.5, fig.cap="\\label{fig::histWQS} Histogram of the weighted quantile sum (WQS) using validation quantiles for Example 1."----
eg1.plots$hist.WQS


## ----eg2-BDLQ1-1-----------------------------------------------------------------------------------------------------------------
q <- make.quantile.matrix(X = simdata87$X.true, n.quantiles = 4)
apply(q, 2, table)


## ----BDLQ1-2---------------------------------------------------------------------------------------------------------------------
q <- make.quantile.matrix(simdata87$X.bdl, n.quantiles = 4, verbose = TRUE)


## ----BDLQ1-3a, echo=FALSE--------------------------------------------------------------------------------------------------------
# First create chemical data X.80 with each chemical having 800 values missing.
X.80 <- simdata87$X.true # Copy the true concentrations
DL <- apply(X.80, 2, function(X) { quantile(X, 0.80) }) # Find the upper threshold
for (j in 1:14) { # For each chemical, substitute...
  X.80[ X.80[, j] < DL[j], j] <- NA
}


## ----BDLQ1-3b--------------------------------------------------------------------------------------------------------------------
q <- make.quantile.matrix(X.80, n.quantiles = 4, verbose = TRUE)


## ----BDLQ1-4---------------------------------------------------------------------------------------------------------------------
q <- make.quantile.matrix(simdata87$X.bdl, n.quantiles = 10, verbose = TRUE)


## ----wqs_BDLQ1, message= FALSE, cache=TRUE---------------------------------------------------------------------------------------
set.seed(50679)
wqs.BDL <- estimate.wqs(
  y = simdata87$y.scenario, X = simdata87$X.bdl, Z = simdata87$Z.sim,
  proportion.train = 0.5,
  n.quantiles = 4,
  place.bdls.in.Q1 = TRUE,
  B = 100, b1.pos = TRUE,
  signal.fn = "signal.converge.only",
  family = "binomial",
  verbose = FALSE
)
wqs.BDL


## ----eg.2.AIC, echo = FALSE------------------------------------------------------------------------------------------------------
AIC.BDL <- round(AIC(wqs.BDL$fit), 0)
AIC.Compl <- round(AIC(wqs.eg1$fit), 0)


## ----eg3-lubin.impute1-----------------------------------------------------------------------------------------------------------
set.seed(472195)
answer <- impute.Lubin(
  chemcol = simdata87$X.bdl[, 1],
  dlcol = simdata87$DL[1],
  Z = simdata87$Z.sim,
  K = 2
)
summary(answer$imputed_values)


## ----action, echo = FALSE--------------------------------------------------------------------------------------------------------
# DL1 <-


## ----lubin.impute2, results = "hold"---------------------------------------------------------------------------------------------
cat("Summary of BDL Values \n")
imp <- answer$imputed_values[, 1] < simdata87$DL[1]
summary(answer$imputed_values[imp, ])


## ----lubin.impute3, message = FALSE----------------------------------------------------------------------------------------------
set.seed(472195)
l.boot <- impute.boot(
  X = simdata87$X.bdl,
  DL = simdata87$DL,
  Z = simdata87$Z.sim,
  K = 2
  )
results.Lubin <- l.boot$X.imputed


## ----lubin.impute4---------------------------------------------------------------------------------------------------------------
apply(results.Lubin, 2:3, f)


## ----lubin.wqs, results = "hold", message = FALSE, cache = TRUE------------------------------------------------------------------
set.seed(50679)
boot.wqs <- do.many.wqs(
  y = simdata87$y.scenario, X.imputed = results.Lubin, Z = simdata87$Z.sim,
  proportion.train = 0.5,
  n.quantiles = 4,
  B = 100,
  b1.pos = TRUE,
  signal.fn = "signal.converge.only",
  family = "binomial"
)


## ----lubin.wqs2------------------------------------------------------------------------------------------------------------------
formatC(boot.wqs$wqs.imputed.estimates, format = "fg", flag = "#", digits = 3)


## ----lubin.pool1-----------------------------------------------------------------------------------------------------------------
boot.est <- pool.mi(
  to.pool = boot.wqs$wqs.imputed.estimates,
  n = nrow(simdata87$X.bdl)
)


## ----lubin.pool1b, echo = FALSE, include = FALSE---------------------------------------------------------------------------------
knitr::kable(
  boot.est[ , -(5)], label = "bootWQS", format = "latex", booktabs = TRUE, digits = 3,
  caption = "WQS estimates using bootstrap multiple imputation. Summary of statistics after performing WQS regression across two datasets."
)


## ----lubin.pool3-----------------------------------------------------------------------------------------------------------------
chemicals <- boot.est[1:14, ]
row.names(chemicals)[chemicals$pooled.mean >= 1 / 14]


## ----lubin.pool4-----------------------------------------------------------------------------------------------------------------
boot.wqs$AIC
boot.AIC <- combine.AIC(boot.wqs$AIC)


## ----AICs, echo = FALSE----------------------------------------------------------------------------------------------------------
BDLQ1.AIC <- sprintf(round(AIC(wqs.BDL$fit), 1), fmt = '%#.1f')
complete.AIC <- round(AIC(wqs.eg1$fit), 1)


## ----eg4-bayes.imp1, cache=TRUE--------------------------------------------------------------------------------------------------
set.seed(472195)
result.imputed <- impute.univariate.bayesian.mi(
  X = simdata87$X.bdl, DL = simdata87$DL,
  T = 6000, n.burn = 400, K = 2
)


## ----bayes.imp1b-----------------------------------------------------------------------------------------------------------------
apply(result.imputed$X.imputed, 2:3, f)


## ----bayes.wqs, results="hold", message=FALSE, cache = TRUE----------------------------------------------------------------------
set.seed(50679)
bayes.wqs <- do.many.wqs(
  y = simdata87$y.scenario, X.imputed = result.imputed$X.imputed,
  Z = simdata87$Z.sim,
  proportion.train = 0.5,
  n.quantiles = 4,
  B = 100,
  b1.pos = TRUE,
  signal.fn = "signal.converge.only",
  family = "binomial"
)
wqs.imputed.estimates <- bayes.wqs$wqs.imputed.estimates


## ----bayes.pool1-----------------------------------------------------------------------------------------------------------------
bayesian.est <- pool.mi(
  to.pool = bayes.wqs$wqs.imputed.estimates,
  n = nrow(simdata87$X.bdl)
)


## ----bayes.pool1b, echo = FALSE, include = FALSE---------------------------------------------------------------------------------
knitr::kable(
  bayesian.est[ , -(5)], label = "bayesian_est",  format = "latex", booktabs = TRUE,
  digits = 3, caption = "Bayesian Multiple Imptuation Weighted Quantile Regression Parameter Estimates. Summary of statistics after performing WQS regression across two datasets." )


## ----bayes.pool2-----------------------------------------------------------------------------------------------------------------
chemicals <- bayesian.est[1:14, ]
row.names(chemicals)[chemicals$pooled.mean >= 1 / 14]


## ----bayes_combine_AIC, echo = FALSE---------------------------------------------------------------------------------------------
AIC.Bayes <- miWQS::combine.AIC(bayes.wqs$AIC)


## ----bayes.pool3-----------------------------------------------------------------------------------------------------------------
bayes.wqs$AIC
miWQS::combine.AIC(bayes.wqs$AIC)


## ----echo=FALSE, include = FALSE-------------------------------------------------------------------------------------------------
bayesian.est - boot.est


## ----fig.decideA, fig.cap="\\label{fig::decide} A decision tree to help researchers in using the miWQS package. The package is flexible and can meet a wide range of needs.", echo=FALSE----

 knitr::include_graphics("figure-word/decision_tree.pdf", auto_pdf = FALSE, dpi = NULL)


## ----comp-detl-1, echo = FALSE---------------------------------------------------------------------------------------------------
# sessioninfo::session_info() # makes a mess!
# Instead
cat(" -- Session info ---------------------------------------------------")
sessioninfo::platform_info()
cat("--  Packages -------------------------------------------------------")
tmp.df <-  sessioninfo::package_info(
  pkgs = c("wqs", "gWQS", "miWQS", "mice", "mi", "norm", "GGally", "survival", "coda", "glm2", "ggplot2", "Hmisc",
           "invgamma", "MCMCpack", "tmvtnorm", "rlist", "Rsolnp", "survival", "truncnorm", "tidyr"), dependencies = FALSE
)
tmp.df$source[18]  <-  substr(tmp.df$source[18], 1, 6)
print(tmp.df)


## ----appendx1_indvchemanalysis---------------------------------------------------------------------------------------------------
analyze.individually(
  y = simdata87$y.scenario, X = simdata87$X.true,
  Z = simdata87$Z.sim, family = "binomial"
)

