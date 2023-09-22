## ----libraries, echo=FALSE, message=FALSE-------------------------------------
# The GGally and sessioninfo packages are also used in this vignette.
if (!requireNamespace("GGally", quietly = TRUE)) {
  message("You need to install the package GGally to show correlation plot.")
} else {
  library("GGally")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("You need to install the package ggplot2 to show correlation plot.")
} else {
  library("ggplot2")
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


## ----setup,  echo=FALSE, cache=FALSE------------------------------------------
# options(prompt = "> ", continue = "+ ", width = 70, useFancyQuotes = FALSE)

knitr::opts_chunk$set(
  cache = TRUE, 
  collapse = TRUE, # If TRUE, all output would be in the code chunk.
  results = "markup",
  comment = NA,
  prompt = TRUE,
  strip.white = TRUE,
  tidy = "styler",
  tidy.opts = list(width.cutoff = 60), # options for tidy to remove blank lines [blank = FALSE] and set the approximate line width to be 80.
  fig.pos = "h",
  fig.show = "asis",
  fig.align = "center"
  # fig.height = 4,  #inches  #Default figure size for vingette is 3 x 3.
  #  fig.width = 4   #inches
)
options(tinytex.verbose = TRUE)


## ----function.f, echo=FALSE---------------------------------------------------
#' Summary Statistics calculated in the imputed bootstrapped and Bayesian arrays using apply()
f <- function(x) {
  a <- c(min(x), quantile(x, 0.05), mean(x), max(x))
  names(a) <- c("min", "P.5", "mean", "max")
  # noquote(sprintf(a, fmt = '%#.4f'))
  a
}



## ----fig.mi, echo=FALSE, fig.pos="h", fig.scap=NULL, fig.ext="pdf", fig.cap="\\label{fig::mi} Multiple Imputation in connection with the Weighted Quantile Sum regression (MI-WQS). Given partially observed correlated chemical exposures that share a common outcome and covariates, (stage 1) researchers impute the below detection limit values (dark circles) K times to form complete datasets. In stage 2, each imputed dataset is analyzed using WQS regression. In stage 3, the coefficient estimates from the K WQS regressions (diamonds) are combined into a final estimate (square)."----

knitr::include_graphics("figure-word/fig_mi_intro.pdf")


## ----data1--------------------------------------------------------------------
library("miWQS")
data("simdata87")


## ----dataDL-------------------------------------------------------------------
simdata87$DL


## ----dataCorr, results="hold",  fig.pos="h", fig.width=5, fig.height=5, fig.cap="\\label{fig::dataCorr} Heat map of the correlations using the fourteen observed chemical logarithmic concentrations in dataset simdata87. The heat map was generated using the GGally package."----

GGally::ggcorr(
  log(simdata87$X.bdl),
  method = c("pairwise", "spearman"),
  geom = "tile", layout.exp = 2, hjust = 0.75,
  size = 3, 
  legend.position = "bottom"
)


## -----------------------------------------------------------------------------
summary(simdata87$Z.sim[, "Age"])
apply(simdata87$Z.sim[, -1], 2, table)


## ----dataY, results="hold"----------------------------------------------------
cat("Counts")
table(simdata87$y.scenario)

## ----dataYii, echo=FALSE------------------------------------------------------
# Percentages
# table(simdata87$y.scenario)/1000


## ----eg1_wqs, cache=TRUE------------------------------------------------------
set.seed(50679)
wqs.eg1 <- estimate.wqs(
  y = simdata87$y.scenario, X = simdata87$X.true, Z = simdata87$Z.sim,
  proportion.train = 0.5,
  n.quantiles = 4,
  place.bdls.in.Q1 = FALSE,
  B = 100, b1.pos = TRUE, 
  signal.fn = "signal.converge.only",
  family = "binomial",
  verbose = FALSE
)
wqs.eg1


## ----eg1-plots----------------------------------------------------------------
eg1.plots <- plot(wqs.eg1)
names(eg1.plots)


## ----histWeight, warning=FALSE,  fig.height=5, fig.width=5, fig.pos="h", echo=FALSE, fig.cap="\\label{fig::histWeight} Histograms of chemical weight estimates across 100 bootstraps for Example 1. Weight estimates are constrained to be between zero and one."----
eg1.plots$hist.weights + 
  theme(axis.text.x = element_text(size = 7.5))


## ----histB1, warning=FALSE, fig.height=3, fig.width=6, fig.pos="h",  echo=FALSE, fig.cap="\\label{fig::histB1} Histogram of overall chemical effect in the training dataset across 100 bootstraps for Example 1. Its constraint is governed by the b1.pos argument in the estimate.wqs() function. In the simdata87 dataset, the overall mixture is constrained to have a positive association with cancer."----
eg1.plots$hist.beta1


## ----histWQS, warning=FALSE, fig.height=3, fig.width=6, fig.pos="h",  echo=FALSE, fig.cap="\\label{fig::histWQS} Histogram of the weighted quantile sum (WQS) using validation quantiles for Example 1."----
eg1.plots$hist.WQS


## ----eg2-BDLQ1-1--------------------------------------------------------------
q <- make.quantile.matrix(
  X = simdata87$X.true,
  n.quantiles = 4
  )
apply(q, 2, table)


## ----BDLQ1-2------------------------------------------------------------------
q <- make.quantile.matrix(
  simdata87$X.bdl, 
  n.quantiles = 4, 
  verbose = TRUE
  )


## ----BDLQ1-3a, echo=FALSE-----------------------------------------------------
# First create chemical data X.80 with each chemical having 800 values missing.
X.80 <- simdata87$X.true # Copy the true concentrations
DL <- apply(X.80, 2, function(X) { quantile(X, 0.80) }) # Find the upper threshold
for (j in 1:14) { # For each chemical, substitute...
  X.80[ X.80[, j] < DL[j], j] <- NA
}


## ----BDLQ1-3b-----------------------------------------------------------------
q <- make.quantile.matrix(X.80, n.quantiles = 4, verbose = TRUE)


## ----BDLQ1-4------------------------------------------------------------------
q <- make.quantile.matrix(simdata87$X.bdl, n.quantiles = 10, verbose = TRUE)


## ----wqs_BDLQ1, cache=TRUE----------------------------------------------------
set.seed(50679)
wqs.BDL <- estimate.wqs(
  y = simdata87$y.scenario, X = simdata87$X.bdl, Z = simdata87$Z.sim,
  proportion.train = 0.5, 
  n.quantiles = 4,
  place.bdls.in.Q1 = TRUE,
  B = 100,
  b1.pos = TRUE, 
  signal.fn = "signal.converge.only",
  family = "binomial",
  verbose = FALSE
)
wqs.BDL


## ----eg.2.AIC, echo=FALSE-----------------------------------------------------
AIC.BDL <- round(AIC(wqs.BDL$fit), 0)
AIC.Compl <- round(AIC(wqs.eg1$fit), 0)


## ----eg3-lubin.impute1--------------------------------------------------------
set.seed(472195)
answer <- impute.Lubin(
  chemcol = simdata87$X.bdl[, 1],
  dlcol = simdata87$DL[1],
  Z = cbind(simdata87$y.scenario, simdata87$Z.sim), 
  K = 2
)
summary(answer$imputed_values)


## ----lubin.impute2, results="hold"--------------------------------------------
cat("Summary of BDL Values \n")
imp <- answer$imputed_values[, 1] < simdata87$DL[1]
summary(answer$imputed_values[imp, ])


## ----lubin.impute3------------------------------------------------------------
set.seed(472195)
l.boot <- impute.boot(
  X = simdata87$X.bdl, 
  DL = simdata87$DL, 
  Z = cbind(simdata87$y.scenario, simdata87$Z.sim), 
  K = 2
  )
results.Lubin <- l.boot$X.imputed


## ----lubin.impute4------------------------------------------------------------
apply(results.Lubin, 2:3, f)


## ----lubin.wqs, results="hold", message=FALSE, cache=TRUE---------------------
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


## ----lubin.wqs2---------------------------------------------------------------
formatC(boot.wqs$wqs.imputed.estimates, format = "fg", flag = "#", digits = 3)


## ----lubin.pool1--------------------------------------------------------------
boot.est <- pool.mi(
  to.pool = boot.wqs$wqs.imputed.estimates, 
  n = nrow(simdata87$X.bdl),
  prt = FALSE
)


## ----lubin.pool1b, echo=FALSE-------------------------------------------------
print(round(boot.est[ , -(5)], 3))


## ----lubin.pool1c, echo=FALSE, include=FALSE----------------------------------
knitr::kable( 
  boot.est[ , -(5)],
  format = "latex", 
  booktabs = TRUE, 
  digits = 3,
  label = "bootWQS",
  caption = "WQS coefficient estimates from bootstrap multiple imputation across two datasets."
)


## ----lubin.OR, echo = FALSE---------------------------------------------------
 OR <- round(exp(boot.est["WQS", 1]), 2)
 vec.ci <- round(exp(boot.est["WQS", c(7,8)]), 2)
 OR.CI <- paste(vec.ci, collapse = ", ")


## ----lubin.pool3--------------------------------------------------------------
chemicals <- boot.est[1:14, ]
row.names(chemicals)[chemicals$pooled.mean >= 1 / 14]


## ----lubin.pool4--------------------------------------------------------------
boot.wqs$AIC
boot.AIC <- combine.AIC(boot.wqs$AIC)


## ----AICs, echo=FALSE---------------------------------------------------------
BDLQ1.AIC <- sprintf(round(AIC(wqs.BDL$fit), 1), fmt = '%#.1f')
complete.AIC <- round(AIC(wqs.eg1$fit), 1)


## ----eg4-bayes.imp1, cache=TRUE-----------------------------------------------
set.seed(472195)
result.imputed <- impute.univariate.bayesian.mi(
  X = simdata87$X.bdl, 
  DL = simdata87$DL,
  T = 6000,
  n.burn = 400,
  K = 2
)


## ----bayes.imp1b--------------------------------------------------------------
apply(result.imputed$X.imputed, 2:3, f)


## ----bayes.wqs, results="hold", message=FALSE, cache=TRUE---------------------
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


## ----bayes.pool1--------------------------------------------------------------
bayesian.est <- pool.mi(
  to.pool = bayes.wqs$wqs.imputed.estimates, 
  n = nrow(simdata87$X.bdl), 
  prt = TRUE
)


## ----bayes.pool1b, echo=FALSE, include=FALSE----------------------------------
knitr::kable( 
  bayesian.est[ , -(5)], label = "bayesian_est", format = "latex", booktabs = TRUE, 
  digits = 3, caption = "Bayesian Multiple Imptuation Weighted Quantile Regression Parameter Estimates. Summary of statistics after performing WQS regression across two datasets." ) 


## ----bayes.pool2--------------------------------------------------------------
chemicals <- bayesian.est[1:14, ]
row.names(chemicals)[chemicals$pooled.mean >= 1 / 14]


## ----bayes_combine_AIC, echo=FALSE--------------------------------------------
AIC.Bayes <- miWQS::combine.AIC(bayes.wqs$AIC)


## ----bayes.pool3--------------------------------------------------------------
bayes.wqs$AIC
miWQS::combine.AIC(bayes.wqs$AIC)


## ----echo=FALSE, include=FALSE------------------------------------------------
bayesian.est - boot.est


## ----fig.decide, fig.pos="h", fig.height=7, echo=FALSE, fig.cap="\\label{fig::decide} A decision tree to help researchers in using the miWQS package. The package is flexible and can meet a wide range of needs."----
knitr::include_graphics("figure-word/Decision_Tree_In_Using_miWQS_Package.pdf")
# previously it was .png but picture quality was poor.


## ----comp-detl-1, echo=FALSE--------------------------------------------------
# sessioninfo::session_info() # makes a mess!
# Instead

cat(" -- Session info ---------------------------------------------------")
sessioninfo::platform_info()
cat("--  Packages -------------------------------------------------------")
tmp.df <-  sessioninfo::package_info(
  pkgs = c("wqs", "gWQS", "miWQS", "mice", "mi", "norm", "GGally",  "coda", "ggplot2", "glm2", "Hmisc", "invgamma", "purrr", "rlist", "Rsolnp", "survival", "tidyr", "truncnorm"), dependencies = FALSE
)
tmp.df$source[18]  <-  substr(tmp.df$source[18], 1, 6)
print(tmp.df)


## ----apdx1_indvchemanalysis---------------------------------------------------
analyze.individually(
  y = simdata87$y.scenario, X = simdata87$X.true,
  Z = simdata87$Z.sim, family = "binomial"
)

