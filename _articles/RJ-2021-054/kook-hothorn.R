## ----setup, echo = FALSE, results = "hide", message = FALSE--------------
set.seed(241068)

knitr::opts_chunk$set(echo = TRUE, results = 'markup', error = FALSE,
                      warning = FALSE, message = FALSE,
                      tidy = FALSE, cache = TRUE, size = "small",
                      fig.width = 5, fig.height = 4, fig.align = "center",
                      out.width = NULL, ###'.6\\linewidth',
                      out.height = NULL,
                      fig.scap = NA)
knitr::render_sweave()  # use Sweave environments
knitr::set_header(highlight = '')  # do not \usepackage{Sweave}
## R settings
options(prompt = "R> ", continue = "+  ", useFancyQuotes = FALSE)  # JSS style
options(width = 75, digits = 3)

# lattice theme for vignette and publication

library("lattice")
library("colorspace")

# Colors

col <- diverge_hcl(2, h = c(246, 40), c = 96, l = c(65, 90))
fill <- diverge_hcl(2, h = c(246, 40), c = 96, l = c(65, 90), alpha = .3)

.cmp <- function(x, y) {
  ret <- cbind(x, y, x - y, (x - y) / x)
  colnames(ret) <- c("tram", "tramnet", "diff", "rel_diff")
  return(ret)
}

from <- function(todistr, n, p, n0p, dd, cfx, ord, support, add, seed) {
  yvar <- numeric_var("y", support = support, add = add)
  yB <- Bernstein_basis(yvar, order = ord, ui = "increasing", log_first = FALSE)
  st <- as.basis(as.formula(
    paste("~", paste0("X", seq_len(p), collapse = " + "), "- 1") ), data = dd)
  m <- ctm(response = yB, shifting = st, todistr = todistr)
  coef(m) <- cfx
  return(m)
}

print_mbo <- function(x, ...) {
  op = x$opt.path
  cat("Recommended parameters:\n")
  cat(paramValueToString(op$par.set, x$x), "\n")
  cat("Objective:", op$y.names[1], "=", round(x$y, 3), "\n")
}

# Dependencies

library("tramnet")
library("penalized")
library("glmnet")
library("mvtnorm")
library("Matrix")
library("coin")

## ----eval=FALSE----------------------------------------------------------
#  m1 <- tram(y | s ~ 1, ...)

## ----eval=FALSE----------------------------------------------------------
#  x <- model.matrix(~ 0 + x, ...)
#  x_scaled <- scale(x)
#  mt <- tramnet(model = m1, x = x_scaled, lambda, alpha, ...)

## ----BostonHousing2_data-------------------------------------------------
data("Prostate", package = "lasso2")
Prostate$psa <- exp(Prostate$lpsa)
Prostate[, 1:8] <- scale(Prostate[, 1:8])

## ----BH_Linear1----------------------------------------------------------
fm_Pr <- psa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45
fm_Pr1 <- update(fm_Pr, ~ 0 + .)
x <- model.matrix(fm_Pr1, data = Prostate)

## ----BH_Linear2, results='hide'------------------------------------------
m0 <- Lm(lpsa ~ 1, data = Prostate)
mt <- tramnet(m0, x = x, alpha = 0, lambda = 0)
mp <- penalized(response = Prostate$lpsa, penalized = x,
                lambda1 = 0, lambda2 = 0)

## ----BH_Linear3----------------------------------------------------------
cfx_tramnet <- coef(mt, as.lm = TRUE)

## ----BH_BoxCox1----------------------------------------------------------
ord <- 7 # flexible baseline transformation
m01 <- BoxCox(psa ~ 1, data = Prostate, order = ord,
              extrapolate = TRUE, log_first = TRUE)
mt1 <- tramnet(m01, x = x, alpha = 0, lambda = 0)

## ----Prostate_baseline_trafo---------------------------------------------
m02 <- BoxCox(psa ~ 1, order = 11, data = Prostate, extrapolate = TRUE)
mt2 <- tramnet(m02, x = x, lambda = 0, alpha = 0)

## ----linear_boxcox-------------------------------------------------------
m0p <- BoxCox(psa ~ 1, order = 1, data = Prostate, log_first = TRUE)
mtp <- tramnet(m0p, x = x, lambda = 0, alpha = 0)

## ----BH_Linear5, echo=FALSE, fig.height=3, fig.width=5-------------------
K <- 1e3
nd <- Prostate[rep(1, K),]
nd$lpsa <- seq(min(Prostate$lpsa), max(Prostate$lpsa), length.out = K)
nd$psa <- exp(nd$lpsa)
nd$pred1 <- predict(mt, type = "trafo", newdata = nd)
nd$pred2 <- predict(mt1, type = "trafo", newdata = nd)
nd$pred3 <- predict(mt2, type = "trafo", newdata = nd)

col3 <- qualitative_hcl(3)
xyplot(pred1 + pred2 + pred3 ~ lpsa, data = nd, type = "l", lwd = 1.5,
       col = col3, ylab = expression(italic(h)(italic(y))),
       xlab = expression(log(psa)), # asp = 1,
       # main = "Estimating the baseline transformation from data",
       panel = function(x, y, ...) {
         panel.grid(h = -1, v = -1)
         panel.xyplot(x, y, ...)
         panel.rug(x = Prostate$lpsa, end = 0.02, col = "grey60")
       }, auto.key = list(
         text = c("mt", "mt1", "mt2"),
         points = FALSE, corner = c(1, 0), x = 0.95, y = 0.05, col = col3, cex = 0.75
         ), scales = list(tck = c(1, 0)),
       par.settings = list(layout.heights = list(bottom.padding = 0,
                                                 top.padding = 0))
       )

## ----table1, results='asis', echo=FALSE----------------------------------
cfx_tab <- as.data.frame(rbind(coef(mp, which = "penalized"), coef(mt), coef(mtp),
                               coef(mt1), coef(mt2)))
cfx_tab$` ` <- c(loglik(mp), logLik(mt), logLik(mtp), logLik(mt1), logLik(mt2))
rownames(cfx_tab) <- paste0("\\code{", c("mp", "mt", "mtp", "mt1", "mt2"), "}")
kbl <- knitr::kable(as.data.frame(cfx_tab), row.names = TRUE, booktabs = TRUE,
                    linesep = "", format = "latex", digits = 2, escape = FALSE)
kableExtra::add_header_above(kbl, header = c("Model" = 1, "Coefficient estimates" = 8,
                                             "logLik" = 1), escape = FALSE,
                             bold = TRUE)

## ----cvl_prof_setup, eval=FALSE------------------------------------------
#  m0 <- BoxCox(lpsa ~ 1, data = Prostate, order = 7, extrapolate = TRUE)
#  mt <- tramnet(m01, x = x, alpha = 1, lambda = 0)

## ----cvl_tramnet, eval=FALSE---------------------------------------------
#  lambdas <- c(0, 10^seq(-4, log10(15), length.out = 4))
#  cvlt <- cvl_tramnet(object = mt, fold = 2, lambda = lambdas, alpha = 1)

## ----cvl_tramnet2, eval=FALSE--------------------------------------------
#  pen_cvl <- optL1(response = lpsa, penalized = x, lambda2 = 0, data = Prostate,
#                   fold = 2)
#  cvlt <- cvl_tramnet(object = mt, lambda = lambdas, alpha = 1,
#                      folds = pen_cvl$fold)

## ----prostate_mbo, results="hide", eval=FALSE----------------------------
#  tmbo <- mbo_tramnet(mt, obj_type = "elnet", fold = 10)
#  mtmbo <- mbo_recommended(tmbo, m0, x)

## ----load_from_dat, echo=FALSE-------------------------------------------
if (file.exists("cache.rda")) {
    load("cache.rda")
} else {
  tmbo <- mbo_tramnet(mt, obj_type = "elnet", fold = 10)
  mtmbo <- mbo_recommended(tmbo, m0, x)
  save(tmbo, mtmbo, file = "cache.rda")
}

## ----prostate_mbo_recommended--------------------------------------------
print_mbo(tmbo)

## ----prostate_coefs------------------------------------------------------
coef(mtmbo)
summary(mtmbo)$sparsity

## ----profiling, eval=FALSE-----------------------------------------------
#  pfl <- prof_lambda(mt)

## ----load_from_dat2, echo=FALSE------------------------------------------
if (file.exists("cache2.rda")) {
    load("cache2.rda")
} else {
  m0 <- BoxCox(lpsa ~ 1, data = Prostate, order = 7, extrapolate = TRUE)
  mt <- tramnet(m01, x = x, alpha = 1, lambda = 0)
  pfl <- prof_lambda(mt)
  save(pfl, file = "cache2.rda")
}

## ----profplotcode, eval=FALSE, echo=TRUE---------------------------------
#  plot_path(pfl, plot_logLik = FALSE, las = 1, col = coll)

## ----plot_setup, include=FALSE, echo=FALSE-------------------------------
coll <- qualitative_hcl(n = ncol(pfl$cfx))

## ----profiling_plot, fig.height=4, fig.width=6.5, out.width="1\\textwidth", echo=FALSE----
plot_path(pfl, plot_logLik = FALSE, las = 1, col = coll)

## ----addconst------------------------------------------------------------
m0 <- BoxCox(lpsa ~ 1, data = Prostate, extrapolate = TRUE)
mt <- tramnet(m0, x, alpha = 0, lambda = 0, constraints = list(diag(8),
                                                               rep(0, 8)))
coef(mt)

## ----addconst_sparsity---------------------------------------------------
summary(mt)$sparsity

## ----addconst_tram_cmp---------------------------------------------------
m <- BoxCox(lpsa ~ . - psa, data = Prostate, extrapolate = TRUE,
            constraints = c("age >= 0", "lcp >= 0"))
max(abs(coef(m) - coef(mt, tol = 0)))

## ----coef_method---------------------------------------------------------
coef(mtmbo, with_baseline = TRUE, tol = 0)

## ----logLik_method-------------------------------------------------------
logLik(mtmbo)
cfx <- coef(mtmbo, with_baseline = TRUE, tol = 0)
cfx[5:8] <- 0.5
logLik(mtmbo, parm = cfx)
logLik(mtmbo, newdata = Prostate[1:10,])
logLik(mtmbo, w = runif(n = nrow(mtmbo$x)))

## ----plot_method_chunk, fig.height=7, fig.width=5, eval=FALSE------------
#  par(mfrow = c(3, 2)); K <- 1e3
#  plot(mtmbo, type = "distribution", K = K, main = "A") # A, default
#  plot(mtmbo, type = "survivor", K = K, main = "B") # B
#  plot(mtmbo, type = "trafo", K = K, main = "C") # C
#  plot(mtmbo, type = "density", K = K, main = "D") # D
#  plot(mtmbo, type = "hazard", K = K, main = "E") # E
#  plot(mtmbo, type = "trafo", newdata = Prostate[1, ], col = 1, K = K, main = "F") # F

## ----plot_method_fig, fig.height=7, fig.width=5, echo=FALSE--------------
par(mfrow = c(3, 2)); K <- 1e3
plot(mtmbo, type = "distribution", K = K, main = "A") # A, default
plot(mtmbo, type = "survivor", K = K, main = "B") # B
plot(mtmbo, type = "trafo", K = K, main = "C") # C
plot(mtmbo, type = "density", K = K, main = "D") # D
plot(mtmbo, type = "hazard", K = K, main = "E") # E
plot(mtmbo, type = "trafo", newdata = Prostate[1, ], col = 1, K = K, main = "F") # F

## ----predict_method------------------------------------------------------
predict(mtmbo, type = "quantile", prob = 0.2, newdata = Prostate[1:5,])

## ----simulate_method-----------------------------------------------------
simulate(mtmbo, nsim = 1, newdata = Prostate[1:5,], seed = 1)

## ----residuals_method----------------------------------------------------
residuals(mtmbo)[1:5]

## ----coin_illustration, eval=FALSE---------------------------------------
#  library("coin")
#  m0 <- BoxCox(lpsa ~ 1, data = Prostate, extrapolate = TRUE)
#  x_no_age_lcp <- x[, !colnames(x) %in% c("age", "lcp")]
#  mt_no_age_lcp <- tramnet(m0, x_no_age_lcp, alpha = 0, lambda = 0)
#  r <- residuals(mt_no_age_lcp)
#  it <- independence_test(r ~ age + lcp, data = Prostate,
#                          teststat = "max", distribution = approximate(1e6))
#  pvalue(it, "single-step")

## ----load_from_dat3, echo=FALSE------------------------------------------
if (file.exists("cache3.rda")) {
    load("cache3.rda")
} else {
  library("coin")
  m0 <- BoxCox(lpsa ~ 1, data = Prostate, extrapolate = TRUE)
  x_no_age_lcp <- x[, !colnames(x) %in% c("age", "lcp")]
  mt_no_age_lcp <- tramnet(m0, x_no_age_lcp, alpha = 0, lambda = 0)
  r <- residuals(mt_no_age_lcp)
  it <- independence_test(r ~ age + lcp, data = Prostate,
                          teststat = "max", distribution = approximate(1e6))
  pvalue(it, "single-step")
  tmp <- pvalue(it, "single-step")
  save(tmp, file = "cache3.rda")
}
tmp

## ----packages, echo = FALSE, results = "hide"----------------------------
if (file.exists("packages.bib")) file.remove("packages.bib")
pkgversion <- function(pkg) {
    pkgbib(pkg)
    packageDescription(pkg)$Version
}
pkgbib <- function(pkg) {
    x <- citation(package = pkg, auto = TRUE)[[1]]
    b <- toBibtex(x)
    b <- gsub("Buehlmann", "B{\\\\\"u}hlmann", b)
    b[1] <- paste("@Manual{pkg:", pkg, ",", sep = "")
    if (is.na(b["url"])) {
        b[length(b)] <- paste("   URL = {http://CRAN.R-project.org/package=",
                              pkg, "}", sep = "")
        b <- c(b, "}")
    }
    cat(b, sep = "\n", file = "packages.bib", append = TRUE)
}
pkg <- function(pkg)
    paste("\\\\pkg{", pkg, "} \\\\citep[version~",
          pkgversion(pkg), ",][]{pkg:", pkg, "}", sep = "")
pkg("tram")
pkg("trtf")
pkg("tbm")

## ----sesinf, echo=FALSE--------------------------------------------------
sessionInfo()

