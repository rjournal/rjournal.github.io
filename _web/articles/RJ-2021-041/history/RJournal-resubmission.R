## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 7, 
                      out.width = "100%", fig.align = "center", fig.path="figs/")
# devtools::install_github("petersonR/bestNormalize")
library(bestNormalize)
library(kableExtra)
library(recipes)
library(caret)
library(tibble)
library(visreg)

set.seed(100)


## ----orq_vis, echo = FALSE, fig.cap="ORQ normalization visualization on Fisher's iris data."----------------------------------

x <- iris$Petal.Width
on <- orderNorm(x, warn = FALSE)
xx <- seq(min(x) - 1, max(x) + 1, length = 1000)
yy <- suppressWarnings(predict(on, xx))
r <- ((rank(x) - .5) / (length(x)))
f  <- suppressWarnings(glm(r ~ x, family = "binomial"))
p <- qnorm(predict(f, newdata = data.frame(x = xx), type = 'response'))

plot(x, on$x.t, pch = 20, xlim = range(xx), ylim = range(p, yy), main = "ORQ Normalization",
      xlab = "Original Value", ylab = "Transformed Value")
lines(xx, p, col = '1', lwd = 1, lty =2)
lines(xx, yy, col = 'slateblue', lwd = 2)

# Add legend
legend('bottomright', 
       c('Original data', 'Transformed values for new data', 
         'Approximation for extrapolation'), 
       bty = 'n', lty = c(0, 1, 2), lwd = c(0, 2,1), 
       pch = c(20, NA, NA), 
       col = c(1, "slateblue", 1, 1))


## ----gen_data-----------------------------------------------------------------------------------------------------------------
x <- rgamma(250, 1, 1)


## ----simple_hist, echo = FALSE, fig.height=4, fig.width=6, out.height="1.3in", out.width="2.5in", fig.cap="Simulated skewed data for simple example."----
MASS::truehist(x, nbins = 12)


## ----vis_code-----------------------------------------------------------------------------------------------------------------
(BNobject <- bestNormalize(x))


## ----bn_plot, fig.cap="The suite of transformations estimated by default in \\pkg{bestNormalize} (trained on simulated right-skewed data)."----
plot(BNobject, leg_loc = "topleft")


## ----hist_best, warning=FALSE, echo = FALSE, fig.height=8, fig.width = 7.8, fig.cap="Summary of transformations performed on simulated right-skewed data."----
par(mfrow = c(2,2), mar = c(4, 4, 4, 1))
MASS::truehist(x, main = "Original Data")
MASS::truehist(BNobject$x.t, main = paste("Best Normalizing Transformation:", class(BNobject$chosen_transform)[1]), nbins = 12)
xx <- seq(min(x), max(x), length = 100) # vector of values over domain of x
pp <- predict(BNobject, newdata = xx)
plot(BNobject, main = "Normalizing transformations", leg_loc = "topleft")
lines(xx, pp, lwd = 5)
plot(BNobject, main = "Normalizing transformations (inverse)", inverse = TRUE, leg_loc = "topleft")
lines(pp, predict(BNobject, newdata = pp, inverse = TRUE),  lwd = 5)


## -----------------------------------------------------------------------------------------------------------------------------
(arcsinh_obj <- arcsinh_x(x))
(boxcox_obj <- boxcox(x))
(yeojohnson_obj <- yeojohnson(x))
(lambert_obj <- lambert(x, type = "s"))
(orderNorm_obj <- orderNorm(x))


## ----vis_data, eval = FALSE---------------------------------------------------------------------------------------------------
## xx <- seq(min(x), max(x), length = 100)
## plot(xx, predict(arcsinh_obj, newdata = xx), type = "l", col = 1)
## lines(xx, predict(boxcox_obj, newdata = xx), col = 2)
## lines(xx, predict(yeojohnson_obj, newdata = xx), col = 3)
## lines(xx, predict(orderNorm_obj, newdata = xx), col = 4)


## ----vis_data2, echo = FALSE, fig.height = 4, fig.width = 6, out.height = "2.5in", out.width = "4.375in", fig.cap="Manually plotting transformations trained on simulated right-skewed data."----
xx <- seq(min(x), max(x), length = 100) # vector of values over domain of x
plot(xx, predict(arcsinh_obj, newdata = xx), type = "l", col = 1, ylim = c(-4, 4),
     xlab = 'x', ylab = "g(x)")
lines(xx, predict(boxcox_obj, newdata = xx), col = 2)
lines(xx, predict(yeojohnson_obj, newdata = xx), col = 3)
lines(xx, predict(orderNorm_obj, newdata = xx), col = 4)

legend("bottomright", legend = c("arcsinh", "Box-Cox", "Yeo-Johnson", "OrderNorm"), 
       col = 1:4, lty = 1, bty = 'n')


## ----hist_trans, fig.height=6, fig.width = 6, echo = FALSE, out.height="4in", out.width="4in", fig.cap="Normalized values for trained transformations on simulated right-skewed data."----
par(mfrow = c(2,2))
MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 12)
MASS::truehist(boxcox_obj$x.t, main = "Box-Cox transformation", nbins = 12)
MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(orderNorm_obj$x.t, main = "OrderNorm (ORQ) transformation", nbins = 12)


## ----boxplot, fig.width=11, out.width="100%", fig.cap="Cross-validation results for each normalization method, where the our estimated normality statistic is plotted on the y-axis."----
boxplot(BNobject$oos_preds, log = 'y')
abline(h = 1, col = "green3")


## -----------------------------------------------------------------------------------------------------------------------------
bestNormalize(x, loo = TRUE)


## ----bn_output----------------------------------------------------------------------------------------------------------------
bestNormalize(x, allow_orderNorm = FALSE, out_of_sample = FALSE)


## ---- eval = FALSE------------------------------------------------------------------------------------------------------------
## cl <- parallel::makeCluster(5)
## b <- bestNormalize(x, cluster = cl, r = 10, quiet = TRUE)
## parallel::stopCluster(cl)


## ----parallel_timings, echo = FALSE, fig.height = 4, fig.width = 4.5, fig.cap="Potential speedup using parallelization functionality."----
knitr::include_graphics("figs/parallel_timings.pdf")


## ---- message = FALSE---------------------------------------------------------------------------------------------------------
rec <- recipe( ~ ., data = iris)  %>%                       # Initialize recipe
  step_best_normalize(all_predictors(), -all_nominal()) %>% # Transform predictors
  prep(iris) %>%                                            # Prep (train) recipe
  bake(iris)                                                # Bake (apply) recipe


## -----------------------------------------------------------------------------------------------------------------------------
## Define custom function
cuberoot_x <- function(x, ...) {
  x.t <- (x)^(1/3)
  
  # Get in-sample normality statistic results
  ptest <- nortest::pearson.test(x.t)
  
  val <- list(
    x.t = x.t,
    x = x,
    n = length(x.t) - sum(is.na(x)), 
    norm_stat = unname(ptest$statistic / ptest$df)
  )
  
  # Assign class, return
  class(val) <- c('cuberoot_x')
  val
}

# S3 method that is used to apply the transformation to newly observed data
predict.cuberoot_x <- function(object, newdata = NULL, inverse = FALSE, ...) {
  
  # If no data supplied and not inverse
  if (is.null(newdata) & !inverse)
    newdata <- object$x
  
  # If no data supplied and inverse transformation is requested
  if (is.null(newdata) & inverse)
    newdata <- object$x.t
  
  # Perform inverse transformation
  if (inverse) {
    # Reverse-cube-root (cube)
    val <-  newdata^3
    
    # Otherwise, perform transformation as estimated
  } else if (!inverse) {
    val <- (newdata)^(1/3)
  }
  
  # Return transformed data
  unname(val)
}

## Optional: print S3 method
print.cuberoot_x <- function(x, ...) {
  cat('cuberoot(x) Transformation with', x$n, 'nonmissing obs.\n')
}



## -----------------------------------------------------------------------------------------------------------------------------
custom_transform <- list(
  cuberoot_x = cuberoot_x,
  predict.cuberoot_x = predict.cuberoot_x,
  print.cuberoot_x = print.cuberoot_x
)

set.seed(123129)
x <- rgamma(100, 1, 1)
(b <- bestNormalize(x = x, new_transforms = custom_transform))


## -----------------------------------------------------------------------------------------------------------------------------
bestNormalize(x, norm_stat_fn = function(x) nortest::lillie.test(x)$stat)


## -----------------------------------------------------------------------------------------------------------------------------
(dont_do_this <- bestNormalize(x, norm_stat_fn = function(x) nortest::lillie.test(x)$p))


## -----------------------------------------------------------------------------------------------------------------------------
best_transform <- names(which.max(dont_do_this$norm_stats))
do_this <- dont_do_this$other_transforms[[best_transform]]
or_this <- bestNormalize(x, norm_stat_fn = function(x) 1-nortest::lillie.test(x)$p)


## ----load_appdata-------------------------------------------------------------------------------------------------------------
data("autotrader")
autotrader$yearsold <- 2017 - autotrader$Year


## ---- echo = FALSE------------------------------------------------------------------------------------------------------------
tab <- arsenal::tableby(~Make + price + mileage + Year + yearsold, data = autotrader, digits = 1) %>% 
  summary(text = TRUE, labelTranslations = list("price" = "Price ($)", "mileage" = "Mileage", "yearsold" = "Age (years old)")) %>% 
  as.data.frame()


# Fix formatting
names(tab)[2] <- "Overall (N=6,283)"

my_meansd <- function(x) {
  m <- format(round(mean(x)), big.mark = ",")
  s <- format(round(sd(x)), big.mark = ",")
  
  paste0(m, " (", s, ")")
}

my_range <- function(x) {
  m <- format(round(min(x)), big.mark = ",")
  s <- format(round(max(x)), big.mark = ",")
  
  paste0(m, " - ", s)
}


tab[4,2] <- paste0(format(1257, big.mark = ","), " (20.0%)")
tab[6,2] <- paste0(format(1029, big.mark = ","), " (16.4%)")
tab[11,2] <- paste0(format(1202, big.mark = ","), " (19.1%)")

tab[14,2] <- my_meansd(autotrader$price)
tab[15,2] <- my_range(autotrader$price)
tab[17,2] <- my_meansd(autotrader$mileage)
tab[18,2] <- my_range(autotrader$mileage)

tab %>% 
  kable(booktabs = TRUE, linesep ="", caption = "Sample characteristics of `autotrader` data.") %>% 
  kable_styling(c("striped", "condensed"), full_width = FALSE)


## ----bn_price-----------------------------------------------------------------------------------------------------------------
(priceBN <- bestNormalize(autotrader$price))


## ----bn_mileage---------------------------------------------------------------------------------------------------------------
(mileageBN <- bestNormalize(autotrader$mileage))


## ----bn_yearsold--------------------------------------------------------------------------------------------------------------
(yearsoldBN <- bestNormalize(autotrader$yearsold))


## ----hist_app, fig.height=3, fig.width=6, echo = FALSE, out.height="2.1in", out.width="4.2in", fig.cap = "Distributions of car variables before and after normalization."----
par(mfcol = c(2, 3), mar = c(4,3,1,1))
MASS::truehist(autotrader$price, main = "", xlab = "Price")
MASS::truehist(priceBN$x.t, main = "", xlab = "Price (transformed)")
MASS::truehist(autotrader$mileage, xlab = "Mileage")
MASS::truehist(mileageBN$x.t, xlab = "Mileage (transformed)")
MASS::truehist(autotrader$yearsold, xlab = "Age")
MASS::truehist(yearsoldBN$x.t, xlab = "Age (transformed)")


## ----tbs_model----------------------------------------------------------------------------------------------------------------
p.t <- priceBN$x.t; m.t <- mileageBN$x.t; yo.t <- yearsoldBN$x.t
fit <- lm(p.t ~ m.t + yo.t)


## ---- echo = FALSE------------------------------------------------------------------------------------------------------------

pvalr <- function (pvals, sig.limit = 0.001, digits = 3) {
    roundr <- function(x, digits = 1) {
        res <- sprintf(paste0("%.", digits, "f"), x)
        zzz <- paste0("0.", paste(rep("0", digits), collapse = ""))
        res[res == paste0("-", zzz)] <- zzz
        res
    }
    sapply(pvals, function(x, sig.limit) {
        if (is.na(x)) 
            return(x)
        if (x < sig.limit) 
            return(sprintf("< %s", format(sig.limit)))
        if (x > 0.1) 
            return(roundr(x, digits = 2))
        else return(roundr(x, digits = digits))
    }, sig.limit = sig.limit)
}

s <- summary(fit)
s$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column("Variable") %>% 
  mutate(Variable = c("Intercept", "g(Mileage)", "g(Age)"),
         `Pr(>|t|)` = pvalr(`Pr(>|t|)`)) %>% 
  kable(align = c("lrrrr"), digits = 3, booktabs = TRUE, caption = "TBS regression results for autotrader data.") %>% 
  kable_styling(full_width = FALSE)


## ----linear_visreg_code, eval = FALSE-----------------------------------------------------------------------------------------
## visreg(fit, "m.t")
## visreg(fit, "m.t",
##        partial = TRUE,
##        trans = function(price.t)
##          predict(priceBN, newdata = price.t, inverse = TRUE)/1000,
##        xtrans = function(mileage.t)
##          predict(mileageBN, newdata = mileage.t, inverse = TRUE)
## )


## ----linear_visreg, out.width = "80%", echo = FALSE, fig.cap = "TBS regression visualized on transformed units (left) and original units (right)."----
mileage.xx <- seq(min(autotrader$mileage), max(autotrader$mileage), len = 100)
price.xx <- seq(min(autotrader$price), max(autotrader$price), len = 100)
yearsold.xx <- seq(min(autotrader$yearsold), max(autotrader$yearsold), len = 100)

par(mfrow = c(1,2), mar = c(4, 4, 1, 1))
visreg(fit, "m.t", ylab = "g(Price)", xlab = "g(Mileage)", 
       points.par = list(col = grey(.1, alpha = .2)))
fit_gam <- mgcv::gam(price ~ s(yearsold) + s(mileage), data = autotrader)
newX <- data.frame(mileage = mileage.xx, yearsold = median(autotrader$yearsold))
p_gam <- predict(fit_gam, newdata = newX, se.fit = TRUE)
# pi_ub <- p_gam$fit + p_gam$se.fit*1.96
# pi_lb <- p_gam$fit - p_gam$se.fit*1.96
visreg(fit, "m.t", ylab = "Price ($1000)", xlab = "Mileage", 
       points.par = list(col = grey(.1, alpha = .2)),
       trans = function(price.t) predict(priceBN, newdata = price.t, inverse = TRUE)/1000, 
       xtrans = function(mileage.t) predict(mileageBN, newdata = mileage.t, inverse = TRUE), 
       partial = TRUE)
lines(mileage.xx, p_gam$fit/1000, lwd = 2, col = 'green3')
# polygon(c(mileage.xx, rev(mileage.xx)), c(pi_ub, rev(pi_lb))/1000, col = rgb(0,1,0,.3), border = NA)
legend(
  'topright',
  c("GAM", "TBS model"),
  lwd = 2,
  col = c("green3", "slateblue"),
  bty = "n"
  )


## ---- eval = FALSE------------------------------------------------------------------------------------------------------------
## # Set up data for plotting line
## new_yo <- seq(min(autotrader$yearsold), max(autotrader$yearsold), len = 100)
## newX <- data.frame(yearsold = new_yo, mileage = median(autotrader$mileage))
## newXt <- data.frame(yo.t = predict(yearsoldBN, newX$yearsold),
##                     m.t = predict(mileageBN, newX$mileage))
## 
## line_vals_t <- predict(fit, newdata = newXt) # Calculate line (transformed)
## line_vals <- predict(priceBN, newdata = line_vals_t, inverse = TRUE)
## plot(autotrader$yearsold, autotrader$price)
## lines(new_yo, line_vals)


## ----gam_tbs_model, out.width = "80%", eval = TRUE, echo = FALSE, fig.cap="Age effect on car price (re-transformed to original unit)."----
new_yo <- seq(min(autotrader$yearsold), max(autotrader$yearsold), len = 100)
newX <- data.frame(yearsold = new_yo, 
                  mileage = median(autotrader$mileage))
newXt <- data.frame(yo.t = predict(yearsoldBN, newX$yearsold), 
                    m.t = predict(mileageBN, newX$mileage))
plot(jitter(autotrader$yearsold, 1.5), autotrader$price, pch = 20,
     col = grey(.1, alpha = .1), 
     main = "",
     xlab = "Age (Jittered)", ylab = "Price")
line_vals <- predict(fit, newdata = newXt)
lines(new_yo, 
      predict(priceBN, newdata = line_vals, inverse = TRUE),
      lwd = 2, col = "slateblue")
lines(yearsold.xx, 
      predict(fit_gam, newdata = newX), 
      lwd = 2, col = 'green3')
legend('topright', c("GAM", "TBS model"),  lwd = 2,
       col = c("green3", "slateblue"),  bty = "n")


## ---- eval = FALSE------------------------------------------------------------------------------------------------------------
## library(tidymodels)
## library(caret)
## library(recipes)
## 
## set.seed(321)
## df_split <- initial_split(autotrader, prop = .9)
## df_train <- training(df_split)
## df_test <- testing(df_split)
## 
## rec <- recipe(price ~ Make + model +  mileage + status + Year, df_train) %>%
##   step_mutate(years_old = 2017 - Year) %>%
##   step_rm(Year) %>%
##   step_log(price) %>%
##   step_best_normalize(all_predictors(), -all_nominal()) %>%
##   step_other(all_nominal(), threshold = 10) %>%
##   step_dummy(all_nominal()) %>%
##   prep()
## 
## fit1 <- train(price ~ ., bake(rec, NULL), method = 'glmnet')
## fit2 <- train(price ~ ., bake(rec, NULL), method = 'earth')
## fit3 <- train(price ~ ., bake(rec, NULL), method = 'rf')
## 
## r <- resamples(fits <- list(glmnet = fit1, earth = fit2, rf = fit3))
## summary(r) # Extra-sample CV results


## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------------------------------------------------------
source("caret_application.R")
tab <- summary(r)$statistics
idx <- sapply(tab, nrow)
names(idx)[3] <- "RSQ"

tab %>% 
  Reduce(rbind, .) %>% 
  kable(digits = 3, booktabs = TRUE, caption = "CV prediction accuracy of various ML methods.") %>% 
  kable_styling(full_width = FALSE) %>%
  kableExtra::group_rows(index = idx)


## ---- eval = FALSE------------------------------------------------------------------------------------------------------------
## # Out of sample prediction accuracy
## results <- lapply(fits, function(x) {
##   p <- c(predict(x, newdata = bake(rec, df_test)))
##   yardstick::metrics(data.frame(est = exp(p), truth = df_test$price),
##                      truth = truth, estimate = est)
## })
## results


## ---- message=FALSE, echo = FALSE---------------------------------------------------------------------------------------------
# Runs application unless the results file is located (may take a while)
res <- lapply(results, function(x) x$.estimate) %>% 
  Reduce(rbind, .)

colnames(res) <- toupper(results$glmnet$.metric)
rownames(res) <- names(results)
res <- rownames_to_column(data.frame(res), "Method")

kable(res, digits = c(0, 0, 3, 0), booktabs = TRUE, caption = "Test data prediction accuracy of various ML methods. RMSE and MAE can be interpreted in terms of 2017 US dollars.") %>% 
  kable_styling(full_width = FALSE)

