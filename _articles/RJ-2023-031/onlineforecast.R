## ----child="sectionIntroduction.Rnw"----------------------------------

## ----echo=FALSE, fig.height=6-----------------------------------------

library(onlineforecast)
D <- Dbuilding

## -----------------------------------------------------------------------------
D$tday <- make_tday(D$t, 1:36)
head(D$tday)

## -----------------------------------------------------------------------------
Dtrain <- subset(D, c("2010-12-15", "2011-01-01"))
Dtrain$scoreperiod <- in_range("2010-12-20", Dtrain$t)


## -----------------------------------------------------------------------------
model <- forecastmodel$new()
model$output = "heatload"
model$add_inputs(Ta = "lp(Ta, a1=0.9)", 
                 I = "lp(I, a1=0.7)", 
                 mu_tday = "fs(tday/24, nharmonics=10)",
                 mu = "one()")
model$add_regprm("rls_prm(lambda=0.9)")


## -----------------------------------------------------------------------------
model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
                    I__a1 =  c(0.4, 0.8, 0.9999),
                    lambda = c(0.9, 0.99, 0.9999))


## ---- results="hide"----------------------------------------------------------
# Optimize the parameters
rls_optim(model, Dtrain, kseq=c(1,18), control=list(maxit=5), cachedir="tmp/cache")


## -----------------------------------------------------------------------------
model$kseq <- 1:36
val <- rls_fit(model$prm, model, D, returnanalysis = TRUE)


## ---- fig.height=3.5----------------------------------------------------------
D$Yhat <- val$Yhat
#plot_ts(D, c("heatload|^y|^Y"), c("2011-01-01","2011-02-01"), kseq = c(1,18))


## ---- fig.height=3.5----------------------------------------------------------
istart <- 204
par(bty="n", mfrow=c(3,1), mar=c(3.5,3.5,0,1), mgp=c(2.5,0.5,0))
iseq <- istart + model$kseq
tmp <- range(trunc(D$t[iseq], unit="days"))
at <- seq(tmp[1], tmp[2]+24*3600, by=6*3600)
for(i in istart:(istart+2)){
    iseq <- i + model$kseq
    plot(D$t[iseq], D$heatload[iseq], type="n", xlab="", ylab="Heat (kW)", xaxt="n", ylim=c(5,10), xlim=range(at[3:9])+c(3600,3600))
    abline(v=at, col = "gray", lty = "dashed")
    lines(D$t[iseq], D$heatload[iseq], col="gray45")
    lines(D$t[iseq], unlist(D$Yhat[i, ]), col="red", lwd=2, lty=1)
    #    axis.POSIXct(1, at=at)
    iseq2 <- (min(iseq)-1):max(iseq)
    axis.POSIXct(1, at=D$t[iseq2], labels=NA)
    # time
    tims <- c(1,c(7,13,19,25,31,37)+istart-i)
    axis.POSIXct(1, at=D$t[iseq2[tims]], labels=strftime(D$t[iseq2[tims]], format="%H:%M", tz="GMT"), col=NA, line=0.9)
    # steps
    axis.POSIXct(1, at=D$t[c(min(iseq)-2,iseq2)+1], labels=0:length(iseq), col=NA)
    #
    if(i == istart){
        legend("topright", c("Observation","Forecast"), col=c("gray45","red"), lwd=1:2, lty=1, bg="white")
    }
    if(i == istart+2){
        title(xlab="Horizon and time", xaxt="s")
    }
}




## ----child="sectionNotation.Rnw"--------------------------------------




## ----child="sectionTwostageModellingProcedure.Rnw"--------------------




## ----child="sectionModelselection.Rnw"--------------------------------




## ----child="sectionExampleNew.Rnw"------------------------------------

## ----results="markup", echo=FALSE, results="hide"---------------------
# The vignettes included in the package, see also the website onlineforecast.org
vignette(package="onlineforecast")


## ----echo=FALSE-------------------------------------------------------
# Dbuilding is a data.list loaded with the package, just to simplify notation call it D
D <- Dbuilding


## ----results="markup"-------------------------------------------------
# See the class
class(D)


## ----results="markup"-------------------------------------------------
# It holds the forecast matrix with ambient temperature forecasts
# simply as data.frame
class(D$Ta)
# It holds the k forecasts in each column 
# and each row is a forecast ahead in time
head(D$Ta[ ,1:8], 4)


## ----results="markup"-------------------------------------------------
# The time stamps are set in the end of the hour
D$t[1:4]


## ----results="markup"-------------------------------------------------
# Heat load observations
D$heatload[1:4]


## ---------------------------------------------------------------------
# New model object
model <- forecastmodel$new()


## ---------------------------------------------------------------------
# Set the variable to be forecasted
model$output <- "heatload"


## ---------------------------------------------------------------------
# Add an input, which is just a linear function of Ta
model$add_inputs(Ta = "Ta")


## ---------------------------------------------------------------------
# Add an intercept to the model
model$add_inputs(mu = "one()")


## ---------------------------------------------------------------------
# Low-pass filtering
model$add_inputs(Ta = "lp(Ta, a1=0.9)")


## ---------------------------------------------------------------------
# Non-linear with basis splines
model$add_inputs(Ta = "bspline(Ta, df=5)")


## ---------------------------------------------------------------------
# Nested functions
model$add_inputs(Ta = "bspline(lp(Ta, a1=0.9), df=5)")


## ----eval=FALSE-------------------------------------------------------
## eval(parse(text=frml), data)


## ---------------------------------------------------------------------
# Remove current inputs
model$inputs <- NULL
# Add intercept and low-pass filtered ambient temperature
model$add_inputs(mu = "one()",
                 Ta = "lp(Ta, a1=0.9)")


## ---------------------------------------------------------------------
D$scoreperiod <- rep(TRUE, length(D$t))


## ----results="markup"-------------------------------------------------
# Horizons to fit and predict
model$kseq <- 1:6
# Fit with linear regression and return RMSE score
lm_fit(c(Ta__a1=0.8), model, D, scorefun=rmse, returnanalysis=FALSE)


## ---------------------------------------------------------------------
# Set parameter for optimization
model$add_prmbounds(Ta__a1 = c(min=0.8, init=0.95, max=0.9999))


## ---------------------------------------------------------------------
# Optimize the low-pass coefficient
lm_optim(model, D, kseq=c(3,18))


## ----results="markup"-------------------------------------------------
# Optimized parameters
model$prm


## ---------------------------------------------------------------------
# The horizons to fit and predict for
model$kseq <- 1:24
# Fit and return the result
fit <- lm_fit(model$prm, model, D)


## ----output.lines=5---------------------------------------------------
# Use LM to predict
lm_predict(model, model$transform_data(D))


## ----echo=-1:-2, fig.height=4-----------------------------------------
# parameters for plot_ts()
p <- par_ts(legendspace=8, legendcex=0.7)
# Take the forecasts from the fit
D$Yhat <- fit$Yhat
# Plot selected horizons of the forecasts synced with the observed output
plot_ts(subset(D,D$scoreperiod), "heatload$|Yhat", kseq=c(1,5,24), p=p)


## ----echo=-1, purl=-1, fig.height=3-----------------------------------
par(mar=c(4,8,1,8))
acf(residuals(fit)$h1, na.action=na.pass, lag.max=96, main="")


## ----echo=-1, purl=-1, fig.height=3-----------------------------------
par(mar=c(4,8,1,8))
# Use the complete_cases function to only include points
# which are forecast on all horizons
inscore <- D$scoreperiod & complete_cases(fit$Yhat)
# The score as a function of the horizon
RMSE <- score(residuals(fit), scoreperiod = inscore)
plot(RMSE, ylim=c(0.75,0.88), xlab="Horizon")



## ----child="appForecastModelNotation.Rnw"-----------------------------




## ----child="appRegression.Rnw"----------------------------------------



