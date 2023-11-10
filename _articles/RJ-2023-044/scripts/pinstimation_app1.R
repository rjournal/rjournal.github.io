
library(PINstimation)

## NOTE: Running the application using the raw data of 58 stocks and 20 model specifications
## takes a prohibitively long time. Therefore, we provide the intermediate results for the
## creation of the tables and results

## Download intermediate results of data from the Internet
## The list 'app1' stores the different tables, and figures in the paper
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
app1 <- readRDS(file = url("https://www.pinstimation.com/data/rpaper_app_1.RDS"), "rb")


## Create a list of model names for each model (PIN models, MPIN models, and AdjPIN models)
## A list of all model names is 'allmethods'
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
method.pin <- c("PIN_EA", "PIN_GWJ", "PIN_YZ", "PIN_YZ_EA", "PIN_EA_LK", "PIN_EA_EHO",
                "PIN_EA_1", "PIN_EA_10")
method.mpin <- c("MPIN.ML_EG", "MPIN.ML_E", "MPIN.ML_EM", "MPIN.EM_100", "MPIN.EM_all")
method.adjpin <- c("ADJPIN_GE", "ADJPIN_RND", "ADJPIN.EM_GE", "ADJPIN.EM_RND",
                   "ADJPIN_M1", "ADJPIN_M2", "ADJPIN_M3")

# allmethods includes the model relative to the layers to be used in Table 5
allmethods <- c(method.pin, method.mpin, method.adjpin, paste(method.mpin, "_layer", sep=""))



## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## USEFUL FUNCTIONS
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 'getheaders' returns the headers of tables given the model name
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
getheaders <- function(model) {
  headers <- switch(
    EXPR=model,
    "PIN" = c("pin", "alpha", "delta", "mu", "eps.b", "eps.s", "likelihood", "runtime"),
    "MPIN" = c("pin", "layer", "alpha", "delta", "mu", "eps.b", "eps.s", "likelihood", "runtime"),
    "ADJPIN" = c("pin", "psos", "alpha", "delta", "theta", "thetap", "eps.b", "eps.s",
                 "mu.b", "mu.s", "d.b", "d.s", "likelihood", "runtime")
  )
  return(headers)
}


## 'createTable4' creates the table number 4 in the paper using tables of estimations
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
createTable4 <- function(pintab, mpintab, adjpintab) {

  pintemp <- lapply(pintab, function(x){mx <- colMeans(x);
  mx[1:3] <-  mx[1:3]* 100; mx})
  pintemp <- data.frame(do.call(rbind, pintemp))
  pintemp <- cbind("method" =names(pintab), pintemp)
  rownames(pintemp) <- NULL
  colnames(pintemp) <- c("method", getheaders("PIN"))
  pintemp <- pintemp[, c(
    "method", "pin", "alpha", "delta", "mu", "eps.b", "eps.s", "runtime")]


  mpintemp <- lapply(mpintab, function(x) {mx <- colMeans(x);
  mx[c(1,3,4)] <-  mx[c(1,3,4)] * 100; mx})
  mpintemp <- data.frame(do.call(rbind, mpintemp))
  mpintemp <- cbind("method" =names(mpintab), mpintemp)
  rownames(mpintemp) <- NULL
  colnames(mpintemp) <- c("method", getheaders("MPIN"))
  mpintemp <- mpintemp[, c(
    "method", "pin", "alpha", "delta", "mu", "eps.b", "eps.s", "runtime")]

  adjpintemp <- lapply(adjpintab, function(x){mx <- colMeans(x);
  mx[1:6] <-  mx[1:6] * 100; mx})
  adjpintemp <- data.frame(do.call(rbind, adjpintemp))
  adjpintemp <- cbind("method" =names(adjpintab), adjpintemp)
  rownames(adjpintemp) <- NULL
  colnames(adjpintemp) <- c("method", getheaders("ADJPIN"))
  adjpintemp$mu <- (adjpintemp$mu.b + adjpintemp$mu.s)/2
  adjpintemp <- adjpintemp[, c(
    "method", "pin", "alpha", "delta", "mu","eps.b", "eps.s", "runtime")]
  tab <- rbind(pintemp, mpintemp, adjpintemp)

}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## LOAD ESTIMATION RESULTS' TABLES INTO DIFFERENT OBJECTS
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


tab.largepin <- app1$largepin
tab.smallpin <- app1$smallpin
tab.pin <- app1$allpin

tab.largempin <- app1$largempin
tab.smallmpin <- app1$smallmpin
tab.mpin <- app1$allmpin

tab.largeadjpin <- app1$largeadjpin
tab.smalladjpin <- app1$smalladjpin
tab.adjpin <- app1$alladjpin


## ########### ##
##   TABLE 4   ##
## ########### ##


Tab_4 <- createTable4(tab.pin, tab.mpin, tab.adjpin)
show(knitr::kable(Tab_4, format="simple", digits = 3,
                  caption = "Mean estimates for PIN and five model parameters"))

app1$table4 <- Tab_4

## ########### ##
##   TABLE 5   ##
## ########### ##


all.large <- c(
  lapply(tab.largepin, function(x) x$pin),
  lapply(tab.largempin, function(x) x$pin),
  lapply(tab.largeadjpin, function(x) x$pin),
  lapply(tab.largempin, function(x) x$layer)
)

all.small <- c(
  lapply(tab.smallpin, function(x) x$pin),
  lapply(tab.smallmpin, function(x) x$pin),
  lapply(tab.smalladjpin, function(x) x$pin),
  lapply(tab.smallmpin, function(x) x$layer)
)

Tab_5 <- lapply(seq_along(all.small), function(x)
  c(mean(all.large[[x]]), mean(all.small[[x]]),
    mean(all.small[[x]])- mean(all.large[[x]]),
    t.test(all.large[[x]], all.small[[x]])$p.value)
  )
Tab_5 <- data.frame(do.call(rbind, Tab_5))
Tab_5[1:20,1:3] <- 100*Tab_5[1:20,1:3]
Tab_5 <- cbind(allmethods, Tab_5)
colnames(Tab_5) <- c("Name", "PIN - Large", "PIN - Small", "Difference", "p-value")
show(knitr::kable(Tab_5, format="simple", digits = 3,
                  caption = "Mean PIN estimates and number of layers for large and small stocks"))

app1$table5 <- Tab_5

## ############ ##
##   TABLE A7   ##
## ############ ##


lpin <- length(getheaders("PIN"))
lmpin <- length(getheaders("MPIN"))
ladjpin <- length(getheaders("ADJPIN"))

all.large <- c(
  lapply(seq_len(lpin), function(x) tab.largepin$PIN_EA[, x]),
  lapply(seq_len(lmpin), function(x) tab.largempin$MPIN.ML_EG[,x]),
  lapply(seq_len(ladjpin), function(x) tab.largeadjpin$ADJPIN_GE[,x])
)

all.small <- c(
  lapply(seq_len(lpin), function(x) tab.smallpin$PIN_EA[ ,x]),
  lapply(seq_len(lmpin), function(x) tab.smallmpin$MPIN.ML_EG[ ,x]),
  lapply(seq_len(ladjpin), function(x) tab.smalladjpin$ADJPIN_GE[ ,x])
)


Tab_A7 <- lapply(seq_len(length(all.small)), function(x)
  c(mean(c(all.large[[x]],all.small[[x]])),
    mean(all.large[[x]]), mean(all.small[[x]]),
    mean(all.small[[x]])-mean(all.large[[x]]),
  t.test(all.large[[x]], all.small[[x]])$p.value)
)

Tab_A7 <- data.frame(do.call(rbind, Tab_A7))

# Multiply the probabilities by 100
Tab_A7[c(1:3, 9:12, 18:23),1:4] <- 100 * Tab_A7[c(1:3, 9:12, 18:23),1:4]

colnames(Tab_A7) <- c("All", "Large", "Small", "Difference", "p-value")
Tab_A7 <- cbind(
  Name = c(getheaders("PIN"), getheaders("MPIN"), getheaders("ADJPIN")), Tab_A7)

# Delete the row relative to the number of layers
Tab_A7 <- Tab_A7[-10, ]
show(knitr::kable(Tab_A7, format="simple", digits = 3,
                  caption = "Mean values for PIN estimates, model parameters and running times"))

app1$tableA2 <- Tab_A7

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CREATE R PAPER FIGURES
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\n\n\rPrinting Figure 2A")

# PIN VALUE for PIN, MPIN and ADJPIN models, 58 observations, large vs small.

pinv <- c(tab.largepin$PIN_EA$pin, tab.smallpin$PIN_EA$pin)
mpinv <- c(tab.largempin$MPIN.ML_EG$pin , tab.smallmpin$MPIN.ML_EG$pin)
adjpinv <- c(tab.largeadjpin$ADJPIN_GE$pin, tab.smalladjpin$ADJPIN_GE$pin)

plot(pinv, type="b", cex=1, pch=18, col="red", ylim = c(0.05, 0.45), xaxt="none",
     xlab=paste0("large stocks", strrep(" ", 125), "small stocks"), ylab="pin")
lines(mpinv, type="b", cex=1, pch=1, col="green", ylim = c(0.05, 0.450))
lines(adjpinv, type="b", cex=1, pch=20, col="blue", ylim = c(0.05, 0.450))
abline(v=(length(pinv) + 1)/2, col="black", lty = 2)
axis(1, 1:length(pinv), 1:length(pinv), las=2, cex.axis=1)

cat("\n\n\rPrinting Figure 2B\n\n")

# Alpha VALUE for PIN, MPIN and ADJPIN models, 58 observations, large vs small.

pinav <- c(tab.largepin$PIN_EA$alpha, tab.smallpin$PIN_EA$alpha)
mpinav <- c(tab.largempin$MPIN.ML_EG$alpha , tab.smallmpin$MPIN.ML_EG$alpha)
adjpinav <- c(tab.largeadjpin$ADJPIN_GE$alpha, tab.smalladjpin$ADJPIN_GE$alpha)

plot(pinav, type="b", cex=1, pch=18, col="red", ylim = c(0, 1), xaxt="none",
     xlab=paste0("large stocks", strrep(" ", 125), "small stocks"), ylab="alpha")
lines(mpinav, type="b", cex=1, pch=1, col="green", ylim = c(0, 1))
lines(adjpinav, type="b", cex=1, pch=20, col="blue", ylim = c(0, 1))
abline(v=(length(pinav) + 1)/2, col="black", lty = 2)
axis(1, 1:length(pinav), 1:length(pinav), las=2, cex.axis=1)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE THE LIST IN AN RDS VARIABLE 'rpaper_app1.RDS'
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(object = app1, file="rpaper_app1.RDS")
