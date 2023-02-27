## Required packages
.packages <- c("nycflights13", "microbenchmark","deming", "zyp",
               "mblm", "RobustLinearReg", "ggplot2", "robslopes")

.missing <- .packages[!.packages %in% rownames(installed.packages())]
if (length(.missing))
    stop("Missing packages: ", paste(.missing, collapse=", "), ".\nConsider running\ninstall.packages(", paste0("'", .missing, "'", collapse=","),")")


#######################
# NYC flights example #
#######################

# load and prepare data
library("nycflights13")
data("flights")
data <- flights[-which(is.na(flights$air_time) ), ]

# Compute the Theil-Sen slope
ts.out.25 <- robslopes::robslope(formula = air_time~distance, 
                                 data = data, alpha = 0.25)
  
ts.out.25
ts.out.50 <- robslopes::robslope(formula = air_time~distance, 
                                 data = data, alpha = 0.5)
ts.out.50
ts.out.75 <- robslopes::robslope(formula = air_time~distance, 
                                 data = data, alpha = 0.75)
ts.out.75

# remove duplicates for plotting
x    <- data$distance
y    <- data$air_time
x_nodups <- x[!duplicated(cbind(x, y))]
y_nodups <- y[!duplicated(cbind(x, y))]

# pdf("flights_example_TS.pdf", height = 5, width = 7)
# par(mar = c(5.1, 4.1, 2.1, 1.1))
plot(x_nodups, y_nodups, pch = 16, col = adjustcolor("gray", alpha.f = 0.75),
     ylab = "air time (in minutes)", xlab = "distance traveled (in miles)", cex.axis = 1.5,
     cex.lab = 1.5)
abline(coef(ts.out.25), col = "red", lwd = 3)
abline(coef(ts.out.50), col = "green", lwd = 3)
abline(coef(ts.out.75), col = "blue", lwd = 3)
legend("topleft", fill = c("red", "green", "blue"), 
       legend = c("TS 25", "TS 50", "TS 75"), cex = 1.5)
# dev.off()


# Compute repeated median slope
rm.out.25 <- robslopes::robslope(formula = air_time~distance, 
                                 data = data, beta = 0.25,
                                 type = "RepeatedMedian")
rm.out.25
rm.out.50 <- robslopes::robslope(formula = air_time~distance, 
                                 data = data, beta = 0.5,
                                 type = "RepeatedMedian")
rm.out.50
rm.out.75 <- robslopes::robslope(formula = air_time~distance, 
                                 data = data, beta = 0.75,
                                 type = "RepeatedMedian")
rm.out.75

# pdf("flights_example_RM.pdf", height = 5, width = 7)
# par(mar = c(5.1, 4.1, 2.1, 1.1))
plot(x_nodups, y_nodups, pch = 16, col = adjustcolor("gray", alpha.f = 0.75),
     ylab = "air time (in minutes)", xlab = "distance traveled (in miles)", cex.axis = 1.5,
     cex.lab = 1.5)
abline(coef(rm.out.25), col = "red", lwd = 3)
abline(coef(rm.out.50), col = "green", lwd = 3)
abline(coef(rm.out.75), col = "blue", lwd = 3)
legend("topleft", fill = c("red", "green", "blue"), 
       legend = c("RM 25", "RM 50", "RM 75"), cex = 1.5)
# dev.off()

################################################################################
################################################################################

######################
# Benchmarking study #
######################

# Note: some of the computations in this benchmarking study can take 
# a long time to finish

library(microbenchmark)
library(deming)
library(zyp)
library(mblm)
library(RobustLinearReg)
library(ggplot2)

# For the Theil-Sen estimator

m <- 100
nvals <- c(10, 10^2, 10^3)

result <- list()
for (i in 1:length(nvals)) {
        set.seed(i)
        n <- nvals[i]
        mbm <- microbenchmark("deming" = deming::theilsen(y~x, data = data),
                              "zyp" = zyp::zyp.sen(y~x, dataframe = data),
                              "mblm" = mblm::mblm(y~x, dataframe = data, repeated = FALSE),
                              "RobustLinearReg" = RobustLinearReg::theil_sen_regression(y~x, data = data),
                              "robslopes" = robslopes::TheilSen(x, y, verbose =  FALSE),
                              setup = {x = rnorm(n); y = rnorm(n);
                              data = as.data.frame(cbind(x, y))}, times = 100)
        result[[i]] <- mbm
}

# leave out the mblm implementation for n = 10^4:
n   <- 10^4
mbm <- microbenchmark("deming" = deming::theilsen(y~x, data = data),
                      "zyp" = zyp::zyp.sen(y~x, dataframe = data),
                      "RobustLinearReg" = RobustLinearReg::theil_sen_regression(y~x, data = data),
                      "robslopes" = robslopes::TheilSen(x, y, verbose =  FALSE),
                      setup = {x = rnorm(n); y = rnorm(n);
                      data = as.data.frame(cbind(x, y))}, times = 100)
result[[4]] <- mbm

# plot the resuting computation times:
autoplot(result[[1]])
autoplot(result[[2]])
autoplot(result[[3]])
autoplot(result[[4]])




# For the Theil-Sen estimator

m <- 100
nvals <- c(10, 10^2, 10^3)

result <- list()
for (i in 1:length(nvals)) {
        set.seed(i)
        n <- nvals[i]
        mbm <- microbenchmark("deming" = deming::theilsen(y~x, data = data),
                              "zyp" = zyp::zyp.sen(y~x, dataframe = data),
                              "mblm" = mblm::mblm(y~x, dataframe = data, repeated = FALSE),
                              "RobustLinearReg" = RobustLinearReg::theil_sen_regression(y~x, data = data),
                              "robslopes" = robslopes::TheilSen(x, y, verbose =  FALSE),
                              setup = {x = rnorm(n); y = rnorm(n);
                              data = as.data.frame(cbind(x, y))}, times = 100)
        result[[i]] <- mbm
}

# leave out the mblm implementation for n = 10^4:
n   <- 10^4
mbm <- microbenchmark("deming" = deming::theilsen(y~x, data = data),
                      "zyp" = zyp::zyp.sen(y~x, dataframe = data),
                      "RobustLinearReg" = RobustLinearReg::theil_sen_regression(y~x, data = data),
                      "robslopes" = robslopes::TheilSen(x, y, verbose =  FALSE),
                      setup = {x = rnorm(n); y = rnorm(n);
                      data = as.data.frame(cbind(x, y))}, times = 100)
result[[4]] <- mbm

# plot the resulting computation times:
autoplot(result[[1]])
autoplot(result[[2]])
autoplot(result[[3]])
autoplot(result[[4]])


# For the repeated median estimator

m <- 100
nvals <- c(10, 10^2, 10^3)

result = list()
for (i in 1:length(nvals)) {
        set.seed(i)
        n = nvals[i]
        mbm = microbenchmark("mblm" = mblm::mblm(y~x, dataframe = data, repeated = TRUE),
                             "RobustLinearReg" = RobustLinearReg::siegel_regression(y~x, data = data),
                             "robslopes" = robslopes::RepeatedMedian(x, y, FALSE),
                             setup = {x = rnorm(n); y = rnorm(n);
                             data = as.data.frame(cbind(x, y))}, times = m)
        result[[i]] = mbm
}

# leave out the mblm implementation for n = 10^4:
n   <- 10^4
mbm <- microbenchmark("RobustLinearReg" = RobustLinearReg::siegel_regression(y~x, data = data),
                     "robslopes" = robslopes::RepeatedMedian(x, y, FALSE),
                     setup = {x = rnorm(n); y = rnorm(n);
                     data = as.data.frame(cbind(x, y))}, times = m)
result[[4]] <- mbm

# plot the resulting computation times:
autoplot(result[[1]])
autoplot(result[[2]])
autoplot(result[[3]])
autoplot(result[[4]])
