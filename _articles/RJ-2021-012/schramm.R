# Before using the package for the first time, it should be installed as well as the following R packages: CompQuadForm, expm and DEoptim
setwd("D:/PostDoc_Canada/KSPM/soumissionRjournal")
# Install dependencies
install.packages("expm")
install.packages("CompQuadForm")
install.packages("DEoptim")

# Install KSPM package
install.packages("KSPM")

# Load the package
library(KSPM)

# R script following the article examples
# Of note, running time for these models may be long.

# CSM example
data(csm)
head(csm)

# for the first model, rho may be estimated by the model directly but it takes more time (if you want to test it, simply remove rho = 61.22 in the following code)
csm.fit1 <- kspm(response = "Ratings", kernel = ~Kernel(~Gross + Budget + Screens + Sequel, kernel.function = "gaussian", rho = 61.22), data = csm)
summary(csm.fit1)

# diagnostic plots
par(mfrow = c(2,2), mar = c(5, 5, 5, 2))
plot(csm.fit1, which = c(1, 3, 5), cex.lab = 1.5, cex = 1.3, id.n = 7)
hist(csm$Ratings, cex.lab = 1.5, main = "Histogram of Ratings", xlab = "Ratings")


# derivatives plots
par(mfrow = c(1,2), mar = c(5,5,5,2))
plot(derivatives(csm.fit1), subset = "Gross", cex.lab = 1.5, cex = 1.3, main = "Pointwise derivatives according to Gross Income")
plot(derivatives(csm.fit1), subset = "Screens", col = csm$Sequel, cex.lab = 1.5, cex = 1.3, pch = 16, main = "Pointwise derivatives according to Number of Screens \n and Sequel")
legend("topleft", fill = palette()[1:7], legend = 1:7, title = "Sequel", horiz = TRUE)

# Second model for AIC comparison
csm.fit2 <- kspm(response = "Ratings", kernel = ~Kernel(~Gross + Budget + Screens + Sequel, kernel.function = "polynomial", rho = 1, gamma = 1, d = 2), data = csm, level = 0)
extractAIC(csm.fit1)
extractAIC(csm.fit2)

# Model with Two kernels

# The commented code may be long to run
# csm.fit3 <- kspm(response = "Ratings", linear = NULL, kernel = ~Kernel(~ Gross + Budget + Screens + Sequel, kernel.function = "gaussian", rho = 61.22) * Kernel(~ Sentiment + Views + Likes + Dislikes + Comments + Aggregate.Followers, kernel.function = "gaussian", rho = 1.562652), data = csm)
# summary(csm.fit3, kernel.test = "Ker1:Ker2", global.test = TRUE)

# you may load instead
load("summary.csm.fit3")
summary.csm.fit3

# prediction for new points
newdata.Ker1 <- data.frame(Genre = c(1, 3, 8), Gross = c(5.0e+07, 50000, 10000),Budget = c(1.8e+08, 5.2e+05, 1.3e+03), Screens = c(3600, 210, 5050), Sequel = c(2, 1, 1))
newdata.Ker2 <- data.frame(Sentiment = c(1, 2, 10), Views = c(293021, 7206, 5692061), Likes = c(3698, 2047, 5025), Dislikes = c(768, 49, 305), Comments = c(336, 70, 150), Aggregate.Followers = c(4530000, 350000, 960000))

# prediction
# new.predictions <- predict(csm.fit3, newdata.kernel = list(Ker1 = newdata.Ker1, Ker2 = newdata.Ker2), interval = "prediction")
load("new.predictions")
new.predictions

# prediction for current points
# pred <- predict(csm.fit3, interval = "confidence")
load("pred")
# predicted versus fitted data
plot(csm$Ratings, pred$fit, xlim = c(2, 10), ylim = c(2, 10), xlab = "Observed ratings", ylab = "Predicted ratings", cex.lab = 1.5)
abline(a = 0, b = 1, col = "red", lty = 2)

# Variable selection procedure, commented cause it may be long to run
# csm.fit4 <- kspm(response = "Ratings", linear = NULL, kernel = ~Kernel(~ Sentiment + Views + Likes + Dislikes + Comments + Aggregate.Followers, kernel.function = "gaussian"), data = csm, level = 0)
# stepKSPM(csm.fit4, kernel.lower = ~1, kernel.upper = ~ Sentiment + Views + Likes + Dislikes + Comments + Aggregate.Followers, direction = "both", k = 2, kernel.param = "change", data = csm)


# ENERGY example
data("energy")
head(energy)

par(mfrow = c(1,2), mar = c(5,5,2,2))
# energy among all the measurements
plot(energy$power, type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n")
axis(1, at = 1 + 24 * (0:21), labels = unique(energy$date))
# examples from three days
plot(c(NA,energy[1:26, "power"]), type = "b", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", col = "darkgreen", lwd = 2, cex = 0.8, xlim = c(-1, 30))
lines(energy[24:50, "power"], type = "b", col = "blue", lwd = 2, cex = 0.8, pch = 0)
lines(energy[48:74, "power"], type = "b", col = "red", lwd = 2, cex = 1, pch = 17)
axis(1, at = c(1, 7, 13, 19, 25), labels = c("0h00", "6h00", "12h00", "18h00", "0h00"))
legend("topleft", col = c("darkgreen", "blue", "red"), legend = c("Sep 13, 2015", "Sep 14, 2015", "Sep 15, 2015"), lwd = 2, pch = c(1, 0, 17))
abline(v = 24.9, lty = 2)
text(25.5, 730, "next day", adj = 0)
text(25.5, 350, "next day", adj = 0)


energy_train_ <- energy[1:408, ]
energy_test_ <- energy[409:504, ]

# first model
energy.fit1 <- kspm(response = "power", linear = ~T, kernel = ~Kernel(~hour.num + P + HR, kernel.function = "gaussian", rho = 0.7) , data = energy_train_)

# models with other value of rho parameter
energy.fit2 <- kspm(response = "power", linear = ~T, kernel = ~Kernel(~hour.num + P + HR, kernel.function = "gaussian", rho = 0.07) , data = energy_train_, level = 0)
energy.fit3 <- kspm(response = "power", linear = ~T, kernel = ~Kernel(~hour.num + P + HR, kernel.function = "gaussian", rho = 7) , data = energy_train_, level = 0)

# Graphics comparing the three models
### parameters for figures panel
par(oma = c(1, 4, 6, 1))
par(mfrow = c(4,3), mar = c(5,5,1,1))

### kspm.fit1 (rho = 0.7)
# predictions with confidence intervals on train_
plot(energy_train_[1:72, "power"], type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", lwd = 2, ylim = c(300, 750))
axis(1, at = 1 + 24 * (0:2), labels = unique(energy_train_$date)[1:3])
pred_train_ <- predict(energy.fit1, interval = "confidence")
lines(pred_train_$fit, col = "red")
lines(pred_train_$lwr, col = "blue", lty = 2)
lines(pred_train_$upr, col = "blue", lty = 2)
# predictions with prediction intervals on test_
plot(energy_test_[1:72, "power"], type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", lwd = 2, ylim = c(300, 750))
axis(1, at = 1 + 24 * (0:2), labels = unique(energy_test_$date)[1:3])
pred_train_ <- predict(energy.fit1, newdata.linear =  energy_test_, newdata.kernel = list(Ker1 = energy_test_), interval = "prediction")
lines(pred_train_$fit, col = "red")
lines(pred_train_$lwr, col = "blue", lty = 2)
lines(pred_train_$upr, col = "blue", lty = 2)
# derivatives
plot(derivatives(energy.fit1), subset = "hour.num", xaxt = "n", ylab = "Derivatives", cex.lab = 1.5, ylim = c(-1000,1000))
axis(1, at = c(0, 6, 12, 18), labels = c("0h00", "6h00", "12h00", "18h00"))

### kspm.fit3 (rho = 0.07)
# predictions with confidence intervals on train_
plot(energy_train_[1:72, "power"], type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", lwd = 2, ylim = c(300, 750))
axis(1, at = 1 + 24 * (0:2), labels = unique(energy_train_$date)[1:3])
pred_train_ <- predict(energy.fit2, interval = "confidence")
lines(pred_train_$fit, col = "red")
lines(pred_train_$lwr, col = "blue", lty = 2)
lines(pred_train_$upr, col = "blue", lty = 2)
# predictions with prediction intervals on test_
plot(energy_test_[1:72, "power"], type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", lwd = 2, ylim = c(300, 750))
axis(1, at = 1 + 24 * (0:2), labels = unique(energy_test_$date)[1:3])
pred_train_ <- predict(energy.fit2, newdata.linear =  energy_test_, newdata.kernel = list(Ker1 = energy_test_), interval = "prediction")
lines(pred_train_$fit, col = "red")
lines(pred_train_$lwr, col = "blue", lty = 2)
lines(pred_train_$upr, col = "blue", lty = 2)
# derivatives
plot(derivatives(energy.fit2), subset = "hour.num", xaxt = "n", ylab = "Derivatives", cex.lab = 1.5, ylim = c(-1000,1000))
axis(1, at = c(0, 6, 12, 18), labels = c("0h00", "6h00", "12h00", "18h00"))

### kspm.fit2 (rho = 7)
# predictions with confidence intervals on train_
plot(energy_train_[1:72, "power"], type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", lwd = 2, ylim = c(300, 750))
axis(1, at = 1 + 24 * (0:2), labels = unique(energy_train_$date)[1:3])
pred_train_ <- predict(energy.fit3, interval = "confidence")
lines(pred_train_$fit, col = "red")
lines(pred_train_$lwr, col = "blue", lty = 2)
lines(pred_train_$upr, col = "blue", lty = 2)
# predictions with prediction intervals on test_
plot(energy_test_[1:72, "power"], type = "l", xlab = "Time", ylab = "Power", cex.lab = 1.5, xaxt = "n", lwd = 2, ylim = c(300, 750))
axis(1, at = 1 + 24 * (0:2), labels = unique(energy_test_$date)[1:3])
pred_train_ <- predict(energy.fit3, newdata.linear =  energy_test_, newdata.kernel = list(Ker1 = energy_test_), interval = "prediction")
lines(pred_train_$fit, col = "red")
lines(pred_train_$lwr, col = "blue", lty = 2)
lines(pred_train_$upr, col = "blue", lty = 2)
# derivatives
plot(derivatives(energy.fit3), subset = "hour.num", xaxt = "n", ylab = "Derivatives", cex.lab = 1.5, ylim = c(-1000,1000))
axis(1, at = c(0, 6, 12, 18), labels = c("0h00", "6h00", "12h00", "18h00"))

# Legends
plot.new()
legend("topleft", lty = c(1,1,2), col = c("black", "red", "blue"), legend = c("True data", "Predictions", "Confidence intervals"), cex = 2, bty = "n")
plot.new()
legend("topleft", lty = c(1,1,2), col = c("black", "red", "blue"), legend = c("True data", "Predictions", "Prediction intervals"), cex = 2,  bty = "n")
plot.new()
legend("topleft", pch = 1, col = c("black"), legend = c("Pointwise derivatives \n 1 point = 1 measure"), cex = 2, bty = "n")


### legends on the left
par(fig = c(0,0.05,0,0.25), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.1, 0.8, "Legend", srt = 90, cex = 2, adj = 0.5)
par(fig = c(0,0.05,0.26,0.5), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.1, 0.5, expression(paste(rho, " = 7")), srt = 90, cex = 2, adj = 0.5)
par(fig = c(0,0.05,0.5,0.72), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.1, 0.5, expression(paste(rho, " = 0.07")), srt = 90, cex = 2, adj = 0.5)
par(fig = c(0,0.05,0.72,0.97), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.1, 0.5, expression(paste(rho, " = 0.7")), srt = 90, cex = 2, adj = 0.5)

### legends on the top
par(fig = c(0.05,0.36,0.92,1), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.5, 0.5, "Predictions on training data set",  cex = 2, adj = 0.5)
par(fig = c(0.37,0.68,0.92,1), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.5, 0.5, "Predictions on test data set",  cex = 2, adj = 0.5)
par(fig = c(0.69,1,0.92,1), oma = c(0,0,0,0), mar = c(0,0,0,0),new = TRUE)
plot.new()
text(0.5, 0.5, "Derivatives for hour.num",  cex = 2, adj = 0.5)


# Gene-Gene interaction example

# Load data 
load("y")
load("geneA")
load("geneB")

# the model (because of the three kernels: geneA, geneB and interaction, it takes time to run)
# gene.fit <- kspm(response = y, kernel = ~ Kernel(geneA, kernel = "linear") * Kernel(geneB, kernel = "linear"))
# summary.gene.fit <- summary(gene.fit, kernel.test = "Ker1:Ker2")

# you can load instead
load("summary_gene_fit")
summary.gene.fit

