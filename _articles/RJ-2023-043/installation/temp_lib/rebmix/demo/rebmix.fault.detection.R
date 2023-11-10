##############################################
## R sources for reproducing the results in ##
##   Marko Nagode, Branislav Panic,         ##
##   Jernej Klemenc, Simon Oman:            ##
##   Fault Detection and Classification     ##
##   with the rebmix R Package              ##
##############################################

options(prompt = "> ", continue = "+ ", width = 70,
  useFancyQuotes = FALSE, digits = 3)

library(rebmix)
library(e1071)
library(FNN)
library(MASS)

data(bearings)
data(steelplates)
data(sensorlessdrive)

bearings <- bearings[, c(1, 5, 6, 9, 10, 11, 14)]
steelplates <- steelplates[, c(2, 9, 17, 19, 24, 28)]

# Data normalization.

range <- apply(bearings, 2, range)

for (i in 1:(ncol(range) - 1)) {
  bearings[, i] <- (bearings[, i] - range[1, i]) / (range[2, i] - range[1, i])
}

range <- apply(steelplates, 2, range)

for (i in 1:(ncol(range) - 1)) {
  steelplates[, i] <- (steelplates[, i] - range[1, i]) / (range[2, i] - range[1, i])
}

range <- apply(sensorlessdrive, 2, range)

for (i in 1:(ncol(range) - 1)) {
  sensorlessdrive[, i] <- (sensorlessdrive[, i] - range[1, i]) / (range[2, i] - range[1, i])
}

# Data split into train and test datasets.

set.seed(1)

Bearings <- split(p = 0.6, Dataset = bearings, class = 7)
Steelplates <- split(p = 0.6, Dataset = steelplates, class = 6)
Sensorlessdrive <- split(p = 0.6, Dataset = sensorlessdrive, class = 4)

########## rebmix ##########

# Bearings dataset.

EM <- new("EM.Control", strategy = "exhaustive", variant = "ECM")

system.time({

bearings.model <- REBMIX(model = "REBMVNORM", 
  Dataset = Bearings@train, 
  Preprocessing  = "k-nearest neighbour",
  Criterion = "AIC",
  EMcontrol = EM)

bearings.class <- RCLSMIX(model = "RCLSMVNORM", 
  x = list(bearings.model), 
  Dataset = Bearings@test, 
  Zt = Bearings@Zt)

})

bearings.class

plot(bearings.class, nrow = 5, ncol = 3)

# Steelplates dataset.

a.strategy(EM) <- "best"
a.variant(EM) <- "EM"

system.time({

steelplates.model <- REBMIX(model = "REBMVNORM", 
  Dataset = Steelplates@train, 
  Preprocessing  = "histogram",
  Criterion = "BIC",
  K = 2:100,
  EMcontrol = EM)

steelplates.class <- RCLSMIX(model = "RCLSMVNORM", 
  x = list(steelplates.model), 
  Dataset = Steelplates@test, 
  Zt = Steelplates@Zt)

})

steelplates.class

plot(steelplates.class, nrow = 2, ncol = 5)

# Sensorlessdrive dataset.

a.strategy(EM) <- "single"

system.time({

bins <- optbins(Dataset = Sensorlessdrive@train, Rule = "Knuth equal", kmin = 2, kmax = 100)

sensorlessdrive.model <- REBMIX(model = "REBMIX", 
  Dataset = Sensorlessdrive@train, 
  Preprocessing  = "histogram",
  pdf = rep("normal", 3), 
  K = bins, 
  EMcontrol = EM)

sensorlessdrive.class <- RCLSMIX(model = "RCLSMIX", 
  x = list(sensorlessdrive.model), 
  Dataset = Sensorlessdrive@test, 
  Zt = Sensorlessdrive@Zt)
})

sensorlessdrive.class

plot(sensorlessdrive.class, nrow = 3, ncol = 1)

# Bearings data preparation.

train <- NULL; Zr <- NULL

for (i in 1:length(Bearings@ntrain)) {
  Zr <- c(Zr, Bearings@Zr[[i]])
  train <- rbind(train, Bearings@train[[i]])
}

rownames(train) <- NULL

Zr <- as.factor(Zr)

test <- Bearings@test; Zt <- as.numeric(Bearings@Zt)

rownames(test) <- NULL

########## svm ##########

system.time({

model <- svm(x = train, y = Zr)

Zp <- predict(model, test)

})

Zp <- as.numeric(Zp)

Error <- 1.0 - sum(Zt == Zp) / length(Zt)

Error

########## knn ##########

system.time({

knn <- knn(train = train, test = test, cl = Zr, k = 10)

Zp <- knn[1:nrow(test)]

})

Zp <- as.numeric(as.vector(Zp))

Error <- 1.0 - sum(Zt == Zp) / length(Zp)

Error

########## lda ##########

system.time({

lda <- lda(train, Zr)

Zp <- predict(lda, test)$class

})

Zp <- as.numeric(Zp)

Error <- 1.0 - sum(Zt == Zp) / length(Zt)

Error

# Steelplates data preparation.

train <- NULL; Zr <- NULL

for (i in 1:length(Steelplates@ntrain)) {
  Zr <- c(Zr, Steelplates@Zr[[i]])
  train <- rbind(train, Steelplates@train[[i]])
}

rownames(train) <- NULL

Zr <- as.factor(Zr)

test <- Steelplates@test; Zt <- as.numeric(Steelplates@Zt)

rownames(test) <- NULL

########## svm ##########

system.time({

model <- svm(x = train, y = Zr)

Zp <- predict(model, test)

})

Zp <- as.numeric(Zp)

Error <- 1.0 - sum(Zt == Zp) / length(Zt)

Error

########## knn ##########

system.time({

knn <- knn(train = train, test = test, cl = Zr, k = 10)

Zp <- knn[1:nrow(test)]

})

Zp <- as.numeric(as.vector(Zp))

Error <- 1.0 - sum(Zt == Zp) / length(Zp)

Error

########## lda ##########

system.time({

lda <- lda(train, Zr)

Zp <- predict(lda, test)$class

})

Zp <- as.numeric(Zp)

Error <- 1.0 - sum(Zt == Zp) / length(Zt)

Error

# Sensorlessdrive data preparation.

train <- NULL; Zr <- NULL

for (i in 1:length(Sensorlessdrive@ntrain)) {
  Zr <- c(Zr, Sensorlessdrive@Zr[[i]])
  train <- rbind(train, Sensorlessdrive@train[[i]])
}

rownames(train) <- NULL

Zr <- as.factor(Zr)

test <- Sensorlessdrive@test; Zt <- as.numeric(Sensorlessdrive@Zt)

rownames(test) <- NULL

########## svm ##########

system.time({

model <- svm(x = train, y = Zr)

Zp <- predict(model, test)

})

Zp <- as.numeric(Zp)

Error <- 1.0 - sum(Zt == Zp) / length(Zt)

Error

########## knn ##########

system.time({

knn <- knn(train = train, test = test, cl = Zr, k = 10)

Zp <- knn[1:nrow(test)]

})

Zp <- as.numeric(as.vector(Zp))

Error <- 1.0 - sum(Zt == Zp) / length(Zp)

Error

########## lda ##########

system.time({

lda <- lda(train, Zr)

Zp <- predict(lda, test)$class

})

Zp <- as.numeric(Zp)

Error <- 1.0 - sum(Zt == Zp) / length(Zt)

Error
