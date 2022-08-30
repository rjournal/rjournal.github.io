############################
# German Breast Cancer Study
############################

library(condSURV)
library(clustcurv)

data(gbcsCS)
head(gbcsCS[, c(5:10, 13, 14)])
table(gbcsCS$nodes)
gbcsCS[gbcsCS$nodes > 13,'nodes'] <- 14
gbcsCS$nodes <- factor(gbcsCS$nodes)

levels(gbcsCS$nodes)[14]<- '>13'
table(gbcsCS$nodes)
dim(gbcsCS)
# Survival Model

fit.gbcs <- survclustcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, 
                            x = gbcsCS$nodes, nboot = 500, seed = 300716, 
                            algorithm = 'kmedians', cluster = TRUE)

summary(fit.gbcs)

# Plot

#autoplot(fit.gbcs, groups_by_colour = FALSE)

pdf("breast_cancer_kmedians.pdf", 12, 8)
autoplot(fit.gbcs, groups_by_colour = TRUE)
dev.off()

pdf("breast_cancer_kmedians_cen.pdf", 12, 8)
autoplot(fit.gbcs, groups_by_colour = TRUE, centers = TRUE)
dev.off()


# Survival Model
fit.gbcs2 <- survclustcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, 
               x = gbcsCS$nodes, algorithm = 'kmeans', seed = 300716, 
               cluster = TRUE, nboot = 500)


fit.gbcs2



# Plot
pdf("breast_cancer_kmeans.pdf", 12, 8)
autoplot(fit.gbcs2, groups_by_colour = TRUE)
dev.off()

# Plot
pdf("breast_cancer_kmeans_cen.pdf", 12, 8)
autoplot(fit.gbcs2, groups_by_colour = TRUE, centers = TRUE)
dev.off()



# Survival Model with k = 3
ksurvcurves(time = gbcsCS$rectime, status = gbcsCS$censrec, x = gbcsCS$nodes,
            seed = 300716, algorithm = 'kmedians', k = 3)


############################
# Multiple Myeloma data
############################

library(survminer)
data(myeloma)
head(myeloma[,1:5])
dim(myeloma)
# Survival Analysis

fit.mye <- survclustcurves(time = myeloma$time, status = myeloma$event, 
                x = myeloma$molecular_group, nboot = 500, seed = 300716, 
                algorithm = 'kmedians', cluster = TRUE)

summary(fit.mye)


fit.mye2 <- survclustcurves(time = myeloma$time, status = myeloma$event, 
                x = myeloma$molecular_group, nboot = 500, seed = 300716, 
                algorithm = 'kmeans', cluster = TRUE)

summary(fit.mye2)


# Plot
pdf("myeloma_cancer_kmedians.pdf", 12, 8)
autoplot(fit.mye, groups_by_colour = TRUE)
dev.off()


# Plot
pdf("myeloma_cancer_kmedians_cen.pdf", 12, 8)
autoplot(fit.mye, groups_by_colour = TRUE, centers = TRUE)
dev.off()

############################
# Example Barnacle data
############################
data("barnacle5")
head(barnacle5)


# Regression Model

fit.bar <- regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
              nboot = 500, seed = 300716, algorithm = 'kmeans', cluster = TRUE)

summary(fit.bar)


# Plot
pdf("barnacle5_kmeans.pdf", 12, 8)
autoplot(fit.bar, groups_by_colour = TRUE)
dev.off()


# Plot
pdf("barnacle5_kmeans_cen.pdf", 12, 8)
autoplot(fit.bar, groups_by_colour = TRUE, centers = TRUE)
dev.off()



#####################
# Simulated data
#####################

m <- function(x, j) {
  y <- numeric(length(x))
  y[j <= 5] <- x[j <= 5] + 1
  y[j > 5 & j <= 10] <- x[j > 5 & j <= 10] ^ 2 + 1
  y[j > 10 & j <= 15] <- 2 * sin(2 * x[j > 10 & j <= 15]) #- 4
  y[j > 15 & j <= 20] <- 2 * sin(x[j > 15 & j <= 20])
  y[j > 20 & j <= 25] <- 2 * sin(x[j > 20 & j <= 25]) + a * exp(x[j > 20 & j <= 25])
  y[j > 25] <- 1
  return(y)
}






seed <- 300716
set.seed(seed)
n <- 5000
a <- 0.0
x <- runif(n, -2, 2)
prob <- sample(c(1, 1.5, 2, 2.5, 3), 30, replace = TRUE)
prob <- prob/sum(prob)
f <- sample(1:30, n, replace = TRUE, prob = prob)
N <- length(unique(f))
error <- rnorm(n,0,1.5)
y <- m(x, f) + (0.5 + 0.05 * m(x, f)) * error # heteroscedastic
data <- data.frame(x, y, f)





# Regression Model

fit.sim <- regclustcurves(x = data$x, y = data$y, z = data$f, nboot = 500, 
                          seed = 300716, algorithm = 'kmedians', cluster = TRUE)

fit.sim

# Plot


pdf("sim_5kmeans.pdf", 12, 8)
autoplot(fit.sim, groups_by_colour = TRUE, centers = TRUE)
dev.off()




seed <- 300716
set.seed(seed)
n <- 5000
a <- 0.4
x <- runif(n, -2, 2)
prob <- sample(c(1, 1.5, 2, 2.5, 3), 30, replace = TRUE)
prob <- prob/sum(prob)
f <- sample(1:30, n, replace = TRUE, prob = prob)
N <- length(unique(f))
error <- rnorm(n,0,1.5)
y <- m(x, f) + (0.5 + 0.05 * m(x, f)) * error # heteros
data2 <- data.frame(x, y, f)


# Regression Model

fit.sim2 <- regclustcurves(x = data2$x, y = data2$y,  z = data$f, nboot = 500, 
                          seed = 300716, algorithm = 'kmedians', cluster = TRUE)
fit.sim2

pdf("sim_6kmeans.pdf", 12, 8)
autoplot(fit.sim2, groups_by_colour = TRUE, centers = TRUE)
dev.off()



