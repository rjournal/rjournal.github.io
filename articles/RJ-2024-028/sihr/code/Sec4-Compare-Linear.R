# Load required libraries
library(SIHR)
library(MASS)
library(ggplot2)
library(ggpubr)
library(AER)
library(Matrix)
library(flare)
library(hdi)

# Source additional required R scripts
source("sources/hdi.R")
source("sources/SSLasso.R")

# Set simulation parameters
nsim <- 500
n <- 200    # Sample sizes to be tested: 200, 400
p <- 300
mu <- rep(0, p)
Cov <- diag(p)
beta <- rep(0, p); beta[c(1, 2, 3)] <- c(0.5, 0.75, 0.25)
loading <- c(1, 0.75, 0.5, 0.25, rep(0, p - 4))
true <- sum(loading * beta)

# Initialize matrices for results
Result.Mat_SIHR <- matrix(NA, nrow = nsim, ncol = 5)
colnames(Result.Mat_SIHR) <- c('est.debias', 'len', 'cov', 'est.plug', 'time')
Result.Mat_hdi <- matrix(NA, nrow = nsim, ncol = 4)
colnames(Result.Mat_hdi) <- c('est.debias', 'len', 'cov', 'time')
Result.Mat_ss <- matrix(NA, nrow = nsim, ncol = 4)
colnames(Result.Mat_ss) <- c('est.debias', 'len', 'cov', 'time')

# Simulation loop
for(i in 1:nsim){
  X <- mvrnorm(n, mu, Cov)
  y <- X%*%beta + rnorm(n)
  
  # SIHR analysis
  start.our <- Sys.time()
  Est<- SIHR::LF(X, y, loading.mat = loading, model = "linear", rescale = 1)
  end.our <- Sys.time()
  prop.point <- Est$est.debias.vec   
  prop.se <- Est$se.vec            
  prop.CI <- prop.point + c(-1, 1) * qnorm(0.975) * prop.se
  Coverage <- (prop.CI[1] < true) * (prop.CI[2] > true)
  
  Result.Mat_SIHR[i,1] <- Est$est.debias.vec
  Result.Mat_SIHR[i,2] <- prop.CI[2]-prop.CI[1]
  Result.Mat_SIHR[i,3] <- Coverage
  Result.Mat_SIHR[i,4] <- Est$est.plugin.vec
  Result.Mat_SIHR[i,5] <- end.our - start.our
  
  # High-Dimensional Inference
  start.hdi <- Sys.time()
  Est_deb<-deb.hdi(X, y, loading = loading, model = "linear")
  end.hdi <- Sys.time()
  prop.point <- Est_deb$hdi.est   
  prop.CI <- Est_deb$hdi.CI
  Coverage <- (prop.CI[1] < true) * (prop.CI[2] > true)
  
  Result.Mat_hdi[i,1] <- prop.point
  Result.Mat_hdi[i,2] <- prop.CI[2]-prop.CI[1]
  Result.Mat_hdi[i,3] <- Coverage
  Result.Mat_hdi[i,4] <- end.hdi - start.hdi
  
  # Sparse Signal Lasso
  start.ss <- Sys.time()
  DeLasso<-SSLasso(X, y)
  end.ss <- Sys.time()
  coef.est <- DeLasso$unb.coef
  Cov.est <- DeLasso$Cov
  prop.point <- sum(loading * coef.est[-1])
  prop.se <- sqrt(t(loading) %*% Cov.est[-1, -1] %*% loading)
  prop.CI <- prop.point + c(-1, 1) * qnorm(0.975) * prop.se
  Coverage <- (prop.CI[1] < true) * (prop.CI[2] > true)

  Result.Mat_ss[i,1] <- prop.point
  Result.Mat_ss[i,2] <- prop.CI[2]-prop.CI[1]
  Result.Mat_ss[i,3] <- Coverage
  Result.Mat_ss[i,4] <- end.ss - start.ss
}

# Bias for each method
bias_SIHR <- mean(Result.Mat_SIHR[,1]) - true
bias_plug <- mean(Result.Mat_SIHR[,4]) - true
bias_hdi <- mean(Result.Mat_hdi[,1]) - true
bias_ss <- mean(Result.Mat_ss[,1]) - true

# Visualization: Effectiveness of bias correction ----------------------------------------

# Create a data frame with the results for both SIHR and Plug-in Lasso
dat <- data.frame(xx = c(Result.Mat_SIHR[, 1], Result.Mat_SIHR[, 4]), 
                  yy = rep(c("SIHR", "Plug-in Lasso"), each = 500))

# Plot for SIHR
p1 <- ggplot(subset(dat, yy == 'SIHR'), aes(x = xx)) +
  geom_histogram(color = 'white', fill = "#377EB8", alpha = 0.4, bins = 20) +
  geom_vline(aes(xintercept = mean(xx)), color = "#377EB8", linewidth = 1) +
  geom_vline(xintercept = true, color = "#E41A1C", linewidth = 1.5) +
  xlim(c(0.15, 1.8)) +
  labs(x = '', y = 'Frequency', title = 'SIHR') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13.5, face = "bold", color = "#377EB8"),
        axis.title.x = element_blank()) +
  geom_segment(x = mean(subset(dat, yy == 'SIHR')[, 1]), y = 70, 
               xend = true, yend = 70, color = "#F8766D",
               arrow = arrow(length = unit(0.15, 'inches'), ends = 'both', type = 'closed')) +
  geom_text(data = data.frame(x = true - 0.3, y = 70, label = paste("Bias", round(bias_ss, 3))), 
            aes(x = x, y = y, label = label), hjust = 0.5, vjust = 0, angle = 0, 
            size = 4, fontface = 'bold', color = "#F8766D", inherit.aes = FALSE)

# Plot for Plug-in Lasso
p2 <- ggplot(subset(dat, yy == 'Plug-in Lasso'), aes(x = xx)) +
  geom_histogram(color = "white", fill = "#4DAF4A", alpha = 0.4, bins = 20) +
  geom_vline(aes(xintercept = mean(xx)), color = "#4DAF4A", linewidth = 1) +
  geom_vline(xintercept = true, color = "#E41A1C", linewidth = 1.5) +
  xlim(c(0.15, 1.8)) +
  labs(x = '', y = 'Frequency', title = 'Plug-in Lasso') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13.5, face = "bold", color = "#4DAF4A"),
        axis.title.x = element_blank()) +
  geom_segment(x = mean(subset(dat, yy == 'Plug-in Lasso')[, 1]), y = 70, 
               xend = true, yend = 70, color = "#F8766D",
               arrow = arrow(length = unit(0.15, 'inches'), ends = 'both', type = 'closed')) +
  geom_text(data = data.frame(x = (mean(subset(dat, yy == 'Plug-in Lasso')[, 1]) + true) / 2, y = 72, 
                              label = paste("Bias", round(bias_plug, 3))), mapping = aes(x = x, y = y, label = label), hjust = 0.5, 
            vjust = 0, angle = 0, size = 4, fontface = 'bold', color = "#F8766D", inherit.aes = FALSE)

# Arrange both plots vertically
ggarrange(p1, p2, nrow = 2)







