library(ggplot2)
library(MASS)
library(clustrd)
library(biplotbootGUI)
library(drclust)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork)
library(forcats)
library(RColorBrewer)
library(grid)
set.seed(92)  
# defining two custom-functions needed for the computations

# Function bsmatrix(): from clustering vector to a membership matrix:
bsmatrix = function(cluster){
  K = max(cluster)
  n = length(cluster)
  id = rep(1, K)
  I = diag(id)
  Uc = matrix(0, nrow = n, ncol = K)
  for(i in 1:n){
    Uc[i,] = I[cluster[i],]
  }
  return(U = Uc)
}

# Function MixSampling(): Generates synthetic data with 2-dimensional clustering structure + 4-dimensional noise 
MixSampling = function(n, Pr, su, sc, seed){
  set.seed(seed)
  # e.g. MixSampling(1000, c(0.2, 0.3, 0.5), 0.3, 0.01)
  ## Input:
  # n: nr. of objects
  # Pr: vector of a priori probabilities (K x 1)
  # su: standard deviation among units
  # sc: standard deviation of equidistant centroids
  
  ## Output:
  # X: data matrix (n x K-1)
  # U: membership matrix
  # v: categorical clustering variable
  
  # K number of cluster
  K = length(Pr)
  
  # Xm: K equidistant centroid matrix
  D = 6*(matrix(1,K,K)-diag(rep(1,K)))
  Jc = diag(rep(1,K)) - (1/K)*(matrix(1,K,K))
  cDc = -0.5*Jc%*%D%*%Jc
  eigs = eigen(cDc)
  B = diag(eigs$values[1:K-1])
  A = eigs$vectors[,1:K-1]
  # centroid matrix
  Xm = A%*%sqrt(B) # falla andare
  
  # Centroid matrix
  mu = t(rep(0, K-1))
  Sigma = sc*diag(rep(1, K-1))
  E = mvrnorm(n = K, mu = mu, Sigma = Sigma)
  
  # add error to the centroids (modify isolation)
  Xmp = Xm+E
  # variance and covariance matrix
  Sigma0 = cov(mvrnorm(n = 100, mu = mu, Sigma = diag(rep(1,K-1)))*su)*(99/100)
  U = t(rmultinom(n = n, prob = Pr, size = 1))
  u = U%*%c(1:K)
  srt = sort(u,)
  us <- u[order(u)]
  ius <- order(u)
  U = U[ius,]
  X = matrix(nrow = n, ncol = 2)
  for(i in 1:n){
    X[i,] = mvrnorm(mu = Xmp[us[i],], Sigma = Sigma0, n = 1) # falla andare
  }
  Y = mvrnorm(n = n, mu = c(0,0,0,0), Sigma = diag(c(1,1,1,1)))*6
  X = cbind(X, Y)
  colnames(X) <- c("x", "y", "n1", "n2", "n3", "n4")
  #p <- ggplot(data = as.data.frame(X[,1:2]), aes(x = x, y = y, color = factor(us))) +
  #  geom_point() +
  #  labs(x = "Dim1", y = "Dim2", title = "Simulated Data") +
  #  scale_color_discrete(name = "Cluster")
  #plot(p)
  
  return(list(X = X, U = U, us = us))
}
##### ------------------------- iris dataset
##  Testing the statistical methods implemented in drclust and comparing with the available alternatives.

# create a table where to allocate the results
IComp <- matrix(ncol = 5, nrow = 9) # iris comparison

# the following 5 parameters are reported for each technique
colnames(IComp) <- c("package", "technique", "runtime", "ARI", "fit")

# load the iris data
data(iris)
iris
irisn <- as.matrix(iris[,-5])
# true classification labels 
Utrue <- bsmatrix(as.numeric(factor(iris$Species)))
# test the RKM, FKM and CPDCA on drclust (RKM, FKM, CDPCA, ...), clustrd (RKM, FKM), biplotbootGUI (CDPCA)

### RKM
pkg <- "clustrd"
met <- "RKM"
runtime <- system.time({
  rkm_a <- clustrd::cluspca(irisn, 3, 2, alpha = 0.5, method = "RKM", nstart = 100)
})
# runtime
rntm <- runtime[3]
# U matrix
Urkm_rd <- bsmatrix(rkm_a$cluster)
# ARI
ari <- mrand(t(Urkm_rd)%*%Utrue)
# Fit
fit <- norm(Urkm_rd%*%rkm_a$centroid%*%t(rkm_a$attcoord), "F")
IComp[1,] <- c(pkg, met, rntm, ari, fit)

pkg <- "drclust"
## drclust
runtime <- system.time({
  rkm_b <- redkm(irisn, 3, 2, Rndstart = 100)
})
# runtime
rntm <- runtime[3]
# ARI
ari <- mrand(t(rkm_b$U)%*%Utrue)
# Fit
fit <- norm(rkm_b$U%*%rkm_b$centers%*%t(rkm_b$A), "F")
IComp[2,] <- c(pkg, met, rntm, ari, fit)

## 
met <- "FKM"
pkg <- "clustrd"

runtime <- system.time({
  fkm_a <- clustrd::cluspca(irisn, 3, 2, alpha = 0, method = "FKM", nstart = 100)
})
# runtime
rntm <- runtime[3]
# U matrix
Ufkm_rd <- bsmatrix(fkm_a$cluster)
# ARI
ari <- mrand(t(Ufkm_rd)%*%Utrue)
# Fit
fit <- norm(Ufkm_rd%*%fkm_a$centroid%*%t(fkm_a$attcoord), "F")
IComp[3,] <- c(pkg, met, rntm, ari, fit)


pkg <- "drclust"
runtime <- system.time({
  fkm_b <- factkm(irisn, 3, 2, Rndstart = 100)
})
# runtime
rntm <- runtime[3]
# ARI
ari <- mrand(t(fkm_b$U)%*%Utrue)
# Fit
fit <- norm(fkm_b$U%*%fkm_b$centers%*%t(fkm_b$A), "F")
IComp[4,] <- c(pkg, met, rntm, ari, fit)

pkg <- "biplotbootGUI"
met <- "DPCAKM"
runtime <- system.time({
  cdpca_a <- biplotbootGUI::CDpca(irisn, P = 3, Q = 2, tol = 1e-6, maxit = 100, r = 100, cdpcaplot = F)
})
# runtime
rntm <- runtime[3]
# Ari
ari <- mrand(t(cdpca_a$U)%*%Utrue)
# Fit
fit <- norm(cdpca_a$U%*%cdpca_a$Ybar%*%t(cdpca_a$A), "F")
IComp[5,] <- c(pkg, met, rntm, ari, fit)

pkg <- "drclust"

runtime <- system.time({
  cdpca_b <- dpcakm(irisn, 3, 2, tol = 1e-6, maxiter = 100, Rndstart = 100)
})
# runtime
rntm<- runtime[3]
# Ari
ari <- mrand(t(cdpca_b$U)%*%Utrue)
fit <- norm(cdpca_b$U%*%cdpca_b$centers%*%t(cdpca_b$A), "F")
IComp[6,] <- c(pkg, met, rntm, ari, fit)

met <- "DPCA"

runtime <- system.time({
  dpca <- dispca(irisn, 2, tol = 1e-6, maxiter = 100, Rndstart = 100)
})
rntm <- runtime[3]
fit <- norm(scale(irisn)%*%dpca$A%*%t(dpca$A), "F")
ari <- NA
IComp[7,] <- c(pkg, met, rntm, ari, fit)

met <- "DFA"
runtime <- system.time({
  dfa<- drclust::disfa(irisn, 2, tol = 1e-6, maxiter = 100, Rndstart = 100)
})
rntm <- runtime[3]
fit <- norm(scale(irisn)%*%dfa$A%*%t(dfa$A), "F")
ari <- NA
IComp[8,] <- c(pkg, met, rntm, ari, fit)

met <- "DKM"
runtime <- system.time({
  dkm <- drclust::doublekm(irisn, 3, 2, Rndstart = 100)
})
rntm <- runtime[3]
ari <- mrand(t(dkm$U)%*%Utrue)
fit <- norm(dkm$U%*%dkm$centers%*%t(dkm$V), "F")
IComp[9,] <- c(pkg, met, rntm, ari, fit)
IComp
IComp <- as.data.frame(IComp)
IComp[,3:5] <- apply(IComp[,3:5], 2, as.numeric)

custom_colors <- c(
  "DPCAKM (biplotbootGUI)" = "#e78ac3",
  "DPCAKM (drclust)"       = "#ffd92f",
  "DKM (drclust)"          = "#cab2d6",
  "FKM (clustrd)"          = "#66c2a5",
  "FKM (drclust)"          = "#fc8d62",
  "RKM (clustrd)"          = "#8da0cb",
  "RKM (drclust)"          = "#a6d854",
  "DPCA (drclust)"         = "#e5c494",
  "DFA (drclust)"          = "#b3b3b3"
)

IComp <- IComp %>%
  mutate(technique_label = paste0(technique, " (", package, ")"))

IComp$technique_label <- factor(IComp$technique_label,
                                levels = IComp %>%
                                  arrange(technique, package) %>%
                                  pull(technique_label) %>%
                                  unique())

metric_labels <- c(ARI = "ARI", fit = "Fit", runtime = "Runtime (s)")

IComp_long <- IComp %>%
  pivot_longer(cols = c(runtime, ARI, fit), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(metric = factor(metric, levels = names(metric_labels), labels = metric_labels))

ggplot(IComp_long, aes(x = technique_label, y = value, fill = technique_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 14)
  ) +
  labs(x = "Technique (library)", y = "Value", fill = "Technique")
## Simulation
# It takes much time, mainly due to the speed of clustrd
# The estimation of the models on the simulated datasets is parallelized


## ------------------------- 3 simulated datasets


# Simulations: 3 Scenarios: High, Medium and Low noise.
# Generated via function

seeds <- sample(1:1e6, 100, replace = FALSE)

High <- lapply(seeds, function(x) MixSampling(1000, c(0.5, 0.3, 0.2), 0.8, 0.01, x))
Medium <- lapply(seeds, function(x) MixSampling(1000, c(0.5, 0.3, 0.2), 0.55, 0.01, x))
Low <- lapply(seeds, function(x) MixSampling(1000, c(0.5, 0.3, 0.2),  0.3, 0.01, x))

names(High) <- paste0("High.", seq_along(High))
names(Medium) <- paste0("Medium.", seq_along(Medium))
names(Low) <- paste0("Low.", seq_along(Low))

Data <- c(High, Medium, Low)

# parameters reported for each technique
cnames <- c("With. Var..", "library", "technique", "runtime", "fit", "ari", "f*-f", "||A*-A||^2")

Noise <- c(rep("High", 100), rep("Medium", 100), rep("Low", 100))

X_sim <- lapply(Data, function(x) x$X)
Utrue_sim <-  lapply(Data, function(x) x$U)

require(doParallel)
cl <- makeCluster(6, outfile = "") # 
registerDoParallel(cl)

f <- function(i) {
  library(clustrd)
  library(drclust)
  library(biplotbootGUI)
  cat("Dataset nr.", i, "\n")
  
  # Atrue [Without Noise]
  Atrue <- matrix(0, nrow = 6, ncol = 2)
  Atrue[1:2,1:2] = diag(c(1,1))
  
  # Xbar
  Xbar_true <- solve(t(Utrue_sim[[i]])%*%Utrue_sim[[i]])%*%t(Utrue_sim[[i]])%*%X_sim[[i]]
  
  # Atrue with noise
  Xs = scale(X_sim[[i]])
  su = colSums(Utrue_sim[[i]])
  XX = t(Xs) %*% (Utrue_sim[[i]] %*% diag(1/su) %*% t(Utrue_sim[[i]])) %*% Xs
  ## 
  eigs = eigen(XX)
  A = as.matrix(eigs$vectors[,1:2])
  
  # Fit of the true model  
  fstar = norm(Utrue_sim[[i]] %*% solve(t(Utrue_sim[[i]])%*%Utrue_sim[[i]]) %*%t(Utrue_sim[[i]]) %*% scale(X_sim[[i]]) %*%A, "F")
  ### RKM
  
  pkg <- "clustrd"
  met <- "RKM"
  
  runtime <- system.time({
    rkm_clustrd <- clustrd::cluspca(X_sim[[i]], nclus =  3, ndim =  2, method = "RKM", nstart = 100)
  })
  rntm <- runtime[3]
  Urkm_rd <- bsmatrix(rkm_clustrd$cluster)
  ari <- mrand(t(Urkm_rd)%*%Utrue_sim[[i]])
  fit <- norm(Urkm_rd %*% solve(t(Urkm_rd)%*%Urkm_rd) %*%t(Urkm_rd) %*% scale(X_sim[[i]]) %*%rkm_clustrd$attcoord, "F")
  
  diff_f <- fstar - fit
  diff_A <- norm(A-rkm_clustrd$attcoord, "F")
  RKM1 <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  
  pkg <- "drclust"
  met <- "RKM"
  
  runtime <- system.time({
    rkm_drclust <- redkm(X_sim[[i]], K =  3, Q =  2, Rndstart = 100)
  })
  rntm <- runtime[3]
  ari <- mrand(t(rkm_drclust$U)%*%Utrue_sim[[i]])
  fit <- norm(rkm_drclust$U %*% solve(t(rkm_drclust$U)%*%rkm_drclust$U) %*%t(rkm_drclust$U) %*% scale(X_sim[[i]]) %*%rkm_drclust$A, "F")
  diff_f <- fstar - fit
  diff_A <- norm(A-rkm_drclust$A, "F")
  RKM2 <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  
  ### FKM
  
  pkg <- "clustrd"
  met <- "FKM"
  
  runtime <- system.time({
    fkm_clustrd <- cluspca(X_sim[[i]],nclus =  3, ndim =  2, method = "FKM", nstart = 100)
  })
  rntm <- runtime[3]
  Ufkm_rd <- bsmatrix(fkm_clustrd$cluster)
  ari <- mrand(t(Ufkm_rd)%*%Utrue_sim[[i]])
  fit <- norm(Ufkm_rd %*% solve(t(Ufkm_rd)%*%Ufkm_rd) %*%t(Ufkm_rd) %*% scale(X_sim[[i]]) %*%fkm_clustrd$attcoord, "F")
  diff_f <- fstar - fit
  diff_A <- norm(A-fkm_clustrd$attcoord, "F")
  FKM1 <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  
  pkg <- "drclust"
  met <- "FKM"
  
  runtime <- system.time({
    fkm_drclust <- factkm(X_sim[[i]], K =  3, Q =  2, Rndstart = 100)
  })
  rntm <- runtime[3]
  ari <- mrand(t(fkm_drclust$U)%*%Utrue_sim[[i]])
  fit <- norm(fkm_drclust$U %*% solve(t(fkm_drclust$U)%*%fkm_drclust$U) %*%t(fkm_drclust$U) %*% scale(X_sim[[i]]) %*%fkm_drclust$A, "F")
  diff_f <- fstar - fit
  diff_A <- norm(A-rkm_clustrd$attcoord, "F")
  FKM2 <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  
  ### CDPCA
  
  pkg <- "biplotbootGUI"
  met <- "DPCAKM"
  runtime <- system.time({
    cdpca_bpbGUI <- CDpca(X_sim[[i]], P =  3, Q =  2, r = 100, maxit=100, cdpcaplot = F)
  })
  rntm <- runtime[3]
  ari <- mrand(t(cdpca_bpbGUI$U)%*%Utrue_sim[[i]])
  fit <- norm(cdpca_bpbGUI$U %*% solve(t(cdpca_bpbGUI$U)%*%cdpca_bpbGUI$U) %*%t(cdpca_bpbGUI$U) %*% scale(X_sim[[i]]) %*%cdpca_bpbGUI$A, "F")
  diff_f <- fstar-fit
  diff_A <- norm(A-cdpca_bpbGUI$A , "F")
  CDPCA1 <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  
  pkg <- "drclust"
  met <- "DPCAKM"
  
  runtime <- system.time({
    cdpca_drclust <- dpcakm(X_sim[[i]], 3, 2, Rndstart = 100)
  })
  rntm <- runtime[3]
  ari <- mrand(t(cdpca_drclust$U)%*%Utrue_sim[[i]])
  fit <- norm(cdpca_drclust$U %*% solve(t(cdpca_drclust$U)%*%cdpca_drclust$U) %*%t(cdpca_drclust$U) %*% scale(X_sim[[i]]) %*%cdpca_drclust$A, "F")
  diff_f <- fstar-fit
  diff_A <- norm(A-cdpca_drclust$A , "F")
  CDPCA2 <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  
  ### DKM
  
  met <- "DKM"
  
  runtime <- system.time({
    dkm <- doublekm(X_sim[[i]], 3, 2, Rndstart = 100)
  })
  rntm <- runtime[3]
  ari <- mrand(t(dkm$U)%*%Utrue_sim[[i]])
  fit <- norm(dkm$U %*% solve(t(dkm$U)%*%dkm$U) %*%t(dkm$U) %*% scale(X_sim[[i]]) %*%dkm$V, "F")
  diff_f <- fstar-fit
  diff_A <- NA
  DKM <- c(Noise[i], pkg, met, runtime[3], fit, ari, diff_f, diff_A)
  
  return(list(DKM = DKM, RKM1 = RKM1, RKM2 = RKM2, FKM1 = FKM1, FKM2 = FKM2, CDPCA1 = CDPCA1, CDPCA2 = CDPCA2, dkm = dkm, rkm_clustrd = rkm_clustrd, rkm_drclust = rkm_drclust, fkm_clustrd = fkm_clustrd, fkm_drclust = fkm_drclust, cdpca_bpbGUI = cdpca_bpbGUI, cdpca_drclust = cdpca_drclust))
}

# This takes ~ 6 hours
# models_out <- foreach(data = 1:300) %dopar% {f(data)}

# The following line loads all the object produced by the script until here
load(url("https://figshare.com/ndownloader/files/58897297"))

# stopCluster(cl)

temp <-lapply(models_out, function(x) c(x$RKM1, x$RKM2, x$FKM1, x$FKM2, x$CDPCA1, x$CDPCA2, x$DKM))
rkm1 <- lapply(temp, function(x) unlist(c((x[1:8]))))
rkm2 <- lapply(temp, function(x) unlist(c((x[9:16]))))
fkm1 <- lapply(temp, function(x) unlist(c((x[17:24]))))
fkm2 <- lapply(temp, function(x) unlist(c((x[25:32]))))
cdpca1 <- lapply(temp, function(x) unlist(c((x[33:40]))))
cdpca2 <- lapply(temp, function(x) unlist(c((x[41:48]))))
dkm <- lapply(temp, function(x) unlist(c((x[49:56]))))


RKM1 <- do.call(rbind, rkm1)              
RKM2 <- do.call(rbind, rkm2)
FKM1 <- do.call(rbind, fkm1)              
FKM2 <- do.call(rbind, fkm2)
CDPCA1 <- do.call(rbind, cdpca1)              
CDPCA2 <- do.call(rbind, cdpca2)
DKM <- do.call(rbind, dkm)
colnames(RKM1) <- cnames
CDPCA1[,3] <- "DPCAKM"
CDPCA2[,3] <- "DPCAKM"

RKM1 <- as.data.frame(RKM1)
RKM2 <- as.data.frame(RKM2)
FKM1 <- as.data.frame(FKM1)
FKM2 <- as.data.frame(FKM2)
CDPCA1 <- as.data.frame(CDPCA1)
CDPCA2 <- as.data.frame(CDPCA2)
DKM <- as.data.frame(DKM)

RKM1[,c(4:8)] <- apply(RKM1[, c(4:8)], 2, as.numeric) 
RKM2[,c(4:8)] <- apply(RKM2[, c(4:8)], 2, as.numeric) 
FKM1[,c(4:8)] <- apply(FKM1[, c(4:8)], 2, as.numeric) 
FKM2[,c(4:8)] <- apply(FKM2[, c(4:8)], 2, as.numeric) 
CDPCA1[,c(4:8)] <- apply(CDPCA1[, c(4:8)], 2, as.numeric) 
CDPCA2[,c(4:8)] <- apply(CDPCA2[, c(4:8)], 2, as.numeric) 
DKM[,c(4:8)] <- apply(DKM[, c(4:8)], 2, as.numeric) 

unique_vals <- unique(RKM1[, 1])

rkm1 <- RKM1[1:3,]
rkm2 <- RKM2[1:3,]
fkm1 <- FKM1[1:3,]
fkm2 <- FKM2[1:3,]
dpcakm1 <- CDPCA1[1:3,]
dpcakm2 <- CDPCA2[1:3,]
dkm <- DKM[1:3,]

for (i in 1:length(unique_vals)) {
  val <- unique_vals[i]
  idx <- which(RKM1[, 1] == val)
  med_vals <- apply(RKM1[idx, 4:8], 2, median)
  rkm1[i, 4:8] <- med_vals
  rkm1[i,1] <- val
  
  med_vals <- apply(RKM2[idx, 4:8], 2, median)
  rkm2[i, 4:8] <- med_vals
  rkm2[i,1] <- val
  
  med_vals <- apply(FKM1[idx, 4:8], 2, median)
  fkm1[i, 4:8] <- med_vals
  fkm1[i,1] <- val
  
  med_vals <- apply(FKM2[idx, 4:8], 2, median)
  fkm2[i, 4:8] <- med_vals
  fkm2[i,1] <- val
  
  med_vals <- apply(CDPCA1[idx, 4:8], 2, median)
  dpcakm1[i, 4:8] <- med_vals
  dpcakm1[i,1] <- val
  
  med_vals <- apply(CDPCA2[idx, 4:8], 2, median)
  dpcakm2[i, 4:8] <- med_vals
  dpcakm2[i,1] <- val
  
  med_vals <- apply(DKM[idx, 4:7], 2, median)
  dkm[i, 4:7] <- med_vals
  dkm[i,1] <- val
  dkm[,8] <- NA
  
}
DKM[,2] <- "drclust"
colnames(RKM1) <- cnames
colnames(RKM2) <- cnames
colnames(FKM1) <- cnames
colnames(FKM2) <- cnames
colnames(CDPCA1) <- cnames
colnames(CDPCA2) <- cnames
colnames(DKM) <- cnames


# ----------- Boxplots
df_list <- list(RKM1, RKM2, FKM1, FKM2, CDPCA1, CDPCA2, DKM)

###

long_data <- purrr::map_df(df_list, function(df) {
  colnames(df)[1] <- "Block"
  method_label <- paste0(df$technique[1], "\n(", df$library[1], ")")
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, "Block")
  
  df %>%
    pivot_longer(cols = dplyr::all_of(numeric_cols),
                 names_to = "Measure", values_to = "Value") %>%
    mutate(Method = method_label)
})

long_data$Block <- factor(long_data$Block, levels = c("High", "Medium", "Low"))

long_data_clean <- long_data %>%
  group_by(Method, Measure, Block) %>%
  filter(any(!is.na(Value))) %>%
  ungroup()

unique_measures <- unique(long_data_clean$Measure)

make_measure_plot <- function(df, misura) {
  ggplot(dplyr::filter(df, Measure == misura),
         aes(x = Method, y = Value, fill = Method)) +
    geom_boxplot(width = 0.5) +
    facet_wrap(~ Block, nrow = 1, scales = "free_x", strip.position = "bottom") +
    coord_flip(clip = "off") +                       
    theme_minimal() +
    labs(title = NULL, x = NULL, y = NULL) +         
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7, face = "bold"),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 9, lineheight = 1.2),
      legend.position = "none",
      panel.spacing.x = unit(1.2, "lines"),
      plot.margin = margin(8, 8, 8, 22)
    )
}

measures_no_runtime <- setdiff(unique_measures, "runtime")
plots <- purrr::map(measures_no_runtime, ~ make_measure_plot(long_data_clean, .x))
names(plots) <- measures_no_runtime

runtime_data   <- dplyr::filter(long_data_clean, Measure == "runtime")
runtime_rkm    <- dplyr::filter(runtime_data, str_detect(Method, "^RKM\\n"))
runtime_others <- dplyr::filter(runtime_data, !str_detect(Method, "^RKM\\n"))

make_runtime_plot <- function(df_runtime) {
  ggplot(df_runtime, aes(x = Method, y = Value, fill = Method)) +
    geom_boxplot(width = 0.5) +
    facet_wrap(~ Block, nrow = 1, scales = "free_x", strip.position = "bottom") +
    coord_flip(clip = "off") +
    theme_minimal() +
    labs(title = NULL, x = NULL, y = NULL) +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 9, lineheight = 1.2),
      strip.text.x = element_text(size = 8, face = "bold"),
      legend.position = "none",
      panel.spacing.x = unit(1.2, "lines"),
      plot.margin = margin(8, 8, 8, 22)
    )
}

# Figure 3-8
p_runtime_rkm    <- make_runtime_plot(runtime_rkm)
p_runtime_others <- make_runtime_plot(runtime_others)
plots
p_runtime_rkm
p_runtime_others
###

# ---------------------------------------------- macro dataset
# The examples are from the section "Application on Real Data"
# Macro dataset (Vichi & Kiers, 2001)
data("macro")
macro <- as.matrix(macro)
irisn <- as.matrix(iris[,-5])

kaiserCrit(macro)

# relaxed pseudoF
apseudoF(irisn, maxK=10, tol = 0.05, model = 2, Q = 3)

# double pseudoF (Rocci & Vichi, 2008)
dpseudoF(irisn, maxK = 10, maxQ = 3)

# double k-means
dkm <- doublekm(irisn,4,3, print=1)

# reduced k-means
rkm <- redkm(irisn, 4, 3, print=1)

# factorial k-means
fkm <- factkm(irisn, 4, 3, print=1, rot = 1)

# clustering with disjoint PCA
cdpca <- dpcakm(irisn, 3, 3, print=1)

# disjoint PCA
# Require GDP and LI variables to lie in the same cluster
out <- dispca(irisn, 3, print = 1, constr = c(1,1,0,0,0,0))

# disjoint FA
out <- disfa(irisn, 3, print=1)

# Kaiser criterion for the choice of Q, the number of latent components
kaiserCrit(macro)

# relaxed pseudoF
apseudoF(macro, maxK=10, tol = 0.05, model = 2, Q = 3)

# double pseudoF (Rocci & Vichi, 2008)
dpseudoF(macro, maxK = 10, maxQ = 5)

# dendrogram of the centroids obtained by FKM
out <- factkm(macro, 10, 3)
centree(out)

# silhouette for the partition obtained via CDPCA
out <- dpcakm(macro, 4, 3)
silhouette(macro, out)

# heatmap of the observations 
# (sorted by distance(centroid, GrandMean), sorted within each cluster by distance(unit, centroid))
# projected on the subspace
out <- doublekm(macro,5,3)
heatm(macro, out)


#-------------- Biplot
library(ggplot2)
library(grid)
library(dplyr)
library(drclust)

# Prepare data and perform a model in drclust
out <- factkm(macro, K = 2, Q = 2, Rndstart = 100)

# Prepare data
Y <- as.data.frame(macro%*%out$A); colnames(Y) <- c("Dim1", "Dim2")
Y$cluster <- as.factor(cluster(out$U))

arrow_scale <- 5
A <- as.data.frame(out$A)[, 1:2] * arrow_scale
colnames(A) <- c("PC1", "PC2")
A$var <- colnames(macro)

# Axis limits
lims <- range(c(Y$Dim1, Y$Dim2, A$PC1, A$PC2)) * 1.2

# Circle
circle <- data.frame(x = cos(seq(0, 2*pi, length.out = 200)) * arrow_scale,
                     y = sin(seq(0, 2*pi, length.out = 200)) * arrow_scale)

ggplot(Y, aes(x = Dim1, y = Dim2, color = cluster)) +
  geom_point(size = 2) +
  geom_segment(data = A, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), inherit.aes = FALSE, color = "gray40") +
  geom_text(data = A, aes(x = PC1, y = PC2, label = var), inherit.aes = FALSE,
            hjust = 1.1, vjust = 1.1, size = 3) +
  geom_path(data = circle, aes(x = x, y = y), inherit.aes = FALSE,
            linetype = "dashed", color = "gray70") +
  coord_fixed(xlim = lims, ylim =  lims) +
  labs(x = "Component 1", y = "Component 2", title = "Biplot") +
  theme_minimal()



#-------------- Parallel Coordinates Plot

library(GGally)

out <- factkm(macro, K = 2, Q = 2, Rndstart = 100)
ggparcoord(data = Y,
           columns = 1:(ncol(Y)-1),     
           groupColumn = "cluster",    
           scale = "uniminmax",        
           showPoints = FALSE,
           alphaLines = 0.5) +
  theme_minimal() +
  labs(title = "Parallel Coordinate Plot",
       x = "Variables", y = "Normalized Value")

