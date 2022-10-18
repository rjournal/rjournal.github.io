# ClusTorus: examples, figures, and tables
# Conformal Prediction section example

# section : Introduction
# code 1 in Introduction (page 2) -------------------------------------------------------
library(ClusTorus)
set.seed(2021)
ex <- clus.torus(SARS_CoV_2)
plot(ex)
# ---------------------------------------------------------------------------------------

# codes for figure 1 --------------------------------------------------------------------
library(tidyverse)
p.clustorus <- plot(ex, panel = 1, out = TRUE)
p.clustorus <- p.clustorus[[1]] + xlab("phi") + ylab("psi") +
  ggtitle(" Clustering by ClusTorus::clus.torus") +
  theme(legend.position = "none")

# ClusTorus::kmeans.torus (#cluster set as 3)
J <- 3
kmeans.out <- kmeans.torus(SARS_CoV_2, centers = J)
p.exkmeans <- SARS_CoV_2 %>% ggplot() +
  geom_point(aes(x = phi, y = psi,color = as.factor(kmeans.out$membership))) +
  theme(legend.position = "none") + ggtitle("Clustering by ClusTorus::kmeans.torus")

# mixtools::mvnormalmixEM (#cluster set as 3)
library(mixtools)
J <- 3
mixtools3 <- mvnormalmixEM(SARS_CoV_2,k=J)
membership = as.factor(apply(mixtools3$posterior, 1, which.max))
{
theta <- seq(0, 2 * pi, length.out = 199)
Z <- cbind(cos(theta), sin(theta))
p2 <- ggplot()
for (j in 1:J){
  mu <- mixtools3$mu[[j]]
  Sinv <- solve(mixtools3$sigma[[j]])
  c.minus.t <- qchisq(0.95,2)
  if (c.minus.t <= 0) {next}
  M <- eigen(Sinv/c.minus.t)
  Mmhalf <- M$vectors %*% diag( sqrt(1/M$values) ) %*% t(M$vectors)
  RR <- Mmhalf %*% t(Z) + mu
  plot.data <- data.frame(angle1 = RR[1,], angle2 = RR[2,], value = 1)
  p2 <- p2 + geom_polygon(aes(x = angle1, y = angle2),data = plot.data,
                           color = "blue",alpha = 0.1)
}
p.mixtools <- p2 + xlab("phi") + ylab("psi") +
 geom_point(aes(x = SARS_CoV_2$phi, y = SARS_CoV_2$psi, color = membership)) +
  scale_x_continuous(breaks = c(0,1,2)*pi,labels = c("0","pi", "2pi")) +
  scale_y_continuous(breaks = c(0,1,2)*pi,labels = c("0","pi","2pi")) +
  coord_cartesian(xlim = c(0, 2*pi), ylim = c(0, 2*pi), expand = FALSE) +
  ggtitle("Gaussian mixture clustering by mixtools") + theme(legend.position = "none")
}

# mclust::Mclust (#cluster fitted)
library(mclust)
mclust.out = mclust::Mclust(SARS_CoV_2)

# Figure 1 ------------------------------------------------------------------------------
library(gridGraphics)
library(patchwork)

old_par <- par(mar = c(0, 0, 0, 0), mgp = c(1, 0.25, 0),
               bg = NA, cex.axis = 0.5, las = 1, tcl = -0.25)
p.clustorus + p.exkmeans + p.mixtools + wrap_elements(panel = ~plot(mclust.out, what = "class"), clip = FALSE) +
 ggtitle("Gaussian mixture clustering by mclust")
# Note: If an error message saying "invalid value specified for graphical parameter
# "pin"" appears and you are using RStudio, enlarge the size of Plots pane.

par(old_par)
# ---------------------------------------------------------------------------------------


# section : Conformal prediction
# code 2 (page 3 below) -----------------------------------------------------------------
cp.kde <- cp.torus.kde(SARS_CoV_2)
cp.kde
# ---------------------------------------------------------------------------------------

# code 3 & Figure 2 (page 4 right below Figure 2)----------------------------------------
plot(cp.kde)
ggsave(filename = "cpkde.png", width = 7, height = 5)
# ---------------------------------------------------------------------------------------



# subsection : Inductive conformal prediction
# (In the example below, data splitting is done internally)
# code 4 (page 5) -----------------------------------------------------------------------
set.seed(2021)
icp.torus.kde <- icp.torus(SARS_CoV_2, model = "kde", concentration = 25)
icp.kde <- icp.torus.eval(icp.torus.kde, level = 0.1)
icp.kde
# ---------------------------------------------------------------------------------------


# code 5 (page 7) & Figure 3 ------------------------------------------------------------
set.seed(2021)
icp.torus.12 <- icp.torus(SARS_CoV_2, J = 12)
plot(icp.torus.12, level = 0.1111)
ggsave(filename = "Ellipses.png", width = 7, height = 5)
# ---------------------------------------------------------------------------------------


# code 6 (page 7) (The resulting graphic is omitted)-------------------------------------
plot(icp.torus.12, ellipse = FALSE)
# ---------------------------------------------------------------------------------------



# section : Clustering by conformal prediction
# code 7 (page 9 below - page 10 above) -------------------------------------------------
c <- cluster.assign.torus(icp.torus.12, level = 0.1111)
c
plot(c, assignment = "log.density")
plot(c, assignment = "outlier")
# ---------------------------------------------------------------------------------------


# cluster assignment : log.density vs outlier
# Figure 4 ------------------------------------------------------------------------------
g_e <- plot(c, assignment = "log.density", out = TRUE)[[1]] + xlab("phi") + ylab("psi") + ggtitle("Clusters based on log.density")
g_1 <- plot(c, assignment = "outlier", out = TRUE)[[1]]+ xlab("phi") + ylab("psi") + ggtitle("Clusters and outliers")

library(cowplot)
plot_grid(g_e, g_1, nrow = 1)
ggsave("mahal.png", width = 9.5, height = 3.9)
# ---------------------------------------------------------------------------------------


# section : Hyperparameter selection
# code 8 (page 10 below - page 11 above) ------------------------------------------------
set.seed(2021)
icp.torus.objects <- icp.torus(SARS_CoV_2, J = 3:35)
hyperparam.out <- hyperparam.torus(icp.torus.objects)
hyperparam.out
# ---------------------------------------------------------------------------------------


# code 9 (page 12) ----------------------------------------------------------------------
hyperparam.risk.out <- hyperparam.torus(icp.torus.objects, option = "risk")
hyperparam.risk.out
# ---------------------------------------------------------------------------------------


# section : clustering data on T^4
# Figure 5 ------------------------------------------------------------------------------
ILE %>% GGally::ggpairs(upper  = list(continuous = "blank"))
ggsave(filename = "ILE.png", width = 6, height = 4)
# ---------------------------------------------------------------------------------------

# applying all procedures to ILE data (on T^4)
# code 10 (page 13 - 14) & Figure 6-7 -----------------------------------------------------
set.seed(2021)
icp.torus.objects <- icp.torus(ILE, J = 10:40)

output_list <- sapply( c("risk", "AIC", "BIC"), function(opt) {
  hyperparam.torus(icp.torus.objects, option = opt)}, simplify = FALSE, USE.NAMES = TRUE)

hyperparam.risk.out <- output_list$risk

plot_grid(plot(output_list$risk),
          plot(output_list$AIC),
          plot(output_list$BIC), nrow = 3)
ggsave(filename = "criterion.png", width = 9, height = 7.5)

cluster.out <- cluster.assign.torus(hyperparam.risk.out)
plot(cluster.out, assignment = "outlier")     # Top panel of Figure 7
plot(cluster.out, assignment = "log.density") # Bottom panel of Figure 7

plot_grid(
  plot(cluster.out, assignment = "outlier"),
  plot(cluster.out, assignment = "log.density"),
  nrow = 2
)
# ---------------------------------------------------------------------------------------



# code 11 & Figure 8 (page 16) --------------------------------------------------------
set.seed(2021)
plot(hyperparam.risk.out$icp.torus, data = ILE[sample(1:nrow(ILE),500),],
     level = hyperparam.risk.out$alphahat)

ggsave(filename = "cluster_ile.png", width = 8, height = 10)
# ---------------------------------------------------------------------------------------


# section : Other methods of clustering on the torus
# 1. extrinsic k-means

# code 12 (page 17) ---------------------------------------------------------------------
set.seed(2021)
exkmeans <- kmeans.torus(SARS_CoV_2, centers = 3, nstart = 30)
head(exkmeans$membership)
# ---------------------------------------------------------------------------------------


# 2. hierarchical Clustering
# code 13 (page 17) ---------------------------------------------------------------------
distmat <- ang.pdist(SARS_CoV_2)
hc <- hclust(distmat, method = "complete")
hc.result <- cutree(hc, k = 3)
head(hc.result)
# ---------------------------------------------------------------------------------------


# Figure 9 ------------------------------------------------------------------------------
g_kmeans1 <- data.frame(phi = SARS_CoV_2[,1], psi = SARS_CoV_2[,2], membership = as.factor(exkmeans$membership)) %>%
  ggplot(aes(phi,psi, color = membership)) + geom_point() +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  ggtitle(paste("Extrinsic Kmeans , K = 3"))

g_h <- data.frame(phi = SARS_CoV_2[,1], psi = SARS_CoV_2[,2], membership = as.factor(hc.result)) %>%
  ggplot(aes(phi,psi, color = membership)) + geom_point() +
  scale_x_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  scale_y_continuous(breaks = c(0,1,2,3,4)*pi/2, labels = c("0","pi/2","pi","3pi/2","2pi"), limits = c(0,2*pi))+
  ggtitle(paste("Hierarchical Clustering , K = 3"))

plot_grid(g_kmeans1, g_h, nrow = 1)
ggsave("others.png", width = 9.5, height = 3.9)
# ---------------------------------------------------------------------------------------

# Appendix : elliptical k-means algorithm
## specifying initial value for either hlcust or kmeans in icp.torus
# code 14 (page 18) ---------------------------------------------------------------------
icp.torus(data = SARS_CoV_2, J = 4, init = "kmeans", nstart = 30)
icp.torus(data = SARS_CoV_2, J = 4, init = "hierarchical", method = "complete")
# ---------------------------------------------------------------------------------------
