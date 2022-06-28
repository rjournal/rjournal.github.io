library(scatterplot3d)

## now
pdf(file = "RLumCarlo_Manuscript/figures/raw/Clusters/Clusters3D_Plots_now.pdf")
par(cex = 1.3)
x <- rep(rep(1:5,each = 5),5)
y <- rep(rep(1:5,5),5)
z <- rep(1:5,each = 25)
scatterplot3d(
  x,
  y,
  z,
  xlim = c(0,5.1),
  ylim = c(0,5.1),
  zlim = c(1,5.1),
  main = "",
  xlab = "",
  ylab = "",
  zlab = "",
  x.ticklabs = "",
  y.ticklabs = "",
  z.ticklabs = "",
  pch = 20,
  color = sample(khroma::color(palette = "contrast")(3), replace = TRUE, 125)
)
dev.off()

##advances
pdf(file = "RLumCarlo_Manuscript/figures/raw/Clusters/Clusters3D_Plots_advanced.pdf")
par(cex = 1.3)
RLumCarlo::create_ClusterSystem(
  125,
  plot = TRUE,
  main = "",
  mtext = "",
  xlab = "",
  ylab = "",
  zlab = "",
  x.ticklabs = "",
  y.ticklabs = "",
  z.ticklabs = "",
  pch = 20
)
dev.off()

# future
pdf(file = "RLumCarlo_Manuscript/figures/raw/Clusters/Clusters3D_Plots_future.pdf")
par(cex = 1.3)
x <- runif(125)
y <- runif(125)
z <- runif(125)
scatterplot3d(
  x,
  y,
  z,
  main = "",
  xlab = "",
  ylab = "",
  zlab = "",
  x.ticklabs = "",
  y.ticklabs = "",
  z.ticklabs = "",
  pch = 20,
  color = sample(khroma::color(palette = "contrast")(3), replace = TRUE, 125)
)
dev.off()
