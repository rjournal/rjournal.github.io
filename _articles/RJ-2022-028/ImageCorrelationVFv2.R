# Here is code to accompany the manuscript entitled: Quantifying 
# Population Movement Using a Novel Implementation of Digital Image Correlation 
# in the ICvectorfields Package

library(ICvectorfields)
library(ggplot2)
library(ggnewscale)
library(metR)
library(png)

#===============================================================================
# Plot showing STIC algorithm

# reading in the data
STIC <- readPNG("STICAlgorithm.png")

# plotting
par(mar = c(0,0,0,0))
plot(1:10,1:10, type = 'n', axes = F, ann = F)
rasterImage(STIC, xleft = 1, ybottom = 1, xright = 10, ytop = 10)

#===============================================================================
# Demonstration 1: Application to simulated data

# import simulated data
data(SimData, package = "ICvectorfields")

# convert to raster stack
SimStack <- ICvectorfields::RastStackData(SimData)

# confirming dimension
#dim(SimStack)

# plotting
layout(matrix(1:6, 2, 3, byrow = TRUE))
#layout.show(6)
terra::plot(SimStack[[1]], legend = FALSE, main = "t1")
terra::plot(SimStack[[2]], legend = FALSE, main = "t2")
terra::plot(SimStack[[3]], legend = FALSE, main = "t3")
terra::plot(SimStack[[4]], legend = FALSE, main = "t4")
terra::plot(SimStack[[5]], legend = FALSE, main = "t5")
terra::plot(SimStack[[6]], legend = FALSE, main = "t6")

# computing displacement
VFdf1 <- DispField(SimStack[[1]], SimStack[[6]], factv1 = 101, facth1 = 101, 
                   restricted = TRUE)
VFdf1

# computing velocity
VFdf2 <- DispFieldST(SimStack, lag1 = 1, factv1 = 101, facth1 = 101, 
                     restricted = TRUE)
VFdf2

# Vector field for radial movement simulated using a convection-diffusion 
# equation. the orthogonal velocity vectors are estimated using the DispFieldST 
# function in the ICvectorfields package.
SimVF <- ggplot() +
  xlim(c(-5, 5)) +
  ylim(c(-5, 5)) +
  geom_raster(data = SimData,
              aes(x = xcoord, y = ycoord, fill = t1)) +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  new_scale("fill") +
  geom_raster(data = SimData,
              aes(x = xcoord, y = ycoord, fill = t6), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  geom_vector(data = VFdf2, 
              aes(x = centx, y = centy, 
                  mag = Mag(dispx, dispy), 
                  angle = Angle(dispx, dispy))) + 
  theme(panel.background = element_rect(fill = "white", color = "grey"),
        legend.key.size = unit(0.4, 'cm'))
SimVF

# Repeating the analysis with restricted = FALSE to demonstrate a pitfall of 
# using cross-correlation to estimate velocities.
VFdf3 <- DispFieldST(SimStack, lag1 = 1, factv1 = 101, facth1 = 101, 
                     restricted = FALSE)
VFdf3

#===============================================================================
# Demonstration 2: Application to larch budmoth data

# import larch budmoth data
data(lbm, package = "ncf")

# convert to raster stack
LBMStack <- ICvectorfields::RastStackData(lbm)

# confirming dimension
#dim(LBMStack)

# visualizing
layout(matrix(1:6, 2, 3, byrow = TRUE))
#layout.show(6)
terra::plot(LBMStack[[1]], legend = FALSE, main = "1961")
terra::plot(LBMStack[[2]], legend = FALSE, main = "1962")
terra::plot(LBMStack[[3]], legend = FALSE, main = "1963")
terra::plot(LBMStack[[4]], legend = FALSE, main = "1964")
terra::plot(LBMStack[[5]], legend = FALSE, main = "1965")

# calculating velocity field for larch budmoth
VFdf3 <- DispFieldSTall(LBMStack[[1:23]], lagmax = 3, factv1 = 3, facth1 = 3, 
                        restricted = FALSE)

# Making the corresponding vector field plot for larch budmoth
LBMVF1 <- ggplot() +
  geom_tile(data = lbm,
            aes(x = x, y = y, 
                fill = X1962)) +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  new_scale("fill") +
  geom_tile(data = lbm,
            aes(x = x, y = y, fill = X1964), alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  geom_vector(data = VFdf3, 
              aes(x = centx, y = centy, 
                  mag = Mag(dispx, dispy), 
                  angle = Angle(dispx, dispy))) + 
  theme(panel.background = element_rect(fill = "white", color = "grey"),
        legend.key.size = unit(0.4, 'cm'))
LBMVF1

# Calculating the average speed of population movement
VFdf3$speed <- sqrt((VFdf3$dispx^2) + VFdf3$dispy^2)

# sub-setting to remove locations where speed is zero
VFdf4 <- subset(VFdf3, speed > 0)

# computing mean, standard deviation and dimension of data frame
# to obtain sample size
mean(VFdf4$speed)
sd(VFdf4$speed)
dim(VFdf4)

# upper and lower Wald-type 95 percent confidence interval on average speed
mean(VFdf4$speed)/1000 + qt(0.975, dim(VFdf4)[1] - 1)*sd(VFdf4$speed)/1000/sqrt(dim(VFdf4)[1] - 1)
mean(VFdf4$speed)/1000 + qt(0.025, dim(VFdf4)[1] - 1)*sd(VFdf4$speed)/1000/sqrt(dim(VFdf4)[1] - 1)
