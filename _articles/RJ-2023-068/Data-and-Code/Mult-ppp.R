##############################################################################
############# Multivariate point patterns analysis using R-INLA ##############
##############################################################################

# In this script, a multivariate point pattern dataset (clmfires, available
# in the spatstat package) is going to be analysed using INLA by defining
# the appropriate stack functions. 

#Load packages
library(sp)
library(INLA)
library(spatstat)
library(ggmap)
library(inlabru)
library(maptools)
library(raster)
library(rgeos)
library(RColorBrewer)


#Load and display the data
data("clmfires"); class(clmfires)
str(clmfires)
plot(clmfires)

#Number of point patterns
n.pp <- 4

# Number of fires of each type
n.lig <- table(clmfires$marks$cause)[1]
n.acc <- table(clmfires$marks$cause)[2]
n.int <- table(clmfires$marks$cause)[3]
n.oth <- table(clmfires$marks$cause)[4]

#Separate the different point patterns
pts <- cbind(clmfires$x, clmfires$y) 
pts.lig <- 
  cbind(clmfires$x, clmfires$y)[which(clmfires$marks$cause == "lightning"), ] 
pts.acc <- 
  cbind(clmfires$x, clmfires$y)[which(clmfires$marks$cause == "accident"), ] 
pts.int <- 
  cbind(clmfires$x, clmfires$y)[which(clmfires$marks$cause == "intentional"), ] 
pts.oth <- 
  cbind(clmfires$x, clmfires$y)[which(clmfires$marks$cause == "other"), ] 

# Prepare the boundary for the mesh construction
bdy <- with(clmfires$window$bdry[[1]], cbind(x, y))
bdy <- rbind(bdy, bdy[1, ])
bdy.SP <- as(owin(poly=bdy) , "SpatialPolygons")

#Transform to data frame to build the spatial object
sp.pts <- as.data.frame(pts)
sp.lig <- as.data.frame(pts.lig)
sp.acc <- as.data.frame(pts.acc)
sp.int <- as.data.frame(pts.int)
sp.oth <- as.data.frame(pts.oth)

#Build spatial object
coordinates(sp.pts) <- ~ V1 + V2
coordinates(sp.lig) <- ~ V1 + V2
coordinates(sp.acc) <- ~ V1 + V2
coordinates(sp.int) <- ~ V1 + V2
coordinates(sp.oth) <- ~ V1 + V2


# Mesh construction using the boundary

#Define the mesh
mesh <- inla.mesh.2d(
  boundary = list(bdy.SP, NULL),
  cutoff = 2,
  max.edge = c(20, 50),
  min.angle = 27,
  offset = c(1, 50),
  n=c(16,16))

# Number of mesh nodes
nv <- mesh$n

# Plot the mesh and the data 
png(file = "clmfires_mesh.png", width =960, height = 720)
multiplot(
  ggplot() + gg(mesh) + labs(title = "Mesh" ) +
    gg(bdy.SP, col="darkgreen") + coord_fixed(ratio = 1),
  ggplot() + gg(mesh) + gg(sp.lig, col="blue") + 
    gg(sp.acc, col="orangered")  + gg(sp.int, col="red3")  + 
    gg(sp.oth, col="darkmagenta") + gg(bdy.SP, col="darkgreen") +
    coord_fixed(ratio = 1) + labs(title = "All fires" ),  
  ggplot() + gg(mesh) + gg(sp.lig, col="blue") +
    gg(bdy.SP, col="darkgreen") + coord_fixed(ratio = 1)+
    labs(title = "Ligthing fires" ),
  ggplot() + gg(mesh) + gg(sp.acc, col="orangered") +
    gg(bdy.SP, col="darkgreen") + coord_fixed(ratio = 1)+
    labs(title = "Accidental fires" ),
  ggplot() + gg(mesh) + gg(sp.int, col="red3") +
    gg(bdy.SP, col="darkgreen") + coord_fixed(ratio = 1)+
    labs(title = "Intentional fires" ),
  ggplot() + gg(mesh) + gg(sp.oth, col="darkmagenta") +
    gg(bdy.SP, col="darkgreen") + coord_fixed(ratio = 1) +
    labs(title = "Other fires" ),
  cols=3)
dev.off()

# Create spde model for the spatial effects
#spde <- inla.spde2.matern(mesh = mesh.poly, alpha = 2, constr = TRUE)
spde <- inla.spde2.pcmatern(mesh = mesh,
  prior.range = c(200, 0.95), # P(practic.range < 200) = 0.95 
  #400km is app the max range an the units are 100 meters
  prior.sigma = c(10, 0.05)) # P(sigma > 25) = 0.05
  # An number of fires greater than 20 per unit area would 
  # be a really high value, which is barely expected. 

# Create an index for the spatial effects
s.index.lig <- 
  inla.spde.make.index(name = "spatial.field.lig", n.spde = spde$n.spde)
s.index.acc <- 
  inla.spde.make.index(name = "spatial.field.acc", n.spde = spde$n.spde)
s.index.int <- 
  inla.spde.make.index(name = "spatial.field.int", n.spde = spde$n.spde)
s.index.oth <- 
  inla.spde.make.index(name = "spatial.field.oth", n.spde = spde$n.spde)


# The following lines calculate the area of the mesh 
# using the Voronoi tesselation
library(deldir)
dd = deldir(mesh$loc[, 1],mesh$loc[, 2])
# Create a list of tiles in a tessellation
mytiles = tile.list(dd)

pl.study = as(bdy, "gpc.poly") # Class for polygons
area.poly(pl.study) # Computing the area of the whole polygon

# Compute weight as area of the polygon given as an
# interaction between Voronoi tiles and domain polygon
w = unlist(lapply(mytiles,
  function(p) area.poly(
    intersect(as(cbind(p$x,p$y), "gpc.poly"), pl.study)
    )
)
)
# Check the total sum of the weights
sum(w)


# Data for the stack function: lightning fires
e.lig <- c(w, rep(0, n.lig))
y.lig <- matrix(NA, nrow = nv + n.lig, ncol = n.pp)
y.lig[, 1] <- rep(0:1, c(nv, n.lig))

# Data for the stack function: accidental fires
e.acc <- c(w, rep(0, n.acc))
y.acc <- matrix(NA, nrow = nv + n.acc, ncol = n.pp)
y.acc[, 2] <- rep(0:1, c(nv, n.acc))

# Data for the stack function: intentional fires
e.int <- c(w, rep(0, n.int))
y.int <- matrix(NA, nrow = nv + n.int, ncol = n.pp)
y.int[, 3] <- rep(0:1, c(nv, n.int))

# Data for the stack function: other fires
e.oth <- c(w, rep(0, n.oth))
y.oth <- matrix(NA, nrow = nv + n.oth, ncol = n.pp)
y.oth[, 4] <- rep(0:1, c(nv, n.oth))

#imat: define imat
imat <- Diagonal(nv, rep(1, nv))

#lmat: define lmat
lmat.lig <- inla.spde.make.A(mesh, pts.lig)
lmat.acc <- inla.spde.make.A(mesh, pts.acc)
lmat.int <- inla.spde.make.A(mesh, pts.int)
lmat.oth <- inla.spde.make.A(mesh, pts.oth)

#Projector matrix: Put together imat and lmat
A.lig <- rbind(imat, lmat.lig)
A.acc <- rbind(imat, lmat.acc)
A.int <- rbind(imat, lmat.int)
A.oth <- rbind(imat, lmat.oth)


# Create the stack for the lighting fires
stk.lig <- inla.stack(
  data = list(y = y.lig, e = e.lig),
  A = list(A.lig, 1),
  effects = list(spatial.field.lig = s.index.lig, 
    data.frame(Intercept.lig = rep(1, nrow(A.lig))) 
    ),
  tag = "Lighting")


# Create the stack for the accidental fires
stk.acc <- inla.stack(
  data = list(y = y.acc, e = e.acc), 
  A = list(A.acc, A.acc, 1), 
  effects = list(
    base.copy.acc = 1:nv, 
    spatial.field.acc = s.index.acc, 
    data.frame(Intercept.acc = rep(1, nrow(A.acc)))
    ),
  tag = "Accidental")

# Create the stack for the intentional fires
stk.int <- inla.stack(
  data = list(y = y.int, e = e.int), 
  A = list(A.int, A.int, 1), 
  effects = list(
    base.copy.int = 1:nv, 
    spatial.field.int = s.index.int, 
    data.frame(Intercept.int = rep(1, nrow(A.int)))),
  tag = "Intentional")

# Create the stack for the other fires
stk.oth <- inla.stack(
  data = list(y = y.oth, e = e.oth), 
  A = list(A.oth, A.oth, 1), 
  effects = list(
    base.copy.oth = 1:nv, 
    spatial.field.oth = s.index.oth, 
    data.frame(Intercept.oth = rep(1, nrow(A.oth)))),
  tag = "Other")


# Create the grid for the prediction using the polygon shape
grid.pr <- raster(extent(bdy.SP))

# Choose the resolution and add the same projection sys.
res(grid.pr) <- 10
proj4string(grid.pr) <- proj4string(bdy.SP)

# Transform this raster into a polygon  
grid.pr <- rasterToPolygons(grid.pr)

# Intersect polygon and grid
grid.pr <- intersect(bdy.SP, grid.pr)
plot(grid.pr)
points(pts.lig, col = "blue")
points(pts.acc, col = "orangered")
points(pts.int, col = "red3")
points(pts.oth, col = "darkmagenta")


# Build projector matrix for prediction
A.pr <- inla.spde.make.A(mesh = mesh, loc = coordinates(grid.pr))
n.pr <- nrow(A.pr)
n.grid.pr <- nrow(grid.pr)

# Stack for predicting log intensity lighting fires
stk.lig.pr <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr, 1),
  effects = list(spatial.field.lig = s.index.lig,
    data.frame(Intercept.lig = rep(1, n.pr))),
  tag = "Lig.pred")


# Stack for predicting log intensity accidental fires
stk.acc.pr <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr, A.pr, 1),
  effects = list(
    base.copy.acc = 1:nv, 
    spatial.field.acc = s.index.acc,
    data.frame(Intercept.acc = rep(1, n.pr))),
  tag = "Acc.pred")

# Stack for predicting log intensity intentional fires
stk.int.pr <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr, A.pr, 1),
  effects = list(
    base.copy.int = 1:nv, 
    spatial.field.int = s.index.int,
    data.frame(Intercept.int = rep(1, n.pr))),
  tag = "Int.pred")

# Stack for predicting log intensity other fires
stk.oth.pr <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr, A.pr, 1),
  effects = list(
    base.copy.oth = 1:nv, 
    spatial.field.oth = s.index.oth,
    data.frame(Intercept.oth = rep(1, n.pr))),
  tag = "Oth.pred")


# Stack with shared effect
stk.shared <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr),
  effects = list(spatial.field.lig = s.index.lig),
  tag = "shared")

# Specific spatial effect accidental fires
stk.acc.spec <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr),
  effects = list(spatial.field.acc = s.index.acc),
  tag = "Acc.spec")

# Specific spatial effect intentional fires
stk.int.spec <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr),
  effects = list(spatial.field.int = s.index.int),
  tag = "Int.spec")

# Specific spatial effect other fires
stk.oth.spec <- inla.stack(
  data = list(y = matrix(NA, nrow = n.grid.pr, ncol = n.pp)),
  A = list(A.pr),
  effects = list(spatial.field.oth = s.index.oth),
  tag = "Oth.spec")

# All stacks together
join.stack <- inla.stack(
  stk.lig, stk.acc, stk.int, stk.oth, 
  stk.lig.pr, stk.acc.pr, stk.int.pr, stk.oth.pr, 
  stk.shared, stk.acc.spec, stk.int.spec, stk.oth.spec)

# Build the formula
form <- y ~ -1 + Intercept.lig + Intercept.acc + 
  Intercept.int + Intercept.oth +
  f(spatial.field.lig, model = spde) +
  f(spatial.field.acc, model = spde) + 
  f(base.copy.acc, copy = "spatial.field.lig", fixed = TRUE) +
  f(spatial.field.int, model = spde) + 
  f(base.copy.int, copy = "spatial.field.lig", fixed = TRUE) +
  f(spatial.field.oth, model = spde) + 
  f(base.copy.oth, copy = "spatial.field.lig", fixed = TRUE)     

# Estimation
t1 <-Sys.time()
pp.res <- inla(
  formula=form, verbose = FALSE, 
  data = inla.stack.data(join.stack, spde = spde), 
  family = rep("poisson", 4), 
  control.predictor = list(A = inla.stack.A(join.stack), compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, 
    cpo = TRUE, mlik = TRUE, po = TRUE)
)
t2 <- Sys.time()

# Summary of the results
summary(pp.res)



# Extract the estimates for plotting lighting fires prediction
idx.lig <- inla.stack.index(join.stack, 'Lig.pred')$data
grid.pr$lig.pr <- pp.res$summary.fitted.values[idx.lig, "mean"]

# Extract the estimates for plotting lighting fires prediction
idx.acc <- inla.stack.index(join.stack, 'Acc.pred')$data
grid.pr$acc.pr <- pp.res$summary.fitted.values[idx.acc, "mean"]

# Extract the estimates for plotting lighting fires prediction
idx.int <- inla.stack.index(join.stack, 'Int.pred')$data
grid.pr$int.pr <- pp.res$summary.fitted.values[idx.int, "mean"]

# Extract the estimates for plotting lighting fires prediction
idx.oth <- inla.stack.index(join.stack, 'Oth.pred')$data
grid.pr$oth.pr <- pp.res$summary.fitted.values[idx.oth, "mean"]

#-- Estimates of the LOG-intensity so that the actual effects are plotted
#   and not exp(effect).

# Extract the estimates of the shared spatial effect
idx.shared <- inla.stack.index(join.stack, 'shared')$data
#grid.pr$shared <- pp.res$summary.fitted.values[idx.shared, "mean"]
grid.pr$shared <- pp.res$summary.linear.predictor[idx.shared, "mean"]

# Extract the estimates of the specific accidental
idx.acc.spec <- inla.stack.index(join.stack, 'Acc.spec')$data
#grid.pr$acc.spec <- pp.res$summary.fitted.values[idx.acc.spec, "mean"]
grid.pr$acc.spec <- pp.res$summary.linear.predictor[idx.acc.spec, "mean"]

# Extract the estimates of the specific intentional
idx.int.spec <- inla.stack.index(join.stack, 'Int.spec')$data
#grid.pr$int.spec <- pp.res$summary.fitted.values[idx.int.spec, "mean"]
grid.pr$int.spec <- pp.res$summary.linear.predictor[idx.int.spec, "mean"]

# Extract the estimates of the specific other
idx.oth.spec <- inla.stack.index(join.stack, 'Oth.spec')$data
#grid.pr$oth.spec <- pp.res$summary.fitted.values[idx.oth.spec, "mean"]
grid.pr$oth.spec <- pp.res$summary.linear.predictor[idx.oth.spec, "mean"]


#Common scale for the posterior estimates of the different intensities
com.sc <- seq(0, 1.5, 0.1)

# Create palletes 
blu <- brewer.pal(9, "Blues")
blu <- colorRampPalette(blu)(length(com.sc))
ora <- brewer.pal(9, "Oranges")
ora <- colorRampPalette(ora)(length(com.sc))
pur <- brewer.pal(9, "Purples")
pur <- colorRampPalette(pur)(20)
reds <- brewer.pal(9, "Reds")
reds <- colorRampPalette(reds)(length(com.sc))
grn <- brewer.pal(9, "Greens")
grn <- colorRampPalette(grn)(20)
gre <- brewer.pal(9, "Greys")
gre <- colorRampPalette(gre)(length(com.sc))




# Plot the post. means of all the fires
pdf(file = "CLMFires-estimates.pdf")
print(
 spplot(grid.pr, c("lig.pr", "acc.pr", "int.pr", "oth.pr"), 
   col.regions = gre, 
   names.attr = c("Ligthing","Accidental", "Intentional", "Other"))
 )
dev.off()
  
# Plot the shared and all the specific spatial effects
pdf(file = "CLMFires_sh_and_all_specific_spatial.pdf")
print(
 spplot(grid.pr, c("shared", "acc.spec", "int.spec", "oth.spec"), 
   col.regions = gre, 
   names.attr = c("Shared effect", "Accidental specific" , 
    "Intentional specific", "Other fires specific"))
)
dev.off()

# Plot the posterior mean of the intensity of lightning fires
png(file = "CLMFires_lightning.png")
print(
  spplot(grid.pr, c("lig.pr"), 
         at=com.sc,
         col.regions = c(blu), 
         main = c("Lightning Fires"))
)
dev.off()

# Plot the posterior mean of the intensity of accidental fires
png(file = "CLMFires_accidental.png")
print(
  spplot(grid.pr, c("acc.pr"),  
         at=com.sc,
         col.regions = c(ora), 
         main = c("Accidental Fires"))
)
dev.off()

# Plot the posterior mean of the intensity of intentional fires
png(file = "CLMFires_intentional.png")
print(
  spplot(grid.pr, c("int.pr"),  
         at=com.sc,
         col.regions = c(reds), 
         main = c("Intentional Fires"))
)
dev.off()


# Plot the posterior mean of the intensity of other fires
png(file = "CLMFires_other.png")
print(
  spplot(grid.pr, c("oth.pr"),  
         at=com.sc,
         col.regions = c(gre), 
         main = c("Other Fires"))
)
dev.off()




# Plot the posterior mean of the shared effect
png(file = "CLMFires_shared.png")
print(
  spplot(grid.pr, c("shared"), 
         col.regions = grn, 
         main = c("Shared effect"))
)
dev.off()

# Plot  the specific effect accidental fires
png(file = "CLMFires_acc_specific.png")
print(
  spplot(grid.pr, c("acc.spec"), 
         col.regions = ora, 
         main = c("Accidental specific effect"))
)
dev.off()

# Plot the specific effect intentional fires
png(file = "CLMFires_int_specific.png")
print(
  spplot(grid.pr, c("int.spec"), 
         col.regions = reds, 
         main = c("Intentional specific effect"))
)
dev.off()

# Plot the specific effect other fires
png(file = "CLMFires_Other_specific.png")
print(
  spplot(grid.pr, c("oth.spec"), 
         col.regions = gre, 
         main = c("Other fires specific effect"))
)
dev.off()

# Save results 
save.image("CLMFires_res_10.RData")

#Time difference 
t2 - t1




