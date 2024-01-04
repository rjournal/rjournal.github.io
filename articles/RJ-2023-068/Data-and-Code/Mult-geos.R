##############################################################################
############# Multivariate geostatistical analysis using R-INLA ##############
##############################################################################

# In this script, a multivariate geostatistical analysis of the concentrations
# of two heavy metals (Lead and Zinc) of the Meuse river dataset (available 
# in the sp package) is performed using R-INLA.  

# Load necessary packages
library(sp)
library(INLA)
library(maptools)
library(RColorBrewer)
library(gstat)
library(inlabru)
library(ggplot2)

# Load the data
data(meuse); 
class(meuse)
str(meuse)

# Create the spatial object
coordinates(meuse) = ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")


##  Create the mesh for the INLA analysis

# Create a grid
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) <- TRUE
n.pred <- nrow(meuse.grid)

# Create the boundary
meuse.poly <- as(meuse.grid, "SpatialPolygons")
#Here a vector of 1s is used to put together all the little squares
meuse.bdy <-
  unionSpatialPolygons(meuse.poly, 
    rep(1, length(meuse.poly) ))

# Extract coordinates of boundary
bdy <- meuse.bdy@polygons[[1]]@Polygons[[1]]@coords

# Create the mesh
mesh<- inla.mesh.2d(
  boundary = meuse.bdy,
  loc = coordinates(meuse),
  max.edge = c(250, 500), 
  offset = c(250, 500), 
  n=c(32, 32))


# Plot the mesh and the data 
png(file = "meuse_Only2_mesh.png", width =720, height = 360)
multiplot(
  ggplot() + gg(mesh) + labs(title = "Mesh" ) +
    gg(meuse.bdy, col="darkgreen", size=1) + coord_fixed(ratio = 1),
   ggplot() + gg(mesh) + gg(meuse, col="blue") +
    gg(meuse.bdy, col="darkgreen", size=1) + coord_fixed(ratio = 1)+
    labs(title = "Mesh and data" ),
  cols=2)
dev.off()

# Number of mesh nodes
nv <- mesh$n


# Create the spde model for the spatial effects
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(2394.16, 0.95), # P(practic.range < 2394.16) = 0.95 
  # 4788.319 meters is aproximately the max distance
  prior.sigma = c(1000, 0.05)) # P(sigma > 1000) = 0.05
  # An sd greater than 1000 would be a really 
  # high value, which is barely expected. 

# NOTE: If you want default priors, comment the above lines and 
# uncomment the following ones. 
# #Create spde with default prior values 
# spde <- inla.spde2.matern(mesh = mesh, alpha=2, constr = TRUE)

######## Build stack object

# Create the projector matrix 
A.m <- inla.spde.make.A(mesh= mesh, loc = coordinates(meuse))

# Create the stack object for the cadmium
stk.lead <- inla.stack(
  data = list(log.y = cbind(log(meuse$lead), NA)),
  A = list(A.m, 1),
  effects = list(
    spatial.field.lead = 1:spde$n.spde,
    data.frame(Intercept.lead = 1, 
      dist.lead = meuse$dist)),
  tag = "Lead")

# Create the stack object for the zinc
stk.zinc <- inla.stack(
  data = list(log.y = cbind(NA, log(meuse$zinc))),
  A = list(A.m, A.m, 1),
  effects = list(
    spatial.field.zinc = 1:spde$n.spde, 
    base.copy.zinc = 1:nv,
    data.frame(Intercept.zinc = 1, 
      dist.zinc = meuse$dist)),
  tag = "Zinc")


# Create the projector matrix for the prediction
A.pr <- inla.spde.make.A(mesh= mesh, loc = coordinates(meuse.grid))

# Prepare the data for the prediction
y.pred <- matrix(NA, nrow = nrow(meuse.grid), ncol = 2)

# Build predicting stack for the cadmium
stk.lead.pr <- inla.stack(
  data = list(log.y = y.pred),
  A = list(A.pr, 1),
  effects = list(
    spatial.field.lead = 1:spde$n.spde,
    data.frame(Intercept.lead = 1, 
      dist.lead = meuse.grid$dist)),
  tag = "Lead.pred")

# Build predicting stack for the zinc
stk.zinc.pr <- inla.stack(
  data = list(log.y = y.pred),
  A = list(A.pr, A.pr, 1),
  effects = list(
    spatial.field.zinc = 1:spde$n.spde, 
    base.copy.zinc = 1:nv,
    data.frame(Intercept.zinc = 1, 
      dist.zinc = meuse.grid$dist)),
  tag = "Zinc.pred")


# Stack for the shared effect
stk.shared <- inla.stack(
  data = list(log.y = y.pred),
  A = list(A.pr),
  effects = list(spatial.field.lead = 1:spde$n.spde),
  tag = "Shared")

# Stack for the specific sp effect zinc
stk.zinc.spec <- inla.stack(
  data = list(log.y = y.pred),
  A = list(A.pr),
  effects = list(spatial.field.zinc = 1:spde$n.spde),
  tag = "Zinc.spec")


# Put all the stacks together
join.stack <- inla.stack(
  stk.lead, stk.zinc, 
  stk.zinc.pr, stk.lead.pr,
  stk.shared, stk.zinc.spec)


t1 <-Sys.time()
#Build the formula
form <- log.y ~ -1 + Intercept.lead + Intercept.zinc + #Intercept.3 + Intercept.4 + 
  dist.lead + dist.zinc + #dist.3 + dist.4 +
  f(spatial.field.lead, model = spde) +
  f(spatial.field.zinc, model = spde) + 
  f(base.copy.zinc, copy = "spatial.field.lead", fixed = TRUE)  

# Fix Gaussian precision to zero
zero.prec <- list(hyper = list(prec = list(initial = 15, fixed = TRUE)))

#Estimation
t1 <- Sys.time(); t1
meuse.res <- inla(
  formula=form, verbose = FALSE, 
  data = inla.stack.data(join.stack, spde = spde),
  family = rep("gaussian", 2), 
  control.family = list(zero.prec, zero.prec),
  control.predictor = list(A = inla.stack.A(join.stack), 
    compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, 
    cpo = TRUE, mlik = TRUE, po = TRUE))#
t2 <- Sys.time(); t2

summary(meuse.res)

ti <- t2-t1; ti
print(
  paste0("This model has used ", 
         ti, 
         " cpu time. And have finished at ", 
         Sys.time())
  )

# Create palletes to plot
blu <- brewer.pal(9, "Blues")
blu <- colorRampPalette(blu)(20)
ora <- brewer.pal(9, "Oranges")
ora <- colorRampPalette(ora)(20)
grn <- brewer.pal(9, "Greens")
grn <- colorRampPalette(grn)(20)
yel <- brewer.pal(9, "YlOrRd")
yel <- colorRampPalette(yel)(20)
gre <- brewer.pal(9, "Greys")
gre <- colorRampPalette(gre)(20)[5:20]



# Extract the estimates for plotting lead density
idx.lead <- inla.stack.index(join.stack, 'Lead.pred')$data
meuse.grid$lead.pr <- meuse.res$summary.fitted.values[idx.lead, "mean"]

# Extract the estimates for plotting zinc density
idx.zinc <- inla.stack.index(join.stack, 'Zinc.pred')$data
meuse.grid$zinc.pr <- meuse.res$summary.fitted.values[idx.zinc, "mean"]

# Extract the post. mean of the shared effect
idx.shared <- inla.stack.index(join.stack, 'Shared')$data
meuse.grid$shared <- meuse.res$summary.fitted.values[idx.shared, "mean"]

# Extract the post. mean of the zinc specific effect 
idx.zinc.spec <- inla.stack.index(join.stack, 'Zinc.spec')$data
meuse.grid$zinc.spec <- meuse.res$summary.fitted.values[idx.zinc.spec, "mean"]


# Plot the posterior estimated mean of the lead intensity
png(file = "Meuse_lead_estimate.png")
print(
  spplot(meuse.grid, c("lead.pr"), 
         col.regions = c(blu), 
         main = c("Lead"))
)
dev.off()

# Plot the posterior estimated mean of the zinc intensity
png(file = "Meuse_zinc_estimate.png")
print(
  spplot(meuse.grid, c("zinc.pr"), 
         col.regions = c(gre), 
         main = c("Zinc"))
)
dev.off()

# Plot the shared sp effect
png(file = "Meuse_Shared_effect.png")
print(
  spplot(meuse.grid, c("shared"), 
         col.regions = grn, 
         main = c("Shared effect"))
)
dev.off()

# Plot the zinc specific effect
png(file = "Meuse_Zinc_specific.png")
print(
  spplot(meuse.grid, c("zinc.spec"), 
         col.regions = ora, 
         main = c("Zinc specific effect"))
)
dev.off()

# Save results
save.image("Meuse_2m_res.RData")

# Time difference
t2 - t1




