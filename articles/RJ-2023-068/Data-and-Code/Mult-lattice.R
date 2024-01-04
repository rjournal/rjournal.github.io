##############################################################################
############ Multivariate disease mapping analysis using R-INLA ##############
##############################################################################

# In this script, a multivariate disease mapping analysis is performed. Cases
# of three different diseases are considered: Bucal cavity, Esophagus and 
# Stomach cancer in Spain per provinces. The data can be found in: 
# https://github.com/becarioprecario/INLAMCMC_spatial_examples/tree/master/dismap_example


# Load necessary packages
library(INLA)
library(spdep)
library(RColorBrewer)
source("utils.R")

# Load data from:
load("dismap_sim_data.RData")
class(OyE.sim)
spplot(OyE.sim)

# Set shorter names
names(OyE.sim)[1:6] <- c("Obs.Cb", "Esp.Cb", "Obs.Eso",
  "Esp.Eso", "Obs.Est", "Esp.Est")

# Create a dataset for INLA (n x 3)
n <- nrow(OyE.sim)
d <- list(OBS = create_multivariate_data(as.data.frame(OyE.sim)[, c("Obs.Cb", "Obs.Eso", "Obs.Est")]))

# Expected cases
d$EXP <- c(OyE.sim$Esp.Cb, OyE.sim$Esp.Eso, OyE.sim$Esp.Est)

# Area ID
d$AREAID <- rep(1:n, 3)

# Define groups in data
d$r <- rep(1:3, each = n)
d$rf <- as.factor(d$r)

# Build the adjacency matrix
nb.spain <- poly2nb(OyE.sim)
W <- as(nb2mat(nb.spain, style = "B"), "Matrix")

# Add an index to copy the shared spatial effect
d$copy1 <- d$AREAID
d$copy1[-c(1:n)] <- NA 
d$copy2 <- d$AREAID
d$copy2[-(n + 1:n)] <- NA 
d$copy3 <- d$AREAID
d$copy3[-(2 * n + 1:n)] <- NA 

# Add an index for disease-specific spatial effects
d$spatial2 <- d$copy2
d$spatial3 <- d$copy3

# Uniform priors for the standard deviation
prior.prec <- list(
    prior = "expression:
    log_dens = 0 - log(2) - theta / 2;
    return(log_dens);",
  initial = 0)

# Formulas for the model
form <- OBS ~ -1 + rf + 
  f(copy1, model = "besag", graph = W, 
    hyper = list(prec = prior.prec)) +
  f(copy2, copy = "copy1", fixed = TRUE) +
  f(copy3, copy = "copy1", fixed = TRUE) +
  f(spatial2, model = "besag", graph = W, 
    hyper = list(prec = prior.prec)) +
  f(spatial3, model = "besag", graph = W, 
    hyper = list(prec = prior.prec)) 


# Fit model
t1 <-Sys.time()
res <- inla(
  formula =  form, 
  data = d, 
  family = rep("poisson", 3), 
  E = d$EXP)
t2 <-Sys.time()

# Extract the summary of the adjusted results
summary(res)

# Extract the means of the shared and specific effects
OyE.sim$Shared <- res$summary.random$copy1[, "mean"]
OyE.sim$Spatial2 <- res$summary.random$spatial2[, "mean"]
OyE.sim$Spatial3 <- res$summary.random$spatial3[, "mean"]

# Create palletes to plot
blu <- brewer.pal(9, "Blues")
blu <- colorRampPalette(blu)(20)
pin <- brewer.pal(9, "RdPu")
pin <- colorRampPalette(pin)(20)
pur <- brewer.pal(9, "Purples")
pur <- colorRampPalette(pur)(20)
gre <- brewer.pal(9, "Greys")
gre <- colorRampPalette(gre)(20)[5:20]

#'at' values
at.values <- seq(-0.45,0.45, length.out = 20)

# Plot the shared sp effect
png(file = "Spain_Shared.png", width = 360, height = 360)
print(
  spplot(OyE.sim, c("Shared"), main = c("Shared effect"),
    col.regions = pur, at = at.values)
  )
dev.off()

# Plot the specific esophagus effect
png(file = "Spain_Esophagus_Specific.png", width = 360, height = 360)
print(
  spplot(OyE.sim, c("Spatial2"),
    main = c("Esophagus specific effect"),
    col.regions = pin, at = at.values)
  )
dev.off()


#Plot the specific stomach effect
png(file = "Spain_Stomach_specific.png", width = 360, height = 360)
print(
  spplot(OyE.sim, c("Spatial3"),
    main = c("Stomach specific effect"),
    col.regions = blu,  at = at.values)
  )
dev.off()


#Plot the shared and specific effects (All together)
pdf(file = "spain_estimates.pdf", width = 12, height = 6)
print(spplot(OyE.sim, c("Shared", "Spatial2", "Spatial3"),
            names.attr = c("Shared", "Esophagus specific", "Stomach specific"),
            col.regions = rev(gray(0:20/20)[2+1:16])),
  at = seq(-0.45,0.45, length.out = 16))
dev.off()

# Save results 
save.image("Lattice_Dataset_res.RData")

#Time difference 
t2 - t1

