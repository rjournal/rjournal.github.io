#---- Code for 'rassta: Raster-based Spatial Stratification Algorithms' ----#


## ----code1 (page 4)
# Load the rassta and terra packages
library(rassta)
library(terra)
# Note that terra imports Rcpp, but if Rcpp is not automatically loaded then:
library(Rcpp)
# Get the data required to run the examples from rassta’s installation folder
wasoil <- system.file("exdat/wasoil.zip", package = "rassta")
file.copy(from = wasoil, to = getwd()) # Copy to current working directory
unzip("wasoil.zip") # Extract files
#
# Set seed
set.seed(963)
# Multi-layer SpatRaster with 4 terrain variables
terr.var <- rast(c("height.tif", "midslope.tif", "slope.tif", "wetness.tif"))
# Scale variables to mean = 0 and standard deviation = 1
terr.varscale <- scale(terr.var)
# Dimensionality reduction and estimation of optimum k (max k to evaluate: 12)
terr.som <- som_gap(terr.varscale, xdim = 10, ydim = 10, K.max = 12)
figure(4, d = list(terr.var, terr.som)) # Plot results


## ----code2 (page 5)
# Rasterization of terrain SOM grid and terrain PAM clustering
terr.sompam <- som_pam(ref.rast = terr.var[[1]], kohsom = terr.som$SOM,
                       k = terr.som$Kopt)
figure(5, d = list(terr.sompam, terr.var)) # Plot results


## ----code3 (page 6)
# Multi-layer SpatRaster with 3 sets of classification units
all.cu <- rast(c("climate.tif", "material.tif", "terrain.tif"))
# Stratification units
su <- strata(cu.rast = all.cu)
figure(6, d = list(su, all.cu)) # Plot results


## ----code4 (page 8)
# Multi-layer SpatRaster with 2 climatic variables
clim.var <- rast(c("precipitation.tif", "temperature.tif"))
# Single-layer SpatRaster with 4 climatic classification units
clim.cu <- rast("climate.tif")
# Automatic selection of statistical distribution functions
clim.difun <- select_functions(cu.rast = clim.cu,
                               var.rast = clim.var,
                               mode = "auto")
figure(8, d = list(clim.difun, clim.cu, clim.var)) # Plot results


## ----code5 (page 8-9)
# Multi-layer SpatRaster of climatic variables and classification units
clim.all <- c(clim.var, clim.cu)
# Ouput table from select_functions()
df <-  clim.difun$distfun
# Predicted distribution functions for climatic variables
clim.pdif <- predict_functions(cuvar.rast = clim.all,
                               cu.ind = 3,
                               cu = df$Class.Unit,
                               vars = df$Variable,
                               dif = df$Dist.Func)
figure(9, d = list(clim.pdif, clim.cu)) # Plot results


## ----code6 (page 9)
# Spatial signatures from distribution functions predicted for climatic variables
clim.sig <- signature(pdif.rast = clim.pdif,
                      inprex = paste(seq(1, 4), "_", sep = ""),
                      outname = paste("climate_", seq(1, 4), sep = ""))
figure(10, d = list(clim.sig, clim.cu)) # Plot results


## ----code7 (page 10)
# Multi-layer SpatRaster with spatial signatures of classification units
clim.sig <- rast(list.files(pattern = "climate_")) # For climatic units
mat.sig <- rast(list.files(pattern = "material_")) # For soil parent material units
terr.sig <- rast(list.files(pattern = "terrain_")) # For terrain units
# Single-layer SpatRaster of stratification units
su <- rast("su.tif")
# Landscape similarity to stratification units
su.ls <- similarity(su.rast = su, sig.rast = c(clim.sig, mat.sig, terr.sig),
                    su.code = list(climate = c(1, 1), material = c(2, 2),
                                   terrain = c(3, 3)))
figure(12, d = list(su.ls, su, clim.sig, mat.sig, terr.sig)) # Plot results


## ----code8 (page 11)
# SpatVector with SOC observations for stratification units
soc.obs <- vect("soc.shp")
# Representative SOC observation for each stratification unit
su.obs <- observation(su.rast = su, obs = soc.obs, col.id = 1, col.resp = 2,
                      method = "mls", ls.rast = su.ls$landsim)
figure(13, d = list(su.obs, soc.obs, su)) # Plot results


## ----code9 (page 12)
# Representative sampling location and its buffer area for each stratification unit
su.samp <- locations(ls.rast = su.ls$landsim, su.rast = su, method = "buffer")
figure(14, d = list(su.samp, su)) # Plot results


## ----code10 (page 14)
# Table with the numeric code of stratification units and representative SOC values
su.soc <- su.obs$su_repobs[, c("SU", "soc")]
# engine() requires a (tiled) SpatVector with the boundaries of the area of interest
aoi <- vect("aoi.shp")
# engine() writes results directly on disk
if (dir.exists("soc") == FALSE) {dir.create("soc")}  # Create directory
# Spatial modeling of SOC across the landscape based on 3 winning stratification units
soc <- engine(ls.rast = su.ls$landsim, n.win = 3, su.repobs = su.soc,
              tiles = aoi, outdir = "soc", overwrite = TRUE)
figure(16, d = list(soc, "soc_valid.shp")) # Plot results


## ----code11 (page 14)
# Multi-layer SpatRaster of soil parent material units
mat.cu <- rast("material.tif")
# Binary layers for each soil parent material unit and their maps
mat.sig <- dummies(mat.cu, preval = 100, absval = 0)
figure(17, d = mat.sig) # Plot results


## ----code12 (page 15)
# Single-layer SpatRaster of terrain elevation the 3D SOC map
elev <- rast("elevation.tif")
plot3D(c(elev, soc), z = 1, ex = 0.2, pals = "Fall", rev = TRUE) # 3D map
