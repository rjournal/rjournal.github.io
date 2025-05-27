# This script is constructed for replicating the computations in the paper
# titled "GeoAdjust: Adjusting for Positional Uncertainty in Geostatistial
#         Analysis of DHS Data"

# This script illustrates the implementation of the package functions based on
# Nigeria 2018 household survey of the Demographic and Health Surveys (DHS)
# program. Access for DHS data sets requires applying for permission. A step by
# step guidance to the application procedure can be found in
# https://dhsprogram.com/data/new-user-registration.cfm. Therefore, it is not
# possible for us to share the data set.
#################

# Listing-8 #########
library(haven)
library(sf)
library(GeoAdjust)
corList = st_read("DHS/NG_2018_DHS_02242022_98_147470/NGGE7BFL/NGGE7BFL.shp")

educationData = read_dta("NGIR7BDT/NGIR7BFL.DTA")

# extract cluster level information:
clusterData = data.frame(clusterIdx = corList$DHSCLUST,
                         urban = corList$URBAN_RURA,
                         long = corList$LONGNUM,
                         lat = corList$LATNUM,
                         admin1 = corList$ADM1NAME)

#  extract individual level information:
individualData = data.frame(clusterIdx = educationData$v001,        # cluster ID
                            age = educationData$v012,               # age
                            secondaryEducation = educationData$v106)# v106

#####################

# Listing-9 #########
# reading admin0 and admin2 shape files :
admin0 = st_read("gadm40_NGA_0.shp")

admin2 = st_read("gadm40_NGA_2.shp")

# remove the lake
admin2 = admin2[-160,] # Nigeria map has a large lake
# The lake corresponds to polygon 160

# reading the covariate raster:
library(terra)
r = terra::rast("Nga_ppp_v2c_2015.tif")

#####################

# Listing-10 #########

# subset data to the age interval:
individualData = subset(individualData, age <= 49 & age >=20)

# number of 20-49 years old women who completed
# secondary education in each household:
individualData$ys = as.numeric((individualData$secondaryEducation>=2))

# merge the cluster level data with the
# subsetted individual level data,
# with respect to the cluster ID:
individualData = merge(individualData, clusterData, by = "clusterIdx")

# add number of trials (for binomial response)
individualData$Ntrials = 1

# aggregate the survey responses to the cluster centers
answers_x = aggregate(individualData$ys,
                      by = list(clusterID = individualData[, 1]),
                      FUN = sum)


answers_n= aggregate(individualData$ys,
                     by = list(clusterID = individualData[, 1]),
                     FUN = length)

# merge
answers_joint = merge(answers_x, answers_n,
                      by="clusterID")

colnames(answers_joint) = c("clusterID", "ys", "ns")

#####################

# Listing-11 #########

# initial data frame
nigeria.data = data.frame(clusterID = corList$DHSCLUST,
                          long = corList$LONGNUM,
                          lat = corList$LATNUM)

# add ys and ns
nigeria.data = merge(nigeria.data, answers_joint, by="clusterID", all=T)

# add strata:
nigeria.data$urbanRuralDHS = corList$URBAN_RURA

# cluster cordinates as an sf POINT object in degrees
crs_degrees = "+proj=longlat +datum=WGS84"
pointsDegrees = data.frame(long = nigeria.data$long,
                           lat = nigeria.data$lat)

pointsDegrees = st_as_sf(pointsDegrees, coords=c("long","lat"),
                         crs = crs_degrees)
#####################

# Listing-12 #########

# add polygon IDs :
admin2$OBJECTID = 1:length(st_geometry(admin2))

#points (cluster centers) within the polygons (admin2) :
check1 = st_join(pointsDegrees, admin2)

# the non-matching ones :
idx = which(is.na(check1$NAME_2))

# drop from the main data set :
nigeria.data = nigeria.data[-idx, ]
pointsDegrees = pointsDegrees[-idx, ]

# cluster cordinates as an sf POINT object in target_crs
target_crs = "+units=km +proj=utm +zone=37 +ellps=clrk80
              +towgs84=-160,-6,-302,0,0,0,0 +no_defs"

pointsKM = st_transform(pointsDegrees, target_crs)
#####################

# Listing-1 #########
# Set target geometry
target_crs = "+units=km +proj=utm +zone=37 +ellps=clrk80
              +towgs84=-160,-6,-302,0,0,0,0 +no_defs"

# transform admin0 borders into target_crs:
admin0_trnsfrmd = sf::st_transform(admin0, target_crs)

# construct the mesh
mesh.s = meshCountry(admin0= admin0_trnsfrmd,
                     max.edge = c(25, 50),
                     offset = -.08, cutoff=4,
                     target_crs = target_crs)

#####################
# Listing-2 #########
# read the covariate raster
library(terra)
r = terra::rast("Nga_ppp_v2c_2015.tif")

target_crs = "+units=km +proj=utm +zone=37 +ellps=clrk80
              +towgs84=-160,-6,-302,0,0,0,0 +no_defs"

inputData = prepareInput(response=list(ys=nigeria.data$ys,ns=nigeria.data$ns),
                         locObs = pointsKM,
                         likelihood = 1,
                         urban = nigeria.data$urbanRuralDHS,
                         mesh.s = mesh.s,
                         adminMap = admin2,
                         covariateData = list(r),
                         target_crs = target_crs)

#####################
# Listing-3 #########

# estimating the parameters
est = estimateModel(
  data = inputData,
  priors = list(beta = c(0,1), range = 114),
  USpatial = 1, alphaSpatial = 0.05,
  UNugget = 1, alphaNug = 0.05,
  n.sims = 1000)

#####################
# Listing-4 #########
# the output of estimateModel() function:
names(est)
#[1] "res"        "obj"        "draws"      "likelihood"

print(est)
# GeoAdjust::estimateModel()
# ----------------------------------
#   Likelihood :          binomial
# ----------------------------------
# parameter estimate    95% CI length
# range     69.7677     NA
# sigma     2.1462      NA
# intercept -1.2998     0.6578
# beta1     0.0069      0.0044
# ----------------------------------

#####################
# Listing-5 #########

# raster and the prediction coordinates:
predComponents = gridCountry(admin0 = admin0,
                             res = 5,
                             target_crs = target_crs)

names(predComponents)
#[1] "loc.pred" "predRast"

# the sf multipoint object containing the prediction locations
loc.pred = predComponents[["loc.pred"]]

print(loc.pred)
# Simple feature collection with 80201 features and 0 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -3803.253 ymin: 565.4467
# xmax: -2223.253 ymax: 1825.447
# Projected CRS: +units=km +proj=utm +zone=37 +ellps=clrk80
#                +towgs84=-160,-6,-302,0,0,0,0 +no_defs
# First 10 features:
#   geometry
# 1  POINT (-3803.253 1825.447)
# 2  POINT (-3798.253 1825.447)
# 3  POINT (-3793.253 1825.447)
# 4  POINT (-3788.253 1825.447)
# 5  POINT (-3783.253 1825.447)
# 6  POINT (-3778.253 1825.447)
# 7  POINT (-3773.253 1825.447)
# 8  POINT (-3768.253 1825.447)
# 9  POINT (-3763.253 1825.447)
# 10 POINT (-3758.253 1825.447)

predRast = predComponents[["predRast"]]

print(predRast)
# class       : SpatRaster
# dimensions  : 253, 317, 1  (nrow, ncol, nlyr)
# resolution  : 5, 5  (x, y)
# extent      : -3805.753, -2220.753, 562.9467, 1827.947
# (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0
#               +units=km +no_defs

#####################
# Listing-6 #########

predictions = predRes(obj = est[["obj"]] , predCoords = loc.pred,
                      draws = est[["draws"]],
                      covariateData = list(r),
                      mesh.s = mesh.s, flag = 1)

head(predictions)
# mean    median         sd       lower     upper
# [1,] 0.2155746 0.2143489 0.02934076 0.165192003 0.2764130
# [2,] 0.3471947 0.2220279 0.33308573 0.001691273 0.9860286
# [3,] 0.3442247 0.2269676 0.32558640 0.002292870 0.9839695
# [4,] 0.3447684 0.2402942 0.32414517 0.001921544 0.9817542
# [5,] 0.3379962 0.2405484 0.31493574 0.002677005 0.9755113
# [6,] 0.3329591 0.2317648 0.30754117 0.003479042 0.9637670

dim(predictions)
#[1] 80201     5


#####################
# Listing-7 #########
admin1 = st_read("gadm40_NGA_shp/gadm40_NGA_1.shp")

plotPred(pred = predictions,
         predRaster = predRast,
         admin0 = admin0,
         admin1 = admin1,
         admin2 = admin2,
         rmPoly = 160,
         target_crs = target_crs)







