if(!require(fields)) {
  install.packages("fields")
  library(fields)
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(RColorBrewer)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if(!require(Kernelheaping)) {
  install.packages("Kernelheaping")
  library(Kernelheaping)
}
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(terra)) {
  install.packages("terra")
  library(terra)
}
if(!require(sf)) {
  install.packages("sf")
  library(sf)
}

if(!require(sp)) {
  install.packages("sp")
  library(sp)
}

if(!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
}


if(!require(rmapshaper)) {
  install.packages("rmapshaper")
  library(rmapshaper)
}

################################################################################
# Download Data: 
# https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_LOR_2015_12.zip
# http://download.geofabrik.de/europe/germany/berlin.html
# https://www.statistik-berlin-brandenburg.de/opendata/EWR201512E_Matrix.csv
# https://www.statistik-berlin-brandenburg.de/opendata/EWRMIGRA201512H_Matrix.csv
################################################################################

# CHANGE PATH HERE
setwd(file.path(getwd(), 
                "KH_data"))
getwd()

# load data
data <- read.csv2("EWR201512E_Matrix.csv")
# load Berlin - 'LOR'-Shapefile ("Lebensweltlich orientierte Räume")
Berlin <- read_sf("RBS_OD_LOR_2015_12/RBS_OD_LOR_2015_12.shp")
Berlin <- st_transform(Berlin, CRS("+proj=longlat +datum=WGS84"))

Berlin$E_E65U80 <- data$E_E65U80
Berlin$E_E65U80density <- Berlin$E_E65U80 / sum(Berlin$E_E65U80)

#Choropleth
ggplot(Berlin) +
  geom_sf(data = Berlin, mapping = aes(fill = E_E65U80density)) + 
  ggtitle("Number of Inhabitants between 65 and 80 years by LOR", 
          "Choropleth Map") +
  scale_fill_gradientn(colours = c("#FFFFFF", "#5c87c2", "#19224e"), "Density") +
  xlab("Longitude")+ylab("Latitude")+
  coord_sf()

Berlin.shp <- as_Spatial(Berlin)
dataIn <- lapply(Berlin.shp@polygons, function(x) x@labpt) %>%
  do.call(rbind, .) %>% cbind(data$E_E65U80)

# takes a few minutes
est <- dshapebivr(data = dataIn, burnin = 5, samples = 10, 
                  adaptive = FALSE, shapefile = Berlin.shp, 
                  gridsize = 325, boundary = TRUE)


Kdata <- data.frame(expand.grid(long = est$Mestimates$eval.points[[1]],
                                lat = est$Mestimates$eval.points[[2]]),
                    Density = est$Mestimates$estimate %>% as.vector) %>% 
  filter(Density > 0)
Kdata$Density <- Kdata$Density*(est$gridx[2] - est$gridx[1])*
  (est$gridy[2] - est$gridy[1])

#Kernel Density
ggplot(Kdata) +
  geom_raster(aes(long, lat, fill = Density)) + 
  ggtitle("Density of Inhabitants between 65 and 80 Years", 
          "obtained by using Kernel Heaping Algorithm") +
  scale_fill_gradientn(colours = c("#FFFFFF", "#5c87c2", "#19224e"))+
  xlab("Longitude")+ylab("Latitude")+
  geom_sf(data = Berlin, fill = NA) + 
  coord_sf()



######################################
######################################
#Part 2
######################################
######################################

#load data
Berlin <- read_sf("RBS_OD_LOR_2015_12/RBS_OD_LOR_2015_12.shp")
Berlin <- st_transform(Berlin, CRS("+proj=longlat +datum=WGS84"))

BerlinN <- read_sf("berlin-latest-free.shp/gis_osm_landuse_a_free_1.shp")
BerlinWater <- read_sf("berlin-latest-free.shp/gis_osm_water_a_free_1.shp")

BerlinN <- BerlinN[!(BerlinN$fclass == "residential"), ]
BerlinGreen <- BerlinN[(BerlinN$fclass %in% c("forest", "grass", "nature_reserve",
                                                   "park", "cemetery","allotments","farm",
                                                   "meadow","orchard","vineyard","heath")), ]
BerlinOther <- BerlinN[!(BerlinN$fclass %in% c("forest", "grass", "nature_reserve",
                                                    "park", "cemetery","allotments","farm",
                                                    "meadow","orchard","vineyard","heath")), ]

BerlinGreen <- st_transform(
  rmapshaper::ms_simplify(BerlinGreen, keep = 0.001, keep_shapes=FALSE),
                           CRS("+proj=longlat +datum=WGS84"))
BerlinOther <- st_transform(
  rmapshaper::ms_simplify(BerlinOther, keep = 0.001, keep_shapes=FALSE),
                           CRS("+proj=longlat +datum=WGS84"))
BerlinWater <- st_transform(
  rmapshaper::ms_simplify(BerlinWater, keep = 0.001, keep_shapes=FALSE),
                           CRS("+proj=longlat +datum=WGS84"))

BerlinWaterUnsimplified <- st_transform(BerlinWater,
                                       CRS("+proj=longlat +datum=WGS84"))
BerlinWaterSimplified <- st_transform(rmapshaper::ms_simplify(BerlinWater, 
                                                              keep = 0.002, 
                                                              keep_shapes = FALSE),
                                     CRS("+proj=longlat +datum=WGS84"))



p1 <- ggplot() +
  ggtitle("Original Version") +
  geom_sf(fill = "white", 
               data = Berlin) +
  geom_sf(fill = "deepskyblue3", 
          data = BerlinWaterUnsimplified,
          alpha = 0.25) +
  xlab("Longitude")+ ylab("Latitude")+
  coord_sf()

p2 <- ggplot() +
  ggtitle("Simplified") +
  geom_sf(fill = "white", 
          data = Berlin) +
  geom_sf(fill = "deepskyblue3", 
          data = BerlinWaterSimplified,
          alpha = 0.25) +
  xlab("Longitude")+ ylab("Latitude")+
  coord_sf()


(p1+p2)+
  plot_annotation(title = 'Water Areas in Berlin')

BerlinMigration <- read.csv2("EWRMIGRA201512H_Matrix.csv")

BerlinMigration$RAUMID <- as.character(BerlinMigration$RAUMID)
BerlinMigration$RAUMID[nchar(BerlinMigration$RAUMID) == 7] <- 
  paste0("0", BerlinMigration$RAUMID[nchar(BerlinMigration$RAUMID) == 7])
BerlinMigration <- BerlinMigration[order(BerlinMigration$RAUMID), ]

Berlin$turkDensity <- BerlinMigration$HK_Turk / BerlinMigration$MH_E
 
# This may take a few minutes
BerlinOther <- st_intersection(BerlinOther, Berlin)
BerlinGreen <- st_intersection(BerlinGreen, Berlin)
BerlinWater <- st_intersection(BerlinWater, Berlin)

ggplot() +
  geom_sf(data = Berlin, mapping = aes(fill = turkDensity)) + 
  ggtitle("Proportion of Turks in Population with Migration Background", 
          subtitle = "Choropleth Map") +
  scale_fill_gradientn(colours = c("#FFFFFF", "coral1"), "Density") +
  geom_sf(fill = "grey20", alpha = .25,color = NA, 
               data = BerlinOther) +
  geom_sf(fill = "darkolivegreen3", color = NA,
               data = BerlinGreen, alpha = 0.25) +
  geom_sf(fill = "deepskyblue3",color = NA,
               data = BerlinWater, alpha = 0.25) +
  xlab("Longitude")+ ylab("Latitude")+
  coord_sf()


BerlinUnInhabitated <- bind_rows(BerlinWater, BerlinGreen, BerlinOther)
BerlinUnInhabitated <- as_Spatial(BerlinUnInhabitated)

dataTurk <- cbind(t(sapply(1:length(Berlin.shp@polygons),
                           function(x) Berlin.shp@polygons[[x]]@labpt)),
                  BerlinMigration$HK_Turk, BerlinMigration$MH_E)

EstTurk <- dshapebivrProp(data = dataTurk, burnin = 5, samples = 10,
                          adaptive = FALSE, deleteShapes = BerlinUnInhabitated,
                          shapefile = Berlin.shp, gridsize = 325, boundary = TRUE,
                          numChains = 4, numThreads = 4)


#prepare map data
gridBerlin <- expand.grid(long = EstTurk$Mestimates$eval.points[[1]],
                          lat = EstTurk$Mestimates$eval.points[[2]])
KdataTurk <- data.frame(gridBerlin,
                        
                        Proportion = EstTurk$proportions %>% as.vector)  %>% 
  filter(Proportion > 0)


ggplot() +
  geom_tile(data = KdataTurk, aes(long, lat, fill = Proportion)) + 
  ggtitle("Proportion of Turks in Population with Migration Background", 
          "obtained by using Kernel Heaping Algorithm") +
  scale_fill_gradientn(colours = c("#FFFFFF", "coral1"), "n") +
  geom_sf(fill = "grey20", alpha = .25,color = NA,
          data = BerlinOther) +
  xlab("Longitude")+ ylab("Latitude")+
  geom_sf(fill = "darkolivegreen3", color = NA,
          data = BerlinGreen, alpha = .25) +
  geom_sf(fill = "deepskyblue3",color = NA,
          data = BerlinWater, alpha = .25) +
  xlab("Longitude")+ ylab("Latitude")+
  coord_sf()




######################################
######################################
#Part 3
######################################
######################################

dataArab <- cbind(t(sapply(1:length(Berlin.shp@polygons),
                           function(x) Berlin.shp@polygons[[x]]@labpt)), BerlinMigration$HK_Arab)
dataSU <- cbind(t(sapply(1:length(Berlin.shp@polygons),
                         function(x) Berlin.shp@polygons[[x]]@labpt)), BerlinMigration$HK_EheSU)
dataPol <- cbind(t(sapply(1:length(Berlin.shp@polygons),
                          function(x) Berlin.shp@polygons[[x]]@labpt)), BerlinMigration$HK_Polen)

EstArab <- dshapebivr(data = dataArab, burnin = 5, samples = 10, adaptive = FALSE,
                      shapefile = Berlin.shp, gridsize = 325, boundary = TRUE)
EstSU <- dshapebivr(data = dataSU, burnin = 5, samples = 10, adaptive = FALSE,
                    shapefile = Berlin.shp, gridsize = 325, boundary = TRUE)
EstPol <- dshapebivr(data = dataPol, burnin = 5, samples = 10, adaptive = FALSE,
                     shapefile = Berlin.shp, gridsize = 325, boundary = TRUE)

Visquantile <- 0.95

KdataArab <- data.frame(gridBerlin, Density = EstArab$Mestimates$estimate %>% as.vector) %>% filter(Density > 0) %>% filter(Density > quantile(Density, Visquantile)) %>% 
  mutate(Density = "Arabian countries")
KdataSU <- data.frame(gridBerlin, Density = EstSU$Mestimates$estimate %>% as.vector) %>%
  filter(Density > 0) %>% filter(Density > quantile(Density, Visquantile)) %>%
  mutate(Density = "Former Soviet Union")
KdataPol <- data.frame(gridBerlin, Density = EstPol$Mestimates$estimate %>% as.vector) %>%  
  filter(Density > 0) %>% filter(Density > quantile(Density, Visquantile)) %>%
  mutate(Density = "Poland")


ggplot() +
  geom_sf(data = Berlin) +
  geom_tile(aes(long, lat, fill = Density), data = KdataArab, alpha = 0.6) + 
  geom_tile(aes(long, lat, fill = Density), data = KdataSU, alpha = 0.6) + 
  geom_tile(aes(long, lat, fill = Density), data = KdataPol, alpha = 0.6) +
  ggtitle("Hotspots of Inhabitants with different Migration Background", 
          "obtained by using Kernel Heaping Algorithm") +
  geom_sf(fill = "grey20", alpha = .25,color = NA,
          data = BerlinOther) +
  xlab("Longitude")+ ylab("Latitude")+
  geom_sf(fill = "darkolivegreen3", color = NA,
          data = BerlinGreen, alpha = .25) +
  geom_sf(fill = "deepskyblue3",color = NA,
          data = BerlinWater, alpha = .25) +
  xlab("Longitude")+ ylab("Latitude")+
  coord_sf()


