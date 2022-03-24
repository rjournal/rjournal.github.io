library(rgdal)
library(ggplot2)
library(rgeos)
library(spatstat)
library(maptools)
library(raster)
library(ggpubr)
library(kableExtra)
library(spNetwork)

setwd("C:/Users/GelbJ/Desktop/paper_spNetwork")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Calculating the times for setting 1 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# road_network <- readOGR("Data/geobase_mtl.shp")
# accidents <- readOGR("Data/accidents_2012_2018.shp")
# accidents$AN <- as.numeric(accidents$AN)
# accidents$nb_bicycle <- as.numeric(accidents$nb_bicycle)
# 
# roads <- subset(road_network,road_network$CLASSE < 8)
# roads <- subset(roads, gIsSimple(roads,byid=T))
# bike_acc <- subset(accidents, accidents$AN>=2017 & accidents$nb_bicycle>0)
# 
# ## splitting the lines as lixels ##
# lixels <- lixelize_lines(roads,100,50)
# 
# ## extracing the center of lixels as sampling points ##
# sample_pts <- lines_center(lixels)

load("Data/Speed_dataset.RData")


## calculating the times ! ##
cores <- seq(1,8)
methods <- c('simple','discontinuous','continuous')
shapes <- c(10,12,14,16,18)


results <- list()

for(method in methods){
  print(paste("----Iterating on method : ",method,sep=""))
  for(core in cores){
    print(paste("    ----Iterating on core : ",core,sep=""))
    for (shape in shapes){
      print(paste("    ----Iterating on shape : ",shape,sep=""))
      future::plan(future::multisession(workers=core))
      t1 <- Sys.time()
      densities <- spNetwork::nkde.mc(lines = roads,
                                      events = bike_acc,
                                      w = rep(1,nrow(bike_acc)),
                                      samples = sample_pts,
                                      kernel_name = "quartic",
                                      bw = 200,
                                      method = method,
                                      div = "bw",
                                      digits = 2, tol = 0.01,
                                      agg = 10, grid_shape = c(shape,shape),
                                      max_depth = 15, sparse = TRUE,
                                      verbose = TRUE,
                                      check = FALSE)
      t2 <- Sys.time()
      future:::ClusterRegistry("stop")
      ecart <- difftime(t2,t1,units = "secs")
      results[[length(results)+1]] <- c(method,core,shape,ecart)
    }
  }
}

df <- data.frame(do.call(rbind,results))
names(df) <- c("method","cores","shape","duration")

save(df,file="durations.rda")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Calculating the times for setting 2 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

methods <- c("simple","discontinuous","continuous")
distances <- seq(500,7000,500)

time_results <- list()
startpt <- bike_acc[802,]

for(dist in distances){
  print(paste("---- Working on distance : ",dist,sep=""))
  buff <- gBuffer(startpt,width = dist)
  sel_events <- subset(bike_acc,as.vector(gIntersects(buff,bike_acc,byid=TRUE)))
  sel_lines <- subset(roads,as.vector(gIntersects(buff,roads,byid=TRUE)))
  n_pt <- nrow(sel_events)
  n_line <- nrow(sel_lines)
  
  lixels <- lixelize_lines(sel_lines,100,50)
  sample_pts <- lines_center(lixels)
  
  for(method in methods){
    print(paste("---------- Working on method : ",method,sep=""))
    t1 <- Sys.time()
    densities <- spNetwork::nkde(lines = sel_lines,
                                 events = sel_events,
                                 w = rep(1,nrow(sel_events)),
                                 samples = sample_pts,
                                 kernel_name = "quartic",
                                 bw = 200,
                                 method = method,
                                 div = "bw",
                                 digits = 2, tol = 0.01,
                                 agg = 10, grid_shape = c(5,5),
                                 max_depth = 15, sparse = TRUE,
                                 verbose = TRUE,
                                 check = FALSE)
    t2 <- Sys.time()
    ecart <- difftime(t2,t1,units = "secs")
    time_results[[length(time_results)+1]] <- c(method,dist,n_pt,n_line,ecart)
  }
}

df2 <- data.frame(do.call(rbind,time_results))
names(df2) <- c("method","distance","n_pt","n_line","ecart")

save(df2,file="durations2.rda")