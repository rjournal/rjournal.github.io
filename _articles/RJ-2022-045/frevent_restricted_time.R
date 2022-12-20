#' This file provides all the code permitting the reproduction of examples in the article submitted
#' More specifically, we display here the maps and curves of the data, and we seek to detect 
#' multi-pollutant clusters: 
#' 1) on the data from which we have removed the temporal component by averaging over time, 
#' 2) on the NO2 pollutant only but taking into account the temporal variability, and 
#' 3) on all pollutants by taking into account the temporal variability.
#' 
#' As the computation time of some spatial scan statistics can be a bit long in file "frevent.R",
#' we provide here a version with fewer permutations for the p-value estimation, that is 99 
#' instead of the default value which is 999. 
#' In the generated html file, the time taken by each method is indicated at the end of each section.


library(HDSpatialScan)

set.seed(123)

#' # Load the data

####################
#  Load the data   #
####################
data("map_sites")
data("multi_data")
data("funi_data")
data("fmulti_data")

#' # Visualize the data

#########################
#  Visualize the data   #
#########################

library(dplyr)
library(RColorBrewer)
library(sp)

mean_data_no2 <- data.frame(canton = map_sites$identifiant, mean_no2 = multi_data[,1])
mean_data_o3 <- data.frame(canton = map_sites$identifiant, mean_o3 = multi_data[,2])
mean_data_pm10 <- data.frame(canton = map_sites$identifiant, mean_pm10 = multi_data[,3])
mean_data_pm2.5 <- data.frame(canton = map_sites$identifiant, mean_pm2.5 = multi_data[,4])

map_sites@data <- left_join(map_sites@data, mean_data_no2, by = c("identifiant" = "canton"))
map_sites@data <- left_join(map_sites@data, mean_data_o3, by = c("identifiant" = "canton"))
map_sites@data <- left_join(map_sites@data, mean_data_pm10, by = c("identifiant" = "canton"))
map_sites@data <- left_join(map_sites@data, mean_data_pm2.5, by = c("identifiant" = "canton"))

# Cut the mean concentrations in 5 classes
classes_no2 <- quantile(multi_data[,1], probs = c(0,0.2,0.4,0.6,0.8,1))
classes_o3 <- quantile(multi_data[,2], probs = c(0,0.2,0.4,0.6,0.8,1))
classes_pm10 <- quantile(multi_data[,3], probs = c(0,0.2,0.4,0.6,0.8,1))
classes_pm2.5 <- quantile(multi_data[,4], probs = c(0,0.2,0.4,0.6,0.8,1))

# Choice of colors
palette <- brewer.pal(n = 5, name = "YlOrRd")

map_sites@data$classe_no2 <- as.character(cut(map_sites@data$mean_no2, breaks = classes_no2, labels = palette, include.lowest = TRUE))
map_sites@data$classe_o3 <- as.character(cut(map_sites@data$mean_o3, breaks = classes_o3, labels = palette, include.lowest = TRUE))
map_sites@data$classe_pm10 <- as.character(cut(map_sites@data$mean_pm10, breaks = classes_pm10, labels = palette, include.lowest = TRUE))
map_sites@data$classe_pm2.5 <- as.character(cut(map_sites@data$mean_pm2.5, breaks = classes_pm2.5, labels = palette, include.lowest = TRUE))

legend_no2 <- as.character(levels(cut(map_sites@data$mean_no2, breaks = classes_no2, include.lowest = TRUE)))
legend_o3 <- as.character(levels(cut(map_sites@data$mean_o3, breaks = classes_o3, include.lowest = TRUE)))
legend_pm10 <- as.character(levels(cut(map_sites@data$mean_pm10, breaks = classes_pm10, include.lowest = TRUE)))
legend_pm2.5 <- as.character(levels(cut(map_sites@data$mean_pm2.5, breaks = classes_pm2.5, include.lowest = TRUE)))

#+ fig.width=11, fig.height=5
plot(map_sites, col = map_sites@data$classe_no2, border = "black", lwd = 0.5)
legend("topleft", legend = legend_no2, fill = palette, cex=1,
       title = expression(paste("Average ", NO[2], " concentration in ", mu, "g/", m^3)),
       bty = "n")

#+ fig.width=11, fig.height=5
plot(map_sites, col = map_sites@data$classe_o3, border = "black", lwd = 0.5)
legend("topleft", legend = legend_o3, fill = palette, cex=1,
       title = expression(paste("Average ", O[3], " concentration in ", mu, "g/", m^3)),
       bty = "n")

#+ fig.width=11, fig.height=5
plot(map_sites, col = map_sites@data$classe_pm10, border = "black", lwd = 0.5)
legend("topleft", legend = legend_pm10, fill = palette, cex=1,
       title = expression(paste("Average ", PM[10], " concentration in ", mu, "g/", m^3)),
       bty = "n")

#+ fig.width=11, fig.height=5
plot(map_sites, col = map_sites@data$classe_pm2.5, border = "black", lwd = 0.5)
legend("topleft", legend = legend_pm2.5, fill = palette, cex=1,
       title = expression(paste("Average ", PM[2.5], " concentration in ", mu, "g/", m^3)),
       bty = "n")

# The curves
times <- c("2020-05-01", "2020-05-02", "2020-05-03", "2020-05-04","2020-05-05","2020-05-06","2020-05-07","2020-05-08","2020-05-09","2020-05-10","2020-05-11","2020-05-12","2020-05-13","2020-05-14","2020-05-15","2020-05-16","2020-05-17","2020-05-18","2020-05-19","2020-05-20","2020-05-21","2020-05-22","2020-05-23","2020-05-24","2020-05-25","2020-05-26","2020-05-27","2020-05-28","2020-05-29","2020-05-30","2020-05-31","2020-06-01","2020-06-02","2020-06-03","2020-06-04","2020-06-05","2020-06-06","2020-06-07","2020-06-08","2020-06-09","2020-06-10","2020-06-11","2020-06-12","2020-06-13","2020-06-14","2020-06-15","2020-06-16","2020-06-17","2020-06-18","2020-06-19","2020-06-20","2020-06-21","2020-06-22","2020-06-23","2020-06-24","2020-06-25")
temp_no2 <- matrix(nrow = length(fmulti_data), ncol = length(times))
temp_o3 <- matrix(nrow = length(fmulti_data), ncol = length(times))
temp_pm10 <- matrix(nrow = length(fmulti_data), ncol = length(times))
temp_pm2.5 <- matrix(nrow = length(fmulti_data), ncol = length(times))

for(i in 1:length(fmulti_data)){
  temp_no2[i,] <- fmulti_data[[i]][1,]
  temp_o3[i,] <- fmulti_data[[i]][2,]
  temp_pm10[i,] <- fmulti_data[[i]][3,]
  temp_pm2.5[i,] <- fmulti_data[[i]][4,]
}

mini <- min(temp_no2)
maxi <- max(temp_no2)
#+ fig.width=10, fig.height=7
plot(NULL, xlim = c(1,ncol(temp_no2)), ylim = c(mini, maxi), xlab = "", xaxt = 'n', ylab = expression(paste(NO[2], " concentration in ", mu, "g/", m^3)), cex.axis = 0.85, cex.lab = 0.85)
axis(1, at = c(1:ncol(temp_no2))[which(c(1:ncol(temp_no2))%%5==1)], labels = times[which(c(1:ncol(temp_no2))%%5==1)], cex.axis = 0.85, cex.lab = 0.85, las = 3)
for(i in 1:nrow(temp_no2)){
  lines(x = c(1:ncol(temp_no2)), y = temp_no2[i,], col = "black", lwd = 1)
}

mini <- min(temp_o3)
maxi <- max(temp_o3)
#+ fig.width=10, fig.height=7
plot(NULL, xlim = c(1,ncol(temp_o3)), ylim = c(mini, maxi), xlab = "", xaxt = 'n', ylab = expression(paste(O[3], " concentration in ", mu, "g/", m^3)), cex.axis = 0.85, cex.lab = 0.85)
axis(1, at = c(1:ncol(temp_o3))[which(c(1:ncol(temp_o3))%%5==1)], labels = times[which(c(1:ncol(temp_o3))%%5==1)], cex.axis = 0.85, cex.lab = 0.85, las = 3)
for(i in 1:nrow(temp_o3)){
  lines(x = c(1:ncol(temp_o3)), y = temp_o3[i,], col = "black", lwd = 1)
}

mini <- min(temp_pm10)
maxi <- max(temp_pm10)
#+ fig.width=10, fig.height=7
plot(NULL, xlim = c(1,ncol(temp_pm10)), ylim = c(mini, maxi), xlab = "", xaxt = 'n', ylab = expression(paste(PM[10], " concentration in ", mu, "g/", m^3)), cex.axis = 0.85, cex.lab = 0.85)
axis(1, at = c(1:ncol(temp_pm10))[which(c(1:ncol(temp_pm10))%%5==1)], labels = times[which(c(1:ncol(temp_pm10))%%5==1)], cex.axis = 0.85, cex.lab = 0.85, las = 3)
for(i in 1:nrow(temp_pm10)){
  lines(x = c(1:ncol(temp_pm10)), y = temp_pm10[i,], col = "black", lwd = 1)
}

mini <- min(temp_pm2.5)
maxi <- max(temp_pm2.5)
#+ fig.width=10, fig.height=7
plot(NULL, xlim = c(1,ncol(temp_pm2.5)), ylim = c(mini, maxi), xlab = "", xaxt = 'n', ylab = expression(paste(PM[2.5], " concentration in ", mu, "g/", m^3)), cex.axis = 0.85, cex.lab = 0.85)
axis(1, at = c(1:ncol(temp_pm2.5))[which(c(1:ncol(temp_pm2.5))%%5==1)], labels = times[which(c(1:ncol(temp_pm2.5))%%5==1)], cex.axis = 0.85, cex.lab = 0.85, las = 3)
for(i in 1:nrow(temp_pm2.5)){
  lines(x = c(1:ncol(temp_pm2.5)), y = temp_pm2.5[i,], col = "black", lwd = 1)
}


#' # Apply a multivariate spatial scan statistic

##################################################
#  Apply a multivariate spatial scan statistic   #
##################################################

#' Is the data gaussian at least for each component ?

hist(multi_data[,1])
hist(multi_data[,2])
hist(multi_data[,3])
hist(multi_data[,4])

qqnorm(multi_data[,1])
qqline(multi_data[,1], col = "red")
qqnorm(multi_data[,2])
qqline(multi_data[,2], col = "red")
qqnorm(multi_data[,3])
qqline(multi_data[,3], col = "red")
qqnorm(multi_data[,4])
qqline(multi_data[,4], col = "red")


#' not really : we apply the MNP

coords <- coordinates(map_sites)

time_start <- Sys.time()
res_mnp <- SpatialScan(method = "MNP", data = multi_data, sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2, type_minimaxi = "sites/indiv", mini_post = 0, maxi_post = 10, type_minimaxi_post = "radius", nbCPU = 7, MC = 99, variable_names = c("NO2", "O3", "PM10", "PM2.5"))$MNP
time_stop <- Sys.time()

# print
print(res_mnp)

# see the location of the most likely cluster
plot(x = res_mnp, type = "schema", system_conv = "+init=epsg:2154", only.MLC = TRUE)
plot(x = res_mnp, type = "map", spobject = map_sites, only.MLC = TRUE)
plot(x = res_mnp, type = "map2", spobject = map_sites, only.MLC = TRUE)

# characteristics
#+ fig.width=10, fig.height=7
summary(res_mnp, type_summ = "nparam", only.MLC = TRUE)
#+ fig.width=10, fig.height=7
plotSummary(res_mnp, type = "median", only.MLC = TRUE)


#' The computation of this spatial scan statistic (MNP) took 
{{ round(difftime(time_stop,time_start, units="mins"),2) }} 
#' min


#' # Apply a functional univariate spatial scan statistic

###########################################################
#  Apply a functional univariate spatial scan statistic   #
###########################################################

time_start <- Sys.time()
res_urbfss <- SpatialScan(method = "URBFSS", data = funi_data, sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2, type_minimaxi = "sites/indiv", mini_post = 0, maxi_post = 10, type_minimaxi_post = "radius", nbCPU = 7, MC = 99)$URBFSS
time_stop <- Sys.time()

# print
print(res_urbfss)

# see the location of the most likely cluster
plot(res_urbfss, type = "map2", spobject = map_sites, only.MLC = TRUE)

# characteristics
#+ fig.width=10, fig.height=7
summary(res_urbfss, type_summ = "nparam", only.MLC = TRUE)
#+ fig.width=10, fig.height=7
plotCurves(res_urbfss, add_median = TRUE, only.MLC = TRUE)
#+ fig.width=10, fig.height=7
plotSummary(res_urbfss, type = "median", only.MLC = TRUE)


#' The computation of this spatial scan statistic (URBFSS) took 
{{ round(difftime(time_stop,time_start, units="mins"),2) }} 
#' min


#' # Apply a functional multivariate spatial scan statistic

#############################################################
#  Apply a functional multivariate spatial scan statistic   #
#############################################################

time_start <- Sys.time()
res_mrbfss <- SpatialScan(method = "MRBFSS", data = fmulti_data, sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2, type_minimaxi = "sites/indiv", mini_post = 0, maxi_post = 10, type_minimaxi_post = "radius", nbCPU = 7, MC = 99, variable_names = c("NO2", "O3", "PM10", "PM2.5"))$MRBFSS
time_stop <- Sys.time()

# print
print(res_mrbfss)

# see the location of the most likely cluster
plot(res_mrbfss, type = "map2", spobject = map_sites, only.MLC = TRUE)

# characteristics
#+ fig.width=10, fig.height=7
summary(res_mrbfss, type_summ = "nparam", only.MLC = TRUE)
#+ fig.width=10, fig.height=7
plotCurves(res_mrbfss, add_median = TRUE, only.MLC = TRUE)
#+ fig.width=10, fig.height=7
plotSummary(res_mrbfss, type = "median", only.MLC = TRUE)

#' The computation of this spatial scan statistic (MRBFSS) took 
{{ round(difftime(time_stop,time_start, units="mins"),2) }} 
#' min

sessionInfo()
