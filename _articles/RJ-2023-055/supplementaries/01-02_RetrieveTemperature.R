



# Created 27.02.2023

# Clean and download temperature


#---------------------------------------------------------------------------------

# Step 1. Download temperature data from ERA5

# and create a new directory to store the output
if(!dir.exists("Output"))
  dir.create("Output/")

# load packages
library(ecmwfr)
library(doParallel)

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# agree with the terms here: https://cds.climate.copernicus.eu/cdsapp/#!/terms/licence-to-use-copernicus-products,
# log in and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.

cds.user <- "" # Insert your CDS user here
cds.key <- "" #"Insert_your_CDS_API_KEY_here"

# Set up the API and UID
wf_set_key(user = cds.user, key = cds.key, service = "cds")

if(is.null(cds.user) | is.null(cds.key)) {
  print("You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, and once you are ok and logged in, click on your name on the top right next to logout and retrieve the information about the API key.")
}



# function to download the temperature

DonwloadTemperature <- function(X){
  
  request <- list(
    dataset_short_name = "reanalysis-era5-land",
    product_type   = "reanalysis",
    format = "netcdf",
    variable = "2m_temperature",
    date = X, # this is to match the ISO weeks
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
             "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
             "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
    # area is specified as N, W, S, E
    area = c(48, 6, 34, 20),
    target = paste0("temperature", sub(pattern = "/", replacement = "_", x=X), ".nc")
  )
  
  if(!file.exists(paste0("Output/temperature", sub(pattern = "/", replacement = "_", x=X), ".nc"))) {
    file <- wf_request(user = cds.user,
                       request = request,
                       transfer = TRUE,
                       path = "Output",
                       time_out = 3600*24,
                       verbose = TRUE)
  }
  
}

# 2014-12-28/2021-01-03

start_date <- as.Date("2014-12-28")
end_date <- as.Date("2021-01-03")
define_dates <- seq(from = start_date, to = end_date, length.out = 60)

toloop <- paste(define_dates[-length(define_dates)], define_dates[-1], sep = "/")


# run on parallel
funpar <- function(k) DonwloadTemperature(X = toloop[k])
  
t_0 <- Sys.time()

# Set up parallel environment
#ncores <- 20
ncores <- detectCores() - 1
k <- 1:length(toloop)
cl_inla <- makeCluster(ncores, methods=FALSE)

# extract packages on parallel environment 
clusterEvalQ(cl_inla, {
  library(ecmwfr)
})

# extract R objects on parallel environment
clusterExport(cl_inla, c("toloop", "DonwloadTemperature", "cds.user", "cds.key"))

# run the the function in parallel
outpar <- parLapply(cl = cl_inla, k, funpar)

# close parallel environment
stopCluster(cl_inla)
t_1 <- Sys.time()
t_1 - t_0 






# Step 2. Clean the temperature files

# load packages
library(ncdf4)
library(plyr)
library(sf)
library(raster)
library(lubridate)
library(patchwork)
library(dplyr)
library(stringr)
library(data.table)
library(abind)
library(ggplot2)

# read the files
files2read <- list.files("Output/")[list.files("Output/") %>% startsWith(.,"temperature")]
temperature <- lapply(paste0("Output/", files2read), nc_open) 
extr.tmp <- lapply(temperature, function(X) ncvar_get(X, varid="t2m"))

# extract space 
lon <- lapply(temperature, function(X) ncvar_get(X,"longitude")) 
lon <- lon[[1]]
lat <- lapply(temperature, function(X) ncvar_get(X,"latitude")) 
lat <- lat[[1]]
# and time
hour <- lapply(temperature, function(X) ncvar_get(X,"time")) 
hour <- do.call(c, hour)
# the format is hours since 1900-01-01:
hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
# Set time zone (UTC)
attr(hour_tr, "tzone") <- "UTC"

# set the correct timezone for Italy
hour_tr <- format(hour_tr, format='%Y-%m-%d', tz = "Europe/Rome")


# and from this string we need to remove the dates outside the 2015-2020 ISO weeks, ie everything before 2014-12-29 and
# after 2021-01-03
datestart <- "2014-12-29"
dateend <- "2021-01-03"

extr.tmp <- abind(extr.tmp, along = 3)
extr.tmp[,,(hour_tr>=datestart) & (hour_tr<=dateend)] -> extr.tmp
hour_tr[(hour_tr>=datestart) & (hour_tr<=dateend)] -> hour_tr

# define the start/end points of each date
dat <- as.data.frame(table(hour_tr))

start <- numeric(nrow(dat))
stop <- numeric(nrow(dat))

start[1] <- 1
stop[1] <- dat$Freq[1]

for(i in 2:nrow(dat)){
  start[i] <- stop[i-1] + 1
  stop[i] <- start[i] + dat$Freq[i] - 1
}

dat$start <- start
dat$stop <- stop


# function to retrieve daily mean

DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.tmp[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y-273.15))
  tmp <- as.data.frame(tmp)
  
  colnames(tmp) <- lat
  rownames(tmp) <- lon
  
  mat2store <- expand.grid(lon, lat)
  colnames(mat2store) <- c("lon", "lat")
  mat2store <- cbind(mat2store, as.vector(as.matrix(tmp)))  
  
  mat2store <- as.data.frame(mat2store)
  colnames(mat2store)[3] <- "temperature"
  
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  
  return(mat2store)
}

# run the DailyMean function across the data
t_0 <- Sys.time()
GetTemperature <- 
  apply(dat, 1, function(X){
    
    return(DailyMean(start = X[3], stop = X[4], date = X[1]))
    
  } 
  ) # approximately 1h
t_1 <- Sys.time()
t_1-t_0

##
## RUN FROM HERE TO CHECK IF FINE



GetTemperature <- do.call(rbind, GetTemperature)

 # create and id by latitude and longitude
GetTemperature %>% 
  dplyr::group_by(lon, lat) %>% 
  dplyr::mutate(ID = dplyr::cur_group_id()) -> GetTemperature


# Now we need the shp in Italy.
mun <- read_sf("data/ProvCM01012020_g_WGS84.shp")

# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))
DT_sf <- st_coordinates(DT_sf)
DT_sf <- as.data.frame(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)


# We also need to get the weekly means
GetTemperature$week <- week(GetTemperature$date)
GetTemperature$year <- year(GetTemperature$date)


# ISO weeks file
EUROSTAT_ISO <- data.frame(
  EURO_TIME = seq(as.Date("2014-12-29"), as.Date("2021-01-03"), by="days")
)

EUROSTAT_ISO %>% mutate(num.week = lubridate::isoweek(EURO_TIME), 
                        YEAR_ISO = lubridate::isoyear(EURO_TIME), 
                        YEAR = year(EURO_TIME)) %>% 
  mutate(CD_EURO = paste0("W", str_pad(num.week, 2, pad = "0")), 
         EURO_LABEL = paste(YEAR_ISO, CD_EURO, sep = "-")) %>% 
  dplyr::select(EURO_TIME, CD_EURO, YEAR, EURO_LABEL) -> EUROSTAT_ISO

# store it because is needed for the other rfiles too.
saveRDS(EUROSTAT_ISO, file = "Output/EUROSTAT_ISO")



# merge the EUROSTAT_ISO with the temperature
GetTemperature <- left_join(GetTemperature, EUROSTAT_ISO, by = c("date" = "EURO_TIME"))
GetTemperature$ID.wy <- paste0(GetTemperature$ID, GetTemperature$EURO_LABEL)

# take the mean per week
GetTemperature %>% dplyr::group_by(ID.wy) %>% 
  dplyr::mutate(weekly.mean = mean(temperature, na.rm = TRUE)) -> GetTemperature


# remove the daily temperature
GetTemperature_tmp <- GetTemperature[!duplicated(GetTemperature$ID.wy),]
GetTemperature_tmp$temperature <- NULL
names(table(GetTemperature_tmp$EURO_LABEL)) -> namtab

# Now I need to overlay it on the shp and take the mean by municipality and week
loopID <- unique(GetTemperature_tmp$EURO_LABEL)

mun$IDSpace <- 1:nrow(mun)
mun$IDSpace <- as.character(mun$IDSpace)

# Work on data.table to speed up the filter() computation 
GetTemperature_tmp <- as.data.table(GetTemperature_tmp)


lapply(1:length(loopID), function(X){
  
  i <- X
  tmp <- GetTemperature_tmp[EURO_LABEL == loopID[i]]
  tmp_sf <- st_as_sf(tmp, coords = c("X", "Y"), crs = st_crs(mun))
  tmp_sf$X <- tmp$X
  tmp_sf$Y <- tmp$Y
  tmp_stjoin <- st_join(mun, tmp_sf)
  
  tmp_stjoin <- as.data.frame(tmp_stjoin)
  tmp_stjoin$geometry <- NULL
  
  # the missings are basically the same week and year, so I will impute accordingly
  tmp_stjoin$EURO_LABEL[is.na(tmp_stjoin$EURO_LABEL)] <- tmp_stjoin$EURO_LABEL[!is.na(tmp_stjoin$EURO_LABEL)][1]
  
  # and calculate mean temperature of points that fall in a particular municipality 
  tmp_stjoin %>% 
    dplyr::group_by(IDSpace) %>% 
    dplyr::mutate(mean.temp = mean(weekly.mean, na.rm = TRUE)) %>% 
    dplyr::filter(!duplicated(IDSpace)) -> tmp_stjoin
  
  tmp_stjoin <- tmp_stjoin[,c("IDSpace", "EURO_LABEL", "mean.temp")]
  tmp_stjoin$IDSpace <- as.character(tmp_stjoin$IDSpace)
  
  return(tmp_stjoin)
}
) -> list.loop


loop.df <- do.call(rbind, list.loop)
tab2link <- as.data.frame(mun[,c("SIGLA", "IDSpace")])
tab2link$geometry <- NULL
loop.df <- left_join(loop.df, tab2link, by = c("IDSpace" = "IDSpace"))
colnames(loop.df)[1] <- "ID"



# The temperature file clean
saveRDS(loop.df, file = "Output/TemperatureWeeklyItaly")




# Code for Figure 2

GetTemperature[GetTemperature$date == "2015-01-01",] -> tmp_points


tmp.nc <- nc_open("Output/temperature2014-12-28_2015-02-03.nc") 
hour2plot <- ncvar_get(tmp.nc,"time")
# the format is hours since 1900-01-01:
hour2plot <- as.POSIXct(hour2plot*3600, origin="1900-01-01 00:00")
attr(hour2plot, "tzone") <- "UTC"
hour2plot <- format(hour2plot, tz = "Europe/Rome")
hour2plot <- which(hour2plot %in% "2015-01-01 00:00:00")
tmp.rstr <- raster("Output/temperature2014-12-28_2015-02-03.nc", band = hour2plot)

# set the correct timezone for Italy



gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- tibble::as_tibble(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}


gplot_r <- gplot_data(tmp.rstr)
gplot_r$value <- gplot_r$value -273.15 
ggplot()  + theme_light() + 
  geom_tile(data = dplyr::filter(gplot_r, !is.na(value)), aes(x = x, y = y, fill = value)) + 
  ylab("") + xlab("") + 
  scale_fill_viridis_c(name = "") +
  ggtitle("A. ERA5 temperature at \n2015-01-01 00:00:00") + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0))  -> p1


tmp_points2 <- tmp_points[sample(size = 2000, x = 1:nrow(tmp_points)),] 
ggplot()  + theme_light() + 
  geom_point(data = tmp_points2, aes(x = X, y = Y), size = 0.01, col = "grey44") + 
  geom_sf(data = mun, fill = "NA", size = 0.4, col = "dodgerblue3") + 
  ylab("") + xlab("") + 
  ggtitle("B. NUTS3 regions and \ncentroid of ERA5 pixels") + 
  theme(text = element_text(size = 6), 
        plot.margin = margin(0, 0, 0, 0))-> p2



loop.df %>% filter(EURO_LABEL %in% "2015-W01") %>% 
  left_join(mun, ., by = ("SIGLA" = "SIGLA")) -> tmp_mun


ggplot()  + theme_light() + 
  geom_sf(data = tmp_mun, aes(fill = mean.temp), col = "grey44", size = 0.4) + 
  ylab("") + xlab("") + 
  scale_fill_viridis_c(name = "") + 
  ggtitle("C. Mean temperature during the \n1st week of 2015") + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> p3

png("Output/ERAPOINTS.png", width = 17, height = 8, units = "cm", res = 300)
p1|p2|p3
dev.off()





##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################



