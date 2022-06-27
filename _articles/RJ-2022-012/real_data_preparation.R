## NOTE: 
## For the real data analyses, 11 real data sets are used. 
## Boston housing and Ames housing data sets are loaded from the R packages mlbench and AmesHousing, respectively.
## The other data sets are downloaded from the UCI Machine Learning Repository.
## The download URLs are explicitly given for these 9 data sets.
## However, if the download URLs won't work in the future, they need to be changed with the working ones.

## required packages
library(AmesHousing)
library(mlbench)
library(readxl)
library(tidyr)
library(utils)

## ENTER DESTINATION PATH FOR REAL DATA DOWNLOADS..
path_realdata <- file.path(getwd(), "real data sets")
if (!file.exists(path_realdata)){
  dir.create(path_realdata)
}

#### abalone ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
filename <- file.path(path_realdata,"abalone.data")
download.file(url, destfile=filename)
abalone <- read.delim(filename, header=FALSE, sep=",", stringsAsFactors=FALSE)
abalone$V1 <- as.factor(abalone$V1)
px <- ncol(abalone) - 1
colnames(abalone) <- c(paste0("x",1:px), "y")
if (sum(is.na(abalone)) > 0) {print("missing in abalone")}

# Name / Data Type / Measurement Unit / Description
# -----------------------------
# x1 Sex / nominal / -- / M, F, and I (infant)
# x2 Length / continuous / mm / Longest shell measurement
# x3 Diameter / continuous / mm / perpendicular to length
# x4 Height / continuous / mm / with meat in shell
# x5 Whole weight / continuous / grams / whole abalone
# x6 Shucked weight / continuous / grams / weight of meat
# x7 Viscera weight / continuous / grams / gut weight (after bleeding)
# x8 Shell weight / continuous / grams / after being dried
# y Rings / integer / -- / +1.5 gives the age in years (output)

saveRDS(abalone, file=file.path(path_realdata,"abalone.rds"))


#### air_quality ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip"
filename <- file.path(path_realdata,"AirQualityUCI.zip")
download.file(url, destfile=filename)
unzip(filename,exdir=path_realdata)
air_quality <- read_excel(file.path(path_realdata,"AirQualityUCI.xlsx"))
air_quality <- as.data.frame(air_quality)
air_quality$Time <- NULL
df <- data.frame(date=air_quality$Date, stringsAsFactors=FALSE)
df <- df %>% separate(date, sep="-", into=c("year", "month", "day"))
air_quality$Year <- as.numeric(df$year)
air_quality$Month <- as.numeric(df$month)
air_quality$Day <- as.numeric(df$day)
air_quality$Date <- NULL

# Missing values are tagged with -200 value.
missing.ix <- list()
for (i in 1:ncol(air_quality)) {
  missing.ix[[i]] <- which(air_quality[, i] == -200)
}
# NMHC(GT) has 8443 missing values, remove this variable.
air_quality$`NMHC(GT)` <- NULL
missing.ix <- c()
for (i in 1:ncol(air_quality)) {
  missing.ix <- c(missing.ix, which(air_quality[, i] == -200))
}
missing.ix <- unique(missing.ix)
air_quality <- air_quality[-missing.ix, ]
air_quality$y1 <- air_quality$RH
air_quality$y2 <- air_quality$AH
air_quality$RH <- NULL
air_quality$AH <- NULL
px <- ncol(air_quality) - 2
colnames(air_quality) <- c(paste0("x",1:px),"y1","y2")

# x1 True hourly averaged concentration CO in mg/m^3 (reference analyzer)
# x2 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)
# x3 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer)
# x4 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)
# x5 True hourly averaged NOx concentration in ppb (reference analyzer)
# x6 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted)
# x8 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)
# x8 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)
# x9 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted)
# x10 Temperature in Â°C
# x11 Year
# x12 Month
# x13 Day
# y1 Relative Humidity (%)
# y2 AH Absolute Humidity

air_quality_rh <- air_quality
air_quality_rh$y <- air_quality_rh$y1
air_quality_rh$y1 <- NULL
air_quality_rh$y2 <- NULL
if (sum(is.na(air_quality_rh)) > 0) {print("missing in air_quality_rh")}
saveRDS(air_quality_rh, file=file.path(path_realdata,"air_quality_rh.rds"))

air_quality_ah <- air_quality
air_quality_ah$y <- air_quality_ah$y2
air_quality_ah$y1 <- NULL
air_quality_ah$y2 <- NULL
if (sum(is.na(air_quality_ah)) > 0) {print("missing in air_quality_ah")}
saveRDS(air_quality_ah, file=file.path(path_realdata,"air_quality_ah.rds"))


#### airfoil_selfnoise ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat"
filename <- file.path(path_realdata,"airfoil_self_noise.dat")
download.file(url, destfile=filename)
airfoil_selfnoise <- read.delim(filename, header=FALSE, stringsAsFactors=FALSE)
px <- ncol(airfoil_selfnoise) - 1
colnames(airfoil_selfnoise) <- c(paste0("x",1:px), "y")
if (sum(is.na(airfoil_selfnoise)) > 0) {print("missing in airfoil_selfnoise")}

# x1: Frequency, in Hertzs.
# x2: Angle of attack, in degrees.
# x3: Chord length, in meters.
# x4: Free-stream velocity, in meters per second.
# x5: Suction side displacement thickness, in meters.
# y: Scaled sound pressure level, in decibels. output

saveRDS(airfoil_selfnoise, file=file.path(path_realdata,"airfoil_selfnoise.rds"))


#### ames_housing ####
ames_housing <- make_ordinal_ames()
# remove an observation with a missing
ames_housing <- ames_housing[!is.na(ames_housing$Electrical),]
# converts the target variable (1=1000)
ames_housing$Sale_Price <- ames_housing$Sale_Price/1000
# get the names of the ordinal variables
ord_vars <- vapply(ames_housing, is.ordered, logical(1))
namored <- names(ord_vars)[ord_vars]
# converts the ordered factors to numeric (this preserves the ordering of the factor)
ames_housing[,namored] <- data.frame(lapply(ames_housing[,namored],as.numeric))
# Sale_Price is the output
ames_housing$y <- ames_housing$Sale_Price
ames_housing$Sale_Price <- NULL
px <- ncol(ames_housing) - 1
colnames(ames_housing) <- c(paste0("x",1:px), "y")
if (sum(is.na(ames_housing)) > 0) {print("missing in ames_housing")}
ames_housing <- as.data.frame(ames_housing)

saveRDS(ames_housing, file=file.path(path_realdata,"ames_housing.rds"))


#### auto_mpg ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
filename <- file.path(path_realdata,"auto-mpg.data")
download.file(url, destfile=filename)
auto_mpg <- read.delim(filename, header=FALSE, sep="", fill=TRUE, stringsAsFactors=FALSE)
# 1. mpg: continuous
# 2. cylinders: multi-valued discrete
# 3. displacement: continuous
# 4. horsepower: continuous
# 5. weight: continuous
# 6. acceleration: continuous
# 7. model year: multi-valued discrete
# 8. origin: multi-valued discrete
# 9. car name: string (unique for each instance)

# car name: string (unique for each instance) is removed (V9)
auto_mpg$V9 <- NULL
del_ix <- which(auto_mpg$V4 == "?")
auto_mpg <- auto_mpg[-del_ix, ]
auto_mpg$V4 <- as.numeric(auto_mpg$V4)

auto_mpg$y <- auto_mpg$V1
auto_mpg$V1 <- NULL
px <- ncol(auto_mpg) - 1
colnames(auto_mpg) <- c(paste0("x",1:px), "y")
if (sum(is.na(auto_mpg)) > 0) {print("missing in auto_mpg")}

# x1 cylinders: multi-valued discrete
# x2 displacement: continuous
# x3 horsepower: continuous
# x4 weight: continuous
# x5 acceleration: continuous
# x6 model year: multi-valued discrete
# x7 origin: multi-valued discrete
# y mpg: continuous

saveRDS(auto_mpg, file=file.path(path_realdata,"auto_mpg.rds"))


#### boston ####
data("BostonHousing")
boston <- BostonHousing
# medv is the output and last variable in data set
px <- ncol(boston) - 1
colnames(boston) <- c(paste0("x",1:px), "y")
if (sum(is.na(boston)) > 0) {print("missing in boston")}

saveRDS(boston, file=file.path(path_realdata,"boston.rds"))


#### computer_hardware ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data"
filename <- file.path(path_realdata,"machine.data")
download.file(url, destfile=filename)
computer_hardware <- read.delim(filename, header=FALSE, sep=',', fill=TRUE, stringsAsFactors=FALSE)

# 1. vendor name: 30
# (adviser, amdahl,apollo, basf, bti, burroughs, c.r.d, cambex, cdc, dec,
#   dg, formation, four-phase, gould, honeywell, hp, ibm, ipl, magnuson,
#   microdata, nas, ncr, nixdorf, perkin-elmer, prime, siemens, sperry,
#   sratus, wang)
# 2. Model Name: many unique symbols
# 3. MYCT: machine cycle time in nanoseconds (integer)
# 4. MMIN: minimum main memory in kilobytes (integer)
# 5. MMAX: maximum main memory in kilobytes (integer)
# 6. CACH: cache memory in kilobytes (integer)
# 7. CHMIN: minimum channels in units (integer)
# 8. CHMAX: maximum channels in units (integer)
# 9. PRP: published relative performance (integer)
# 10. ERP: estimated relative performance from the original article (integer)

# remove V1, V2, V10
computer_hardware$V1 <- NULL
computer_hardware$V2 <- NULL
computer_hardware$V10 <- NULL
# V9 is the output
px <- ncol(computer_hardware) - 1
colnames(computer_hardware) <- c(paste0("x",1:px), "y")
if (sum(is.na(computer_hardware)) > 0) {print("missing in computer_hardware")}

saveRDS(computer_hardware, file=file.path(path_realdata,"computer_hardware.rds"))


#### concrete_compression ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"
filename <- file.path(path_realdata,"Concrete_Data.xls")
download.file(url, destfile=filename)
concrete_compression <- read_excel(filename)
concrete_compression <- as.data.frame(concrete_compression)
concrete_compression$y <- concrete_compression$`Concrete compressive strength(MPa, megapascals)`
concrete_compression$`Concrete compressive strength(MPa, megapascals)` <- NULL
px <- ncol(concrete_compression) - 1
colnames(concrete_compression) <- c(paste0("x",1:px), "y")
if (sum(is.na(concrete_compression)) > 0) {print("missing in concrete_compression")}

saveRDS(concrete_compression, file=file.path(path_realdata,"concrete_compression.rds"))


#### concrete_slump ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/slump/slump_test.data"
filename <- file.path(path_realdata,"slump_test.data")
download.file(url, destfile=filename)
concrete_slump <- read.delim(filename, header=TRUE, sep=',', fill=TRUE, stringsAsFactors=FALSE)
concrete_slump$No <- NULL

# Input variables (7)(component kg in one M^3 concrete):
# Cement
# Slag
# Fly ash
# Water
# SP
# Coarse Aggr.
# Fine Aggr.
# 
# Output variables (3):
# SLUMP (cm)
# FLOW (cm)
# 28-day Compressive Strength (Mpa)
# we will use "28-day Compressive Strength (Mpa)" as the output variable
px <- 7
concrete_slump <- concrete_slump[,c(1:px,10)]
colnames(concrete_slump) <- c(paste0("x",1:px), "y")
if (sum(is.na(concrete_slump)) > 0) {print("missing in concrete_slump")}

saveRDS(concrete_slump, file=file.path(path_realdata,"concrete_slump.rds"))


#### energy_efficiency ####
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx"
filename <- file.path(path_realdata,"ENB2012_data.xlsx")
download.file(url, destfile=filename)
energy_efficiency <- read_excel(filename)
energy_efficiency <- as.data.frame(energy_efficiency)

# X1 Relative Compactness
# X2 Surface Area
# X3 Wall Area
# X4 Roof Area
# X5 Overall Height
# X6 Orientation
# X7 Glazing Area
# X8 Glazing Area Distribution
# y1 Heating Load
# y2 Cooling Load

energy_efficiency_hl <- energy_efficiency
energy_efficiency_hl$y <- energy_efficiency_hl$Y1
energy_efficiency_hl$Y1 <- NULL
energy_efficiency_hl$Y2 <- NULL
px <- ncol(energy_efficiency_hl) - 1
colnames(energy_efficiency_hl) <- c(paste0("x",1:px), "y")
if (sum(is.na(energy_efficiency_hl)) > 0) {print("missing in energy_efficiency_hl")}
saveRDS(energy_efficiency_hl, file=file.path(path_realdata,"energy_efficiency_hl.rds"))

energy_efficiency_cl <- energy_efficiency
energy_efficiency_cl$y <- energy_efficiency_cl$Y2
energy_efficiency_cl$Y1 <- NULL
energy_efficiency_cl$Y2 <- NULL
px <- ncol(energy_efficiency_cl) - 1
colnames(energy_efficiency_cl) <- c(paste0("x",1:px), "y")
if (sum(is.na(energy_efficiency_cl)) > 0) {print("missing in energy_efficiency_cl")}
saveRDS(energy_efficiency_cl, file=file.path(path_realdata,"energy_efficiency_cl.rds"))


#### servo ####
data(Servo)
servo <- Servo
servo$y <- servo$Class
servo$Class <- NULL
px <- ncol(servo) - 1
colnames(servo) <- c(paste0("x",1:px), "y")
if (sum(is.na(servo)) > 0) {print("missing in servo")}

saveRDS(servo, file=file.path(path_realdata,"servo.rds"))


## remove downloaded files
file.remove(
  file.path(path_realdata, 
            setdiff(list.files(path_realdata), list.files(path_realdata, pattern="*.rds$", full.names=FALSE)))
)