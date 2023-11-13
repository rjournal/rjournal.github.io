



# Created 9.10.2021


# Clean the mortality file in Italy. 


#---------------------------------------------------------------------------------

# One can download the mortality data for 2015-2020 at 
# https://www.istat.it/it/archivio/240401. We selected the file that includes
# deaths till end of January. After downloading this file, put it in the data folder.


library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(lubridate)

# Unzip data
unzip("data/comuni_giornaliero_31gennaio21.zip")

deaths <- read_csv("data/comuni_giornaliero_31gennaio21.csv")

# subset the dataset
deaths %>% select_at(
  vars("PROV", "NOME_PROVINCIA", "CL_ETA", "GE",
                  paste0("M_", 14:21), 
                  paste0("F_", 14:21))
  ) -> deaths


# Change to long format
deaths <- gather(deaths, agesex, deaths, M_14:F_21, factor_key=TRUE)

deaths %>% mutate(
  sex = substr(agesex, start = 1, stop = 1),
  year = as.numeric(paste0("20",substr(agesex, start = 3, stop = 4)))
) -> deaths

deaths$agesex <- NULL

# Fix the age. The CL_ETA is the age variable denoting the following age groups:

# 0=0
# 1=1-4
# 2=5-9
# 3=10-14
# 4=15-19
# 5=20-24
# 6=25-29
# 7=30-34
# 8=35-39
# 9=40-44
# 10=45-49
# 11=50-54
# 12=55-59
# 13=60-64
# 14=65-69
# 15=70-74
# 16=75-79
# 17=80-84
# 18=85-89
# 19=90-94
# 20=95-99
# 21=100+
# see also https://www.istat.it/it/archivio/240401



deaths$ageg <- NA
deaths$ageg[deaths$CL_ETA %in% 0:8] <- "less40"
deaths$ageg[deaths$CL_ETA %in% 9:12] <- "40-59"
deaths$ageg[deaths$CL_ETA %in% 13:14] <- "60-69"
deaths$ageg[deaths$CL_ETA %in% 15:16] <- "70-79"
deaths$ageg[deaths$CL_ETA %in% 17:21] <- "80plus"
deaths$CL_ETA <- NULL


# Fix the date
deaths %>% mutate(
  date = paste0(year, "-", 
                substr(GE, start = 1, stop = 2), "-", 
                substr(GE, start = 3, stop = 4))
) %>% mutate(date = as.Date(date)) -> deaths

deaths <- deaths[!is.na(deaths$date),] # the NAs are "false" leap years

# keep everything after 2014-12-29, as it is the first day of the first ISO week in 2015
deaths %>% filter(date>="2014-12-29") -> deaths

# ISO weeks file
EUROSTAT_ISO <- readRDS("Output/EUROSTAT_ISO")

# merge the EUROSTAT_ISO with the deaths
deaths <- left_join(deaths, EUROSTAT_ISO, by = c("date" = "EURO_TIME"))
deaths <- deaths[!is.na(deaths$EURO_LABEL),]


# Aggregate by ISO week and age group
deaths %>% select(PROV, NOME_PROVINCIA, sex, ageg, EURO_LABEL, deaths) %>% 
  group_by(PROV, NOME_PROVINCIA, sex, ageg, EURO_LABEL) %>% 
  summarise(deaths = sum(as.numeric(deaths), na.rm = TRUE)) -> tmp


# the numbers do not add up because some dates from the original file are
# missing. These dates have 0 death counts.

expand.grid(
  PROV = unique(deaths$PROV), 
  sex = c("M", "F"), 
  ageg = unique(tmp$ageg), 
  EURO_LABEL = unique(tmp$EURO_LABEL)
) -> expgrid

findata <- left_join(expgrid, tmp)

summary(is.na(findata))
findata[is.na(findata$EURO_LABEL),]
findata$deaths[is.na(findata$deaths)] <- 0
findata$NOME_PROVINCIA <- NULL
sum(is.na(findata)) # no NAs


# Now link the findata with temperature, holidays and population.
temperature <- readRDS("Output/TemperatureWeeklyItaly")
holidays <- readRDS("Output/holiday_df")
population <- readRDS("Output/pop_weekly")

# for the linkage I will need the NUTS318CD, ie the acronyms of the NUTS3 regions.
shp <- read_sf("data/ProvCM01012020_g_WGS84.shp")
linkage <- data.frame(ID = shp$COD_PROV, NUTS318CD = shp$SIGLA)
linkage$ID <- str_pad(linkage$ID, 3, pad = "0")

findata <- left_join(findata, linkage, by = c("PROV" = "ID"))


findata <- left_join(findata, temperature[,-1], by = c("EURO_LABEL" = "EURO_LABEL", "NUTS318CD" = "SIGLA"))

holidays$hol <- 1
holidays$Data <- holidays$Week <- holidays$Year <- NULL
holidays <- holidays[!duplicated(holidays$EURO_LABEL),]
findata <- left_join(findata, holidays, by = c("EURO_LABEL" = "EURO_LABEL"))
findata$hol[is.na(findata$hol)] <- 0


population$sex[population$sex %in% "male"] <- "M"
population$sex[population$sex %in% "female"] <- "F"
findata <- left_join(findata, population, by = c("ageg" = "age", 
                                                 "sex" = "sex", 
                                                 "NUTS318CD" = "NUTS318CD", 
                                                 "EURO_LABEL" = "EURO_LABEL"))

# and store the findata. 

saveRDS(findata, file = "Output/findata")



######################################################################################
######################################################################################
######################################################################################
######################################################################################


