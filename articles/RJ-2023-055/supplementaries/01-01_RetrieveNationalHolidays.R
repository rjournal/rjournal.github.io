# Created 08.10.2021

# Clean and download national holidays


#---------------------------------------------------------------------------------

# load packages
library(timeDate)
library(lubridate)
library(dplyr)

# Here we extract the date of the main national holidays in Italy for the selected years. For instance
# Easter(2015), gives the date of the Easter in 2015.
holidays_vec = sort(c(
    ymd(NewYearsDay(2015:2021)),
    ymd(ITEpiphany(2015:2020)),
    ymd(Easter(2015:2020)),
    ymd(ITLiberationDay(2015:2020)),
    ymd(LaborDay(2015:2020)),
    seq(ymd('2015-06-02'), ymd('2020-06-02'), by='years'),
    ymd(ITAssumptionOfVirginMary(2015:2020)),
    ymd(ITAllSaints(2015:2020)),
    ymd(ITImmaculateConception(2015:2020))
)
)

# put the data in a data.frame format
holiday_df = data.frame(Data = holidays_vec)
holiday_df$Week = isoweek(holiday_df$Data)
holiday_df$Year = year(holiday_df$Data)

# set the years to correspond to the ISO weeks
holiday_df$Year[which(holiday_df$Data == "2016-01-01")] = 2015
holiday_df$Year[which(holiday_df$Data == "2017-01-01")] = 2016
holiday_df$Year[which(holiday_df$Data == "2021-01-01")] = 2020

holiday_df = holiday_df %>% 
  mutate(EURO_LABEL = paste0(Year, "-W", sprintf("%02d", Week)))

# Create 'Output' dir
if(!dir.exists("Output"))
  dir.create("Output")

# store output
saveRDS(holiday_df, file="Output/holiday_df")



##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
