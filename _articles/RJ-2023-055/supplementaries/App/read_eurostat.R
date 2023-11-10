# From postanalysis.R

# read the EUROSTAT week file
EUROSTAT <- read_excel("./data/EUROSTAT_ISO_HMEROLOGIO.xls")
EUROSTAT$EURO_TIME <- as.Date(format(as.POSIXct(EUROSTAT$EURO_TIME, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
EUROSTAT$year <- as.numeric(format(EUROSTAT$EURO_TIME, "%Y"))
EUROSTAT$month <- format(EUROSTAT$EURO_TIME, "%m")
EUROSTAT$month.an <- month.abb[as.numeric(EUROSTAT$month)]
EUROSTAT$weekID = as.numeric(substring(EUROSTAT$CD_EURO,2,3))

# get the months as the xaxis
euro_xaxis <- EUROSTAT %>%
  filter(ETOS_EURO==2020) %>%
  select(weekID,month.an) %>%
  filter(!(weekID==1 & month.an=="Dec"))
euro_xaxis <- euro_xaxis[!duplicated(euro_xaxis$month.an),]

