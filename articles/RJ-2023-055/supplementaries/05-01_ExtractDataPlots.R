


# Created 17.11.2021


#  Extracts data for the App and plots


#---------------------------------------------------------------------------------

# load packages
library(sf)
library(dplyr)

# Age-sex groups
agesex_grps <- expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"),
                           sex = c("Females", "Males"))
# Add index (to select data)
agesex_grps$IDX <- 1:nrow(agesex_grps)

# Load data
DB <- list()
DB$IT <- readRDS("Output/poisson_samples_all")


# Read maps
MAPS <- list()
mymap <- read_sf("data/ProvCM01012020_g_WGS84.shp") 
MAPS$IT <- mymap 

# Number of weeks
n.weeks <- length(unique(DB$IT[[1]]$EURO_LABEL))

# Nubmer of regions
n.regions <- nrow(MAPS$IT)

# Link table
link_table = readRDS("data/link_table")


# Load functions 
source("functions.R")


# Merge map and link table
MAPS$IT <- left_join(MAPS$IT, link_table, by = "SIGLA")
MAPS$IT$COUNTRY <- "Italy"

MAPS$IT %>% mutate(DEN_UTS = DEN_UTS.x) %>% select(-DEN_UTS.y, -DEN_UTS.x) -> MAPS$IT

MAPS$IT <- list(
  country = MAPS$IT %>% group_by(COUNTRY) %>% summarise() %>% st_simplify(dTolerance = 500),
  region = MAPS$IT %>% group_by(NAMNUTS2) %>% summarise() %>% st_simplify(dTolerance = 500),
  province = MAPS$IT %>% select(ID_space, SIGLA, DEN_UTS) %>% st_simplify(dTolerance = 500)
)

# transform into lat long
MAPS$IT$country <- st_transform(MAPS$IT$country, '+proj=longlat +datum=WGS84')
MAPS$IT$region <- st_transform(MAPS$IT$region, '+proj=longlat +datum=WGS84')
MAPS$IT$province <- st_transform(MAPS$IT$province, '+proj=longlat +datum=WGS84')

# Add NAME
MAPS$IT$country$NAME <- "Italy"

# Define strata for aggregation (argument 'stratify.by')
strata <-  c("none","age","sex", "agesex")  
geo.res = c("province", "region", "country")


# RE-sale REM variables to be as percentage
# obj: Object with data (tidy object)

rescale_REM <- function(obj) {
  mutate(obj, mean.REM = 100 * mean.REM,          
         median.REM = 100 * median.REM,
         sd.REM = 100 * sd.REM,
         LL.REM = 100 * LL.REM,
         UL.REM = 100 * UL.REM
  )
}


# Extract the data for all age*sex strata and spatial resolution as defined above, i.e.
# strata <-  c("none","age","sex","agesex")  and geo.res = c("province", "region", "country")
# when the temporal aggregation is the 2020 (yearly total).

d <- lapply(geo.res, function(GEO) {
  res <- lapply(strata, function(STRATA) {
    aux <- get2020data(DB$IT, geo.res = GEO, link_table = link_table,
                       stratify.by = STRATA)
    
    # Get geo.name
    if(GEO == "country")  geo.name <- "COUNTRY"
    if(GEO == "region")   geo.name <- "NAMNUTS2"
    if(GEO == "province") geo.name <- "ID_space"
    
    if(STRATA == "none") {
      aux2 <- compute.excess(aux, mortality="REM", geo.name = geo.name)
      aux3 <- compute.excess(aux, mortality="NED", geo.name = geo.name)

      aux <- cbind(select(aux, !starts_with("V")),
                   select(aux2, !starts_with(c("xs", geo.name))),
                   select(aux3, !starts_with(c("xs", geo.name))))
      
      aux <- merge(MAPS$IT[[GEO]], aux)
      aux$median.pred.1 <- aux$LL.pred.1 <- aux$UL.pred.1 <- NULL
      
      # Re-scale REM
      aux <- rescale_REM(aux)

    } else {
      aux <- lapply(aux, function(X) {
        
        aux2 <- compute.excess(X, mortality="REM", geo.name = geo.name)
        aux3 <- compute.excess(X, mortality="NED", geo.name = geo.name)
  
        res <- cbind(select(X, !starts_with("V")),
                     select(aux2, !starts_with(c("xs", geo.name))),
                     select(aux3, !starts_with(c("xs", geo.name))))
      
        res <- merge(MAPS$IT[[GEO]], res)
        res$median.pred.1 <- res$LL.pred.1 <- res$UL.pred.1 <- NULL
        
        # Re-scale REM
        res <- rescale_REM(res)
        
        return(res)
      })
    }
    
    return(aux)
  })
  # Add strata names
  names(res) <- strata
  
  return(res)
})
# Add geo names
names(d) <- geo.res



# Extract the data for all age*sex strata and spatial resolution as defined above, i.e.
# strata <-  c("none","age","sex","agesex")  and geo.res = c("province", "region", "country")
# when the temporal aggregation is weeks.

d_week <- lapply(geo.res, function(GEO) {
  res <- lapply(strata, function(STRATA) {
    aux <- get2020weeklydata(DB$IT, geo.res = GEO, link_table = link_table,
                             stratify.by = STRATA)
    
    # Get geo.name
    if(GEO == "country")  geo.name <- "COUNTRY"
    if(GEO == "region")   geo.name <- "NAMNUTS2"
    if(GEO == "province") geo.name <- "ID_space"
    
    if(STRATA == "none") {
      aux2 <- compute.excess(aux, mortality="REM", geo.name = geo.name)
      aux3 <- compute.excess(aux, mortality="NED", geo.name = geo.name)

      aux <- cbind(select(aux, !starts_with("V")),
                   select(aux2, !starts_with(c("xs", geo.name))),
                   select(aux3, !starts_with(c("xs", geo.name))))
      
      aux <- aux[,!duplicated(colnames(aux), fromLast = TRUE)]
      # Re-scale REM
      aux <- rescale_REM(aux)
    
      
    } else {
      aux <- lapply(aux, function(X){
        
        aux2 <- compute.excess(X, mortality="REM", geo.name = geo.name)
        aux3 <- compute.excess(X, mortality="NED", geo.name = geo.name)
        res <- cbind(select(X, !starts_with("V")),
                     select(aux2, !starts_with(c("xs", geo.name))),
                     select(aux3, !starts_with(c("xs", geo.name))))
        
        res <- res[,!duplicated(colnames(res), fromLast = TRUE)]

        # Re-scale REM
        res <- rescale_REM(res)

        return(res)
      })
    }
    
    return(aux)
  })
  # Add strata names
  names(res) <- strata
  
  return(res)
})
 # Add geo names
names(d_week) <- geo.res


save(file = "Output/Italy.RData", list = c("d", "d_week"))


names(d)
names(d$province)
names(d$province$age)
names(d$province$sex)
names(d$province$agesex)
head(d$province$none)


################################################################
################################################################
################################################################
################################################################
################################################################



