# This script contains information about the packages used for the analysis. 
# Run this script first in order to install all the required packages.

pkgs <- c("dplyr", "ecmwfr", "ISOweek",  "ggplot2", 
  "lubridate", "ncdf4", "patchwork", "plyr", "shinyalert", "shinydashboard",
  "raster", "RColorBrewer", "readr", "reshape2", "rgdal", "sf",  
  "spdep", "stringr", "tidyr", "abind", "shiny", "leafpop",
  "timeDate", "viridis", "data.table", "doParallel")
install.packages(pkgs, dep = TRUE)

# Install the INLA package (off CRAN)
install.packages("INLA", repos = c(getOption("repos"), 
  INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
