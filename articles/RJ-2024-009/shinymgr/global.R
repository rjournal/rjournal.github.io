# clean out objects from environment
rm(list = ls())

# set global variables 
shinyMgrPath <- getwd()

# load required shiny framework packages
library(shinymgr)

# load required module packages (parse headers)
app_mods <- list.files(
  path = paste0(shinyMgrPath,"/modules"), 
  full.names = TRUE
)
modPackages <- vector()
for (modPath in app_mods) {
  modPackages <- c(modPackages, shinymgr::mod_header_parser(modPath)[[4]]$packageName)
}

modPackages <- unique(modPackages)

# load the packages
for (package in modPackages) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

# source in all manager (framework) modules
mgr_mods <- list.files(
  path = paste0(shinyMgrPath,"/modules_mgr"), 
  full.names = TRUE
)

sapply(mgr_mods, FUN = source)

# source in all user modules

sapply(app_mods, FUN = source)

# source in all manager (framework) modules
app_mods <- list.files(
  path = paste0(shinyMgrPath, "/modules_app"),
  full.names = TRUE
)

sapply(app_mods, FUN = source)
