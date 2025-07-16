# Simple automobile example

library(SurveyCC) # attach the package

load("../data/auto.rds") # load the data

# Simple example
design_object <-
  survey::svydesign(
    ids = ~1,
    data = auto
  )
var.x <- c("length", "weight", "headroom", "trunk")
var.y <- c("displacement", "mpg", "gear_ratio", "turn")
howmany <- 3
surveycc(design_object = design_object, var.x = var.x,
         var.y = var.y, howmany = howmany, selection = "ROWS")