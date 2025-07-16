# NYTS example

library(SurveyCC) # attach the package

load("../data/reducedNYTS2021data.rds") # load the data

design_object <-
  survey::svydesign(
    ids = ~psu2,
    nest = TRUE,
    strata = ~v_stratum2,
    weights = ~finwgt,
    data = reducedNYTS2021data
  )
var.x <- c("qn9", "qn38", "qn40", "qn53", "qn54", "qn64", "qn69",
           "qn74","qn76", "qn78", "qn80", "qn82", "qn85", "qn88",
           "qn89")
var.y <- c("qn128", "qn129", "qn130", "qn131", "qn132", "qn134")
howmany <- 3
# Here we call the surveycc function...
surveycc(design_object = design_object, var.x = var.x,
         var.y = var.y, howmany = howmany, selection = "ROWS")