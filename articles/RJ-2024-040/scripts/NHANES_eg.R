# NHANES example

library(SurveyCC) # attach the package
library(dplyr) # used for filtering ages below

load("../data/NHANESdata.rds") # load the data

reducedNHANESdata <- NHANESdata |>
  dplyr::filter(ridageyr <= 64 & ridageyr >= 45)

design_object <-
  survey::svydesign(
    id = ~sdmvpsu,
    weights = ~wtmec4yr,
    strata = ~sdmvstra,
    nest = TRUE,
    data=reducedNHANESdata
  )
var.x <- c("bmxwaist", "bmxbmi", "bpxpls", "bpxdi1", "bpxsy1")
var.y <- c("ridageyr", "indhhin2")
surveycc(design_object = design_object,
                var.x = var.x, var.y = var.y,
                howmany = 2, selection = "ROWS")