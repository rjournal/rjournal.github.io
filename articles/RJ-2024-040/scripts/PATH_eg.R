# PATH example

library(SurveyCC) # attach the package

load("../data/reducedPATHdata.rds") # load the data

design_object <-
  survey::svrepdesign(
    id = ~PERSONID,
    weights = ~R01_A_PWGT,
    repweights = "R01_A_PWGT[1-9]+",
    type = "Fay",
    rho = 0.3,
    data=reducedPATHdata,
    mse = TRUE
  )
var.x <- c("R01_AC1022", "R01_AE1022", "R01_AG1022CG")
var.y <- c("R01_AX0075", "R01_AX0076")
howmany <- 2
out<-surveycc(design_object, var.x, var.y, howmany = howmany,
                selection = "ROWS")
out
plot(out, dim1 = 1, dim2 = 2)