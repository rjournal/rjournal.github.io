## R Journal reproducibility script
## Reproduces all tables and figures in the article
##
## Replication requires:
##   MSTest >= 0.1.9
##
## The file article.R is distributed with the MSTest package
## (inst/examples/article.R) and is also available on GitHub (https://github.com/roga11/MSTest/blob/main/inst/examples/article.R).
## All replication results were produced using MSTest version 0.1.9,
## available from CRAN. Earlier versions do not include the required
## functions and bug fixes, and use a different random number generator,
## so they will not reproduce the numbers reported in the article.
##
## NOTE: The y-axis of figure produced here may be different than ones included 
##    in text, but values are the same

if (packageVersion("MSTest") < "0.1.9") {
  stop(
    "Replication requires MSTest >= 0.1.9. ",
    "Please update the package from CRAN."
  )
}

source(system.file("examples", "article.R", package = "MSTest"))
