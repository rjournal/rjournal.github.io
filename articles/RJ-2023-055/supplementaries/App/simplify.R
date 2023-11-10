# Simplify boundaries of the datasets for the App
# IMPORTANT: This script has not actually been used

library(sf)
library(parallel)
options(mc.cores = 3)

load("Spain.RData")

res <- lapply(d, function(X) {
  print(names(X))

  res <- list(none = st_simplify(X$none, dTolerance = 100 * 0.008))

  aux <- lapply(X[-1], function(Y) {
    mclapply(Y, function(Z) {
      st_simplify(Z, dTolerance = 100 * 0.008)
    })
  })

  c(res, aux)
})


# Compare sizes
object.size(d)
object.size(res)

