runSimulation = function() {
  set.seed(1)
  s = c(10, 100, 1000, 10000, 100000, 1000000)
  
  for (distribution in 1:28) {
    for (samples in s) {
      write(t(compareEstimates(distribution, sampleSize = samples, trials = 1, types = c("pdfe", "kde", "kernsmooth", "np"))),
            "SimulationResults.txt", append = TRUE, ncolumns = 1)
    }
  }
}