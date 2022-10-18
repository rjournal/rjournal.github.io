# Produces comparison plots for chi-squared error and computational time,
# using files produced in runSimulation.

plotComparisons = function(d = 1:28, plotAccuracy = TRUE, 
                           plotTime = TRUE, projectNP = FALSE,
                           plotScatter = FALSE, plotLegend = FALSE,
                           yAxis = TRUE, xAxis = TRUE) {
  #s = c(50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000)
  s = c(10, 100, 1000, 10000, 100000, 1000000)
  
  pdfTypes = c("pdfe", "kde", "kernsmooth", "np")     #all supported estimation
  colorTypes = c("cyan4", "navy", "orangered",  "deeppink4")
  legendTypes = c("estimatePDF()", "density()", "bkde()", "npudens()")
  npSamples = 3
  nSamples = c(length(s), length(s), length(s), npSamples)
  nTypes = length(pdfTypes)
  metricName = "Chi-Squared"

  averageAccuracy = array(data = 0, dim = c(nTypes, length(s)))
  averageTime = array(data = 0, dim = c(nTypes, length(s)))

  filename = "SimulationResults.txt"
  if (!file.exists(filename)) {
    source("runSimulation.R")
    runSimulation()
  }
  info = read.table(filename)
  
  cDistribution = 0
  for (distribution in d) {
    cDistribution = cDistribution + 1
    accuracy = array(data = 0, dim = c(nTypes, length(s)))
    clockTime = array(data = 0, dim = c(nTypes, length(s)))
    cSample = 0
    rowStart = (distribution - 1) * length(s) * nTypes + 1
    for (samples in s) {
      rowEnd = (rowStart + nTypes - 1)
      cSample = cSample + 1
      accuracy[1:nTypes, cSample] = info[rowStart:rowEnd, 1]
      clockTime[1:nTypes, cSample] = info[rowStart:rowEnd, 2]
      rowStart = rowEnd + 1
    }
    
    averageAccuracy = averageAccuracy + accuracy
    averageTime = averageTime + clockTime
  }

  averageAccuracy = averageAccuracy / length(d)
  averageTime = log10(averageTime / length(d))
  
  if (plotScatter) {
    metricFinite = is.finite(averageTime)
    maxX = max(averageTime[metricFinite])
    minX = min(averageTime[metricFinite])
    metricFinite = is.finite(averageAccuracy)
    maxY = max(averageAccuracy[metricFinite])
    minY = min(averageAccuracy[metricFinite])
    cType = 0
    for (types in pdfTypes) {
      cType = cType + 1
      finiteTime = is.finite(averageTime[cType, ])
      finiteAccuracy = is.finite(averageAccuracy[cType, ])
      if (cType == 1) {
        plot((averageTime[cType, finiteTime]), 
             averageAccuracy[cType, finiteTime], 
             col = colorTypes[cType], type = "l", lwd = 2,
             xlim = c(minX, maxX), ylim = c(minY, maxY),
             ylab = 'accuracy', xlab = 'log(seconds)')
      } else {
        lines((averageTime[cType, finiteTime]), 
              averageAccuracy[cType, finiteTime],
              col = colorTypes[cType], type = "l", lwd = 2,
              xlim = c(minX, maxX), ylim = c(minY, maxY),
              ylab = 'accuracy', xlab = 'log(seconds)')
      }
    }
  }
  
  if (xAxis) {
    xlabel = "log(sample size)"
  } else {
    xlabel = ""
  }
  
  if (plotTime) {
    metricFinite = is.finite(averageTime)
    maxY = max(averageTime[metricFinite])
    minY = min(averageTime[metricFinite])
    cType = 0
    for (types in pdfTypes) {
      cType = cType + 1 
      if (yAxis) {
        ylabel = "log(seconds)"
      } else {
        ylabel = ""
      }
      finiteTime = is.finite(averageTime[cType, 1:nSamples[cType]])
      if (projectNP) {
          maxY = 8
      }
     
      if (cType == 1) {
        plot((log10(s[finiteTime])), 
             averageTime[cType, finiteTime], 
             type = "l", col = colorTypes[cType], 
             lwd = 2,
             xlab = xlabel, ylab = ylabel, ylim = c(minY, maxY),
             xlim = c(log10(min(s)), log10(max(s))))
      } else {
        lines((log10(s[finiteTime])), 
             averageTime[cType, finiteTime], 
             type = "l", col = colorTypes[cType], 
             lwd = 2,
             xlab = xlabel, ylab = ylabel, ylim = c(minY, maxY),
             xlim = c(log10(min(s)), log10(max(s))))
      }

    }
    if (projectNP) {
      times = c(6, 7)
      seconds = c(5.8554, 7.7836)
      lines(times, seconds, type = "p", col = "deeppink4", xlim = c(log10(min(s)), log10(max(s))), ylim = c(minY, maxY), xlab = "", ylab = "")
    }
  }

  if (plotAccuracy) {
    metricFinite = is.finite(averageAccuracy)
    maxY = max(averageAccuracy[metricFinite])
    minY = 0
    cType = 0
    for (types in pdfTypes) {
      cType = cType + 1
      if (yAxis) {
        ylabel = metricName
      } else {
        ylabel = ""
      }
      if (cType == 1) {
        plot((log10(s[1:nSamples[cType]])), 
             averageAccuracy[cType, 1:nSamples[cType]], 
             type = "l", col = colorTypes[cType], lwd = 2,
             xlab = xlabel, ylab = ylabel, ylim = c(minY, maxY),
             xlim = c(log10(min(s)), log10(max(s))))
      } else {
        lines((log10(s[1:nSamples[cType]])), 
             averageAccuracy[cType, 1:nSamples[cType]], 
             type = "l", col = colorTypes[cType], lwd = 2,
        xlab = xlabel, ylab = ylabel, ylim = c(minY, maxY),
        xlim = c(log10(min(s)), log10(max(s))))
      }
    }
  }
  
  if (plotLegend) {
    legend(x = "topright", col = colorTypes[1:nTypes], 
         lwd = 1, cex = 0.6, bty = "n", 
         legend = legendTypes[1:nTypes])
  }
}