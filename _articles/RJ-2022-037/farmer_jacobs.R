#Running this function reproduces all figures in the 
# manuscript.  farmer_jacobs() requires the following source files:
#
#   farmer_jacobs.R
#   compareEstimates.R
#   getEstimate.R
#   plotComparisons.R
#   runSimulation.R (optional)
#
# If the SimulationResults.txt file is not found, runSimulation() is 
# automatically invoked to run the comparison simulation. The simulation 
# can be expected to complete within one hour on most platforms.

farmer_jacobs = function() {
  
  if(!require(PDFEstimator)){
    install.packages("PDFEstimator", type = "source")
    library(PDFEstimator)
  }
  if(!require(benchden)){
    install.packages("benchden")
    library(benchden)
  } 
  if(!require(bootstrap)){
    install.packages("bootstrap")
    library(bootstrap)
  }
  if(!require(philentropy)){
    install.packages("philentropy")
    library(philentropy)
  }
  if(!require(KernSmooth)){
    install.packages("KernSmooth")
    library(KernSmooth)
  }
  if(!require(np)){
    install.packages("np")
    library(np)
  }
  
  source("plotComparisons.R")
  source("compareEstimates.R")
  source("getEstimate.R")
  
  pdfPointSize = 10
  
  #Figure 1
  
  nTrials = 1000;      
  n = 10000;
  qz = array(data = NA, dim = nTrials);
  
  for (iTrial in 1:nTrials) {
    r = sort(runif(n, min = 0, max = 1));  
    u = (1:n) / (n + 1); 
    sigma = sqrt((u * (1 - u)) / (n + 2));
    z = (r - u) / sigma;
    qz[iTrial] = -sum(z ^ 2) / n;
  }
  
  PDFe = estimatePDF(qz);
  pdf(file = "Figure1a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(PDFe, xlab = "score", xlim = c(-4, 0));
  dev.off()
  pdf(file = "Figure1b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(PDFe$x, PDFe$cdf, type = "l", xlab = "score", ylab = "CDF", 
       xlim = c(-4, 0), lwd = 2);
  dev.off()
  
  
  #Figure 2
  pdf(file = "Figure2a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  j = seq(0, 1, 1/50)
  plotBeta(samples = j, xPlotRange = range(j))
  confidence = c(80, 95, 99)
  legendColors = c("red", "blue", "black")
  par(new = TRUE)
  i = 1
  targets = suppressWarnings(getTarget(length(j), confidence[i]))
  plot(j, targets[ , 1], type = "l", lty = 1, col = legendColors[i], 
        ylim = c(-2, 2), xlab = "order fraction", ylab = "SQR")
  lines(j, targets[ , 2], type = "l", lty = 1, col = legendColors[i])
  for(i in 2:3) {
    targets = suppressWarnings(getTarget(length(j), confidence[i]))
    lines(j, targets[ , 1], type = "l", lty = 1, col = legendColors[i])
    lines(j, targets[ , 2], type = "l", lty = 1, col = legendColors[i])
  }
  
  legend("topright", inset = 0.01, horiz = TRUE, legend = c("80%", "95%", "99%"), 
         col = legendColors, lty=1, cex=0.6, box.lty = 0)
  dev.off()
  
  
  pdf(file = "Figure2b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  j = seq(0, 1, 1/1000)
  plotBeta(samples = j, xPlotRange = range(j))
  confidence = c(80, 95, 99)
  legendColors = c("red", "blue", "black")
  par(new = TRUE)
  i = 1
  targets = suppressWarnings(getTarget(length(j), confidence[i]))
  plot(j, targets[ , 1], type = "l", lty = 1, col = legendColors[i], 
       xlab = "order fraction", ylab = "SQR", ylim = c(-2, 2))
  lines(j, targets[ , 2], type = "l", lty = 1, col = legendColors[i])
  for(i in 2:3) {
    targets = suppressWarnings(getTarget(length(j), confidence[i]))
    lines(j, targets[ , 1], type = "l", lty = 1, col = legendColors[i])
    lines(j, targets[ , 2], type = "l", lty = 1, col = legendColors[i])
  }
  
  legend("topright", inset = 0.01, horiz = TRUE, legend = c("80%", "95%", "99%"), 
         col = legendColors, lty=1, cex=0.6, box.lty = 0)
  dev.off()
  
 
  #Figure 3
  
  set.seed(1)
  est = estimatePDF(rberdev(n = 100000, dnum = 3))
  pdf(file = "Figure3a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(est)
  dev.off()
  pdf(file = "Figure3b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(est, plotShading = TRUE, plotSQR = TRUE, showOutlierPercent = 99,
       shadeResolution = 300)
  dev.off()
  
  #Figure 4
  
  set.seed(1)
  est = estimatePDF(rberdev(n = 100000, dnum = 27))
  pdf(file = "Figure4a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(est$x, dberdev(est$x, dnum = 27), col = "darkgray", type = "l", 
       lwd = 2, xlab = "x", ylab = "PDF")
  lines(est)
  dev.off()
  pdf(file = "Figure4b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(est, col = "darkgray", showOutlierPercent = 80, outlierColor =
         "darkcyan")
  dev.off()
  pdf(file = "Figure4c.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(est, plotSQR = TRUE, plotShading = TRUE, showOutlierPercent = 80,
       plotPDF = FALSE, outlierColor = "darkcyan", sqrColor = "black")
  dev.off()
  pdf(file = "Figure4d.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(est, plotSQR = TRUE, plotShading = TRUE, showOutlierPercent = 99,
       plotPDF = FALSE)
  dev.off()
  
  #Figure 5
  
  set.seed(3)
  sample = rberdev(10000, dnum = 19)
  e = estimatePDF(sample)
  kde = density(sample)
  pdf(file = "Figure5a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(e, plotPDF = FALSE, plotSQR = TRUE, plotShading = TRUE,
       showOutlierPercent = 99, sqrPlotThreshold = 6)
  dev.off()
  KDEtoPDFe = convertToPDFe(sample, kde$x, kde$y)
  pdf(file = "Figure5b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  plot(KDEtoPDFe, plotPDF = FALSE, plotSQR = TRUE, plotShading = TRUE,
       showOutlierPercent = 99, sqrPlotThreshold = 25)
  dev.off()
  
  #Figure 6
  
  left = sample <= -0.01
  middle = (sample > -0.01) & (sample < 0.01)
  right = sample >= 0.01
  est = estimatePDF(sample, lowerBound = -0.01, upperBound = 0.01)
  est2 = estimatePDF(sample, upperBound = max(e$x), lowerBound = 0.01)
  est1 = estimatePDF(sample, lowerBound = min(e$x), upperBound = -0.01)
  kest = density(sample[middle], from = -0.01, to = 0.01)
  kest2 = density(sample[right], to = max(kde$x), from = 0.01)
  kest1 = density(sample[left], from = min(kde$x), to = -0.01)
  KDEtoPDFe = convertToPDFe(sample[middle], kest$x, kest$y)
  KDEtoPDFe1 = convertToPDFe(sample[left], kest1$x, kest1$y)
  KDEtoPDFe2 = convertToPDFe(sample[right], kest2$x, kest2$y)
  pdf(file = "Figure6a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(0, 0, 4, 0), font.main = 1)
  plot(est1, plotPDF = F, plotSQR = T, main = "Left Tail", ylab = NA, 
       plotShading = T, showOutlierPercent = 99, sqrPlotThreshold = 12)
  mtext("estimatePDF()", side = 3, font = 2, adj = 0)
  dev.off()
  pdf(file = "Figure6b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(0, 0, 4, 0), font.main = 1)
  plot(est, plotPDF = F, plotSQR = T, ylab = NA, main = "Center",
       plotShading = T, showOutlierPercent = 99, sqrPlotThreshold = 12)
  dev.off()
  pdf(file = "Figure6c.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(0, 0, 4, 0), font.main = 1)
  plot(est2, plotPDF = F, plotSQR = T, ylab = NA, main = "Right Tail",
       plotShading = T, showOutlierPercent = 99, sqrPlotThreshold = 12)
  dev.off()
  pdf(file = "Figure6d.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(0, 0, 4, 0), font.main = 1)
  plot(KDEtoPDFe1, plotPDF = F, plotSQR = T, main = "",
       plotShading = T, showOutlierPercent = 99, sqrPlotThreshold = 12)
  mtext("density()", side = 3, font = 2, adj = 0)
  dev.off()
  pdf(file = "Figure6e.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(0, 0, 4, 0), font.main = 1)
  plot(KDEtoPDFe, plotPDF = F, plotSQR = T, ylab = NA, main = "",
       plotShading = T, showOutlierPercent = 99, sqrPlotThreshold = 12)
  dev.off()
  pdf(file = "Figure6f.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(0, 0, 4, 0), font.main = 1)
  plot(KDEtoPDFe2, plotPDF = F, plotSQR = T, ylab = , main = "",
       plotShading = T, showOutlierPercent = 99, sqrPlotThreshold = 12)
  dev.off()
  
  #Figure 7
  
  pdf(file = "Figure7a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(4, 4, 1, 1))
  plotComparisons(plotTime = FALSE, plotLegend = TRUE)
  dev.off()
  pdf(file = "Figure7b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(4, 4, 1, 1))
  plotComparisons(plotAccuracy = FALSE)
  dev.off()
  
  #Figure 8
  
  pdf(file = "Figure8a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(1, 4, 3, 0))
  plotComparisons(c(1:5,7,11,15:17,22,28), plotLegend = TRUE, plotTime = FALSE,
                  xAxis = FALSE)
  dev.off()
  pdf(file = "Figure8b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(1, 4, 3, 0))
  plotComparisons(c(6,9,10,20), plotTime = FALSE, yAxis = FALSE, xAxis = FALSE)
  dev.off()
  pdf(file = "Figure8c.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(4, 4, 0, 0))
  plotComparisons(c(14, 19), plotTime = FALSE)
  dev.off()
  pdf(file = "Figure8d.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  par(mar = c(4, 4, 0, 0))
  plotComparisons(c(21,23,24,27), plotTime = FALSE, yAxis = FALSE)
  dev.off()
  
  #Figure 9
  
  pdf(file = "Figure9a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  compareEstimates(27, 1000, types = c("pdfe", "np"), plotDistribution = TRUE,
                   methodName = "", colorTypes = c("cyan4","deeppink4"), ylim = c(0, 0.12))
  dev.off()
  
  pdf(file = "Figure9b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  compareEstimates(27, 50000, types = c("pdfe", "kde", "kernsmooth"), 
                   methodName = "", plotDistribution = TRUE, ylim = c(0, 0.12))
  dev.off()
  
  #Figure 10
  
  pdf(file = "Figure10a.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  pnts = seq(from = 0.065, to = 0.135, by = 0.0003)
  np = npudens(stamp$Thickness, edat = pnts)
  npPDFe = convertToPDFe(stamp$Thickness, pnts, np$dens)
  plot(npPDFe, col = "deeppink4")
  kde = density(stamp$Thickness)
  kdePDFe = convertToPDFe(stamp$Thickness, kde$x, kde$y)
  lines(kdePDFe, col = "navy")
  dev.off()
  
  pdf(file = "Figure10b.pdf", width = 3, height = 3, pointsize = pdfPointSize)
  iterations = 20
  surd = vector("numeric", length = iterations)
  average = vector("numeric", length = length(pnts))
  pdfe = estimatePDF(stamp$Thickness, estimationPoints = pnts)
  plot(pdfe, col = "gray")
  for(i in 1:iterations) {
    pdfe = estimatePDF(stamp$Thickness, estimationPoints = pnts)
    lines(pdfe, col = "gray")
    surd[i] = pdfe$threshold
    average = average + pdfe$pdf
  }
  average = average / iterations
  lines(pnts, average, lwd = 2)
  dev.off()
  
}