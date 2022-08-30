############################################################################################
##                                                                                        ##
## Replication code in Fernandez et al. (2021) FMM: An R Package for Modeling Rhythmic    ##
## Patterns in Oscillatory Systems                                                        ##
##                                                                                        ##
## Last modified 15/12/2021                                                               ##
##                                                                                        ##
############################################################################################

##----------------------------------------------------------------##
## 0. Load required packages
library("FMM")
library("RColorBrewer")
library("ggplot2")
library("grid")
library("gridExtra")
library("cowplot")


############################################################################################
## Results presented in section 4: simulated example                                      ##
############################################################################################
# four waves with two blocks of beta and omega
set.seed(115)
rfmm.data <-generateFMM(M = 3, A = c(4,3,1.5,1), alpha = c(3.8,1.2,4.5,2),
                        beta = c(rep(3,2),rep(1,2)), omega = c(rep(0.1,2),rep(0.05,2)),
                        plot = FALSE, outvalues = TRUE,
                        sigmaNoise = 0.3)
fit.rfmm <- fitFMM(vData = rfmm.data$y, timePoints = rfmm.data$t, nback = 4,
                   betaOmegaRestrictions = c(rep(1,2),rep(2,2)))
summary(fit.rfmm)

# Plot the fitted FMM model
titleText <- "Simulation of four restricted FMM waves"
defaultrFMM2 <- plotFMM(fit.rfmm, use_ggplot2 = TRUE, textExtra = titleText)
defaultrFMM2 <- defaultrFMM2 + theme(plot.margin=unit(c(1,0.25,1.3,1), "cm")) +
  ylim(-5, 6)
comprFMM2 <- plotFMM(fit.rfmm, components=TRUE, use_ggplot2 = TRUE, textExtra = titleText)
comprFMM2 <- comprFMM2 + theme(plot.margin=unit(c(1,0.25,0,1), "cm")) +
  ylim(-5, 6) + scale_color_manual(values = brewer.pal("Set1",n = 8)[3:6])

grid.arrange(defaultrFMM2, comprFMM2, nrow = 1)

# save the plot in the *.pdf format
fig2 <- arrangeGrob(defaultrFMM2, comprFMM2, nrow = 1)
ggsave("Figure_Sim2.pdf", plot = fig2, device = "pdf", width = 15)



############################################################################################
## Results presented in section 5: real data examples                                     ##
############################################################################################
##----------------------------------------------------------------##
## 5.0. Define functions used in this section
## to fit COS model
fourierDC<-function(m_t,N,x){

  # matrix of combinations
  nn <- N
  l <- rep(list(0:1), nn)
  expand<-expand.grid(l)+1

  # Fourier transform
  ff<-fft(m_t,inverse=FALSE)/length(m_t)

  # extract A0
  A0<-Re(ff[1])

  # extract fft values distinguishing between positives and negatives
  mConj<-matrix(0,2,N)
  # extract positive and negative harmonics
  mConj[1,]<-ff[2:(N+1)]
  mConj[2,]<-ff[length(m_t):(length(m_t)-N+1)]

  # combine fft values
  mComb<-matrix(0,nrow(expand),ncol(expand))
  for( i in 1:nrow(expand)){
    for(j in 1:length(expand[i,])){
      mComb[i,j]<-mConj[expand[i,j],j]
    }
  }

  # compute different Fourier time series and coefficients
  mOutCoefRe<-c();mOutCoefIm<-c()
  mOutSerie<-c()
  mse<-99999999999999999
  for( i in 1:nrow(mComb)){
    realC<-Re(mComb[i,])*2
    imC<-Im(mComb[i,])*2

    sumRe<-0;sumIm<-0

    for(j in 1:N){
      sumRe<-sumRe+realC[j]*cos(j*x)
      sumIm<-sumIm+imC[j]*sin(j*x)
    }
    suma<-A0+sumRe+sumIm
    ecm2<-sum((suma-m_t)^2)
    if(ecm2<mse){
      mOutCoefRe<-realC
      mOutCoefIm<-imC
      mOutSerie<-suma
      mse<-ecm2
    }
  }

  return(list(mOutSerie,A0,mOutCoefRe,mOutCoefIm))
}

## to build a sequence of equally time points spaced in range [0,2*pi]
secTimes<-function(peri){
  newTime<-seq(0,2*pi,by=2*pi/peri)
  newTime<-newTime[-length(newTime)]
  return(newTime)
}

##----------------------------------------------------------------##
## 5.1. Use of the FMM package in chronobiology
data("mouseGeneExp", package = "FMM")

## Fit the FMM model with nback = 1 component and nPeriods = 2
fitGene <- fitFMM(vData=mouseGeneExp, nPeriods = 2, nback = 1, showProgress = FALSE)
summary(fitGene)

## Fit the COS model
# mean at each time point across the 2 periods
meanGeneData <- fitGene@summarizedData
nHarmonicsFD <- 1
timePoints <- secTimes(length(meanGeneData))
fitFD <- fourierDC(meanGeneData,nHarmonicsFD,timePoints)
# R^2
1-sum((meanGeneData - fitFD[[1]])^2)/sum((meanGeneData - mean(meanGeneData))^2)


## Output the results to Figure 3
plotFMM(objFMM = fitGene, components = FALSE, plotAlongPeriods = TRUE,
        use_ggplot2 = TRUE, legendInComponentsPlot = TRUE, textExtra = "") +
  geom_path(aes(x = 1:length(mouseGeneExp), y = rep(fitFD[[1]],2), color="COS"),inherit.aes = FALSE,
            size = 2,lineend = "round",linejoin = "round") +
  theme(text = element_text(size=15),
        plot.title = element_blank(),
        legend.position = "bottom") +
  scale_color_brewer(name="Model",palette = "Set1", direction = -1)

# save the plot in the *.pdf format
ggsave("Figure_gene_Example.pdf", device = "pdf", height = 6, width = 9, dpi = 320)



##----------------------------------------------------------------##
## 5.2. Use of the FMM package in electrocardiography
data("ecgData", package = "FMM")

## Fit the FMM model with nback = 5 component
fitEcg <- fitFMM(ecgData, nback = 5, showProgress = FALSE)
summary(fitEcg)

## Output the results to Figure 4
#  Data preprocessing
peaksInfo <- getFMMPeaks(objFMM = fitEcg, timePointsIn2pi = FALSE)
mainWaves <- c(1,2,5)
colorECG <- brewer.pal(9, "Set1")[c(2:6)][c(2,4,3,5,1)]
peakData <- data.frame("Times" = peaksInfo$tpeakU[mainWaves],
                       "Values" = peaksInfo$ZU[mainWaves],
                       "Comp" = factor(c("R","P","T")),
                       "Color" = colorECG[mainWaves],
                       "LabelValues" = peaksInfo$ZU[mainWaves] + c(-0.2,0.03,+0.03))


# Figure 4.a
basicFit <- plotFMM(objFMM = fitEcg, use_ggplot2 = TRUE)
modelFit <- basicFit +
  geom_point(data = peakData, aes(x = Times, y = Values, fill = Comp),
             stroke = 1.2, size = 3, inherit.aes = FALSE) +
  geom_text(data = peakData, aes(x = Times, y = Values + 0.15, label = Comp),
            color = peakData$Color, size = 8, fontface = "bold") +
  ggtitle(label="") +
  coord_cartesian(ylim = c(4.35,6.5)) +
  theme(plot.margin = unit(c(1,0.25,1,1), "cm"))


# Figure 4.b
compFit <- plotFMM(objFMM = fitEcg, components = TRUE, use_ggplot2 = TRUE,
                   legendInComponentsPlot = FALSE)
waveFit <- compFit +
  annotation_custom(textGrob(expression(t[P])), xmin = peakData$Times[2],
                    xmax = peakData$Times[2], ymin = 4.17, ymax = 4.17) +
  annotation_custom(textGrob(expression(t[R])), xmin = peakData$Times[1],
                    xmax = peakData$Times[1], ymin = 4.17, ymax = 4.17) +
  annotation_custom(textGrob(expression(t[T])), xmin = peakData$Times[3],
                    xmax = peakData$Times[3], ymin = 4.17, ymax = 4.17) +
  scale_color_manual(values = colorECG) +
  geom_segment(data = peakData, aes(x = Times, xend = Times, y = 4.24,
                                    yend = Values + c(-0.2,0,0)), linetype="dashed",
               color = peakData$Color, size = 1, inherit.aes = FALSE) +
  geom_point(data = peakData, aes(x = Times, y = LabelValues, fill = Comp),
             stroke = 1.2, size = 3, inherit.aes = FALSE) +
  geom_text(data = peakData, aes(x = Times, y = LabelValues + 0.15, label = Comp),
            color = peakData$Color, size = 8, fontface = "bold",
            inherit.aes = FALSE) +
  ggtitle(label = "") +
  coord_cartesian(ylim = c(4.35,6.5)) +
  theme(plot.margin = unit(c(1,0.25,1,1),"cm"), axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "white"))

waveFit$layers[[1]]$aes_params$size <- 2
waveFit <- ggplotGrob(waveFit)
waveFit$layout$clip[waveFit$layout$name == "panel"] <- "off"


grid.arrange(modelFit, waveFit, nrow = 1)

# save the plot in the *.pdf format
fig3 <- arrangeGrob(modelFit, waveFit, nrow = 1)
ggsave("Figure_ECG_Example.pdf", plot = fig3, device = "pdf", width = 15)



##----------------------------------------------------------------##
## 5.3. Use of the FMM package in neuroscience
### 5.3.1. Example 1: single AP
data("neuronalSpike", package = "FMM")

### Fit the FMM model with nback = 2 component
fitSingleAP <- fitFMM(neuronalSpike, nback = 2, showProgress = FALSE)
summary(fitSingleAP)

### Fit the FD model
fdfit<-fourierDC(neuronalSpike,4,seqTimes(length(neuronalSpike)))[[1]]

### Output the results to Figure 5
## Color definition
usedColor<-brewer.pal(name="Set1",n=8)
colorsForModels <- usedColor[1:2]
names(colorsForModels) <- c("FMM", "FD")

## Plot of the figure
(figure5<-plotFMM(fitSingleAP,use_ggplot2 = T)+
    geom_path(aes(x=1:length(neuronalSpike), y=fdfit, color="FD"),inherit.aes = FALSE,
              size=2,lineend = "round",linejoin = "round")+
    scale_color_manual(name="Model", values = colorsForModels)+
    theme(text = element_text(size=15),
          plot.title = element_blank(),
          legend.position = "bottom"))

# save the plot in the *.pdf format
ggsave("Figure_singleAP_Example.pdf",figure5,
       height = 6, width = 9, dpi=320, device=cairo_pdf)



### 5.3.2. Example 2: AP train
data("neuronalAPTrain", package = "FMM")

## Fit the restricted FMM model
nAPs <- 3
restriction <- c(rep(1,nAPs),rep(2,nAPs))
fitAPTrain<-fitFMM(neuronalAPTrain, nback = nAPs*2,
                   betaOmegaRestrictions = restriction,
                   showProgress = FALSE, parallelize = TRUE)
summary(fitAPTrain)

### Output the results to Figure 6
## Color and theme definition
usedColor<-brewer.pal(name="Set1",n=8)
colorsForComponents<-usedColor[3:8]
usedTheme<-theme(plot.title = element_blank(),
                 plot.margin=unit(c(1,0.25,1,1), "cm"))
yLimits<-c(-30,115)

## Figure 6.a
plotFittedModel<-plotFMM(fitAPTrain, use_ggplot2 = TRUE)+
  usedTheme+ylim(yLimits)

## Figure 6.b
plotComponentsModel<-plotFMM(fitAPTrain, use_ggplot2 = TRUE, components = T) +
  usedTheme +
  theme(legend.position = "none") +
  guides(color=guide_legend(ncol=6,title.position = "left", title = NULL)) +
  scale_color_manual(values=colorsForComponents) + ylim(yLimits)

figure6<-plot_grid(plotFittedModel,plotComponentsModel,ncol=2,align="v")

# save the plot in the *.pdf format
ggsave(filename="Figure_APTrain_Example.pdf", width=9, height=5, dpi=320, plot=figure6, device = cairo_pdf)
