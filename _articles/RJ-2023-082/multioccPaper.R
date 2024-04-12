#########################################################################
######## Code to reproduce the example in the manuscript ################
######## Note this is also available as the vignette on CRAN ############
#########################################################################

library(interp)
library(MCMCpack)
library(tmvtnorm)
library(truncnorm)
library(multiocc)
library(MASS)
library(corrplot)
library(fields)

##### the data sets are included in the multiocc packages
data(detection)
data(occupancy)
data(coords)

#### create the DataNames list
DataNames <- list("species"=colnames(detection)[4:9],
                  "detection"=c("duration"),"occupancy"=c("forest","elev"))

#### run the multioccbuild function to generate model.input
model.input <- multioccbuild(detection, occupancy, coords, DataNames, threshold = 15000)

#### generate the plots included as EDA in the manuscript
par(mfrow=c(1,3))
hist(occupancy$forest, main="", xlab="Forest")
hist(occupancy$elev, main="", xlab="Elevation")
hist(detection$duration, main="", xlab="Duration")

par(mfrow=c(3,2), mar=c(3,3,3,1))
quilt.plot(coords[,2:3], occupancy$forest[1:267], main="Forest Cover", zlim=c(-1.5,3))
fit <- Tps(coords[,2:3], occupancy$forest[1:267])
out <- predictSurface(fit, df=100)
image.plot(out, main="Forest Cover (interpolated)", zlim=c(-1.5,2))

quilt.plot(coords[,2:3], occupancy$elev[1:267], main="Elevation", zlim=c(-1.5,3.5))
fit <- Tps(coords[,2:3], occupancy$elev[1:267])
out <- predictSurface(fit, df=100)
image.plot(out, main="Elevation (interpolated)", zlim=c(-1.5,2))

quilt.plot(coords[,2:3], detection$duration[1:267], main="Duration", zlim=c(-2.5,3))
fit <- Tps(coords[,2:3], detection$duration[1:267])
out <- predictSurface(fit, df=100)
image.plot(out, main="Duration (Survey 1)", zlim=c(-2.5,2.5))

### a short MCMC run for demonstration
mcmc.out <- GibbsSampler(M.iter=10, M.burn=1, M.thin=1, model.input, q=10, sv=FALSE)

### the longer MCMC that was run for the manuscript
mcmc.out <- GibbsSampler(M.iter=50000, M.burn=20000, M.thin=1, model.input, q=10, sv=FALSE)

### summarize output
summary(mcmc.out$samples$alpha)
summary(mcmc.out$samples$rho)

### visualize the residual correlation matrix
par(mfrow=c(1,1), mar=c(3,3,3,1))
sigout <- mcmc.out$samples$sig
Sig <- matrix(colMeans(sigout),6,6)
SpeciesCor <- cov2cor(Sig)
rownames(SpeciesCor) <- DataNames$species
colnames(SpeciesCor) <- DataNames$species
corrplot::corrplot(SpeciesCor)

### make predictions from the fitted model
y.agg1 <-  aggregate(model.input$y[,1], by=list(model.input$detection.info$siteID, 
                                                model.input$detection.info$season), FUN=sum, na.rm=TRUE)
y.plot1 <- 1*(y.agg1$x>0)

y.agg2 <- aggregate(model.input$y[,2], by=list(model.input$detection.info$siteID, 
                                               model.input$detection.info$season), FUN=sum, na.rm=TRUE)
y.plot2 <- 1*(y.agg2$x>0)

y.agg3 <- aggregate(model.input$y[,3], by=list(model.input$detection.info$siteID, 
                                               model.input$detection.info$season), FUN=sum, na.rm=TRUE)
y.plot3 <- 1*(y.agg3$x>0)

y.agg4 <- aggregate(model.input$y[,4], by=list(model.input$detection.info$siteID, 
                                               model.input$detection.info$season), FUN=sum, na.rm=TRUE)
y.plot4 <- 1*(y.agg4$x>0)

y.agg5 <- aggregate(model.input$y[,5], by=list(model.input$detection.info$siteID, 
                                               model.input$detection.info$season), FUN=sum, na.rm=TRUE)
y.plot5 <- 1*(y.agg5$x>0)

y.agg6 <- aggregate(model.input$y[,6], by=list(model.input$detection.info$siteID, 
                                               model.input$detection.info$season), FUN=sum, na.rm=TRUE)
y.plot6 <- 1*(y.agg6$x>0)

for (yr in c(1,4,7,10)){
  print(yr)
  
  range <- which(model.input$occupancy.info$season == yr)
  
  psiout <- mcmc.out$samples$psi
  #pout <- mcmc.out$p
  dim(psiout)
  
  psi1 <- apply(psiout[,0*2670+range],2,mean)
  psi2 <- apply(psiout[,1*2670+range],2,mean)
  psi3 <- apply(psiout[,2*2670+range],2,mean)
  psi4 <- apply(psiout[,3*2670+range],2,mean)
  psi5 <- apply(psiout[,4*2670+range],2,mean)
  psi6 <- apply(psiout[,5*2670+range],2,mean)
  
  par(mfrow=c(3,2), mar=c(1,3,3,1))
  fit <- Tps(coords[1:267,2:3], psi1)
  out <- predictSurface(fit, df=100)
  image.plot(out, main="Great Tit", zlim=c(-0.01,1.01))
  mtext(paste("Year",yr), side=3, line=-2, outer=TRUE)
  
  y.plot1.in <- y.plot1[which(model.input$occupancy.info$season ==yr)]
  points(coords[which(y.plot1.in==1),2:3])
  
  fit <- Tps(coords[1:267,2:3], psi2)
  out <- predictSurface(fit, df=100)
  image.plot(out, main="Blue Tit", zlim=c(-0.01,1.01))
  
  y.plot2.in <- y.plot2[which(model.input$occupancy.info$season ==yr)]
  points(coords[which(y.plot2.in==1),2:3])
  
  fit <- Tps(coords[1:267,2:3], psi3)
  out <- predictSurface(fit, df=100)
  image.plot(out, main="Coal Tit", zlim=c(-0.01,1.01))
  
  y.plot3.in <- y.plot3[which(model.input$occupancy.info$season ==yr)]
  points(coords[which(y.plot3.in==1),2:3])
  
  fit <- Tps(coords[1:267,2:3], psi4)
  out <- predictSurface(fit, df=100)
  image.plot(out, main="Crested Tit", zlim=c(-0.01,1.01))
  
  y.plot4.in <- y.plot4[which(model.input$occupancy.info$season ==yr)]
  points(coords[which(y.plot4.in==1),2:3])
  
  fit <- Tps(coords[1:267,2:3], psi5)
  out <- predictSurface(fit, df=100)
  image.plot(out, main="Marsh Tit", zlim=c(-0.01,1.01))
  
  y.plot5.in <- y.plot5[which(model.input$occupancy.info$season ==yr)]
  points(coords[which(y.plot5.in==1),2:3])
  
  fit <- Tps(coords[1:267,2:3], psi6)
  out <- predictSurface(fit, df=100)
  image.plot(out, main="Willow Tit", zlim=c(-0.01,1.01))
  
  y.plot6.in <- y.plot6[which(model.input$occupancy.info$season ==yr)]
  points(coords[which(y.plot6.in==1),2:3])
}
