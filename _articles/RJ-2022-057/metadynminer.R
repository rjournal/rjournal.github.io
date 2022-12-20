# Load packages metadynminer and metadynminer3d
library(metadynminer)
library(metadynminer3d)

# This demo use built-in datasets acealanme and acealanme3d, you can
# obtain identical plots using data at
# https://www.metadynamics.cz/metadynminer/data/

# Figure 1 was assembled in Inkscape from R and Visual Molecular
# Dynamics software (https://www.metadynamics.cz/metadynminer/data/).
# All data are available in fig1.tar.gz

# Reading input HILLS file
hillsfile <- read.hills("HILLS.txt", per=c(TRUE, TRUE))

# Generate Figure 2
png("fig2.png", height=7, width=7, units='cm', res=600, pointsize=6)
plot(hillsfile, xlab="phi", ylab="psi", pch=19, cex=0.5, col=gray(0, 0.1))
dev.off()

# Generate Figure 3
png("fig3.png", height=7, width=7, units='cm', res=600, pointsize=6)
plotheights(hillsfile)
dev.off()

# Generate Figure 4
fesurface<-fes(hillsfile)
png("fig4.png", height=7, width=7, units='cm', res=600, pointsize=6)
plot(fesurface, xlab="phi", ylab="psi")
dev.off()

# Generate Figure 5
minima <- fesminima(fesurface)
summary(minima)
png("fig5.png", height=7, width=7, units='cm', res=600, pointsize=6)
plot(minima, xlab="phi", ylab="psi")
dev.off()

# Generate Figure 6
prof <- feprof(minima)
png("fig6.png", height=7, width=7, units='cm', res=600, pointsize=6)
plot(prof)
dev.off()

# Generate Figure 7
nebAD <- neb(minima, min1="A", min2="D")
png("fig7.png", height=7, width=7, units='cm', res=600, pointsize=6)
plot(minima, xlab="phi", ylab="psi")
linesonfes(nebAD, lwd=4)
dev.off()

# Generate Figure 8 (this figure was reasmpled and cropped to be
# compatible with other figures)
hillsfile3d <- read.hills3d("HILLS3d.txt", per=c(TRUE, TRUE, TRUE))
fesurface <- fes(hillsfile3d)
r3dDefaults <- getr3dDefaults()
r3dDefaults$windowRect <- c(0,0, 1080, 1080)
plot(fesurface, level=-30, xlab="phi", ylab="psi", zlab="omega")
rgl.snapshot("fig8.png", fmt="png", top=TRUE)

# Generate Figure 9
bf <- 15
kT <- 8.314*300/1000
npoints <- 50
maxfes <- 75
outfes <- 0*fes(hillsfile, npoints=npoints)
step <- 1:50*length(hillsfile$time)/50
s1 <- sapply(step, FUN=function(x) {
          sum(exp(-fes(hillsfile, imax=x)$fes/kT))
})
s2 <- sapply(step, FUN=function(x) {
          sum(exp(-fes(hillsfile, imax=x)$fes/kT/bf))
})
ebetac <- s1/s2
cvs <- read.table("COLVAR.txt")
nsamples <- nrow(cvs)
xlim <- c(-pi,pi)
ylim <- c(-pi,pi)
step <- (1:nsamples-1)*50/nsamples+1
ix <- npoints*(cvs[,2]-xlim[1])/(xlim[2]-xlim[1])+1
iy <- npoints*(cvs[,3]-ylim[1])/(ylim[2]-ylim[1])+1
for(i in 1:nsamples) {
  outfes$fes[ix[i],iy[i]] <- outfes$fes[ix[i],iy[i]] + exp(cvs[i,4]/kT)/
ebetac[step[i]]
}
outfes$fes <- -kT*log(outfes$fes)
outfes <- outfes - min(outfes)
outfes$fes[outfes$fes>maxfes] <- maxfes
png("fig9.png", height=7, width=7, units='cm', res=600, pointsize=6)
plot(outfes, xlab="phi", ylab="psi")
dev.off()

