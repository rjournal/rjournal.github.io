#Splinets -- orthonormal bases of splines
#Reading a raw spectrum obtained from the original data set (A01.csv).

#The file-path needs to be adequate to the location of data
Path='/Users/mats-ksp/Library/CloudStorage/Box-Box/Shared/HibaRani/MassSpectrometryData/LowResolution/Ovarian/Ovarian\ Data\ WCX2\ CSV/cancer/Group\ A/A01.csv'
Ovarian=read.table(Path,header=TRUE,sep=",",col.names=c('ms','Intensity'))

pdf('Fig4LeftTopMS.pdf') 
par(cex.axis=1.5,cex.lab=1.5)
plot(Ovarian$ms,Ovarian$Intensity,type='h',xlab='m/s',ylab='Intensity',col='orange',axes=FALSE)#,xaxt='n')
axis(1, at=seq(0, 20000, by=200),col = "blue")
axis(2, col = "blue")   
rect(9500,20,20700,40,col='aliceblue')
arrows(9500,20,11000,0,col='blue',length=0.1)
arrows(20700,20,19300,0,col='blue',length=0.1)
lines(9500+1.2*(Ovarian$ms[Ovarian$ms>11000]-11000),20+10*Ovarian$Intensity[Ovarian$ms>11000],col='orange')
dev.off()

install.packages("Splinets")
library(Splinets)

#Equally spaced 200 knots,
xi1=seq(min(Ovarian$ms),max(Ovarian$ms),length.out=200)
so1 = splinet(xi1)  #Orthogonal basis of splines
OvSpl1=project(as.matrix(Ovarian),basis=so1$os) #Projection to the basis


#pdf('Fig4RightTopMS.pdf') 
par(cex.axis=1.5,cex.lab=1.5)
plot(Ovarian$ms,Ovarian$Intensity,type='h',xlab='m/s',ylab='Intensity',col='orange',axes=FALSE)#,xaxt='n')
axis(1, at=seq(0, 20000, by=200),col = "blue")
axis(2, col = "blue")
lines(OvSpl1$sp)
rect(9500,20,20700,40,col='aliceblue')
arrows(9500,20,11000,0,col='blue',length=0.1)
arrows(20700,20,19300,0,col='blue',length=0.1)
lines(9500+1.2*(Ovarian$ms[Ovarian$ms>11000]-11000),20+10*Ovarian$Intensity[Ovarian$ms>11000],col='orange')
EvOvSpl1=evspline(OvSpl1$sp,x=Ovarian$ms[Ovarian$ms>11000])
lines(9500+1.2*(EvOvSpl1[,1]-11000),20+10*EvOvSpl1[,2],col='deepskyblue4')
#dev.off()

wghts=abs(Ovarian$Intensity)/sum(abs(Ovarian$Intensity)) #Weights for knot selection
xi2=sort(sample(Ovarian$ms,200,prob=wghts)) #Random knots
xi2[1]=Ovarian$ms[1] #To cover the entire range
xi2[200]=Ovarian$ms[length(Ovarian$ms)] #with knots
         so2 = splinet(xi2) #Orthogonal basis of splines
         OvSpl2=project(as.matrix(Ovarian),basis=so2$os) #Projection to the basis
#pdf('Fig4LeftBottomMS.pdf')
par(cex.axis=1.5,cex.lab=1.5)
plot(Ovarian$ms,Ovarian$Intensity,type='h',xlab='m/s',ylab='Intensity',col='orange',axes=FALSE)#,xaxt='n')
axis(1, at=seq(0, 20000, by=200),col = "blue")
axis(2, col = "blue")
lines(OvSpl2$sp,col='red')
rect(9500,20,20700,40,col='aliceblue')
arrows(9500,20,11000,0,col='blue',length=0.1)
arrows(20700,20,19300,0,col='blue',length=0.1)
lines(9500+1.2*(Ovarian$ms[Ovarian$ms>11000]-11000),20+10*Ovarian$Intensity[Ovarian$ms>11000],col='orange')
EvOvSpl2=evspline(OvSpl2$sp,x=Ovarian$ms[(Ovarian$ms>11000)])
lines(9500+1.2*(EvOvSpl2[,1]-11000),20+10*EvOvSpl2[,2],col='red')
#dev.off()

#Intervals of non interest:
#0-700 - it seems to be a noise
#5400-6500 - flat
#10800-11400 - flat
#12100-13800 - flat
#15900-16500 - flat
#18800-20000 - flat
#Proportion remaining: (20000-700+5400-6500+10800-11400+12100-13800+15900-16500+18800-20000)/20000=0.705
#Total number of equally spaced knots in order to  70.5%=200 is 100 = x if 70.5 = 200, x=100*200/70.5=283.6879
#Round this up to 300

xi3=seq(min(Ovarian$ms),max(Ovarian$ms),length.out=300)
so3 = splinet(xi3)
OvSpl3a=project(as.matrix(Ovarian),basis=so3$os)
# plot(Ovarian$ms,Ovarian$Intensity,type='h',xlab='m/s',ylab='Intensity',col='orange',axes=FALSE)#,xaxt='n')
# axis(1, at=seq(0, 20000, by=200),col = "blue")
# axis(2, col = "blue")
# lines(OvSpl3$sp,col='blue')
# plot(OvSpl3$sp,col='blue',vknots=FALSE)

IndKn=1:length(OvSpl3a$sp@knots)
Nsupp=5 #the number of support intervals
LE=c(700,6500,11400,13600,16500)
RE=c(5400,10800,12100,15900,18800)

OvSpl3=OvSpl3a$sp
OvSpl3@supp=list()
for(l in 1:Nsupp){
  J=IndKn[OvSpl3a$sp@knots>LE[l] & OvSpl3a$sp@knots<RE[l]]
  OvSpl3@supp[[l]]=matrix(c(min(J),max(J)),nrow=1)
}


OvSpl3@der=list()
Der=sym2one(OvSpl3a$sp@der[[1]])
for(l in 1:Nsupp){
  OvSpl3@der[[l]]=sym2one(Der[OvSpl3@supp[[l]][1,1]:OvSpl3@supp[[l]][1,2],],inv=TRUE)
}

OvSpl3@knots[c(OvSpl3@supp[[1]][1,1],OvSpl3@supp[[1]][1,2])]
OvSpl3@knots[c(OvSpl3@supp[[2]][1,1],OvSpl3@supp[[2]][1,2])]
OvSpl3@knots[c(OvSpl3@supp[[3]][1,1],OvSpl3@supp[[3]][1,2])]
OvSpl3@knots[c(OvSpl3@supp[[4]][1,1],OvSpl3@supp[[4]][1,2])]
OvSpl3@knots[c(OvSpl3@supp[[5]][1,1],OvSpl3@supp[[5]][1,2])]

# plot(Ovarian$ms,Ovarian$Intensity,type='h',xlab='m/s',ylab='Intensity',col='orange',axes=FALSE)#,xaxt='n')
# axis(1, at=seq(0, 20000, by=200),col = "blue")
# axis(2, col = "blue")
# lines(OvSpl4)
pdf('Fig4RightBottomMS.pdf')
par(cex.axis=1.5,cex.lab=1.5)
plot(Ovarian$ms,Ovarian$Intensity,type='h',xlab='m/s',ylab='Intensity',col='grey',axes=FALSE)#,xaxt='n')
axis(1, at=seq(0, 20000, by=200),col = "blue")
axis(2, col = "blue")   
lines(OvSpl3)
rect(9500,20,20700,40,col='aliceblue')
arrows(9500,20,11000,0,col='blue',length=0.1)
arrows(20700,20,19300,0,col='blue',length=0.1)
lines(9500+1.2*(Ovarian$ms[Ovarian$ms>11000]-11000),20+10*Ovarian$Intensity[Ovarian$ms>11000],col='orange')
EvOvSpl3=evspline(OvSpl3,x=Ovarian$ms[(Ovarian$ms>11000)])
lines(9500+1.2*(EvOvSpl3[,1]-11000),20+10*EvOvSpl3[,4],col='darkorchid4',lwd='2')
lines(9500+1.2*(EvOvSpl3[,1]-11000),20+10*EvOvSpl3[,5],col='darkolivegreen4',lwd='2')
lines(9500+1.2*(EvOvSpl3[,1]-11000),20+10*EvOvSpl3[,6],col='deepskyblue',lwd='2')
dev.off()


# dev.off()




# The wine data set example

# Install DDK to get the Wine data set
# install.packages("devtools")
# devtools::install_github("ranibasna/ddk")
library(DDK) # the DDK package is needed to get the wine data
# get the data from the DKK pacakge
data("Wine")
# data preparation
f_data_wine <- Wine$x.learning
t_df_wine <- seq(1, dim(f_data_wine)[2])
# test data
f_data_wine_test <- Wine$x.test
t_df_wine_test <- seq(1, dim(f_data_wine_test)[2])
# remove raw 84 since it is an outlier
f_data_wine <- f_data_wine[-84,]

# selected knots
knots <- c(0, 21, 28, 40, 83, 139, 167, 186, 193, 256)

# prepare the knots
Wine_DDKnots <- knots / max(knots) * (max(t_df_wine) - min(t_df_wine)) + min(t_df_wine)
Wine_DDKnots <- as.numeric(format(round(Wine_DDKnots, 4), nsmall = 1))


# Prepare the data for projection
f_ready_data <- t(f_data_wine)
ready_data <- cbind(t_df_wine, f_ready_data)
WineData <- as.matrix(ready_data)


# Project the data and perform the eigen decomposition

WineProj = project(WineData, Wine_DDKnots) # Project wine data onto spline bases
Sigma = cov(WineProj$coeff)  # Covariance matrix of the projection coefficients
Spect = eigen(Sigma, symmetric = T)  # eigen decomposition of the covariance matrix
EigenSp = lincomb(WineProj$basis, t(Spect$vec))


# Functional eigenfunctions 

# Create a functional eigenfunctions  by linearly combining the splinets basis functions (from ProjObj basis) with the eigenvectors.


# Plot the eigenfunction-eigenvalue scaled for EigenNumber = 3
y <- evspline(EigenSp, sID = 1:3)
Arg <- y[,1]
Val <- y[,-1,drop=FALSE]
#pdf('Fig5RightEigenFuncWine.pdf')
par(cex.axis=1.5,cex.lab=1.5)
plot(Arg, Val[,1] * sqrt(Spect$values[1]), type='l', bty='n', col='deepskyblue4', lty=1, lwd=2, xlab="", ylab="")
ourcol <- c('darkorange3', 'goldenrod', 'darkorchid4', 'darkolivegreen4', 'deepskyblue', 'red4', 'slateblue','deepskyblue4')
for(i in 2:3) {
  lines(Arg, Val[,i] * sqrt(Spect$values[i]), col=ourcol[(i-2) %% 8 + 1], lty=1, lwd=2)
}
abline(v = EigenSp@knots, lty = 3, lwd = 0.5)
abline(h = 0, lwd = 0.5)
#dev.off()
#pdf('Fig5LeftFhatWine.pdf')
CmatWine=WineProj$coeff %*% Spect$vec
par(cex.axis=1.5,cex.lab=1.5)
matplot(WineData[,1],WineData[,2],type='l',lty=1,xlab='',ylab='', bty="n", col="deepskyblue4", xlim = c(-1.5,dim(t(WineData[, -1]))[2]))
lines(WineProj$sp,sID=2-1,col='goldenrod',lty=1,lwd=1)
lines(lincomb(subsample(EigenSp, 1),CmatWine[1,1,drop=F]),col='darkorange3')
abline(v = EigenSp@knots, lty = 3, lwd = 0.5)
#dev.off()

#Alternative plots using the features of the Splinets package
ScEigenSp=lincomb(EigenSp,diag(sqrt(Spect$values)))
plot(ScEigenSp)
WineSpl1=subsample(WineProj$sp,c(1))
plot(WineSpl1,ylim=c(-0.2,0.5),col=2,lty=2)
lines(WineData[,1],WineData[,2],type='l',lty=1,col="deepskyblue4")
cf=gramian(WineSpl1,Sp2=EigenSp)
WineSpl3D=lincomb(subsample(EigenSp,1:3),matrix(cf[1,1:3],ncol=3))
lines(WineSpl3D)
