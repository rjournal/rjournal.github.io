#Splinets -- orthonormal bases of splines
#Reading a raw spectrum obtained from the original data set (A01.csv).

#The file-path needs to be adequate to the location of data
Path='Ovarian\ Data\ WCX2\ CSV/cancer/Group\ A/A01.csv'
Ovarian=read.table(Path,header=TRUE,sep=",",col.names=c('ms','Intensity'))

pdf('Fig4LeftTopMS.pdf') 
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
