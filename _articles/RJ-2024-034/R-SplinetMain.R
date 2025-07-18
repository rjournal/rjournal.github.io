######################
#The code accompanying the R-journal paper on Splinets package
# PART I - The code recreating all figures from the paper
######################

##### Installing and upploading package

install.packages("Splinets")
library(Splinets)

####################
#FIGURE 1
########
#Left figure - a splinet over an interval and non-equidistant knots

k=3 #the order of splines

#Explicitely, defined knots
xi=c(-0.1,0.0,0.03,0.05,0.09,0.36,0.38,0.49,0.56,
     0.62,0.63,0.77,0.79,0.85,0.86,0.865,0.91,0.945,0.95,1.1)

so = splinet(xi,smorder = k)

plot(so$os,type='dyadic') #Splinet


########
#Right figure - a full-dyadic periodic splinet over equidistant knots

k=3 #the order of splines
N=4 #the number of levels in the dyadic structure

n_knots=2^N*k-1 #the number of internal knots in the dyadic case

#Knots both internal and at the end
xi = seq(0, 1, length.out = n_knots+2) 

#Defining and ploting a periodic B-splines and corresponding splinet

so = splinet(xi,smorder = k, periodic = TRUE)

plot(so$os,type='dyadic') #Splinet

##################
#FIGURE 2
#######
#Left figure - an example of spline. 

k=3 #the order of splines

#Explicitely, defined knots
set.seed(2)
n=90
xi=cumsum(runif(n+2,min=0.2))

S=matrix(rnorm((n+2)*(k+1)),ncol=(k+1)) #Creating random matrix of derivatives
spl = construct(xi,k,S) #Function for constructing spline that corrects the matrix of derivatives

#pdf('Fig2Left.pdf',width=7,height=3)
plot(spl) #Splinet
#dev.off()

#########
#Middle figure - an example of spline over support spread over disjoint intervals


supp=c(2,16) #definition of the single interval support
SS=S[2:16,] #The matrix of derivatives
#over the support range
sspl=construct(xi,k,SS,supp=supp) #construction of a spline as the 'Splinets' object
#with the given support range, single interval
plot(sspl)



supp2=c(30,44) #definition of the single interval support
SS2=S[30:44,] #The matrix of derivatives
#over the support range
sspl2=construct(xi,k,SS2,supp=supp2) #construction of a spline as the 'Splinets' object
#with the given support range, single interval
plot(sspl2)

sspl=gather(sspl,sspl2)

supp3=c(75,86) #definition of the single interval support
SS3=S[75:86,] #The matrix of derivatives
#over the support range
sspl3=construct(xi,k,SS3,supp=supp3) #construction of a spline as the 'Splinets' object
#with the given support range, single interval
plot(sspl3)

sspl=gather(sspl,sspl3)

A=t(as.matrix(c(0.3,-2,-0.8)))

lcsppl=lincomb(sspl,A)

#pdf('Fig2Middle.pdf',width=7,height=3)
plot(lcsppl)
#dev.off()

#########
#Right figure - an example of a piecwise function but not a spline

spl@der[[1]]=S #random matrix of derivatives (smoothness conditions are not satisfied)

#pdf('Fig2Right.pdf',width=7,height=3)
plot(spl) #a piecewise cubic function
#dev.off()

##################
#FIGURE 3
#######
#Left figure - B-splines on a full dyadic net 

k=3 #the order of splines
N=5 #the number of layers
n=k*2^N-1 #
#Explicitely, defined knots
set.seed(2)

xi=cumsum(runif(n+2,min=0.2))
so = splinet(xi, k); 
Bsplines=so$bs



#pdf('Fig3Left.pdf',width=7,height=5.5)
plot(splnt$bs,type='dyadic') #B-splines
#dev.off()

#######
#Right figure - splinet 
Splinet=so$os
plot(Splinet)
#pdf('Fig3Right.pdf',width=7,height=5.5)
plot(splnt$os) #OB-splines - splinet
#dev.off()

##################
#FIGURE 4
#######
#All four figures illustrating orthogonal projection in the package

#Creating discretized data
k=3
xi=c(-0.1,0.0,0.03,0.05,0.09,0.36,0.38,0.49,0.56,0.62,0.63,0.77,0.79,0.85,0.86,0.865,0.91,0.945,0.95,1.1)

so = splinet(xi,smorder = k)

set.seed(2)
A=matrix(rnorm(10*16),nrow=10)
splinesample=lincomb(so$bs,A) #linear combination of the element of the splinet 'so$os'


Data=evspline(splinesample)
Data[,2:11]=Data[,2:11]+0.1*matrix(rnorm(10*4750),ncol=10) #adding noise to the data

project(Data,basis=so$os,graph=T) #The four graphs are generated

file.create("/Users/hibna/Documents/Revision2/R-SplinetMain.R")

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


#prepare Fashion-MINST
# dwnld -- a logical flag to download data using 'keras' and 'tensorflow' data already in a file 'dwnld=FALSE'
# dname -- choice for the original data name
# pathfile -- the path to the data (either they are already there will be put there)
data_upload=function(dname='fashion_mnist',dwnld=FALSE,pathfile){
  if(dwnld==TRUE)
  {
    install.packages("keras")
    install.packages("tensorflow")
    library(tensorflow)
    install_tensorflow()
    library(keras)
    
    
    # Load the data ----
    #Running approx. 1min on MacBook Pro, Processsor: Apple M1 Pro, Memmory: 16GB 
    dname <- keras::dataset_fashion_mnist()
    
    # Saving the data so the uploading using `keras` and `tensorflow` will not be anymore
    # required. 53MB Disk space.
    
    save(dname,file=pathfile)
    
  }
  ############
  # Here it starts if the data has been previously saved. 
  load("Data/fashion_mnist.RData") #This is equivalent to reading from the packages
  return(fashion_mnist)
}

#Preparing the data into the final format with data points in columns and separated
#for training and testing. The preparation also involve padding data with zeros to 
#fit the complete Hilbert curve case
# dname -- choice for the original data list name
#gghilbet - a flag to indicate if the package '' should be used to generate the Hilber curve
#Returns a list of four elements 'data_prep$coltrain',data_prep$coltest,data_prep$Hilberttrain,data_prep$Hilberttest'
#The datapoints are columns, the last column is the values of the arguments

data_prep=function(dname='fashion_mnist',gghilbert=FALSE){
  
  # Since for the Hilbert curve it is more natural to consider 2^k x 2^k image matrices
  # and the data are only 28 x 28 image matrices, we add two columns on each side and two rows
  # on each side, the intensity for the pixel intensity is set to zero at the added rows and columns.
  
  cl=matrix(0,ncol=2,nrow=28)
  rw=matrix(0,ncol=32,nrow=2)
  
  train=rep(0,60000*32*32)
  dim(train)=c(60000,32,32)
  
  for(i in 1:60000){
    a=cbind(cl,fashion_mnist$train$x[i,,],cl)
    train[i,,]=rbind(rw,a,rw)
  }
  
  # The simplest transformation to vectors - vectorizing, i.e. column stacking
  coltrain=train
  dim(coltrain)=c(60000,32*32) #vectorization
  
  test=rep(0,10000*32*32)
  dim(test)=c(10000,32,32)
  
  for(i in 1:10000){
    a=cbind(cl,fashion_mnist$test$x[i,,],cl)
    test[i,,]=rbind(rw,a,rw)
  }
  
  coltest=test
  dim(coltest)=c(10000,32*32) #vectorization
  if(gghilbert==TRUE){
    # Hilbert curve method
    install.packages("gghilbertstrings")
    library(gghilbertstrings)
    
    df=data.frame(1:(32*32))
    colnames(df)=c("Index")
    ndf=create_coordinates(df,Index)
    HilbertCurve=ndf[c(3,4)]
  }else{
    #################
    # Explicit definition of the Hilbert curve of the fifth order
    HilbertCurve=matrix(0,ncol=2,nrow=(32*32))
    HilbertCurve[,1]=c(0,0,1,1,2,3,3,2,2,3,3,2,1,1,0,0,0,1,1,0,0,0,1,1,2,2,3,3,3,2,2,3,4,5,5,4,4,4,5,5,6,6,7,7,7,6,6,7,7,7,6,6,5,4,4,5,5,4,4,5,6,6,7,7,8,9,9,8,8,8,9,9,10,10,11,11,11,10,10,11,12,12,13,13,14,15,15,14,14,15,15,14,13,13,12,12,12,12,13,13,14,15,15,14,14,15,15,14,13,13,12,12,11,10,10,11,11,11,10,10,9,9,8,8,8,9,9,8,8,9,9,8,8,8,9,9,10,10,11,11,11,10,10,11,12,12,13,13,14,15,15,14,14,15,15,14,13,13,12,12,12,12,13,13,14,15,15,14,14,15,15,14,13,13,12,12,11,10,10,11,11,11,10,10,9,9,8,8,8,9,9,8,7,7,6,6,5,4,4,5,5,4,4,5,6,6,7,7,7,6,6,7,7,7,6,6,5,5,4,4,4,5,5,4,3,2,2,3,3,3,2,2,1,1,0,0,0,1,1,0,0,0,1,1,2,3,3,2,2,3,3,2,1,1,0,0,0,1,1,0,0,0,1,1,2,2,3,3,3,2,2,3,4,4,5,5,6,7,7,6,6,7,7,6,5,5,4,4,4,4,5,5,6,7,7,6,6,7,7,6,5,5,4,4,3,2,2,3,3,3,2,2,1,1,0,0,0,1,1,0,0,0,1,1,2,3,3,2,2,3,3,2,1,1,0,0,0,1,1,0,0,0,1,1,2,2,3,3,3,2,2,3,4,5,5,4,4,4,5,5,6,6,7,7,7,6,6,7,7,7,6,6,5,4,4,5,5,4,4,5,6,6,7,7,8,8,9,9,10,11,11,10,10,11,11,10,9,9,8,8,8,9,9,8,8,8,9,9,10,10,11,11,11,10,10,11,12,13,13,12,12,12,13,13,14,14,15,15,15,14,14,15,15,15,14,14,13,12,12,13,13,12,12,13,14,14,15,15,15,14,14,15,15,15,14,14,13,13,12,12,12,13,13,12,11,11,10,10,9,8,8,9,9,8,8,9,10,10,11,11,11,11,10,10,9,8,8,9,9,8,8,9,10,10,11,11,12,13,13,12,12,12,13,13,14,14,15,15,15,14,14,15,16,17,17,16,16,16,17,17,18,18,19,19,19,18,18,19,20,20,21,21,22,23,23,22,22,23,23,22,21,21,20,20,20,20,21,21,22,23,23,22,22,23,23,22,21,21,20,20,19,18,18,19,19,19,18,18,17,17,16,16,16,17,17,16,16,16,17,17,18,19,19,18,18,19,19,18,17,17,16,16,16,17,17,16,16,16,17,17,18,18,19,19,19,18,18,19,20,21,21,20,20,20,21,21,22,22,23,23,23,22,22,23,23,23,22,22,21,20,20,21,21,20,20,21,22,22,23,23,24,24,25,25,26,27,27,26,26,27,27,26,25,25,24,24,24,25,25,24,24,24,25,25,26,26,27,27,27,26,26,27,28,29,29,28,28,28,29,29,30,30,31,31,31,30,30,31,31,31,30,30,29,28,28,29,29,28,28,29,30,30,31,31,31,30,30,31,31,31,30,30,29,29,28,28,28,29,29,28,27,27,26,26,25,24,24,25,25,24,24,25,26,26,27,27,27,27,26,26,25,24,24,25,25,24,24,25,26,26,27,27,28,29,29,28,28,28,29,29,30,30,31,31,31,30,30,31,31,31,30,30,29,28,28,29,29,28,28,29,30,30,31,31,31,30,30,31,31,31,30,30,29,29,28,28,28,29,29,28,27,26,26,27,27,27,26,26,25,25,24,24,24,25,25,24,24,24,25,25,26,27,27,26,26,27,27,26,25,25,24,24,23,22,22,23,23,23,22,22,21,21,20,20,20,21,21,20,19,19,18,18,17,16,16,17,17,16,16,17,18,18,19,19,19,19,18,18,17,16,16,17,17,16,16,17,18,18,19,19,20,21,21,20,20,20,21,21,22,22,23,23,23,22,22,23,23,22,22,23,23,23,22,22,21,21,20,20,20,21,21,20,19,19,18,18,17,16,16,17,17,16,16,17,18,18,19,19,19,19,18,18,17,16,16,17,17,16,16,17,18,18,19,19,20,21,21,20,20,20,21,21,22,22,23,23,23,22,22,23,24,24,25,25,26,27,27,26,26,27,27,26,25,25,24,24,24,25,25,24,24,24,25,25,26,26,27,27,27,26,26,27,28,29,29,28,28,28,29,29,30,30,31,31,31,30,30,31,31,31,30,30,29,28,28,29,29,28,28,29,30,30,31,31)
                       HilbertCurve[,2]=c(0,1,1,0,0,0,1,1,2,2,3,3,3,2,2,3,4,4,5,5,6,7,7,6,6,7,7,6,5,5,4,4,4,4,5,5,6,7,7,6,6,7,7,6,5,5,4,4,3,2,2,3,3,3,2,2,1,1,0,0,0,1,1,0,0,0,1,1,2,3,3,2,2,3,3,2,1,1,0,0,0,1,1,0,0,0,1,1,2,2,3,3,3,2,2,3,4,5,5,4,4,4,5,5,6,6,7,7,7,6,6,7,7,7,6,6,5,4,4,5,5,4,4,5,6,6,7,7,8,8,9,9,10,11,11,10,10,11,11,10,9,9,8,8,8,9,9,8,8,8,9,9,10,10,11,11,11,10,10,11,12,13,13,12,12,12,13,13,14,14,15,15,15,14,14,15,15,15,14,14,13,12,12,13,13,12,12,13,14,14,15,15,15,14,14,15,15,15,14,14,13,13,12,12,12,13,13,12,11,11,10,10,9,8,8,9,9,8,8,9,10,10,11,11,11,11,10,10,9,8,8,9,9,8,8,9,10,10,11,11,12,13,13,12,12,12,13,13,14,14,15,15,15,14,14,15,16,16,17,17,18,19,19,18,18,19,19,18,17,17,16,16,16,17,17,16,16,16,17,17,18,18,19,19,19,18,18,19,20,21,21,20,20,20,21,21,22,22,23,23,23,22,22,23,23,23,22,22,21,20,20,21,21,20,20,21,22,22,23,23,24,25,25,24,24,24,25,25,26,26,27,27,27,26,26,27,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,27,26,26,27,27,27,26,26,25,25,24,24,24,25,25,24,24,25,25,24,24,24,25,25,26,26,27,27,27,26,26,27,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,27,26,26,27,27,27,26,26,25,25,24,24,24,25,25,24,23,23,22,22,21,20,20,21,21,20,20,21,22,22,23,23,23,22,22,23,23,23,22,22,21,21,20,20,20,21,21,20,19,18,18,19,19,19,18,18,17,17,16,16,16,17,17,16,16,16,17,17,18,19,19,18,18,19,19,18,17,17,16,16,16,16,17,17,18,19,19,18,18,19,19,18,17,17,16,16,16,17,17,16,16,16,17,17,18,18,19,19,19,18,18,19,20,21,21,20,20,20,21,21,22,22,23,23,23,22,22,23,23,23,22,22,21,20,20,21,21,20,20,21,22,22,23,23,24,25,25,24,24,24,25,25,26,26,27,27,27,26,26,27,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,27,26,26,27,27,27,26,26,25,25,24,24,24,25,25,24,24,25,25,24,24,24,25,25,26,26,27,27,27,26,26,27,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,28,28,29,29,30,31,31,30,30,31,31,30,29,29,28,28,27,26,26,27,27,27,26,26,25,25,24,24,24,25,25,24,23,23,22,22,21,20,20,21,21,20,20,21,22,22,23,23,23,22,22,23,23,23,22,22,21,21,20,20,20,21,21,20,19,18,18,19,19,19,18,18,17,17,16,16,16,17,17,16,16,16,17,17,18,19,19,18,18,19,19,18,17,17,16,16,15,14,14,15,15,15,14,14,13,13,12,12,12,13,13,12,11,11,10,10,9,8,8,9,9,8,8,9,10,10,11,11,11,11,10,10,9,8,8,9,9,8,8,9,10,10,11,11,12,13,13,12,12,12,13,13,14,14,15,15,15,14,14,15,15,15,14,14,13,12,12,13,13,12,12,13,14,14,15,15,15,14,14,15,15,15,14,14,13,13,12,12,12,13,13,12,11,10,10,11,11,11,10,10,9,9,8,8,8,9,9,8,8,8,9,9,10,11,11,10,10,11,11,10,9,9,8,8,7,7,6,6,5,4,4,5,5,4,4,5,6,6,7,7,7,6,6,7,7,7,6,6,5,5,4,4,4,5,5,4,3,2,2,3,3,3,2,2,1,1,0,0,0,1,1,0,0,0,1,1,2,3,3,2,2,3,3,2,1,1,0,0,0,1,1,0,0,0,1,1,2,2,3,3,3,2,2,3,4,4,5,5,6,7,7,6,6,7,7,6,5,5,4,4,4,4,5,5,6,7,7,6,6,7,7,6,5,5,4,4,3,2,2,3,3,3,2,2,1,1,0,0,0,1,1,0)
  }  
  
  HilbertCurve=HilbertCurve+1
  #plot(HilbertCurve,type='l')     
  #Defining 1D Hilbert curve data
  
  Hilberttrain=train
  dim(Hilberttrain)=c(60000,32*32)
  
  for(k in 1:60000)
  {
    for(j in 1:(32*32))
    {
      Hilberttrain[k,j]=train[k,HilbertCurve[j,1],HilbertCurve[j,2]]
    }
  }
  
  Hilberttest=test
  dim(Hilberttest)=c(10000,32*32)
  
  for(k in 1:10000)
  {
    for(j in 1:(32*32))
    {
      Hilberttest[k,j]=test[k,HilbertCurve[j,1],HilbertCurve[j,2]]
    }
  }
  # We accept the convention that individual data point stands as a column thus
  coltrain=t(coltrain)
  coltest=t(coltest)
  
  Hilberttrain=t(Hilberttrain)
  Hilberttest=t(Hilberttest)
  
  # We add the column of indexes  in the last (to preserve numbering of the images) column 
  # as the values of argument of functions representing data
  
  coltrain=cbind(coltrain,1:(32*32))
  coltest=cbind(coltest,1:(32*32))
  
  Hilberttrain=cbind(Hilberttrain,1:(32*32))
  Hilberttest=cbind(Hilberttest,1:(32*32))
  a=list(coltrain=coltrain,coltest=coltest,Hilberttrain=Hilberttrain,Hilberttest=Hilberttest)
  return(a)
}

#Generating fitting to the noise criterion for the stopping rule
#requires 'ddk' package (function 'add_knots()')
#M - number of MC repetitions of adding knots to the noise to smooth the curve
#KK- the length of the curve (there will be 'KK+NInit+2' knots and KK relative errors).
#     Since the algorithm starts with NInit+2 initial knots the relative errors 
#     as defined in the paper correspond to (NInit+1):(NInit+KK+1) knots
#     It should a multiplicity of 10
#Generates (KK+NInit+2) vector yielding the relative error of fitting 
#to the noise with the respective number of knots. For the initial values 
#errors are set to zero (since not computed). When knots are not added in 'ddk'
#then zero may appear at the end. 

StopCurve=function(M=1000,KK=150,NInit=10){
  AllKnts=2:1023 #the knots to be selected from
  RelNErr=matrix(0,nrow=M,ncol=KK+NInit+2) #The first 
  for(i in 1:M){
    InKn=sort(sample(AllKnts,NInit,rep=F)) #the random knots to start the algorithm
    InKn=c(1,InKn,1024) #adding the first and the last knot
    Noise=rnorm((32*32))
    KNoise=invisible(add_knots(Noise,knots=InKn,L=KK)) #DDK-package function it may compute smaller number of knots
    #while reports all errors (for non-existing knots it yields NaN)
    
    xx=-diff(KNoise$APPRERR[1:(length(KNoise$Fknots)-NInit-1)])
    RelNErr[i,(NInit+2):(length(KNoise$Fknots)-1)]=xx/KNoise$APPRERR[2:(length(KNoise$Fknots)-NInit-1)]
  }
  AvRelErr=colMeans(RelNErr,na.rm=TRUE) 
  AREFilt=AvRelErr #Since we smooth by 10 average filter the drop of AMSE's in the data
  #we also smooth the curve
  AREFilt[(NInit+2):(NInit+10)]=cumsum(AvRelErr[(NInit+2):(NInit+10)])/(1:9)
  
  xx=filter(AvRelErr,c(1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10),sides=1)
  #plot(xx[(NInit+11):length(xx)],type='l',lwd=2,col='red',xlab='Number of Selected Knots',ylab='Relative Error Drop')
  
  AREFilt[(NInit+11):length(xx)]=xx[(NInit+11):length(xx)]
  #plot((NInit+2):(NInit+KK+1),AREFilt[(NInit+2):(NInit+KK+1)],type='l',lwd=2,col='red',xlab='Number of Selected Knots',ylab='Relative Error Drop')
  
  return(AREFilt)
}


#The code to select knots for the means
#Mns -- matrix of mean functions columnwise
#AllKnts -- all possible knots (the arguments corresponding to the values in 'Mns')
#NKnt -- the maximal number of knots to be generated
#NInit -- the number of initial random knots (does not include the endpoints)
#AvRelErr -- Stopping curve
#L -- the number of added knots at each step
#The function return te list of three: 
# KNTS$knts - the list of the length K of vectors of knots for K classes
# KNTS$err - the list of the length K of vectors of errors corresponding to K classes 
#           (the first NInit+2 values are set to zero, as they are not available)
# KNTS$nknots - the K vector of the number of knots per class

MeansKnots=function(Mns,AllKnts,NKnt,NInit=10,AvRelErr,L=10)
{
  nkn=NInit+2 #Number of the knots including endpoints
  K=dim(Mns)[2]
  
  err=list() #We keep the errors in the list due to varying number of the lengths
  knts=list() #The list of knots
  nknots=rep(nkn,K) #The number of initial knots chosen at random plus the endpoints. 
  
  for(k in 1:K)
  { 
    InKn=sort(sample(AllKnts,NInit,rep=FALSE)) #the random knots to start the algorithm
    InKn=c(1,InKn,1024) #the number of initial knots is now NInit+2
    knts[[k]]=InKn
    err[[k]]=rep(0,nknots[[k]]) #The first errors are unknown, so we set them to zero
    
    Succ=F #Flag to mark the stopping rule criterion
    while(Succ==FALSE)
    {   
      KN=add_knots(MeanTr[,k],knots=knts[[k]],L=L) #It adds L knots to the initial 'knts[[k]]' for the total 'nknots[k]+L'
      #It computes L+1 errors (the last from the initial
      #step and all the new ones)
      #It supposed to add 10 knots to the initial 
      #Sommetimes 'add_knots' cannot add the proper number
      #of knots, then there is a warning and a correction for this
      #is needed since the size of the returned APPRERR is bigger than size of FKnots
      xxx=length(KN$Fknots)-nknots[k] #normally it should be L
      
      if(xxx==0){Succ=TRUE} #No new knots are added, the algorithm should stop
      
      nknots[k]=length(KN$Fknots) #the new number of knots
      knts[[k]]=KN$Fknots   
      newerrors=KN$APPRERR[1:(xxx+1)] #the first one is not truly new (the same as the last in 'err[[k]]')
      err[[k]]=c(err[[k]][1:(length(err[[k]])-1)],newerrors) #rewrites the last error from the previous step
      #but it should be the same in value
      if(Succ==FALSE){
        rerr=mean(-diff(newerrors)/newerrors[2:length(newerrors)]) #This is the mean error drop over this step 
        if(rerr<AvRelErr[nknots[k]]){
          Succ=TRUE
        }
      }
    }
  }
  KNTS=list(knts=knts,err=err,nknots=nknots)
  return(KNTS)
}


#The code to select knots for the centered data
#CenDat -- the list of matrices of centered functions columnwise
#AllKnts -- all possible knots (the arguments corresponding to the values in 'Mns')
#NKnt -- the maximal number of knots to be generated
#InitKnots -- the list of initial knots
#AvRelErr -- Stopping curve
#L -- the number of added knots at each step
#The function return te list of three: 
# KNTS$knts - the list of the length K of vectors of knots for K classes
# KNTS$err - the list of the length K of vectors of errors corresponding to K classes 
#           (the first NInit+2 values are set to zero, as they are not available)
# KNTS$nknots - the K vector of the number of knots per class

CentDatKnots=function(CenDat,AllKnts,InitKnots,NKnt=150,AvRelErr,L=10)
{
  #Initial knots 'borowed' from the means
  errcd=list() #We keep the errors in the list due to varying number of the lengths
  knotscd=InitKnots #The list of initial knots
  K=length(CenDat)
  nknotscd=vector('numeric',K) #The number of knots from the case of the means. 
  #Adding knots until the noise fitting criterium is met
  
  for(k in 1:K)
  { 
    nknotscd[k]=length(InitKnots[[k]])
    knotscd[[k]]=InitKnots[[k]] #The knots from the means
    errcd[[k]]=rep(0,nknotscd[k]) #The first errors are unknown since they are not 
    #computed as the corresponding knots are `borowed` from the means, so we set them to zero
    
    Succ=F #Flag to mark the stopping rule criterion
    while(Succ==F)
    {   
      SKN=add_knots(t(CenDat[[k]]),knots=knotscd[[k]],L=L) #It supposed to add L knots to the initial 
      #Sommetimes 'add_knots' cannot add the proper number
      #of knots, then there is a warning and a correction for this
      #is needed since the size APPRERR is bigger than size of FKnots
      xxx=length(SKN$Fknots)-nknotscd[k] #normally it should be L but sometimes is smaller
      
      if(xxx==0){Succ=TRUE} #No new knots are added, the algorithm should stop
      
      nknotscd[k]=length(SKN$Fknots) #the new number of knots
      knotscd[[k]]=SKN$Fknots  
      newerrors= SKN$APPRERR[1:(xxx+1)] #the first one is not truly new (the same as the last in 'err[[k]]')
      errcd[[k]]=c(errcd[[k]][1:(length(errcd[[k]])-1)],newerrors) #rewrites the last error from the previous step
      #but it should be the same in value
      if(Succ==FALSE){
        rerrcd=mean(-diff(newerrors)/newerrors[2:length(newerrors)]) #This is the mean error drop 
        #in this step
        if(rerrcd<AvRelErr[nknotscd[k]]){
          Succ=TRUE
        }
      }
    }
  }
  KNTS=list(knts=knotscd,err=errcd,nknots=nknotscd)
  return(KNTS)
}

#Classification procedure 
#DiscrData - d x n+1 matrix with the data columnwise, first column is the vector of arguments.
#Mean - list of the length K of the outcomes from projecting the refined mean spline object of 
#       K the mean splines with the knots developed for the centered data.
#Spect - a list of the length K of lists of two elements $values that is a vector of eignevalues
#        and $vector that is a K N_k x N_k matrix, k=1,...,K representing columnwise eigenvectors. 
#nn - k vector of values of the number of eigenfunctions to be used, if a coordinate is -1,
#     then only projection to the class space is used (without mean)
#SplData - list of K splinet objects containing the projections to the knots for a given class, 
#          if not available it is computed and returned as the third element of the output list
# The function returns a list of three elements: 
# Classify$wghts -K x n matrix of classification weights (positive and sum to one) rowwise 
# Classify$clss - n vector of integer, the indices of the largest weights
# Classify$splDt -  list of K splinet objects containing the projections to the knots for a given class

Classify=function(DiscrData,Mean,Spect,nn,SplData=list()){
  #Evaluation the squared L_2 distance from the projection, the function is considered to be constant on 
  #the intervals between the arguments. ||x-Px||^2=||x||^2-||Px||^2
  #
  K=length(Mean) #the number of classes
  
  #L_2 norm of the discrete data - ||x||^2
  dxn1=dim(DiscrData)
  n=dxn1[2]-1
  Dx=diff(DiscrData[,1])
  MidVal=(DiscrData[1:(dxn1[1]-1),2:dxn1[2]]+ DiscrData[2:(dxn1[1]),2:dxn1[2]])/2
  IntX2=t(MidVal)^2%*%Dx
  
  Weights=matrix(0,ncol=(dxn1[2]-1),nrow=K) #The list of the norms of the residuals
  
  for(k in 1:K)
  {
    #Distance of the discret data from the spline space
    if(length(SplData)==(k-1)){
      SplData[[k]]=project(DiscrData,Mean[[k]]$sp@knots)
    }
    
    
    NormPr=rowSums(SplData[[k]]$coeff^2) #By the isometry, these are the norms of projection splines
    Weights[k,]=IntX2-NormPr #Column of the distances of the data from the projections: ||x-Px||^2
    #At this stage only the knot locations enter the classification process
    
    if(nn[k]>-1){
      #Next the means are used to improve the classification rate
      CD=t(SplData[[k]]$coeff)-matrix(Mean[[k]]$coeff,byrow=FALSE,ncol=n,nrow=dim(SplData[[k]]$coeff)[2])
      #Centered Data
      
      DD=colSums(CD^2)
      Weights[k,]=Weights[k,]+DD 
      DD=0
      #At this stage only the knot locations and the class means enter the classification process
      
      
      #Here the eigenfunctions are utilized to improve on the classification
      if(nn[k]>0){
        EA=t(Spect[[k]]$vectors[,1:nn[k]])%*%CD
        DD=colSums(EA^2)
      }
      Weights[k,]=Weights[k,]-DD 
    }
  }
  
  #Normalizing weights
  sw=colSums(Weights)
  for(i in 1:n){
    Weights[,i]=Weights[,i]/sw[i]
  }
  #Finding the class
  Clss=apply(Weights,2,min)
  vv=1:K
  for(i in 1:n)
  {
    Clss[i]=vv[Weights[,i]==Clss[i]]
  }
  
  
  Clsf=list(wghts=Weights,clss=Clss,spldt=SplData)
  return(Clsf)
}

#For a given data that is also represented as splines, and given mean and eigen values
#the function returns two measures of the quality of approximation, the average accuracy and 
#the mean distance from the class
#xvl - the arguments (x-values)
#CV - the K-list of d x n matrices with the data columnwise from K classes.
#Mn - list of the length K of the outcomes from projecting the refined mean spline object of 
#       K the mean splines with the knots developed for the centered data.
#E - a list of K N_k x N_k matrices, k=1,...,K representing columnwise eigenvectors. 
#nn - k vector of values of the number of eigenfunctions to be used, if a coordinate is -1,
#     then only projection to the class space is used (without mean)
#SD - list of K splinet objects containing the projections to the knots for a given class, 
#          if not available it is computed and returned as the third element of the output list
# The function returns a list of three elements: 
# CrossClassify$acc -K vector of classification averaged across classes accuracies 
# CrossClassify$dist -K vector of averaged distances from the class 


CrossClassify=function(xvl=xval,CV=CrossValid,SD=SplData,Mn=Mean,E=Eg,nn){
  aaa=vector('numeric',K)
  vvv=aaa
  for(k in 1:K){
    DiscrData=cbind(xval,CrossValid[[k]])
    ClV=Classify(DiscrData,Mean,Eg,nn,SplData[[k]])
    vvv[k]=mean(ClV$clss==k)
    aaa[k]=mean(ClV$wghts[k,])
  }
  loss=list(acc=mean(vvv),dist=mean(aaa))
  return(loss)
}


#The function to illustrate the search for the optimal numbers of eigenvalues.
#It searches over the largest (coordinatewise) increase of the average accuracy 
#xvl - the arguments (x-values)
#CV - the K-list of d x n matrices with the data columnwise from K classes.
#Mn - list of the length K of the outcomes from projecting the refined mean spline object of 
#       K the mean splines with the knots developed for the centered data.
#E - a list of K N_k x N_k matrices, k=1,...,K representing columnwise eigenvectors. 
#nn - k vector of values of the number of eigenfunctions to be used, if a coordinate is -1,
#     then only projection to the class space is used (without mean)
#SD - list of K splinet objects containing the projections to the knots for a given class, 
#NumbFail - the stopping time equal to the number of steps when there is no-growth in
#           any direction, bigger the number more steps in the algorithm    
#It returns the accuracy evaluation over the search the trajectory of the search, 
#i.e. the used number of eigenfunctions per class. 
# Cross$Accu -- the list of evaluated averaged accuracies
# Cross$Traj -- the list of K vectors of the numbers of eigenvalues in each class 

Cross=function(xvl=xval,CV=CrossValid,SD=SplData,Mn=Mean,E=Eg,nn,NumbFail=5){
  Loss=CrossClassify(nn=nn) #Starting loss
  K=length(Mean)
  vek=1:K
  Traj=list() #The list of  K vectors of integers showing the number of eigenvalues throughout the
  #search
  LossTr=list() #The list of the accuracy values accross the search.
  I=1
  LossTr[[I]]=Loss$acc
  Traj[[I]]=nn
  
  RedLossAcc=vector('numeric',K)
  
  while(NumbFail>0){
    for(k in 1:K){#Checking the increase along different direction (partial derivative)
      nnn=nn
      nnn[k]=nn[k]+1
      NewLoss=CrossClassify(nn=nnn)
      RedLossAcc[k]=NewLoss$acc-Loss$acc
    }
    mx=max(RedLossAcc) #The best gain when increasing # of eigenfunctions by one
    #Check if there is positive gain when reducing the number of eigenfunctions
    if(mx<0){ #if negative gain 
      NumbFail=NumbFail-1
      #in this failing case the number of knots is only increasing to avoid loops
      k0=min(vek[RedLossAcc==mx]) #minimum to get a single value when there are ties
      nn[k0]=nn[k0]+1
    }else{ #the positive gain is possible
      k0=min(vek[RedLossAcc==mx])
      nn[k0]=nn[k0]+1
    }
    #the end of chosing a new `nn`
    Loss=CrossClassify(nn=nn) #running classification with the new decided vector of 
    #the numbers of eigenfunctions, it either increases or the number of knots increases
    #thus the loops in the search are avoided. 
    I=I+1
    Traj[[I]]=nn
    LossTr[[I]]=Loss$acc
  }
  return(list(Tr=Traj,Lss=LossTr))
}
