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
xi=c(-0.1,0.0,0.03,0.05,0.09,0.36,0.38,0.49,0.56,0.62,0.63,0.77,0.79,0.85,0.86,0.865,0.91,0.945,0.95,1.1)

so = splinet(xi,smorder = k)

par(cex.axis=2)
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

n=20; k=3 #Number of knots and the order of splines
set.seed(10)
xi=sort(rbeta(n+2,2,5)); xi[1]=0; xi[n+1]=1 #Randomly distributed knots

S=matrix(rnorm((n+2)*(k+1)),ncol=(k+1)) #Random matrix of derivatives
spl = construct(xi,k,S) #Function for constructing spline that corrects the matrix of derivatives

#pdf('Fig2LeftTopSpl.pdf',width=7,height=3)
par(cex.axis=1.5)
plot(spl) 
#dev.off()

#########
#Bottom figure - a random sample of splines


y=rspline(spl,10) #Random spline generator
#pdf('Fig2LeftBottomSpl.pdf',width=7,height=3)
par(cex.axis=1.5)
plot(y)
#dev.off()

xi2=seq(0,1,by=1/(n+1))
spl2=construct(xi2,k,S) #Another spline object
#pdf('Fig2RightTopSpl.pdf',width=7,height=3)
plot(spl2)
#dev.off()

y2=rspline(spl2,10) #Another random sample


#pdf('Fig2RightBottomSpl.pdf',width=7,height=3)
plot(y2)
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

#pdf('Fig3LeftTopSplnt.pdf',width=7,height=4.5)
par(cex.axis=1.2)
plot(Bsplines) #B-splines
#dev.off()

#pdf('Fig3LeftBottomSplnt.pdf',width=7,height=5.5)
par(cex.axis=1.7)
plot(Bsplines,type='dyadic') #B-splines
#dev.off()

#######
#Right figure - splinet 
Splinet=so$os

#pdf('Fig3RightTopSplnt.pdf',width=7,height=3.5)
par(cex.axis=1.2)
plot(Splinet,type='simple') #OB-splines - splinet
#dev.off()


#pdf('Fig3RightBottomSplnt.pdf',width=7,height=5.5)
par(cex.axis=1.7)
plot(Splinet) #OB-splines - splinet
#dev.off()

