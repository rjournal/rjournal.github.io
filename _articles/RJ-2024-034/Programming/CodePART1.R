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

