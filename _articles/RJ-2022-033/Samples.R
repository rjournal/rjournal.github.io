#-----------
# Install
#-----------
#
install.packages("kStatistics")
library("kStatistics")

#-----------
# Example 2
#-----------
# Run mkmSet(c(2,1),TRUE) to get all the partitions of (2, 1):
# [( 0 1 )( 1 0 )( 1 0 ),  1 ]
# [( 0 1 )( 2 0 ),  1 ]
# [( 1 0 )( 1 1 ),  2 ]   
# [( 2 1 ),  1 ] 
#
mkmSet(c(2,1),TRUE)


#-----------
# Example 3
#-----------
# Run mkmSet(c(3),TRUE) to get all the partitions of 3
# [( 1 )( 1 )( 1 ), 1 ]
# [( 1 )( 2 ), 3 ]
# [( 3 ), 1 ]
# 
mkmSet(c(3),TRUE)


#-----------
# Example 4
#-----------
# Run intPart(4,TRUE) lists all the partitions of the integer 4
# [ 1 1 1 1 ]
# [ 1 1 2 ]
# [ 2 2 ]
# [ 1 3 ]
# [ 4 ]
# 
intPart(4,TRUE)


#-----------
# Example 5
#-----------
#
data<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 
        16.08, 19.43, 8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 
        15.02, 16.83, 16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)
#
# nKS(1, data)  returns 14.02, which is the sample mean.
nKS(1, data)

# nKS(2, data)  returns 12.65, the sample variance.
nKS(2, data) 

# nKS(3, data)/sqrt(nKS(2, data))^3 returns -0.030, the estimated skewness 
nKS(3, data)/sqrt(nKS(2, data))^3

# nKS(4, data)/nKS(2, data)^2 + 3 returns 2.11, the estimated sample kurtosis.
nKS(4, data)/nKS(2, data)^2 + 3


#-----------
# Example 6
#-----------
#
data1<-list(c(5.31,11.16),  c(3.26,3.26),  c(2.35,2.35), c(8.32,14.34), 
            c(13.48,49.45), c(6.25,15.05), c(7.01,7.01), c(8.52,8.52),
            c(0.45,0.45),   c(12.08,12.08),c(19.39,10.42))

# Run nKM(c(2,1),data1) to estimate the joint  cumulant  c_2,1
#
nKM(c(2,1),data1)


data2<-list(c(5.31,11.16,4.23),c(3.26,3.26,4.10),c(2.35,2.35,2.27),
            c(4.31,10.16,6.45),c(3.1,2.3,3.2),c(3.20, 2.31, 7.3))

# Run nKM(c(2,2,2),data2) to estimate the joint  cumulant  c_2,2,2
nKM(c(2,2,2),data2)


#-----------
# Example 7
#-----------
# Suppose we need to estimate the square of the variance of the population 
# character Y from which the data of Example 5 have been sampled. 
# Computing nKS(2,data)^2 or var(data)^2 returns 160.0225
#

nKS(2,data)^2 
var(data)^2

# An unbiased estimator of such a square is the polykay of order (2,2). 
# Running nPS(c(2,2),data) we get 154.1177.
#
nPS(c(2,2),data)


#-----------
# Example 8
#-----------
# Multivariate polykays are unbiased estimators of products of multivariate 
# cumulants and the nPM function returns a numerical value for these estimators
# For the same data1 employed in Example 6, run 
# nPM(list(c(2,1),c(1,0)),data1) to get 48.43243
#
nPM(list(c(2,1),c(1,0)),data1)


#-----------
# Example 9
#-----------
# Run GCBellPol(c(1,1),2) to get h(1,1)(y1, y2)
#
GCBellPol(c(1,1),2)


#-----------
# Example 10
#-----------
# Run e_GCBellPol(c(1,1),2,"g1[0,1]=1,g1[1,0]=2,g1[1,1]=3,
#     g2[0,1]=4, g2[1,0]=5, g2[1,1]=6") to plug the values 
#     from 1 to 6 respectively into the coefficients 
#     of h(1,1)(y1, y2) as given in Example 9
#
e_GCBellPol(c(1,1),2,"g1[0,1]=1,g1[1,0]=2,g1[1,1]=3,g2[0,1]=4, g2[1,0]=5, g2[1,1]=6")

# Run e_GCBellPol(c(1,1),2,"y1=1,y2=5,g1[0,1]=1,g1[1,0]=2,
#     g1[1,1]=3,g2[0,1]=4, g2[1,0]=5, g2[1,1]=6") 
#     to recover 600, which is the value of the previous 
#     polynomial in (1, 5), that is h(1,1)(1,5).
#
e_GCBellPol(c(1,1),2,"y1=1,y2=5,g1[0,1]=1,g1[1,0]=2,g1[1,1]=3,g2[0,1]=4, g2[1,0]=5, g2[1,1]=6")

#-----------
# Example 11
#-----------
# 
GCBellPol(c(1,1),2,TRUE)

#-----------
# Example 12
#-----------
# rum GCBellPol(c(1,1),1) to get h(1,1)(y)
#
GCBellPol(c(1,1),1) 


#-----------
# Example 13
#-----------
# Run mom2cum(c(3,1)) to get m(3,1) in terms of k(i,j) 
mom2cum(c(3,1))

# Run cum2mom(c(3,1)) to get k(3,1) in terms of m(i,j) 
cum2mom(c(3,1))



#-----------
# Example 14
#-----------
# run GCBellPol(c(3),1) to het h3(y)
#
GCBellPol(c(3),1)


#-----------
# Example 15
#-----------
# Run gpPart(4) to get G_4(a1,a2,a3,a4; y1,y2,y3,y4)
# 
gpPart(4)

#-----------
# Example 16
#-----------
# Run eBellPol(5,3) to get B_5,3(y1,y2,y3)
#
eBellPol(5,3)

# Run eBellPol(4) to get G4(1,1,1,1; y1,y2,y3,y4):
# 
eBellPol(4)

#-----------
# Example 17
#-----------
# Run oBellPol(5,3) to get ^B_5,3(y1,y2,y3)
# 
oBellPol(5,3)

# Run oBellPol(4) to get ^G_3(y1,y2,y3,y4)
# 
oBellPol(4)

#-----------
# Example 18
#-----------
# Run e_eBellPol(5,3) or e_eBellPol(5,3,c(1,1,1,1,1)) 
#     to get S(5, 3)=25. 

e_eBellPol(5,3)
e_eBellPol(5,3,c(1,1,1,1,1))

# Run e_eBellPol(5) to  get the 5-th Bell number B5=52. 

e_eBellPol(5)

# Run e_eBellPol(5,3, c(1,-1,2,-6,24)) to get s(5,3)=35.

e_eBellPol(5,3, c(1,-1,2,-6,24))

#-----------
# Example 19
#-----------
# Run mkT(c(2,1),2,TRUE)) to get the elements of the set ~p(2,(2, 1)):
#[( 0 1 )( 2 0 )]
#[( 2 0 )( 0 1 )]
#[( 1 0 )( 1 1 )]
#[( 1 1 )( 1 0 )]
#[( 2 1 )( 0 0 )]
#[( 0 0 )( 2 1 )]
#
mkT(c(2,1),2,TRUE)

#-----------
# Example 20
#-----------
# Run MFB(c(1,1),2) to get h(1,1)
#
MFB(c(1,1),2)

#-----------
# Example 21
#-----------
# To evaluate the output of Example 20 for some numerical 
# values of the coefficients
#
cfVal<-"f[0,1]=2, f[0,2]=5, f[1,0]=13, f[1,1]=-4, f[2,0]=0"
cgVal<-"g1[0,1]=-2.1, g1[1,0]=2,g1[1,1]=3.1,g2[0,1]=5,g2[1,0]=0,g2[1,1]=6.1"
cVal<-paste0(cfVal,",",cgVal)
e_MFB(c(1,1),2,cVal)

#-----------
# Example 22
#-----------
# To get h5 in (41) run MFB(c(5), 1)
MFB(c(5), 1)


#-----------
# Example 23
#-----------
# Run pPart(5) to get F5(y):
pPart(5)

#-----------
# Example 24
#-----------
# The following example shows how to evaluate F11(y) in y = 7.
#
s<-pPart(11)         # generate  the  partition polynomial  of  degree 11 
s<-paste0("1",s)     # add the  coefficient to  the  first term
s<-gsub(" y","1y",s) # replace  the  variable y without coefficient 
s<-gsub("y", "*7",s) # assign  y = 7
eval(parse(text=s))  # evaluation of  the  expression

#-----------
# Example 25  
#-----------
# See e2p.R file
#

