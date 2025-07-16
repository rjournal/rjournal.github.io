#####################################
#The code accompanying the R-journal paper on Splinets package
# PART II - The code illustrating the classification workflow
#####################################

#####################################
#STEP 1 - DATA PREPROCESSING
##############
#The Fashion MNIST are uploaded using the `keras` package
#that in turn needs the `tensorflow` package

##########
#The following installations are needed merely to get access to the data

source("WorkFlowAuxFunction.R") #This source all important R-functions helping in the workflow, 
                                #for example, 'data_upload()' and 'data_prep()' are defined in this file
fashion_mnist=data_upload(pathfile="Data/fashion_mnist.RData") #check parameters of this function 
                                                               #in 'WorkFlowAuxFunction.R' if the data
                                                               #are not previously uploaded. 

OneD=data_prep(dname='fashion_mnist')


#Spliting the training data into classes
K=10
ArgH=OneD$Hilberttrain[,60001] #common vector of arguments 
ValH=OneD$Hilberttrain[,1:60000] #values

Htrain=list() #the classes are made the elements of a list
for(k in 1:K){
  Htrain[[k]]=ValH[,fashion_mnist$train$y==(k-1)] 
}

#This conclude Step 1 in the workflow 
#####################################
####################
#FIGURE 5 -illustrating the two formats of the data against the original images
########

#Left graphs - original images
#pdf('Fig5LeftBottom.pdf',width=7,height=7)
image(t(fashion_mnist$test$x[1,28:1,]),xaxt="n",yaxt="n")
#dev.off()

#pdf('Fig5LeftTop.pdf',width=7,height=7)
image(t(fashion_mnist$test$x[20,28:1,]),xaxt="n",yaxt="n")
#dev.off()

###

#Middle graphs - images transformed by vectorization
#pdf('Fig5MiddleBottom.pdf',width=7,height=7)
plot(OneD$coltrain[,60001],OneD$coltrain[,1],type='l',xlab='',ylab='')
#dev.off()

#pdf('Fig5MiddleTop.pdf',width=7,height=7)
plot(OneD$coltrain[,60001],OneD$coltrain[,20],type='l',xlab='',ylab='')
#dev.off()

###

#Right graphs - images transformed by the Hilbert curve
#pdf('Fig5RightBottom.pdf',width=7,height=7)
plot(OneD$Hilberttrain[,60001],OneD$Hilberttrain[,1],type='l',xlab='',ylab='')
#dev.off()

#pdf('Fig5RightTop.pdf',width=7,height=7)
plot(OneD$Hilberttrain[,60001],OneD$Hilberttrain[,20],type='l',xlab='',ylab='')
#dev.off()

#####################################
#STEP 2 - OPTIMAL KNOT SELECTION
##############

#Estimating mean values of discrete data.

MeanTr=matrix(0,nrow=(32*32),ncol=10)
for(k in 1:K)
{
  MeanTr[1:(32*32),k]=rowMeans(Htrain[[k]])
}

# The main work in this step is done by the 'DDK' package. 
# The critical function from this package is 'add_knots()'.

#install.packages("devtools")
#library(devtools)
#devtools::install_github("ranibasna/ddk")
library(DDK)

#Generating fitting to the noise criterion for the stopping rule
set.seed(1)
AvRelErr=StopCurve() #the function is described in the sourced file, it takes some time.
#save(AvRelErr,file = 'StCv.rda') #If planned to be used in the future it is good to save the results not run it again.
#load('StCv.rda')
#The end of computing the curve for the stopping rule 
########

#Running the code to select knots for the mean
NInit=10 #the number of the initial to be randomly chosen knots
NKnt=150+NInit+2
set.seed(1)
MnKnts=MeansKnots(MeanTr,ArgH,NKnt,NInit,AvRelErr,L=2)

#The end of selection of knots for the means

#Running the code to select knots for the centered data
NK=1000
CDTrain=list()
for(k in 1:K){
  CDTrain[[k]]=Htrain[[k]][,1:NK]
  CDTrain[[k]]=CDTrain[[k]]-MeanTr[,k] #Centering the data
}

#The main part, selection of the knots for the centered data
CDKnts=CentDatKnots(CDTrain,AllKnts,MnKnts$knts,NKnt=150,AvRelErr)

 
#The end of selection of knots for the means and the centered data

#####################################
#STEP 3 - DATA PROJECTION TO SPLINE SPACES
##############

#Projecting discrete training data to functional spaces of splines
#Requires 'Splinets'-package
install.packages("Splinets")
library(Splinets)
#Projecting means to the spline space

MeanSplines=list()
for(k in 1:K){
  MeanSplines[[k]]=project(cbind(ArgH,MeanTr[,k]),MnKnts$knts[[k]])
}

#Projecting data -- this is time consuming 
HSplns=list()
for(k in 1:K){
  HSplns[[k]]=project(cbind(ArgH,Htrain[[k]]),CDKnts$knts[[k]])
}
#the end of STEP 3
##########################################

####################
#FIGURE 6 - illustrates the knot selection process 
########

#Top graph - the mean `T-Shirt'
k=1
prmean=MeanSplines[[k]]

#pdf('Fig6Top.pdf',width=9,height=3)
plot(prmean$sp,lty=2,lwd=3)
lines(ArgH,MeanTr[,k],type='l',col='red') #T-shirt mean
lines(prmean$sp,lty=2,lwd=3)
#dev.off()

##

#Middle graph - the centered `T-Shirt'

prTSh=project(cbind(ArgH,CDTrain[[k]][,1]),CDKnts$knts[[k]])

#pdf('Fig6Middle.pdf',width=9,height=3)
plot(prTSh$sp,lty=2)
lines(ArgH,CDTrain[[k]][,1],type='l',col='red') #T-shirt centered
lines(prTSh$sp,lty=2,lwd=2)
#dev.off()

##

#Left-Bottom graph - the stopping rule curve and the stopping time for the means
NInit=10; KK=150
#pdf('Fig6BottomLeft.pdf',width=5,height=3)
plot((NInit+2):(NInit+KK+1),AvRelErr[(NInit+2):(NInit+KK+1)],type='l',lwd=2,col='red',xlab='Number of Selected Knots',ylab='Relative Error Drop')
for(k in 1:K){
  nonzerr=MnKnts$err[[k]][(NInit+2):MnKnts$nknots[k]]
  RelErr=-diff(nonzerr)/nonzerr[2:length(nonzerr)]
  RelErr[1:10]=cumsum(RelErr[1:10])/(1:10)
  avpar=3
  xx=filter(RelErr,rep(1/avpar,avpar),sides=1)
  RelErr[11:length(xx)]=xx[11:length(xx)]
  lines((NInit+2):(NInit+length(RelErr)+1),RelErr,type='l',col=k)
}
#dev.off()

##

#Bottom-Right graph - the stopping rule curve and the stopping time for the centered data

#pdf('Fig6BottomRight.pdf',width=5,height=3)
plot((NInit+2):(NInit+KK+1),AvRelErr[(NInit+2):(NInit+KK+1)],type='l',lwd=2,col='red',xlab='Number of Selected Knots',ylab='Relative Error Drop')

for(k in 1:K){
  nonzerr=CDKnts$err[[k]][MnKnts$nknots[k]:length(CDKnts$err[[k]])]
  nonzerr[nonzerr==0]=rep(min(nonzerr[nonzerr!=0]),length(nonzerr[nonzerr==0]))
  RelErr=-diff(nonzerr)/nonzerr[2:length(nonzerr)]
  RelErr[1:10]=cumsum(RelErr[1:10])/(1:10)
  #xx=filter(RelErr,c(1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10,1/10),sides=1)
  avpar=3
  xx=filter(RelErr,rep(1/avpar,avpar),sides=1)
  RelErr[11:length(xx)]=xx[11:length(xx)]
  lines(MnKnts$nknots[k]:(CDKnts$nknots[k]-1),RelErr,type='l',col=k)
}
#dev.off()

#######

############################################
#STEP 4 - PRINCIPAL COMPONENT ANALYSIS
##############

#Computing the covariance matrix of the isometric vectors from the `splinets' object

Sig=list()
Spct=list()
for(k in 1:K){
  Sig[[k]]=cov(HSplns[[k]]$coeff) #The estimates of covariance 
                                  #(variables as columns observations as rows)
  Spct[[k]]=eigen(Sig[[k]]) #columns of Spct[[k]]$vectors are eigenvectors as columns
}
#the end of STEP 4
##########################################

#####################
#FIGURE 7 - illustration of the role of eigenfunctions
########
k=1 #T-Shirt

#Eigenfunctions using 'Splinets'-package
EigHSplns=lincomb(HSplns[[k]]$basis,t(Spct[[k]]$vectors)) 

#####
#Top-Left plot --
#The first three eigenfunctions for 'T-Shirts' scaled by squareroots of the eigenvalues:

#pdf('Fig7TopLeft.pdf',width=7,height=4)
ScEHSp=lincomb(subsample(EigHSplns,1:3),diag(c(sqrt(Spct[[k]]$values[1:3]))))
plot(ScEHSp)
#dev.off()
#####

###
#Top-Left Graph: projections of the centered data
k=1
HS1=subsample(HSplns[[k]]$sp,1) #Chosing the functional data point

#Embeding mean to the spline space with the knots for eigenfunctions
RFMeanSplines=refine(MeanSplines[[k]]$sp,newknots=CDKnts$knts[[k]])

#Centering the data point
CHS1=lincomb(gather(HS1,RFMeanSplines),t(c(1,-1)))

neg=20 #The number 'neg' of the eigenfunctions
EHSNeg=subsample(EigHSplns,1:neg) #Extracting `neg` eigenvalues

A1=gather(RFMeanSplines,EHSNeg) #Combining the mean with the eigenfunctions
A2=gather(A1,CHS1) #Adding the centered funtional data point
IP=gramian(A2)  

#Aproximations of the centered data:
CtAppNg=lincomb(EHSNeg,t(IP[2:(neg+1),2+neg]))  #'neg' eigenvalues

CtAppThree=lincomb(subsample(EHSNeg,1:3),t(IP[2:4,2+neg])) #three eigenvalues

#pdf('Fig7TopRight.pdf',width=7,height=4)
plot(CHS1,sID=1,lty=2)
lines(CtAppThree,lty='dotdash',col="blue")
lines(CtAppNg,lwd=1,col='darkorange3')
#dev.off() 
##

###
#Bottom-Left - projections of the functional 'T-Shirt' data point centered around the class mean

A=t(c(1,IP[2:(1+neg),2+neg])) #Coefficients of the projections to the eigenfunctions

ApprxNeg=lincomb(A1,A) #The approximation with the first `neg` eigenfunctions
A1=subsample(A1,1:4) #Taking 3 eigenfunctions
A=t(c(1,IP[2:4,2+neg]))
ApprxThree=lincomb(A1,A)

#pdf('Fig7BottomLeft.pdf',width=7,height=4)
plot(HSplns[[k]]$sp,sID=1,lty=2)
lines(MeanSplines[[k]]$sp,type='l',lwd=3,col='red') #Mean value spline
lines(ApprxThree,lty='dotdash',col="blue")
lines(ApprxNeg,lwd=1,col='darkorange3')
#dev.off()
##

###
#Bottom-Right - projections to the 'T-Shirt' of the functional 'Boot' data point centered around the 'T-Shirt' class mean 
m=10 #The boot clase

Boot2TSh=project(cbind(ArgH,Htrain[[m]][,1]),CDKnts$knts[[k]]) #projection of boots to 'T-Shirt'

#Centering the 'Boot' data point with the 'T-Shirt' mean
CDBoot2TSh=lincomb(gather(Boot2TSh$sp,RFMeanSplines),t(c(1,-1)))

A1=gather(RFMeanSplines,EHSNeg) #Combining the mean with the eigenfunctions
A2=gather(A1,CDBoot2TSh) #Adding the centered funtional data point
IP=gramian(A2)  
A=t(c(1,IP[2:(1+neg),2+neg])) #Coefficients of the projections to the eigenfunctions

ApprxNegB2T=lincomb(A1,A) #The approximation with the first `neg` eigenfunctions
#pdf('Fig7BottomRight.pdf',width=7,height=4)
plot(ApprxNegB2T,lwd=1,col='darkorange3')
lines(cbind(ArgH,Htrain[[m]][,1]),type='l')
#dev.off()
##
##############

###########################################
#STEP 5 - DETERMINING SIGNIFICANT NUMBER OF THE EIGENFUNCTIONS
##############

#Computing the covariance matrix of the isometric vectors from the `splinets' object
#Preparing the arguments for Classify function

#Embeding mean to the spline space with the knots for eigenfunctions
Mean=list()
for(k in 1:K){
  Mean[[k]]=refine(MeanSplines[[k]]$sp,newknots=CDKnts$knts[[k]]) 
  Mean[[k]]=project(Mean[[k]],knots=CDKnts$knts[[k]]) #To get also the coefficients in the splinet
}

#computing a list of eigenvectors based on 90% training data to make cross-validation 10% sample
#unbiased on the computation of covariance
CrossValid=list()
Eg=list()
set.seed(1)
for(k in 1:K){
  xy=dim(Htrain[[k]])
  ns=sample(1:xy[2],floor(xy[2]/10),rep=FALSE) #choosing a random sample for cross-validation
  NN=1:xy[2]
  
  CrossTrainCff=HSplns[[k]]$coeff #Columnwise coefficients in the basis
  #removing 10% of the splines:
  CrossTrainCff=CrossTrainCff[-ns,]
  CrossValid[[k]]=Htrain[[k]][,ns] #Discrete 10% data on which crossvalidation is made
  
  Sigma=cov(CrossTrainCff)
  Eg[[k]]=eigen(Sigma) #columns of Eg[[k]]$vectors are eigenvectors
}

#The initial run of classification, it also create the Spline representation of the data
#that later significantly save time, when Classify is run again (it needs then to be given as the 
#last parameter to 'Classify()').

nn=rep(-1,K) #only knot selection utilized for classification
ClValue=list()

for(k in 1:K){
  DiscrData=cbind(ArgH,CrossValid[[k]])
  ClValue[[k]]=Classify(DiscrData,Mean,Eg,nn)
}

#the following yields the mean values of scaled distances for classes k=1,...,K
#This is what classification one can get when only using the knot selection.
for(k in 1:K){
  print(mean(ClValue[[k]]$wghts[k,]))
}

SplData=list()
for(k in 1:K)
{
  SplData[[k]]=ClValue[[k]]$spldt
}  

#The beginning of searching for the optimal number of the eigenvalues
nn=rep(0,K) #The means and the knots are used for classification
ClValue=list()

for(k in 1:K){
  DiscrData=cbind(ArgH,CrossValid[[k]])
  ClValue[[k]]=Classify(DiscrData,Mean,Eg,nn,SplData[[k]])
}


#the following yields the mean values of scaled distances for classes k=1,...,K
#This is what classification one can get when only using the mean and the knot selection.
zz=1:K
for(k in 1:K){
  #print(mean(ClValue[[k]]$wghts[k,]))
  zz[k]=mean(ClValue[[k]]$cls==k)
  print(mean(ClValue[[k]]$cls==k))
}



####################
#FIGURE 8
########

#Visualization of the optimization algorithm for the number of eigenfunctions



#######
#Left - objective function along the optimization trajectory

#pdf('Fig8Left.pdf',width=7,height=4)
xval=ArgH #used due to the name of the default in 'Cross()'

nn=rep(0,K)

CV1=Cross(nn=nn,NumbFail = 60)

LT=length(CV1$Tr)
aaa=vector('numeric',LT)
for(i in 1:LT)aaa[i]=CV1$Lss[[i]]
for(i in 1:LT)bbb[i]=sum(CV1$Tr[[i]])

plot(bbb,aaa,type='l',xlab='The number of eigenfunctions',ylab='Accuracy',col='red',lwd=2,ylim=c(0.65,0.8))


MC=10 #Monte Carlo sample for the ini
MxAccu=vector('numeric',MC+1)
MxAccuArg=matrix(0,ncol=MC+1,nrow=K)
ind=1:LT
nn0=min(ind[max(aaa)==aaa])
MxAccu[1]=CV1$Lss[[nn0]]
MxAccuArg[,1]=CV1$Tr[[nn0]]

set.seed(1)
for(j in 1:MC){
  newn=nn
  for(k in 1:K){
    newn[k]=ceiling(runif(1)*MxAccuArg[k,j])
  }
  CV1=Cross(nn=newn,NumbFail = 40)
  LT=length(CV1$Tr)
  aaa=vector('numeric',LT)
  bbb=aaa
  for(i in 1:LT)aaa[i]=CV1$Lss[[i]]
  for(i in 1:LT)bbb[i]=sum(CV1$Tr[[i]])
  lines(bbb,aaa,type='l',col=j)
  ind=1:LT
  nn0=min(ind[max(aaa)==aaa])
  MxAccu[j+1]=CV1$Lss[[nn0]]
  MxAccuArg[,j+1]=CV1$Tr[[nn0]]
}
#dev.off()

#The optimal number of the eigenfunction and its classwise decomposition
jjj=1:(MC+1)
j0=min(jjj[MxAccu==max(MxAccu)])
MxAccu[j0]
nn0=MxAccuArg[,j0]
sum(nn0)

#obtaining classwise trajectories
ClassTraj=matrix(0,nrow=K,ncol=LT)

for(k in 1:K){
  DiscrData=cbind(ArgH,CrossValid[[k]])
  for(i in 1:LT){
    AB=Classify(DiscrData,Mean,Eg,CV1$Tr[[i]],SplData[[k]])
    ClassTraj[k,i]=mean(AB$clss==k)
  }
}

xxx=sum(CV1$Tr[[1]]):sum(CV1$Tr[[LT]]) #the trajectory range by the total number of eigenfunctions
#######
#Right- the objective function against classwise accuracy

#pdf('Fig8Right.pdf',width=7,height=4)
plot(xxx,aaa,ylim=c(0,1),type='l',axes='False',lwd=3, col='red',ylab='Accuracy',xlab='The total number of eigenfunctions')
axis(1)
axis(2)
for(k in 1:K){
  lines(xxx,ClassTraj[k,],type='l',col=k)
}
lines(xxx,aaa,lwd=3, col='red')
#dev.off()






#############################################
#STEP 6 - TESTING THE DEVELOPED PROCEDURE
##


#Preparation of the testing data sets
#Spliting the testing data into classes 
ArgH=OneD$Hilberttest[,10001] #common vector of arguments 
TestH=OneD$Hilberttest[,1:10000] 

Htest=list() #the classes become elements of a list
for(k in 1:K){
  Htest[[k]]=TestH[,fashion_mnist$test$y==(k-1)] 
}
#end of the testing data preparation

#Running the test of the classification procedure on testing data, takes some time.
TesT=list()
for(k in 1:K){
  DiscrData=cbind(ArgH,Htest[[k]])
  TesT[[k]]=Classify(DiscrData,Mean,Spct,nn0) #nn0-contains the number of eigenvectors for each class
                                              #Spct - the spectrum based on all training data
}

Acc=vector('numeric',K)
for(k in 1:K){
  Acc[k]=mean(TesT[[k]]$clss==k) #Computing accuracies per class
}

Acc*
mean(Acc)


Dist=vector('numeric',K)
for(k in 1:K){
  Dist[k]=mean(TesT[[k]]$wghts[k,]) #Computing distances per class
}

round(Dist,3)


#############################################
#STEP 7 - FINAL EVALUATION AND CONCLUSIONS
##

#Creating values to the confusion matrix

Confusion=matrix(0,ncol=K,nrow=K)

for(l in 1:K){
  for(k in 1:K){
    Confusion[l,k]=mean(TesT[[k]]$clss==l)
  }
}

