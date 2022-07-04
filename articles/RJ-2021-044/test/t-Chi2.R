####

library(fitdistrplus)
library(OneStep)

n<-10000

df<-8
D<-rchisq(n,df=df)

sigma.hat<-as.numeric(mledist(D,"chisq",start=list(df=df))$estimate)
sigma.tilde<-mean(D)

sigma.tilde+ 4/trigamma(sigma.tilde/2)*mean(1/2*log(D/2)-1/2*digamma(sigma.tilde/2))
onestep(D,"chisq")

p<-n^(2/7)
k<-floor(n^(3/5))

Dstar<-sort(D)

sigma.star<-1/2*sum((Dstar[(1+k):n]+Dstar[1:(n-k)])*(Dstar[(1+k):n]-Dstar[1:(n-k)])^(-p))/sum((Dstar[(1+k):n]-Dstar[1:(n-k)])^(-p))+2

sigma.star+ 4/trigamma(sigma.star/2)*mean(1/2*log(D/2)-1/2*digamma(sigma.star/2))
onestep(D,"chisq",init=list(df=sigma.star))

M<-10000
res1<-0
res2<-0
res3<-0
res4<-0

for (m in 1:M){
	
	##Simulation echantillon
	D<-rchisq(n,df=df)
	
	##Estimation
	
	#MLE
	sigma.hat<-as.numeric(mledist(D,"chisq",start=list(df=5))$estimate)
	
	#ME
	sigma.tilde<-mean(D)
	
	#Mode-type
	Dstar<-sort(D)
	sigma.star<-1/2*sum((Dstar[(1+k):n]+Dstar[1:(n-k)])*(Dstar[(1+k):n]-Dstar[1:(n-k)])^(-p))/sum((Dstar[(1+k):n]-Dstar[1:(n-k)])^(-p))+2
	
	#LCE
	sigma.cech<-as.numeric(onestep(D,"chisq",init=list(df=sigma.star))[[1]])

		
	#Erreur statistique renormalisée
	
	res1[m]<-sqrt(n)*(sigma.hat-df)
	res2[m]<-sqrt(n)*(sigma.tilde-df)
	res3[m]<-sqrt(n)*(sigma.star-df)
	res4[m]<-sqrt(n)*(sigma.cech-df)
	
}


layout(matrix(1:4,2,2))
x<-seq(-20,20,length=100)

hist(res1,freq=FALSE,ylim=c(0,0.3),xlim=c(-20,20),nclass=40)
y1<-dnorm(x,mean=0,sd=2/sqrt(trigamma(df/2)))
lines(x,y1,col="red")

hist(res2,freq=FALSE,ylim=c(0,0.3),xlim=c(-20,20),nclass=40)
y2<-dnorm(x,mean=0,sd=sqrt(2*df))
lines(x,y1,col="red")
lines(x,y2,col="blue")

hist(res3,freq=FALSE,ylim=c(0,0.3),xlim=c(-20,20),nclass=40)

f<-function(x){dchisq(x,df=df)}
fprimeprime<-function(x){dchisq(x,df=df)*((df/2-1)*(df/2-2)*x^(-2)-(df/2-1)*x^(-1)+1/4)}
Sigmalim<-1/4/(pi*f(df-2)*abs(fprimeprime(df-2)))^(1/2)*n^(6/14)

y3<-dnorm(x,mean=mean(res3),sd=sqrt(Sigmalim))
lines(x,y3,col="green")

hist(res4,freq=FALSE,ylim=c(0,0.3),xlim=c(-20,20),nclass=40)
lines(x,y1,col="red")


