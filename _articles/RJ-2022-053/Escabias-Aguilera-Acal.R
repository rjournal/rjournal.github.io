#Reading the fda.usc package for using data of the package

library(fda.usc)
data(aemet)
Temp<-aemet$temp$data
Prec<-exp(aemet$logprec$data)
Wind<-aemet$wind.speed$data
StationsVars<-aemet$df[,c("ind","altitude","longitude","latitude")]
StationsVars$North<-c(1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,1,1)

#Mean monthly data calculation from daily data

TempMonth<-matrix(0,73,12)
for (i in 1:nrow(TempMonth)){
  TempMonth[i,1]<-mean(Temp[i,1:31])
  TempMonth[i,2]<-mean(Temp[i,32:59])
  TempMonth[i,3]<-mean(Temp[i,60:90])
  TempMonth[i,4]<-mean(Temp[i,91:120])
  TempMonth[i,5]<-mean(Temp[i,121:151])
  TempMonth[i,6]<-mean(Temp[i,152:181])
  TempMonth[i,7]<-mean(Temp[i,182:212])
  TempMonth[i,8]<-mean(Temp[i,213:243])
  TempMonth[i,9]<-mean(Temp[i,244:273])
  TempMonth[i,10]<-mean(Temp[i,274:304])
  TempMonth[i,11]<-mean(Temp[i,305:334])
  TempMonth[i,12]<-mean(Temp[i,335:365])
  }

PrecMonth<-matrix(0,73,12)
for (i in 1:nrow(PrecMonth)){
  PrecMonth[i,1]<-mean(Prec[i,1:31])
  PrecMonth[i,2]<-mean(Prec[i,32:59])
  PrecMonth[i,3]<-mean(Prec[i,60:90])
  PrecMonth[i,4]<-mean(Prec[i,91:120])
  PrecMonth[i,5]<-mean(Prec[i,121:151])
  PrecMonth[i,6]<-mean(Prec[i,152:181])
  PrecMonth[i,7]<-mean(Prec[i,182:212])
  PrecMonth[i,8]<-mean(Prec[i,213:243])
  PrecMonth[i,9]<-mean(Prec[i,244:273])
  PrecMonth[i,10]<-mean(Prec[i,274:304])
  PrecMonth[i,11]<-mean(Prec[i,305:334])
  PrecMonth[i,12]<-mean(Prec[i,335:365])
  }

WindMonth<-matrix(0,73,12)
for (i in 1:nrow(WindMonth)){
  WindMonth[i,1]<-mean(Wind[i,1:31])
  WindMonth[i,2]<-mean(Wind[i,32:59])
  WindMonth[i,3]<-mean(Wind[i,60:90])
  WindMonth[i,4]<-mean(Wind[i,91:120])
  WindMonth[i,5]<-mean(Wind[i,121:151])
  WindMonth[i,6]<-mean(Wind[i,152:181])
  WindMonth[i,7]<-mean(Wind[i,182:212])
  WindMonth[i,8]<-mean(Wind[i,213:243])
  WindMonth[i,9]<-mean(Wind[i,244:273])
  WindMonth[i,10]<-mean(Wind[i,274:304])
  WindMonth[i,11]<-mean(Wind[i,305:334])
  WindMonth[i,12]<-mean(Wind[i,335:365])
  }

#Reading the fda package

library(fda)

#Defining basis

FourierBasis<-create.fourier.basis(rangeval = c(1,12),nbasis=7)
BsplineBasis<-create.bspline.basis(rangeval = c(1,12),nbasis=8)

#Functional data objects from discrete observations

TempMonth.fd<-Data2fd(argvals = c(1:12), y=t(TempMonth),basisobj = FourierBasis)
PrecMonth.fd<-Data2fd(argvals = c(1:12), y=t(PrecMonth),basisobj = BsplineBasis)
WindMonth.fd<-Data2fd(argvals = c(1:12), y=t(WindMonth),basisobj = BsplineBasis)

#Reading the logitFD package

library(logitFD)

#Fitting the extended functional principal component logit regression model for modeling weather stations location from curves of Temperature and Precipitation and "altitude" and "longitude" as nonfunctional variables. Three and four functional principal components are considered for temperature and precipitation functional data respectively.

Fit1<-logitFD.pc(Response=StationsVars$North,FDobj=list(TempMonth.fd,PrecMonth.fd),
ncomp = c(3,4),nonFDvars = StationsVars[,c("altitude","longitude")])

#Showing the different objects that provide the function

#Summary of the multivariate fit
summary(Fit1$glm.fit)

#Classification table
table(StationsVars$North,round(predict(Fit1$glm.fit,type="response")))

#Correct classification rate
100*sum(diag(table(StationsVars$North,round(predict(Fit1$glm.fit,type="response")))))/nrow(StationsVars)

#Intercept of the fit
Fit1$Intercept

#plot of the functional parameters of the model

#Functional parameter associated to temperature curves
plot(Fit1$betalist[[1]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_1(t)")
#Functional parameter associated to precipitation curves
plot(Fit1$betalist[[2]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_2(t)")

#Evaluation of the functional parameters in a time-points grid

data.frame("Months"=names(monthLetters),"Beta1"=eval.fd(c(1:12),Fit1$betalist[[1]]),"Beta2"=eval.fd(c(1:12),Fit1$betalist[[2]]))

#summary of the variability explanation of the functional principal components
Fit1$PC.variance

#ROC object
Fit1$ROC

plot(Fit1$ROC)

#Fitting the extended filtered functional principal component logit regression model for modeling weather stations location from curves of Temperature and Precipitation and "altitude" and "longitude" as nonfunctional variables. Three and four functional principal components are considered for temperature and precipitation functional data respectively.


Fit2<-logitFD.fpc(Response=StationsVars$North,FDobj=list(TempMonth.fd,PrecMonth.fd),ncomp = c(3,4),nonFDvars = StationsVars[,c("altitude","longitude")])

#No commets are added to every resulting object

summary(Fit2$glm.fit)
table(StationsVars$North,round(predict(Fit2$glm.fit,type="response")))
100*sum(diag(table(StationsVars$North,round(predict(Fit2$glm.fit,type="response")))))/nrow(StationsVars)
Fit2$Intercept
plot(Fit2$betalist[[1]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_1(t)")
plot(Fit2$betalist[[2]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_2(t)")
cbind(eval.fd(c(1:12),Fit2$betalist[[1]]),eval.fd(c(1:12),Fit2$betalist[[2]]))
Fit2$PC.variance
Fit2$ROC
plot(Fit2$ROC)

#Fitting the extended functional principal component logit regression model for modeling weather stations location from curves of Temperature and Precipitation and "altitude" and "longitude" as nonfunctional variables. Stepwise selection method is considered for functional principal components and nonfunctional variables selection.


Fit3<-logitFD.pc.step(Response=StationsVars$North,FDobj=list(TempMonth.fd,PrecMonth.fd),nonFDvars = StationsVars[,c("altitude","longitude")])

#Fitting the extended filtered functional principal component logit regression model for modeling weather stations location from curves of Temperature and Precipitation and "altitude" and "longitude" as nonfunctional variables. Stepwise selection method is considered for functional principal components and nonfunctional variables selection.

Fit4<-logitFD.fpc.step(Response=StationsVars$North,FDobj=list(TempMonth.fd,PrecMonth.fd),nonFDvars = StationsVars[,c("altitude","longitude")])

#No commets are added to every resulting object

summary(Fit3$glm.fit)
table(StationsVars$North,round(predict(Fit3$glm.fit,type="response")))
100*sum(diag(table(StationsVars$North,round(predict(Fit3$glm.fit,type="response")))))/nrow(StationsVars)
Fit3$Intercept
plot(Fit3$betalist[[1]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_1(t)")
plot(Fit3$betalist[[2]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_2(t)")
cbind(eval.fd(c(1:12),Fit3$betalist[[1]]),eval.fd(c(1:12),Fit3$betalist[[2]]))
Fit3$PC.variance
Fit3$ROC
plot(Fit3$ROC)

#No commets are added to every resulting object

summary(Fit4$glm.fit)
table(StationsVars$North,round(predict(Fit4$glm.fit,type="response")))
100*sum(diag(table(StationsVars$North,round(predict(Fit4$glm.fit,type="response")))))/nrow(StationsVars)
Fit4$Intercept
plot(Fit4$betalist[[1]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_1(t)")
plot(Fit4$betalist[[2]],xlab="Month",ylab="Functional parameter",main="Functional parameter beta_2(t)")
cbind(eval.fd(c(1:12),Fit4$betalist[[1]]),eval.fd(c(1:12),Fit4$betalist[[2]]))
Fit4$PC.variance
Fit4$ROC
plot(Fit4$ROC)


data.frame("Months"=names(monthLetters),"Fit3.Beta1"=eval.fd(c(1:12),Fit3$betalist[[1]]),"Fit3.Beta2"=eval.fd(c(1:12),Fit3$betalist[[2]]),"Fit4.Beta1"=eval.fd(c(1:12),Fit4$betalist[[1]]),"Fit4.Beta2"=eval.fd(c(1:12),Fit4$betalist[[2]]))

#Ahora tendríamos que ver cómo replicar estos cálculos con las funciones de fda.usc

#Fit1<-logitFD.pc(Response=StationsVars$North,FDobj=list(TempMonth.fd,PrecMonth.fd),ncomp = c(3,4),nonFDvars = StationsVars[,c("altitude","longitude")])

DataAemet<-list("df"=StationsVars,"Temperatura"=TempMonth.fd,"Lluvia"=PrecMonth.fd)

Ajuste1<-fregre.glm(North~altitude+longitude+Temperatura+Lluvia,data = DataAemet,basis.x = list(TempMonth.fd$basis,PrecMonth.fd$basis),family = binomial())
#Este no me sale

#Pruebo con objetos de fda.usc

#TempMonth.fd<-Data2fd(argvals = c(1:12), y=t(TempMonth),basisobj = FourierBasis)
#PrecMonth.fd<-Data2fd(argvals = c(1:12), y=t(PrecMonth),basisobj = BsplineBasis)

DataAemet<-list("df"=StationsVars,"Temperatura"=fdata(t(TempMonth),argvals = c(1:12)),"Lluvia"=fdata(t(PrecMonth),argvals = c(1:12)))

Ajuste1<-fregre.glm(North~altitude+longitude+Temperatura+Lluvia,data = DataAemet,basis.x = list(TempMonth.fd$basis,PrecMonth.fd$basis),basis.b =list(TempMonth.fd$basis,PrecMonth.fd$basis) ,family = binomial())

#Con los objetos fdata, sí funciona, aunque siempre devuelve 5 coeficientes básicos para los objetos funcionales.

#Voy a intentar hacer esto mismo pero con ACP en las bases

DataAemet<-list("df"=StationsVars,"Temperatura"=fdata(t(TempMonth),argvals = c(1:12)),"Lluvia"=fdata(t(PrecMonth),argvals = c(1:12)))

Ajuste2<-fregre.glm(North~altitude+longitude+Temperatura+Lluvia,data = DataAemet,basis.x = list(pca.fd(TempMonth.fd,nharm = 3),pca.fd(PrecMonth.fd,nharm = 4)),family = binomial())

#Voy a intentar hacer esto mismo pero con ACP en las curvas

DataAemet<-list("df"=StationsVars,"Temperatura"=pca.fd(TempMonth.fd,nharm=3),"Lluvia"=pca.fd(PrecMonth.fd,nharm = 4))

Ajuste3<-fregre.glm(North~altitude+longitude+Temperatura+Lluvia,data = DataAemet,basis.x = list(pca.fd(TempMonth.fd,nharm = 3),pca.fd(PrecMonth.fd,nharm = 4)),family = binomial())

#Este directamente no funciona pues pide introducir una variable funcional, esto es, no permite introducir como funcional otra cosa que no sea un fdata.

#-----------------------------------------------------------------------------

AjusteOviedo<-logitFD.fpc(Response=tecator$df$fat15,FDobj=list(Data2fd(argvals=tecator$absorp.d2$argvals,y=t(tecator$absorp.d2$data),basisobj = create.bspline.basis(rangeval=c(850,1050),nbasis = 5))),ncomp = 5)
plot(AjusteOviedo$betalist[[1]])

AjusteOviedo2<-logitFD.pc(Response=tecator$df$fat15,FDobj=list(Data2fd(argvals=tecator$absorp.d2$argvals,y=t(tecator$absorp.d2$data),basisobj = create.bspline.basis(rangeval=c(850,1050),nbasis = 5))),ncomp = 5)
plot(AjusteOviedo2$betalist[[1]])

table(tecator$df$fat15,ifelse(AjusteOviedo$glm.fit$fitted.values<=0.5,0,1))
