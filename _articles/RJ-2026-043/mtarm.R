 library(mtarm)
 
 ######################################################################
 ############## Example 1: Rainfall and two river flows in Colombia
 ###################################################################### 

 ########################################################
 ####### Data loading
 ######################################################## 
 data(riverflows)
 str(riverflows)

 ########################################################
 ####### Data plotting
 ######################################################## 
 dev.new()
 par(mfrow=c(3,1))
 with(riverflows,{plot(Date, Rainfall, type="l", lty=1, col="black", xlab="", 
	                     ylab="Millimeters", main="Rainfall")
	                plot(Date, Bedon, type="l", lty=1, col="black", xlab="", 
	                     ylab="Cubic meters per second", main="Bedon river")
	                plot(Date, LaPlata, type="l", lty=1, col="black", xlab="", 
	                     ylab="Cubic meters per second", main="La Plata river")})

 ########################################################
 ####### Model estimation
 ######################################################## 
 set.seed(2000)
 model_TAR <- mtar(~ Bedon + LaPlata | Rainfall, row.names=Date, dist="Gaussian", 
                   data=riverflows, subset={Date<="2009-04-04"}, ssvs=TRUE,
                   ars=ars(nregim=2,p=5,d=5), n.burnin=3000, n.sim=3000, n.thin=2)
 model_VAR <- update(model_TAR, ars=ars(nregim=1,p=5))

 ########################################################
 ####### Model comparison
 ########################################################
 DIC(model_TAR, model_VAR)
 WAIC(model_TAR, model_VAR)

 ########################################################
 ####### Residual analysis
 ########################################################
 set.seed(0202)
 res_model_TAR <- residuals(model_TAR)
 plot(res_model_TAR, col="blue")

 ########################################################
 ########################################################
 #Improving the fit: exploration of alternative models
 ########################################################
 ########################################################
	
 ########################################################
 ####### Model estimation
 ######################################################## 
 set.seed(0220)
 models <- mtar_grid(~ Bedon + LaPlata | Rainfall, row.names=Date, dist="Laplace",
                     data=riverflows, subset={Date<="2009-04-04"}, nregim.min=1, 
                     nregim.max=4, p.min=1, p.max=5, n.burnin=3000, n.sim=3000, 
                     n.thin=2, ssvs=TRUE, plan_strategy="multisession")
 models

 ########################################################
 ####### Model selection
 ########################################################
 
 ###########
 ########### Adjusted within-sample predictive accuracy measures
 ###########
 
 ########### DIC
 DICs <- matrix(DIC(models),5,4)
 rownames(DICs) <- paste0("p*=",1:5)
 colnames(DICs) <- c("VAR(p*)","TAR(2;p*,p*)","TAR(3;p*,p*,p*)","TAR(4;p*,p*,p*,p*)")
 round(DICs,2)

 ########### WAIC
 WAICs <- matrix(WAIC(models),5,4)
 rownames(WAICs) <- rownames(DICs)
 colnames(WAICs) <- colnames(DICs)
 round(WAICs,2)

 ###########
 ########### Out-of-sample predictive accuracy measures
 ########### 
 set.seed(0220)
 future.obs <- subset(riverflows, Date>"2009-04-04")                  
 oos <- out_of_sample(models, newdata=future.obs, n.ahead=nrow(future.obs), FUN=mean)

 ########### log-score
 LSs <- matrix(oos[,1],nrow=5,ncol=4)
 rownames(LSs) <- rownames(DICs)
 colnames(LSs) <- colnames(DICs)
 round(LSs,2)

 ########### Energy Score 
 ESs <- matrix(oos[,2],nrow=5,ncol=4)
 rownames(ESs) <- rownames(DICs)
 colnames(ESs) <- colnames(DICs)
 round(ESs,2)

 ########### APE: Bedon river flow 
 APEs.1 <- matrix(oos[,5],nrow=5,ncol=4)
 rownames(APEs.1) <- rownames(DICs)
 colnames(APEs.1) <- colnames(DICs)
 round(APEs.1,2)

 ########### APE: La Plata river flow
 APEs.2 <- matrix(oos[,6],nrow=5,ncol=4)
 rownames(APEs.2) <- rownames(DICs)
 colnames(APEs.2) <- colnames(DICs)
 round(APEs.2,2)

 ########################################################
 ####### Overview of the chosen model
 ########################################################

 ########### print() method for objects of class mtar
 models[["Laplace.3.5"]]
 
 ########### summary() method for objects of class mtar
 summary(models[["Laplace.3.5"]], credible=0.95)

 ########### fitted values	
 a <- fitted(models[["Laplace.3.5"]])
 plot(a, observed=list(type="b",pch=20,col="black",lty=3), last=250, 
         fitted=list(type="l",col="blue",lty=3,ylab=rep("Cubic meters per second",2)))

 ########################################################
 ####### Residual analysis
 ########################################################
 set.seed(0220)
 res <- residuals(models[["Laplace.3.5"]])

 ########### histogram and the normal QQ plot of the quantile-type residuals
 plot(res, col="blue")

 ########################################################
 ####### Convergence diagnostics
 ########################################################

 ########### Geweke's convergence diagnostic
 geweke_diagTAR(models[["Laplace.3.5"]])

 ########### Geweke's plot convergence diagnostic
 geweke_plotTAR(models[["Laplace.3.5"]])
 
 ########### Effective sample size
 effectiveSize_TAR(models[["Laplace.3.5"]])

 
 ########################################################
 ####### Forecasting
 ########################################################
 set.seed(0220)
 out <- predict(models[["Laplace.3.5"]], newdata=future.obs, n.ahead=nrow(future.obs), 
                credible=0.95, row.names=Date)

 ########### Forecasting for Bedon river flow								
 round(cbind(out[["summary"]][,c(1:3)], Bedon=future.obs[,"Bedon"]),2)

 ########### Forecasting for La Plata river flow 
 round(cbind(out[["summary"]][,c(4:6)], LaPlata=future.obs[,"LaPlata"]),2)

 ########### Plot of ten-step-ahead forecasts
 dev.new()
 plot(out, last=300,
      historical=list(type="l",col="black",lty=1),
      forecasts=list(type="l",col="black",lty=3,ylab=rep("Cubic meters per second",2)),
      forecasts.PI=list(col="light gray",border=NULL))

 ######################################################################
 ############## Simulating from a TAR model
 ######################################################################
 
 ########### Example 1
 set.seed(0220)
 n <- 2000
 k <- 2
 myars <- ars(nregim=1, p=2)
 Intercept <- TRUE
 trend <- "linear"
 nseason <- 4
 dist <- "Laplace"
 deterministic <- Intercept + switch(trend,"linear"=1,"quadratic"=2,"none"=0) + 
                              ifelse(is.null(nseason),0,nseason-1)
 parms <- list()
 parms[[1]] <- list()
 np <- deterministic + myars$p*k
 parms[[1]]$location <- ifelse(runif(np*k)<=0.5,1,-1)*rbeta(np*k,shape1=4,shape2=16)
 parms[[1]]$location <- matrix(parms[[1]]$location,np,k)
 parms[[1]]$scale    <- rexp(k,rate=1)*diag(k)

 myVAR <- simtar(n=n, k=k, ars=myars, dist=dist, Intercept=Intercept, trend=trend, 
                 nseason=nseason, parms=parms, Verbose=TRUE)

 str(myVAR)
 fit <- mtar( ~ Y1 + Y2, data=myVAR, ars=myars, Intercept=Intercept, trend=trend,
             nseason=nseason, dist=dist, n.burnin=2000, n.sim=3000, n.thin=2)

 
 ########### Example 2
 set.seed(0220)
 n <- 2000
 k <- 2
 myars <- ars(nregim=2, p=c(1,2))
 setar <- 2
 thresholds <- 0.3
 delay <- 1
 Intercept <- TRUE
 dist <- "Student-t"
 extra <- 4
 parms <- list()
 for(j in 1:myars$nregim){
     np <- Intercept + myars$p[j]*k
     parms[[j]] <- list()
     parms[[j]]$location <- ifelse(runif(np*k)<=0.5,1,-1)*rbeta(np*k,shape1=4,shape2=16)
     parms[[j]]$location <- matrix(parms[[j]]$location,np,k)
     parms[[j]]$scale    <- rexp(k,rate=1)*diag(k)
 }
 mySETAR <- simtar(n=n, k=k, ars=myars, Intercept=Intercept, setar=setar, parms=parms, 
              thresholds=thresholds, delay=delay, dist=dist, extra=extra, Verbose=TRUE)

 str(mySETAR)							
 fit <- mtar( ~ Y1 + Y2, data=mySETAR, ars=myars, setar=setar, dist=dist, 
             n.burnin=2000, n.sim=3000, n.thin=2)

 
 ######################################################################
 ############## Example 2: Iceland river flow
 ###################################################################### 

 ########################################################
 ####### Data loading
 ######################################################## 
 data(iceland.rf)
 str(iceland.rf)

 ########################################################
 ####### Model estimation
 ########################################################
 set.seed(0220)
 models <- mtar_grid(~ Jokulsa + Vatnsdalsa | Temperature | Precipitation, data=iceland.rf, 
                     subset={Date<="1974-12-21"}, row.names=Date, dist=c("Gaussian",
                     "Student-t","Skew-normal","Skew-Student-t"), nregim.min=2, nregim.max=2,
                     p.min=15, p.max=15, q.min=4, q.max=4, d.min=2, d.max=2, n.burnin=5000, 
                     n.sim=4000, n.thin=2, ssvs=TRUE, plan_strategy="multisession")

 models
 
 ########################################################
 ####### Model selection
 ########################################################
 
 ###########
 ########### Adjusted within-sample predictive accuracy measures
 ###########
 
 ########### DIC and WAIC
 DICs <- DIC(models)
 WAICs <- WAIC(models)
 round(cbind(DICs,WAICs),2)

 ###########
 ########### Out-of-sample predictive accuracy measures
 ###########
 future.obs <- subset(iceland.rf, Date>"1974-12-21") 
 set.seed(0220)
 oos <- out_of_sample(models, credible=0.95, newdata=future.obs,
                      n.ahead=nrow(future.obs), FUN=mean)

 ########### log-score, Energy Score, width and coverage rate of 95% prediction intervals 
 round(oos[,c(1,2,9:12)], 2)           

 ########################################################
 ####### Overview of the chosen model
 ########################################################
 summary(models[["Skew-normal.2.15.4.2"]])

 ########################################################
 ####### Forecasting
 ########################################################
 set.seed(0220)
 out <- predict(models[["Skew-normal.2.15.4.2"]], newdata=future.obs,
                n.ahead=nrow(future.obs), credible=0.95, row.names=Date)

 ########### Forecasting for Jokulsa river flow						
 round(cbind(out[["summary"]][,1:3], Jokulsa=future.obs[,"Jokulsa"]), 2)

 ########### Forecasting for Vatnsdalsa river flow						
 round(cbind(out[["summary"]][,4:6], Vatnsdalsa=future.obs[,"Vatnsdalsa"]),2)

 