#This script corresponds to an example of the running code of the
#CvmortalityMult R-package which was developed by David Atance and Ana Debon

#There two ways to install the package.
#1. Using cran with the following command:
install.packages("CvmortalityMult")
library(CvmortalityMult)

#2. Using the GitHub account of one of the owner of the library.
devtools::install_github("davidAtance/CvmortalityMult")
library(CvmortalityMult)

#forecast.fitLCmulti(), multipopulation_cv(), plot.forLCmulti(), fitLCmulti()

#Once, the library is installed, we can begin using the code.
#First, we display the mortality dataset regarding the regions of Spain:
SpainRegions

#Before, to start with the application of the multi-population mortality models,
#we need to construct a vector containing the ages in the dataset
ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40,
          45, 50, 55, 60, 65, 70, 75, 80, 85, 90)

#Before proceeding, we need to load a couple of additional libraries:
library(gnm)
library(forecast)
library(StMoMo)
library(sf)

##########################################################
#FITTING PROCESS
#It is necessary to select between the five multi-population mortality models:
#additive, multiplicative, common-factror, augmented common-factor or joint-K
#However, if no model is specified, the function will default to the additive approach.

#Additionally, we have create help functions to all the function in the package.
#These describe in deep the function in detail, including usage, arguments, values provided
#references in the literature, and a example.

#To show how the fitting process works,
?fitLCmulti
#we fit the additive multi-population mortality model for male population
additive_Spainmales <- fitLCmulti(qxt = SpainRegions$qx_male,
                                  periods = c(1991:2020), ages = c(ages),
                                  model = "additive",
                                  nPop = 18, lxt = SpainRegions$lx_male)
#A brief summary of the fitting results can be displayed
#by calling the name of the created variable
additive_Spainmales

#Additionally, the parameters for the multipopulation mortality model
#can be shown by callying the generic plot function
?plot.fitLCmulti
plot(additive_Spainmales)

#Another example, fitting the additive multi-population mortality model for Spain female regions
additive_Spainfemales <- fitLCmulti(qxt = SpainRegions$qx_female,
                                    periods = c(1991:2020), ages = c(ages),
                                    model = "additive",
                                    nPop = 18, lxt = SpainRegions$lx_female)
#Equally, with a brief summary of the fitting process
additive_Spainfemales
#and we can access to the main parameters with the plot function
plot(additive_Spainfemales)

#Once, we fitted the additive multi-population mortality model,
#we can apply the multiplicative model, using the
#same function but specifying the name of the other model, 'multiplicative'.
#The multiplicative multi-population mortality model
#for males in Spain Regions can be fitted as follows:
multi_Spainmales <- fitLCmulti(qxt = SpainRegions$qx_male,
                               periods = c(1991:2020), ages = c(ages),
                               model = "multiplicative",
                               nPop = 18, lxt = SpainRegions$lx_male)
#Equally, with a brief summary of the fitting process
multi_Spainmales
#and we can access to the main parameters with the plot function.
plot(multi_Spainmales)

#Similarly for the female regions of Spain
multi_Spainfemales <- fitLCmulti(qxt = SpainRegions$qx_female,
                                 periods = c(1991:2020), ages = c(ages),
                                 model = "multiplicative",
                                 nPop = 18, lxt = SpainRegions$lx_female)
multi_Spainfemales
plot(multi_Spainfemales)

#To highlight the differences among the multi-population mortality models,
#the function shows the equation used for each model which can be obtained as follows:
additive_Spainmales$formula
#[1] "Binomial model with predictor: logit q[x,t,i] = a[x] + b[x] k[t] + I[i]"
multi_Spainmales$formula
#[1] "Binomial model with predictor: logit q[x,t,i] = a[x] + b[x] k[t] I[i]"

#where the different model structure is shown.

#Additionally, we have incorporated a function
#to display an indicator over the different regions of Spain in a map.
#Here, we present the results for the Ii indicator which corresponds to
#the apportation of each region to the mortality:

SpainMap(regionvalue = additive_Spainmales$Ii[2:18],
         main = c("Additive for males"),
         name = c("Ii"))
SpainMap(regionvalue = multi_Spainmales$Ii[2:18],
         main = c("Multi for males"),
         name = c("Ii"),
         bigred = FALSE)

SpainMap(regionvalue = additive_Spainfemales$Ii[2:18],
         main = c("Additive for females"),
         name = c("Ii"))
SpainMap(regionvalue = multi_Spainfemales$Ii[2:18],
         main = c("Multi for females"),
         name = c("Ii"),
         bigred = FALSE)

####################################################################
#Additionally,  if only one population is provided, the functions will fit the
#single population version of the Lee-Carter model.
#For this purpose, we created another dataset with the total national population of Spain:
SpainNat

#The single population version of the Lee-Carter model can be fitted using the same function,
#which automatically detects one population and fits the appropriate model (single LC):
LC_SpainNatmales <- fitLCmulti(qxt = SpainNat$qx_male,
                               periods = c(1991:2020), ages = c(ages),
                               model = "additive",
                               nPop = 1, lxt = SpainNat$lx_male)

#Calling the function reveals which model was fitted:
LC_SpainNatmales
#As before, we can plot the parameters using the generic plot function:
plot(LC_SpainNatmales)

#Regardless of the model provided, if nPop is set to one,
#the function will use the single population version of the Lee-Carter model.
#Here, we use a different model assumption for females:
LC_SpainNatfemales <- fitLCmulti(qxt = SpainNat$qx_female,
                                 periods = c(1991:2020), ages = c(ages),
                                 model = "multiplicative",
                                 nPop = 1, lxt = SpainNat$lx_female)
#As before, using nPop = 1, the model fits the single version of the Lee-Carter model:
LC_SpainNatfemales
plot(LC_SpainNatfemales)

###################################################
#PROJECTING FUTURE MORTALITY RATES
#For the additive or multiplicative model, we create only one function
#to forecast the mortality rates.

#It should be noted that we have incorporated different functions
#to forecast the trend value kt, providing the user with various options in the projection process.
#First, we forecast using the additive model for the male population in Spain
#using the best ARIMA model for the trend parameter.
?forecast.fitLCmulti
fut_additive_Spainmales <- forecast(object = additive_Spainmales,
                                    nahead = 10, ktmethod = 'arimapdq')

#As before, calling the name of the variable shows what has been done:
fut_additive_Spainmales

#We have incorporated a plot function using the generic plot function:
?plot.forLCmulti
plot(fut_additive_Spainmales)

#Second option to forecast with a random walk with drift ARIMA (0,1,0),
#using the following code: ktmethod = 'arima010'
fut2_additive_Spainmales <- forecast(object = additive_Spainmales,
                                    nahead = 10, ktmethod = 'arima010')
fut2_additive_Spainmales

plot(fut2_additive_Spainmales)

#Third option to forecast with ktmethod = 'arimauser'.
#The user selects the arima to be applied,
#in this case, an ARIMA (1,2,1)
fut3_additive_Spainmales <- forecast(object = additive_Spainmales,
                                     nahead = 10, ktmethod = 'arimauser',
                                     order = c(1,2,1))
fut3_additive_Spainmales

plot(fut3_additive_Spainmales)

#For the multiplicative model for males in Spain,
#we apply again three option.
#First, the best ARIMA for the trend parameter:
fut_multiplicative_Spainmales <- forecast(object = multi_Spainmales,
                                          nahead = 10, ktmethod = 'arimapdq')

fut_multiplicative_Spainmales
plot(fut_multiplicative_Spainmales)

#Second option, a random walk with drift ARIMA (0,1,0)
fut2_multiplicative_Spainmales <- forecast(object = multi_Spainmales,
                                          nahead = 10, ktmethod = 'arima010')

fut2_multiplicative_Spainmales
plot(fut2_multiplicative_Spainmales)

#Third option, the user selects the arima to be applied.
fut3_multiplicative_Spainmales <- forecast(object = multi_Spainmales,
                                           nahead = 10, ktmethod = 'arimauser',
                                           order = c(0,1,0))

fut3_multiplicative_Spainmales
plot(fut3_multiplicative_Spainmales)


#We repeat the process for the female regions of Spain.
#for the additive model for females in Spain
fut_additive_Spainfemales <- forecast(object = additive_Spainfemales,
                                      nahead = 10, ktmethod = 'arimapdq')
fut_additive_Spainfemales
plot(fut_additive_Spainfemales)

#For the multiplicative model for males in Spain using the random walk with drift
fut_multiplicative_Spainfemales <- forecast(object = multi_Spainfemales,
                                            nahead = 10, ktmethod = 'arima010')
fut_multiplicative_Spainfemales
plot(fut_multiplicative_Spainfemales)

#Similarly, for a single population forecasting the single version of the Lee-Carter model:
fut_LC_Spainmales <- forecast(object = LC_SpainNatmales,
                              nahead = 10, ktmethod = 'arimapdq')
fut_LC_Spainmales
plot(fut_LC_Spainmales)

#Test the option when the user chosen to apply a custom ARIMA model
fut2_LC_Spainmales <- forecast(object = LC_SpainNatmales,
                              nahead = 10, ktmethod = 'arimauser',
                              order = c(1,3,1))
fut2_LC_Spainmales
plot(fut2_LC_Spainmales)

#Equally for Spain female regions
fut_LC_Spainfemales <- forecast(object = LC_SpainNatfemales,
                                nahead = 10, ktmethod = 'arima010')
fut_LC_Spainfemales
plot(fut_LC_Spainfemales)

################################################################
#CROSS-VALIDATION FOR MULTI-POPULATION MORTALITY MODELS
################################################################
#In this section, we present how to apply 2 cross-validation
#for the 5 multi-population mortality models included in the package
#for the regions of Spain, using the data set:
SpainRegions

#1. Rolling-Origin (RO) recalibration using a common-CV method for Spain male regions
#which is defined by default with fixed_train_origin = TRUE
#1.1. For the multiplicative multi-population mortality model, as follows:
?multipopulation_cv
cv_SM_multi <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                  model = c('multiplicative'),
                                  periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                  lxt = SpainRegions$lx_male,
                                  trainset1 = 8, nahead = 5,
                                  ktmethod = c('arimapdq'), measures = c("MSE"))

#As before, calling the name of the variable shows the results of the cross-validation process:
cv_SM_multi


#1.2. For the additive multi-population mortality model as:
cv_SM_addit <- multipopulation_cv(qxt = SpainRegions$qx_male, model = c('additive'),
                                  periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_male,
                                  trainset1 = 8, nahead = 5, ktmethod = c('arimapdq'), measures = c("MSE"))

cv_SM_addit

#1.3. For the common-factor (CFM) multi-population mortality model as:
cv_SM_CFM <- multipopulation_cv(qxt = SpainRegions$qx_male, model = c('CFM'),
                                periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_male,
                                trainset1 = 8, nahead = 5, ktmethod = c('arimapdq'), measures = c("MSE"))

cv_SM_CFM

#1.4. For the joint-K multi-population mortality model as:
cv_SM_joiK <- multipopulation_cv(qxt = SpainRegions$qx_male, model = c('joint-K'),
                                 periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_male,
                                 trainset1 = 8, nahead = 5, ktmethod = c('arimapdq'), measures = c("MSE"))

cv_SM_joiK

#1.5. For the augmented common-factor (ACFM) multi-population mortality model as:
cv_SM_ACFM <- multipopulation_cv(qxt = SpainRegions$qx_male, model = c('ACFM'),
                                 periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_male,
                                 trainset1 = 8, nahead = 5, ktmethod = c('arimapdq'), measures = c("MSE"))

cv_SM_ACFM
#31 warning you received after execute the previous function.
#Indeed, the packages allows to know in which populations
#the model do not converged with:
cv_SM_ACFM$warn_msgs

#Comparing the five models, we can decide which model produces better results
#for the Spain male region data set. Indeed, our functions provide different
#forecasting accuracy test to allow the user select the best one for his/her propose
cv_SM_multi$meas_total
cv_SM_addit$meas_total
cv_SM_CFM$meas_total
cv_SM_joiK$meas_total
cv_SM_ACFM$meas_total


cv_SM_multi$meas_ages
cv_SM_multi$meas_periodsfut
cv_SM_multi$meas_pop

cv_SM_multi$meas_ages
cv_SM_multi$meas_periodsfut
cv_SM_multi$meas_pop

cv_SM_CFM$meas_ages
cv_SM_CFM$meas_periodsfut
cv_SM_CFM$meas_pop

cv_SM_joiK$meas_ages
cv_SM_joiK$meas_periodsfut
cv_SM_joiK$meas_pop

cv_SM_ACFM$meas_ages
cv_SM_ACFM$meas_periodsfut
cv_SM_ACFM$meas_pop

#2. Rolling-Origin (RO) recalibration using the LOOCV method for Spanish regions for males
#2.1. For the multiplicative multi-population mortality model, as follows:
loocv_SF_multi <- multipopulation_cv(qxt = SpainRegions$qx_female, model = c('multiplicative'),
                                     periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_female,
                                     trainset1 = 10, nahead = 1, ktmethod = c('arimapdq'), measures = c("MSE"))
#It will take some time due to the number of recalibrations.

#Equally, as in the RO recalibration with the standard CV, users can access
#some important information regarding the forecasting accuracy by writing
#the created object as follows:
loocv_SF_multi

#2.2. For the additive multi-population mortality model, as follows:
loocv_SF_addit <- multipopulation_cv(qxt = SpainRegions$qx_female, model = c('additive'),
                                     periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_female,
                                     trainset1 = 10, nahead = 1, ktmethod = c('arimapdq'), measures = c("MSE"))

loocv_SF_addit

#2.3. For the additive multi-population mortality model, as follows:
loocv_SF_CFM <- multipopulation_cv(qxt = SpainRegions$qx_female, model = c('CFM'),
                                   periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_female,
                                   trainset1 = 10, nahead = 1, ktmethod = c('arimapdq'), measures = c("MSE"))

loocv_SF_CFM

#2.4. For the joint-K multi-population mortality model, as follows:
loocv_SF_joiK <- multipopulation_cv(qxt = SpainRegions$qx_female, model = c('joint-K'),
                                    periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_female,
                                    trainset1 = 10, nahead = 1, ktmethod = c('arimapdq'), measures = c("MSE"))

loocv_SF_joiK

#2.5. For the ACFM multi-population mortality model, as follows:
loocv_SF_ACFM <- multipopulation_cv(qxt = SpainRegions$qx_female, model = c('ACFM'),
                                    periods =  c(1991:2020), ages = c(ages), nPop = 18, lxt = SpainRegions$lx_female,
                                    trainset1 = 10, nahead = 1, ktmethod = c('arimapdq'), measures = c("MSE"))

loocv_SF_ACFM
#50 warning you received after execute the previous function.
#Indeed, the packages allows to know in which populations and periods (iteration)
#the model do not converged with:
loocv_SF_ACFM$warn_msgs


#Finally, we construct several plots to compare the results and evaluate
#the forecasting accuracy of the all models with the two methods
val <- c(cv_SM_multi$meas_ages, cv_SM_addit$meas_ages,
         cv_SM_CFM$meas_ages, cv_SM_joiK$meas_ages, cv_SM_ACFM$meas_ages,
         loocv_SF_multi$meas_ages, loocv_SF_addit$meas_ages,
         loocv_SF_CFM$meas_ages, loocv_SF_joiK$meas_ages, loocv_SF_ACFM$meas_ages)
max1 <- max(val); min1 <- min(val)
par(mfrow=c(1,2))
plot(ages, cv_SM_multi$meas_ages, type='l',
     col='black', xlab = 'ages', ylab='MSE',
     main='RO-recalibration-CV', lty = 1, ylim=c(min1, max1))
lines(ages, cv_SM_addit$meas_ages,
      col ='red', lty = 2)
lines(ages, cv_SM_CFM$meas_ages,
      col ='blue', lty = 3)
lines(ages, cv_SM_joiK$meas_ages,
      col ='purple', lty = 5)
lines(ages, cv_SM_ACFM$meas_ages,
      col ='green', lty = 4)
legend('topleft', lty=c(1:5), col=c('black', 'red', "blue", "purple", "green"),
       c('Multiplicative', 'Additive', 'CFM', 'joint-K', 'ACFM'),
       cex = 0.8, lwd = 1)

plot(ages, loocv_SF_multi$meas_ages, type='l',
     col='black', xlab = 'ages', ylab='MSE',
     main='RO-recalibration-LOOCV', lwd = 1, lty = 1, ylim=c(min1, max1))
lines(ages, loocv_SF_addit$meas_ages,
      col ='red', lwd =1, lty = 2)
lines(ages, loocv_SF_CFM$meas_ages,
      col ='blue', lwd =1, lty = 3)
lines(ages, loocv_SF_joiK$meas_ages,
      col ='purple', lwd =1, lty = 5)
lines(ages, loocv_SF_ACFM$meas_ages,
      col ='green', lwd =1, lty = 4)
legend("topleft", lty=c(1:5), col=c("black", "red", "blue", "purple", "green"),
       c('Multiplicative', 'Additive', 'CFM', 'joint-K', 'ACFM'),
       cex = 0.8, lwd=1)

val2 <- c(cv_SM_multi$meas_periodsfut, cv_SM_addit$meas_periodsfut,
          cv_SM_CFM$meas_periodsfut, cv_SM_joiK$meas_periodsfut,
          cv_SM_ACFM$meas_periodsfut,
          loocv_SF_multi$meas_periodsfut, loocv_SF_addit$meas_periodsfut,
          loocv_SF_CFM$meas_periodsfut, loocv_SF_joiK$meas_periodsfut,
          loocv_SF_ACFM$meas_periodsfut)
periods <- c("1999-2003", "2004-2008", "2009-2013", "2014-2018", "2019-2021")
max2 <- max(val2); min2 <- min(val2)
par(mfrow=c(1,2))
plot(c(1:5), cv_SM_multi$meas_periodsfut, type='l',
     col='black', xlab = 'periods', ylab='MSE',
     main='RO-recalibration-CV', lwd = 1, lty = 1, ylim = c(min2, max2), xaxt = "n",)
axis(1, 1:5, c("1999-2003", "2004-2008", "2009-2013", "2014-2018", "2019-2021"))
lines(c(1:5), cv_SM_addit$meas_periodsfut, col ='red', lty =2)
lines(c(1:5), cv_SM_CFM$meas_periodsfut, col ='blue', lty =3)
lines(c(1:5), cv_SM_joiK$meas_periodsfut, col ='purple', lty =5)
lines(c(1:5), cv_SM_ACFM$meas_periodsfut, col ='green', lty =4)
legend('topleft', lty=c(1:5), col=c('black', 'red', "blue", "purple", "green"),
       c('Multiplicative', 'Additive', "CFM", "joint-K", "ACFM"), cex = 0.75, lwd=1)

plot(c(2001:2020), loocv_SF_multi$meas_periodsfut, type='l',
     col='black', xlab = 'fut-periods', ylab='MSE',
     main='RO-recalibration-LOOCV', lty = 1, lwd = 1)
lines(c(2001:2020), loocv_SF_addit$meas_periodsfut, col ='red', lty =2)
lines(c(2001:2020), loocv_SF_CFM$meas_periodsfut, col ='blue', lty =3)
lines(c(2001:2020), loocv_SF_joiK$meas_periodsfut, col ='purple', lty =5)
lines(c(2001:2020), loocv_SF_ACFM$meas_periodsfut, col ='green', lty =4)
legend('topleft', lty=c(1:5), col=c('black', 'red', 'blue', 'purple', 'green'),
       c('Multiplicative', 'Additive', 'CFM', 'joint-K', 'ACFM'), cex = 0.7, lwd=1)

SpainMap(regionvalue = cv_SM_multi$meas_pop[2:18],
         main = "RO-recalibration-CV males Multiplicative", name = "MSE")
SpainMap(regionvalue = cv_SM_addit$meas_pop[2:18],
         main = "RO-recalibration-CV males Additive", name = "MSE")
SpainMap(regionvalue = cv_SM_CFM$meas_pop[2:18],
         main = "RO-recalibration-CV males CFM", name = "MSE")
SpainMap(regionvalue = cv_SM_joiK$meas_pop[2:18],
         main = "RO-recalibration-CV males joint-K", name = "MSE")
SpainMap(regionvalue = cv_SM_ACFM$meas_pop[2:18],
         main = "RO-recalibration-CV males ACFM", name = "MSE")

SpainMap(regionvalue = loocv_SF_multi$meas_pop[2:18],
         main = "RO-recalibration-LOOCV females Multiplicative", name = "MSE")
SpainMap(regionvalue = loocv_SF_addit$meas_pop[2:18],
         main = "RO-recalibration-LOOCV females Additive", name = "MSE")
SpainMap(regionvalue = loocv_SF_CFM$meas_pop[2:18],
         main = "RO-recalibration-LOOCV females CFM", name = "MSE")
SpainMap(regionvalue = loocv_SF_joiK$meas_pop[2:18],
         main = "RO-recalibration-LOOCV females joint-K", name = "MSE")
SpainMap(regionvalue = loocv_SF_ACFM$meas_pop[2:18],
         main = "RO-recalibration-LOOCV females ACFM", name = "MSE")


##########################################################
#ADDITIONAL CODE TO RUN THE COMMON-FACTOR, AUGMENTED COMMON-FACTOR and JOINT-K
#1. FITTING PROCESS
#It is necessary to select between the multi-population mortality models (CFM, ACFM and joint-K)
#However, if no model is specified, the function will fit by default to the additive approach.

#To show how the fitting process works,
#1.1 The CFM multi-population mortality model for the male population
cfm_Spainmales <- fitLCmulti(qxt = SpainRegions$qx_male,
                             periods = c(1991:2020), ages = c(ages),
                             model = "CFM",
                             nPop = 18, lxt = SpainRegions$lx_male)
#A brief summary of the fitting results can be displayed
#by calling the name of the created variable
cfm_Spainmales

#Additionally, the parameters for the multipopulation mortality model
#can be shown by callying the generic plot function
plot(cfm_Spainmales)

#1.2 The ACFM multi-population mortality model for male population
acfm_Spainmales <- fitLCmulti(qxt = SpainRegions$qx_male,
                              periods = c(1991:2020), ages = c(ages),
                              model = "ACFM",
                              nPop = 18, lxt = SpainRegions$lx_male)
#You received 6 warnings after after executing the previous function.
#Indeed, the packages allows to know in which populations
#the model do not converged with:
acfm_Spainmales$warn_msgs

#A brief summary of the fitting results can be displayed
#by calling the name of the created variable
acfm_Spainmales

#Additionally, the parameters for the multipopulation mortality model
#can be shown by callying the generic plot function
plot(acfm_Spainmales)

#1.3 The joint-K multi-population mortality model for male population
jointK_Spainmales <- fitLCmulti(qxt = SpainRegions$qx_male,
                                periods = c(1991:2020), ages = c(ages),
                                model = "joint-K",
                                nPop = 18, lxt = SpainRegions$lx_male)
#A brief summary of the fitting results can be displayed
#by calling the name of the created variable
jointK_Spainmales

#Additionally, the parameters for the multipopulation mortality model
#can be shown by calling the generic plot function
plot(jointK_Spainmales)

#2. PROJECTING FUTURE MORTALITY RATES with the different ARIMA methods
#2.1 we forecast using the cfm for the male population in Spain
#with ktmethod = 'arimapdq'
fut_cfm_Spainmales <- forecast(object = cfm_Spainmales,
                               nahead = 10, ktmethod = 'arimapdq')

#As before, calling the name of the variable shows what has been done:
fut_cfm_Spainmales

#We have incorporated a plot function using the generic plot function:
plot(fut_cfm_Spainmales)

#with ktmethod = 'arima010'
fut2_cfm_Spainmales <- forecast(object = cfm_Spainmales,
                               nahead = 10, ktmethod = 'arima010')
fut2_cfm_Spainmales
plot(fut2_cfm_Spainmales)

#with ktmethod = 'arimauser'
fut3_cfm_Spainmales <- forecast(object = cfm_Spainmales,
                                nahead = 10, ktmethod = 'arimauser',
                                order = c(1,2,1))
fut3_cfm_Spainmales
plot(fut3_cfm_Spainmales)

#2.2 we forecast using the acfm for the male population in Spain
#with ktmethod = 'arimapdq'
fut_acfm_Spainmales <- forecast(object = acfm_Spainmales,
                                nahead = 10, ktmethod = 'arimapdq')

#As before, calling the name of the variable shows what has been done:
fut_acfm_Spainmales

#We have incorporated a plot function using the generic plot function:
plot(fut_acfm_Spainmales)

#with ktmethod = 'arima010'
fut2_acfm_Spainmales <- forecast(object = acfm_Spainmales,
                                nahead = 10, ktmethod = 'arima010')
fut2_acfm_Spainmales
plot(fut2_acfm_Spainmales)

#with ktmethod = 'arimauser'
fut3_acfm_Spainmales <- forecast(object = acfm_Spainmales,
                                nahead = 10, ktmethod = 'arimauser',
                                order = matrix(c(0,1,0),
                                               nrow = acfm_Spainmales$nPop, ncol = 3,
                                               byrow = T))
fut3_acfm_Spainmales
plot(fut3_acfm_Spainmales)

#2.3 we forecast using the joint-K for the male population in Spain
fut_jointk_Spainmales <- forecast(object = jointK_Spainmales,
                                  nahead = 10, ktmethod = 'arimapdq')

#As before, calling the name of the variable shows what has been done:
fut_jointk_Spainmales

#We have incorporated a plot function using the generic plot function:
plot(fut_jointk_Spainmales)

#ktmethod = 'arima010'
fut2_jointk_Spainmales <- forecast(object = jointK_Spainmales,
                                  nahead = 10, ktmethod = 'arima010')
fut2_jointk_Spainmales
plot(fut2_jointk_Spainmales)

#ktmethod = 'arimauser'
fut3_jointk_Spainmales <- forecast(object = jointK_Spainmales,
                                  nahead = 10, ktmethod = 'arimauser',
                                  order = c(2,2,1))
fut3_jointk_Spainmales
plot(fut3_jointk_Spainmales)

#3. CROSS-VALIDATION with different values for ktmethod
#3.1. CROSS-VALIDATION with fixed_train_origin = TRUE (default-value)
# which implies a fixed-origin evaluation or rolling-origin recalibration evaluation.
#In this case, we apply the second one.
#First, as above, we apply the best ARIMA (p,d,q) for each trend parameter with
#ktmethod = 'arimapdq'
cv_SM_multi <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                  model = c('multiplicative'),
                                  periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                  lxt = SpainRegions$lx_male,
                                  trainset1 = 8, nahead = 5,
                                  ktmethod = c('arimapdq'), measures = c("MSE"))
cv_SM_multi
#Second, we can use the random-walk with drift
#a widely used option in the actuarial and demography literature
#with ktmethod = 'arima010'
cv2_SM_multi <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                  model = c('multiplicative'),
                                  periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                  lxt = SpainRegions$lx_male,
                                  trainset1 = 8, nahead = 5,
                                  ktmethod = c('arima010'),
                                  measures = c("MSE"))
cv2_SM_multi

#Third, we specify a custom ARIMA model, for instance c(1,2,1)
#another common choice in the actuarial and demography literature
#with ktmethod = 'arimauser' and providing order = c(1,2,1)
cv3_SM_multi <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                   model = c('multiplicative'),
                                   periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                   lxt = SpainRegions$lx_male,
                                   trainset1 = 8, nahead = 5,
                                   ktmethod = c('arimauser'),
                                   order = c(1,2,1),
                                   measures = c("MSE"))
cv3_SM_multi
#3.2. CROSS-VALIDATION with fixed_train_origin = FALSE (default-value)
#which implies a rolling-origin recalibration evaluation
#First, as above, we apply the best ARIMA (p,d,q) for each trend parameter with
#ktmethod = 'arimapdq'
cv_SM_CFM <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                model = c('CFM'),
                                periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                lxt = SpainRegions$lx_male,
                                trainset1 = 8, nahead = 5,
                                fixed_train_origin = FALSE,
                                ktmethod = c('arimapdq'),
                                measures = c("MSE"))
cv_SM_CFM

#Second, we can use the random-walk with drift
#a widely used option in the actuarial and demography literature
#with ktmethod = 'arima010'
cv2_SM_CFM <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                 model = c('CFM'),
                                 periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                 lxt = SpainRegions$lx_male,
                                 trainset1 = 8, nahead = 5,
                                 fixed_train_origin = FALSE,
                                 ktmethod = c('arima010'),
                                 measures = c("MSE"))
cv2_SM_CFM

#Third, we specify a custom ARIMA model, for instance c(1,2,1)
#another common choice in the actuarial and demography literature
#with ktmethod = 'arimauser' and providing order = c(1,2,1)
cv3_SM_CFM <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                 model = c('multiplicative'),
                                 periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                 lxt = SpainRegions$lx_male,
                                 trainset1 = 8, nahead = 5,
                                 fixed_train_origin = FALSE,
                                 ktmethod = c('arimauser'),
                                 order = c(0,1,0),
                                 measures = c("MSE"))
cv3_SM_CFM

#3.3. CROSS-VALIDATION with fixed_train_origin = 'add_remove1'
#which implies a rolling-origin recalibration evaluation but
#only adding and deleting one period in each iteration.
#First, as above, we apply the best ARIMA (p,d,q) for each trend parameter with
#ktmethod = 'arimapdq'
cv_SM_ACFM <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                model = c('ACFM'),
                                periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                lxt = SpainRegions$lx_male,
                                trainset1 = 8, nahead = 5,
                                fixed_train_origin = 'add_remove1',
                                ktmethod = c('arimapdq'),
                                measures = c("MSE"))
cv_SM_ACFM

#Second, we can use the random-walk with drift
#a widely used option in the actuarial and demography literature
#with ktmethod = 'arima010'
cv2_SM_ACFM <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                 model = c('ACFM'),
                                 periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                 lxt = SpainRegions$lx_male,
                                 trainset1 = 8, nahead = 5,
                                 fixed_train_origin = 'add_remove',
                                 ktmethod = c('arima010'),
                                 measures = c("MSE"))
cv2_SM_ACFM

#Third, we specify a custom ARIMA model, for instance c(1,1,0)
#In this case, as we apply the ACFM, which have nPop trend-parameters.
#Therefore, the user must provide a matrix with the order
#of the ARIMA-specifications for each trend-parameter.
cv3_SM_ACFM <- multipopulation_cv(qxt = SpainRegions$qx_male,
                                 model = c('ACFM'),
                                 periods =  c(1991:2020), ages = c(ages), nPop = 18,
                                 lxt = SpainRegions$lx_male,
                                 trainset1 = 8, nahead = 5,
                                 fixed_train_origin = 'add_remove',
                                 ktmethod = c('arimauser'),
                                 order = matrix(c(0,1,0),
                                                nrow = 18, ncol = 3,
                                                byrow = T),
                                 measures = c("MSE"))

cv3_SM_ACFM
cv3_SM_ACFM$kt.order

