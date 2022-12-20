
#------------------------------------------------------------------------------------------------
#---Source code for paper 
#---WLinfer
#------------------------------------------------------------------------------------------------

install.packages('WLinfer')
library('WLinfer')


#Page 2, Point estimation example code-----------------------------------------------------------
data(fail_fiber) # fail_fiber is included in WLinfer package.
MLEc_WL(fail_fiber) # Closed form MLE-like estimator
MME_WL(fail_fiber) # Method of moment estimator
MMEm_WL(fail_fiber) # Modified method of moment estimator
MLE_WL(fail_fiber,init =1) # Initial value for phi is required.



#Page 3, Bias correction example code------------------------------------------------------------
WL(fail_fiber, est_method = "MLE",bias_cor = "firth") # Firth's method
WL(fail_fiber, est_method = "MLEc",bias_cor = "coxsnell") # Cox and Snell's method
WL(fail_fiber, est_method = "MLEc",bias_cor = "boots") # The bootstrap method


#Page 4, Interval estimation example code--------------------------------------------------------
WL(fail_fiber,est_method="MLEc")$CI_list #asymptotic CI for MLEc
WL(fail_fiber,est_method="MLE")$CI_list #asymptotic CI for MLE
# Bootstrap CI for MLEc corrected by Cox and Snell's method
WL(fail_fiber,est_method = "MLEc", bias_cor = "coxsnell")$CI_list



#Page4, Likelihood ratio test (LRT) example code-------------------------------------------------
wilks.test(fail_fiber,estimator = MME_WL(fail_fiber),side = "two")
wilks.test(fail_fiber,estimator=c(1,1),side="less")



#Page5, Illustration example code----------------------------------------------------------------
data("fail_fiber")
fiber = WL(fail_fiber,dist_test = "ks", est_method ="MLEc",
             wilks_alpha=0.05, wilks_side="two")
summary(fiber)
par(mfrow=c(2,2))
plot(fiber)
par(mfrow=c(1,1))
