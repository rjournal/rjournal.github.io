###################################
##### install the package #########
###################################

install.packages('BMRMM')
library(BMRMM)

##################################
######### foxp2 data set #########
##################################

res_fp2 <- BMRMM(foxp2,2)

# Figure 2
get_global_test_results(res_fp2$results_trans)
get_global_test_results(res_fp2$results_duration)

# Figure 3
trans_cov_labels <- matrix(c('F','W','','U','L','A'),nrow=2,byrow=TRUE)
state_labels <- c('d','m','s','u')
cov_levels <- matrix(c(1,2,1,3,2,2),nrow=3,byrow=TRUE)
get_estimated_post_mean_and_sd(res_fp2$results_trans,trans_cov_labels,state_labels,cov_levels)

# Figure 4
get_tp_local_test_results(res_fp2$results_trans,cov=1,delta=0.02,trans_cov_labels,state_labels)

# Figure 5
get_histogram_with_post_mean(res_fp2$results_duration,x_range=c(0,1))
get_histogram_by_component(res_fp2$results_duration)

# Printed results on page 11 
duration_cov_labels <-  matrix(c('F','W','','','U','L','A','','d','m','s','u'),nrow=3,byrow=TRUE)
get_mixture_params(res_fp2$results_duration)
get_mixture_probs_by_cov(res_fp2$results_duration,duration_cov_labels)

# Figure 6
cov_levels <- matrix(c(1,1),nrow=1)
get_tp_diagnostic_plots(res_fp2$results_trans,from=2,to=3,cov_levels,trans_cov_labels,state_labels)
get_duration_diagnostic_plots(res_fp2$results_duration)


###################################
######### asthma data set #########
###################################

# please download the data set first

asthma <- read.csv('asthma.csv')

res_asm <- BMRMM(asthma,3,duration_num_comp=3)

# Figure 7
get_global_test_results(res_asm$results_trans)
get_global_test_results(res_asm$results_duration)

# Figure 8
trans_cov_labels <- matrix(c('Mild-Moderate','Severe', 'BMI<25','BMI>=25','Women','Men'),nrow=3,byrow=TRUE)
cov_levels <- matrix(c(2,2,1,2,2,2),nrow=2,byrow=TRUE)
get_estimated_post_mean_and_sd(res_asm$results_trans,trans_cov_labels,cov_levels=cov_levels)

# Figure 9
get_tp_local_test_results(res_asm$results_trans,cov=2,delta=0.02,trans_cov_labels,decimal_pts=3)

# Figure 10
get_histogram_with_post_mean(res_asm$results_duration)
get_histogram_by_component(res_asm$results_duration)

# Printed results on page 15 & 16
duration_cov_labels <- matrix(c('Mild-Moderate','Severe','', 
                                'BMI<25','BMI>=25','',
                                'Women','Men','',
                                'State1','State2','State3'),nrow=4,byrow=TRUE)
get_mixture_probs_by_cov(res_asm$results_duration,duration_cov_labels)

# Figure 11
get_tp_diagnostic_plots(res_asm$results_trans,from=1,to=1,cov_levels=matrix(c(1,2,1),nrow=1), cov_labels=trans_cov_labels)
get_duration_diagnostic_plots(res_asm$results_duration)




