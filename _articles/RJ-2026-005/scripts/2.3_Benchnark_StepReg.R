########################################################
#  This script is intended for benchmarking StepReg results against SAS.
#  We have also implemented unit tests that build upon and extend this script, using
#  SAS-derived results as ground truth. These unit tests are bundled with the StepReg
#  package to ensure consistent performance with SAS in every release.
########################################################


########################################################
# Load the StepReg package
########################################################
# package dependencies
# imports: ggplot2, ggrepel, MASS, survival, flextable, pROC, survAUC
# suggests: knitr, testthat, BiocStyle, kableExtra, cowplot
library(StepReg)


########################################################
# Dataset used for benchmarking against SAS
########################################################
# 1. mtcars dataset for linear regression
# 2. remission dataset for logistic regression
# 3. lung dataset for Cox regression
# 4. affairs dataset for Poisson regression
# 5. tobacco dataset for multivariate multiple regression
# 
# For detailed descriptions of the datasets:
# 1) Please refer to the "Use Cases" section in the StepReg package vignette:
#  https://cran.r-project.org/web/packages/StepReg/vignettes/StepReg.html
# 2) Alternatively, you can access the documentation for each dataset directly by using:
#  ?mtcars, ?remission, ?lung, ?affairs, and ?tobacco




########################################################
# Linear regression using the mtcars dataset
########################################################


# In this example, we specified the formula: mpg ~ ., which means the response variable is "mpg" and the predictor variables are all the other variables in the dataset.
# We used the following variable selection strategies: "forward", "backward", and "bidirection".
# The metrics included: c("AIC", "AICc", "BIC", "CP", "adjRsq", "SL", "SBC"), which are the selection criteria for variable selection.
# We set the significance level for entry to 0.15 and the significance level for stay to 0.15.


data(mtcars)
str(mtcars)
head(mtcars)
model_linear <- stepwise(type ="linear", 
                         formula= mpg ~ ., 
                         data= mtcars, 
                         strategy= c("forward", "backward", "bidirection"), 
                         metric= c("AIC", "AICc", "BIC", "CP", "adjRsq", "SL", "SBC"),
                         sle=0.15,
                         sls=0.15)




## Overview of the variable selection process
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.


model_linear$overview




## Variables in the final model
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated  metric values the same in each step?"
## and in Supplementary Table 5.
model_linear


########################################################
## Logistic stepwise regression
########################################################
# In this example, we specified the formula: remiss ~ ., which means the response variable is "remiss" and the predictor variables are all the other variables in the dataset.
# We used the following variable selection strategies: "forward", "backward", "bidirection", and "subset".
# The metrics included: "SL", which is the significance level for variable selection.
# We set the significance level for entry to 0.05 and the significance level for stay to 0.05.
# We set the best_n to Inf, which means all models will be selected for each variable count.


data(remission)
str(remission)
head(remission)
model_logit <- stepwise(type ="logit",
                        formula= remiss ~ .,
                        data= remission,
                        strategy= c("forward", "backward",  "bidirection", "subset"),
                        metric="SL",
                        sle=0.05,
                        sls=0.05,
                        best_n = Inf)


## Overview of the variable selection process
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_logit$overview


## Variables in the final model
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_logit


########################################################
## Cox stepwise regression
########################################################
# In this example, we specified the formula: Surv(time, status) ~ ., which means the response variable is "Surv(time, status)" and the predictor variables are all the other variables in the dataset.
# We used the following variable selection strategies: "forward", "backward", "bidirection", and "subset".
# The metrics included: "SL", which is the significance level for variable selection.
# We set the significance level for entry to 0.05 and the significance level for stay to 0.05.
# We set the test_method_cox to "breslow", which is the test method for Cox regression.
# We set the best_n to Inf, which means all models will be selected for each variable count.


data(lung)
str(lung)
head(lung)
lung <- na.omit(lung)
lung$sex <- factor(lung$sex, levels = c(1, 2))
model_cox <- stepwise(type ="cox",
                      formula= Surv(time, status) ~ .,
                      data= lung,
                      strategy= c("forward", "backward", "bidirection",  "subset"),
                      metric="SL",
                      sle=0.05, 
                      sls=0.05, 
                      test_method_cox = "breslow", 
                      best_n = Inf)


## Overview of the variable selection process
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_cox$overview


## Variables in the final model
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_cox




########################################################
## Poisson stepwise regression
########################################################
# In this example, we specified the formula: affairs ~ ., which means the response variable is "affairs" and the predictor variables are all the other variables in the dataset.
# We used the following variable selection strategies: "forward", "backward", and "bidirection".
# The metrics included: "SL", which is the significance level for variable selection.
# We set the significance level for entry to 0.05 and the significance level for stay to 0.05.


data(affairs)
str(affairs)
head(affairs)
model_poisson <- stepwise(type = "poisson",
                          formula = affairs ~ .,
                          data = affairs,
                          strategy = c("forward", "backward", "bidirection"),
                          metric = "SL",
                          sle=0.05,
                          sls=0.05)


## Overview of the variable selection process
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_poisson$overview


## Variables in the final model
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_poisson




########################################################
## Multivariate multiple stepwise regression
########################################################
# In this example, we specified the formula: cbind(cigarette, sugar, nicotine) ~ ., which means the response variables are "cigarette", "sugar", and "nicotine" and the predictor variables are all the other variables in the dataset.
# We used the following variable selection strategies: "subset".
# The metrics included: c("AIC", "AICc", "HQ", "SBC"), which are the selection criteria for variable selection.
# We set the best_n to Inf, which means all models will be selected for each variable count.


data(tobacco)
str(tobacco)
head(tobacco)
model_multivariate <- stepwise(type = "linear",
                              formula = cbind(cigarette, sugar, nicotine) ~ .,
                              data = tobacco,
                              strategy = "subset",
                              metric = c("AIC", "AICc", "HQ", "SBC"),
                              best_n = Inf)


## Overview of the variable selection process
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_multivariate$overview


## Variables in the final model
## We compared these results to the corresponding SAS output for each strategy and metric.
## The comparison result is detailed in the column "Are the calculated metric values the same in each step?"
## and in Supplementary Table 5.
model_multivariate


########################################################
## sessionInfo() 
########################################################
# R version 4.4.1 (2024-06-14)
# Platform: x86_64-apple-darwin20
# Running under: macOS Sonoma 14.6
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] StepRegShiny_1.6.1 testthat_3.2.1.1   StepReg_1.6.1      devtools_2.4.5     usethis_3.1.0     
# 
# loaded via a namespace (and not attached):
#   [1] RColorBrewer_1.1-3      rstudioapi_0.17.1       magrittr_2.0.3         
# [4] TH.data_1.1-3           magick_2.8.6            farver_2.1.2           
# [7] rmarkdown_2.29          fs_1.6.6                ragg_1.4.0             
# [10] vctrs_0.6.5             memoise_2.0.1           askpass_1.2.1          
# [13] base64enc_0.1-3         tinytex_0.57            htmltools_0.5.8.1      
# [16] polspline_1.1.25        curl_6.2.3              Formula_1.2-5          
# [19] pROC_1.18.5             htmlwidgets_1.6.4       desc_1.4.3             
# [22] plyr_1.8.9              sandwich_3.1-1          zoo_1.8-14             
# [25] lubridate_1.9.4         cachem_1.1.0            uuid_1.2-1             
# [28] mime_0.13               lifecycle_1.0.4         pkgconfig_2.0.3        
# [31] Matrix_1.7-0            R6_2.6.1                fastmap_1.2.0          
# [34] rcmdcheck_1.4.0         shiny_1.10.0            digest_0.6.37          
# [37] colorspace_2.1-1        ps_1.9.1                shinycssloaders_1.1.0  
# [40] rprojroot_2.0.4         pkgload_1.4.0           textshaping_1.0.1      
# [43] Hmisc_5.2-3             timechange_0.3.0        compiler_4.4.1         
# [46] remotes_2.5.0           fontquiver_0.2.1        withr_3.0.2            
# [49] pander_0.6.6            rjtools_1.0.18          htmlTable_2.4.3        
# [52] backports_1.5.0         ggcorrplot_0.1.4.1      pak_0.9.0              
# [55] pkgbuild_1.4.4          MASS_7.3-61             quantreg_6.1           
# [58] openssl_2.3.3           sessioninfo_1.2.2       tools_4.4.1            
# [61] foreign_0.8-87          zip_2.3.3               httpuv_1.6.16          
# [64] nnet_7.3-19             glue_1.8.0              callr_3.7.6            
# [67] nlme_3.1-166            promises_1.3.3          grid_4.4.1             
# [70] rsconnect_1.3.1         checkmate_2.3.2         cluster_2.1.6          
# [73] reshape2_1.4.4          generics_0.1.4          gtable_0.3.6           
# [76] tidyr_1.3.1             data.table_1.17.4       xml2_1.3.8             
# [79] ggrepel_0.9.6           pillar_1.10.2           stringr_1.5.1          
# [82] later_1.4.2             splines_4.4.1           dplyr_1.1.4            
# [85] pryr_0.1.6              lattice_0.22-6          survival_3.7-0         
# [88] SparseM_1.84-2          tidyselect_1.2.1        fontLiberation_0.1.0   
# [91] rms_8.0-0               miniUI_0.1.1.1          knitr_1.50             
# [94] fontBitstreamVera_0.1.1 gridExtra_2.3           svglite_2.1.3          
# [97] xfun_0.52               brio_1.1.5              rapportools_1.2        
# [100] matrixStats_1.5.0       DT_0.33                 stringi_1.8.7          
# [103] xopen_1.0.1             yaml_2.3.10             hunspell_3.0.6         
# [106] kableExtra_1.4.0        evaluate_1.0.3          codetools_0.2-20       
# [109] officer_0.6.10          tcltk_4.4.1             gdtools_0.4.2          
# [112] tibble_3.2.1            BiocManager_1.30.25     cli_3.6.5              
# [115] rpart_4.1.23            shinythemes_1.2.0       xtable_1.8-4           
# [118] systemfonts_1.2.3       processx_3.8.6          roxygen2_7.3.2         
# [121] dichromat_2.0-0.1       Rcpp_1.0.14             summarytools_1.1.4     
# [124] survAUC_1.3-0           MatrixModels_0.5-4      ellipsis_0.3.2         
# [127] ggplot2_3.5.2           prettyunits_1.2.0       profvis_0.4.0          
# [130] urlchecker_1.0.1        viridisLite_0.4.2       mvtnorm_1.3-3          
# [133] scales_1.4.0            purrr_1.0.4             flextable_0.9.9        
# [136] yesno_0.1.3             rlang_1.1.6             cowplot_1.1.3          
# [139] multcomp_1.4-28 