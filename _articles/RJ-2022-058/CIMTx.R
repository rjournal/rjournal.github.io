library(CIMTx)
# Data Simulation -----------------------------------------------------
set.seed(1)
data <- data_sim(
  sample_size = 500,
  n_trt = 3,
  x = c(
    "rnorm(0, 0.5)",# x1
    "rbeta(2, .4)",   # x2
    "runif(0, 0.5)",# x3
    "rweibull(1, 2)",  # x4
    "rbinom(1, .4)"# x5
  ),
  # linear terms in parallel response surfaces
  lp_y = rep(".2*x1 + .3*x2 - .1*x3 - .1*x4 - .2*x5", 3),
  # nonlinear terms in parallel response surfaces
  nlp_y  = rep(".7*x1*x1  - .1*x2*x3", 3),
  align = F,# different predictors used in treatment and outcome models
  # linear terms in treatment assignment model
  lp_w = c(".4*x1 + .1*x2  - .1*x4 + .1*x5",    # w = 1
           ".2 * x1 + .2 * x2  - .2 * x4 - .3 * x5"),  # w = 2
  nlp_w = c("-.5*x1*x4  - .1*x2*x5", # w = 1
              "-.3*x1*x4 + .2*x2*x5"), # w = 2
  tau = c(-1.5,0,1.5),
  delta = c(0.5,0.5),
  psi = 1
)
data$ratio_of_units # the ratio of units in the simulated dataset

data$y_prev # The outcome prevalences within each treatment group

data$overlap_fig # The distributions of true GPS for each treatment group
# ggplot2::ggsave(file = "../plot/data_simulation_moderate_covariate_overlap.png", height = 3, width = 7)

#  set psi = 0.1 corresponds to a strong covariate overlap and keep other parameters the same
set.seed(1)
data_strong_overlap <- data_sim(
  sample_size = 500,
  n_trt = 3,
  x = c(
    "rnorm(0, 0.5)",# x1
    "rbeta(2, .4)",   # x2
    "runif(0, 0.5)",# x3
    "rweibull(1, 2)",  # x4
    "rbinom(1, .4)"# x5
  ),
  lp_y = rep(".2*x1 + .3*x2 - .1*x3 - .1*x4 - .2*x5", 3),
  nlp_y  = rep(".7*x1*x1  - .1*x2*x3", 3),
  align = F,
  lp_w = c(".4*x1 + .1*x2  - .1*x4 + .1*x5",    # w = 1
           ".2 * x1 + .2 * x2  - .2 * x4 - .3 * x5"),  # w = 2
  nlp_w = c("-.5*x1*x4  - .1*x2*x5", # w = 1
            "-.3*x1*x4 + .2*x2*x5"), # w = 2
  tau = c(-1.5,0,1.5),
  delta = c(0.5,0.5),
  psi = 0.1 #  psi = 0.1 corresponds to a strong covariate overlap
)

data_strong_overlap$overlap_fig
# ggplot2::ggsave(file = "../plot/data_simulation_strong_covariate_overlap.png", height = 3, width = 7)


# Regression adjustment---------------------------------------------------
set.seed(1)
ra_ate_res <- ce_estimate(y = data$y, x = data$covariates, w = data$w, method = "RA", estimand = "ATE", ndpost = 100)
summary(ra_ate_res)

set.seed(1)
ra_att_res <- ce_estimate(y = data$y, x = data$covariates,w = data$w, method = "RA", estimand = "ATT", ndpost = 100, reference_trt = 1)
summary(ra_att_res)
# Inverse Probability of Treatment Weighting--------------------------------------------------------------------
iptw_multi_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-Multinomial", estimand = "ATE")

set.seed(111)
iptw_sl_trim_ate_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-SL", estimand = "ATE", sl_library =  c("SL.glm", "SL.glmnet", "SL.rpart"), trim_perc = c(0.05,0.95), boot = TRUE, nboots = 100, verbose_boot = T)
summary(iptw_sl_trim_ate_res)
# BART--------------------------------------------------------------------
set.seed(111)
bart_res <- ce_estimate(y = data$y, x = data$covariates, w = data$w, method = "BART", estimand = "ATT", discard = FALSE, ndpost=100, reference_trt = 1)
summary(bart_res)
# Regression adjustment with multivariate spline of GPS--------------------------------------------------------------------
set.seed(111)
rams_multi_res <- ce_estimate(
  y = data$y,
  x = data$covariates ,
  w = data$w,
  method = "RAMS-Multinomial",
  estimand = "ATE",
  boot = TRUE,
  nboots = 100, verbose_boot = T
)
# Vector matching-------------------------------------------------------------
set.seed(11)
vm_att_res <-
  ce_estimate(y = data$y, x = data$covariates, w = data$w, method = "VM", estimand = "ATT", reference_trt = 1, n_cluster = 3)
vm_att_res$number_matched
# TMLE----------------------------------------------------------------
set.seed(111)
tmle_res_boot <- ce_estimate(y = data$y, x = data$covariates, w = data$w, method = "TMLE",estimand = "ATE", sl_library = c("SL.glm", "SL.glmnet", "SL.rpart"), boot = TRUE, nboots = 100)
summary(tmle_res_boot)
# Identification of a common support region -------------------------------
iptw_multi_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-Multinomial", estimand = "ATE")
iptw_multi_trim_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-Multinomial", estimand = "ATE", trim_perc = c(0.05,0.95))
set.seed(111)
iptw_sl_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-SL", estimand = "ATE",sl_library = c("SL.glm", "SL.glmnet", "SL.rpart"))
set.seed(111)
iptw_sl_trim_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-SL", estimand = "ATE", sl_library =  c("SL.glm", "SL.glmnet", "SL.rpart"), trim_perc = c(0.05,0.95))
set.seed(111)
iptw_gbm_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-GBM", estimand = "ATE")
set.seed(111)
iptw_gbm_trim_res <- ce_estimate(y = data$y, x = data$covariates , w = data$w, method = "IPTW-GBM", estimand = "ATE", trim_perc = c(0.05,0.95))
plot(iptw_multi_res,
     iptw_multi_trim_res,
     iptw_sl_res,
     iptw_sl_trim_res,
     iptw_gbm_res,
     iptw_gbm_trim_res)
# ggplot2::ggsave(file = "../plot/p_weight_comparison.png", height = 3, width = 7)

set.seed(87)
bart_dis_res <- ce_estimate(y = data$y, x = data$covariates, w = data$w, method = "BART", estimand = "ATT", discard = TRUE, ndpost = 100, reference_trt = 1)
bart_dis_res$n_discard



# Sensitivity analysis----------------------------------------------------------------------
#  Simulate a small dataset in a simple causal inference setting to illustrate the key steps of the approach. The are two binary confounders:x1 is measured and x2 is unmeasured.
set.seed(111)
data_SA <- data_sim(
  sample_size = 100,
  n_trt = 3,
  x = c("rbinom(1, .5)", # x1:measured confounder
        "rbinom(1, .4)"),# x2:unmeasured confounder
  lp_y = rep(".2*x1+2.3*x2", 3),# parallel response surfaces
  nlp_y = NULL,
  align = F, # w model is not the same as the y model
  lp_w = c("0.2 * x1 + 2.4 * x2", # w = 1
           "-0.3 * x1 - 2.8 * x2"),
  nlp_w = NULL,
  tau = c(-2, 0, 2),
  delta = c(0, 0),
  psi = 1
)

# Step 1: Estimate the GPS for each individual
sample_gap <- 10
m1 <- 50
w_model <- BART::mbart2(x.train = data_SA$covariates, y.train = data_SA$w, ndpost = m1 * sample_gap) # gap-sampling to reduce the dependence in MCMC
# Step 2: Draw the GPS for each individual from the fitted  multinomial probit BART model
gps <-
  array(w_model$prob.train[seq(1, m1 * sample_gap, sample_gap), ],
        dim = c(m1, # 1st dimension is M1
                length(unique(data_SA$w)), # 2nd dimension is the n_trt
                dim(data_SA$covariates)[1])) # 3rd dimension is sample size
dim(gps)

# Step 3: Specify the prior distributions of the confounding functions and the number of draws(M2) for the confounding functions c(w, w′, x).  In this illustrative simulation example, we use the true values of the confounding functions within each stratum of x1. This represents the strategy (i) point mass prior.
x1 <- data_SA$covariates[, 1, drop = F]
x2 <- data_SA$covariates[, 2, drop = F] # x2 as the unmeasured confounder
w <- data_SA$w
x1w_data <- cbind(x1, w)
Y1 <- data_SA$y_true[, 1]
Y2 <- data_SA$y_true[, 2]
Y3 <- data_SA$y_true[, 3]
y <- data_SA$y

# Calculate the true confounding functions within x1 = 1 stratum
c_1_x1_1 <- mean(Y1[w == 1 & x1 == 1]) - mean(Y1[w == 2 & x1 == 1])# c(1,2)
c_2_x1_1 <- mean(Y2[w == 2 & x1 == 1]) - mean(Y2[w == 1 & x1 == 1])# c(2,1)
c_3_x1_1 <- mean(Y2[w == 2 & x1 == 1]) - mean(Y2[w == 3 & x1 == 1])# c(2,3)
c_4_x1_1 <- mean(Y1[w == 1 & x1 == 1]) - mean(Y1[w == 3 & x1 == 1])# c(1,3)
c_5_x1_1 <- mean(Y3[w == 3 & x1 == 1]) - mean(Y3[w == 1 & x1 == 1])# c(3,1)
c_6_x1_1 <- mean(Y3[w == 3 & x1 == 1]) - mean(Y3[w == 2 & x1 == 1])# c(3,2)
c_x1_1 <- cbind(c_1_x1_1, c_2_x1_1, c_3_x1_1, c_4_x1_1, c_5_x1_1, c_6_x1_1)
# The true values of the confounding functions within the stratum x1= 0 can be calculated in a similar way.
c_1_x1_0 <- mean(Y1[w == 1 & x1 == 0]) - mean(Y1[w == 2 & x1 == 0])# c(1,2)
c_2_x1_0 <- mean(Y2[w == 2 & x1 == 0]) - mean(Y2[w == 1 & x1 == 0])# c(2,1)
c_3_x1_0 <- mean(Y2[w == 2 & x1 == 0]) - mean(Y2[w == 3 & x1 == 0])# c(2,3)
c_4_x1_0 <- mean(Y1[w == 1 & x1 == 0]) - mean(Y1[w == 3 & x1 == 0])# c(1,3)
c_5_x1_0 <- mean(Y3[w == 3 & x1 == 0]) - mean(Y3[w == 1 & x1 == 0])# c(3,1)
c_6_x1_0 <- mean(Y3[w == 3 & x1 == 0]) - mean(Y3[w == 2 & x1 == 0])# c(3,2)
c_x1_0 <- cbind(c_1_x1_0, c_2_x1_0, c_3_x1_0, c_4_x1_0, c_5_x1_0, c_6_x1_0) # True c functions among x1 = 0

# True c functions within the stratum of x1
true_c_fun <- true_c_fun_cal(x = x1, w = w)

# Step 4: Calculate  the  confounding  function  adjusted  outcomes  the  drawn  values  of  GPS  andconfounding functions

i <- 1;j <- 1
ycf <- ifelse(
  # w = 1, x1 = 1
  x1w_data[, "w"] == 1 &  x1 == 1,
  y - (c_x1_1[i, 1] * gps[j, 2,] + c_x1_1[i, 4] * gps[j, 3,]),
  # w = 1, x1 = 0
  ifelse(
    x1w_data[, "w"] == 1 & x1 == 0,
    y - (c_x1_0[i, 1] * gps[j, 2,] + c_x1_0[i, 4] * gps[j, 3,]),
    # w = 2, x1 = 1
    ifelse(
      x1w_data[, "w"] == 2 & x1 == 1,
      y - (c_x1_1[i, 2] * gps[j, 1,] + c_x1_1[i, 3] * gps[j, 3,]),
      # w = 2, x1 = 0
      ifelse(
        x1w_data[, "w"] == 2 & x1 == 0,
        y - (c_x1_0[i, 2] * gps[j, 1,] + c_x1_0[i, 3] * gps[j, 3,]),
        # w = 3, x1 = 1
        ifelse(
          x1w_data[, "w"] == 3 & x1 == 1,
          y - (c_x1_1[i, 5] * gps[j, 1,] + c_x1_1[i, 6] * gps[j, 2,]),
          # w = 3, x1 = 0
          y - (c_x1_0[i, 5] * gps[j, 1,] + c_x1_0[i, 6] * gps[j, 2,])
        )
      )
    )
  )
)

# Step 5: Use the adjusted outcomes to estimate the causal effect
bart_mod_sa <- BART::wbart(x.train = x1w_data, y.train = ycf, ndpost= 1000)
predict_1_ate_sa <- BART::pwbart(cbind(x1, w = 1), bart_mod_sa$treedraws)
predict_2_ate_sa <- BART::pwbart(cbind(x1, w = 2), bart_mod_sa$treedraws)
predict_3_ate_sa <- BART::pwbart(cbind(x1, w = 3), bart_mod_sa$treedraws)
RD_ate_12_sa <- rowMeans(predict_1_ate_sa - predict_2_ate_sa)
RD_ate_23_sa <- rowMeans(predict_2_ate_sa - predict_3_ate_sa)
RD_ate_13_sa <- rowMeans(predict_1_ate_sa - predict_3_ate_sa)
predict_1_att_sa <- BART::pwbart(cbind(x1[w==1,], w = 1), bart_mod_sa$treedraws)
predict_2_att_sa <- BART::pwbart(cbind(x1[w==1,], w = 2), bart_mod_sa$treedraws)
RD_att_12_sa <- rowMeans(predict_1_att_sa - predict_2_att_sa) # w=1 is the reference

# Demonstrate the sa()function
set.seed(11111)
SA_adjust_result <-
              sa(
                m1 = 50,
                m2 = 1,
                x = x1,
                y = data_SA$y,
                w = data_SA$w,
                ndpost = 100,
                estimand = "ATE",
                prior_c_function =  true_c_fun,
                nCores = 3
              )
summary(SA_adjust_result)
# We compare  the  sensitivity  analysis  results to 1) the results where we had access to x2
set.seed(112)
bart_with_x2_res <- ce_estimate(y = data_SA$y, x = cbind(x1, x2), w = data_SA$w, method = "BART", estimand = "ATE", ndpost = 100)

# 2) the naive estimators where we ignore the unmeasured confounder x2
set.seed(11)
bart_without_x2_res <- ce_estimate(y = data_SA$y, x = x1, w = data_SA$w, method = "BART", estimand = "ATE", ndpost = 100)

# Code to make Figure 4
`%>%` <- purrr::`%>%`
p_12 <- summary(bart_with_x2_res)$ATE12[1,] %>%
  dplyr::bind_rows(summary(bart_without_x2_res)$ATE12[1,]) %>%
  dplyr::bind_rows(summary(SA_adjust_result)["ATE_RD12",]) %>%
  dplyr::mutate(group = c("With x2",
                   "Without x2",
                   "SA")) %>%
  dplyr::mutate(group = factor(group, levels = c("SA", "With x2", "Without x2"))) %>%
  ggplot2::ggplot( ggplot2::aes(y = EST, x = group))+
  ggplot2::geom_errorbar( ggplot2::aes(ymin = LOWER, ymax = UPPER), width = 0.2)+
  ggplot2::geom_point()+
  ggplot2::labs(x = "", y = bquote(ATE["1,2"]))+
  ggplot2::scale_x_discrete(labels = c("SA",expression(italic(x[2])~ "included"),expression(italic(x[2])~ "ignored")))+
  ggplot2::theme_bw()

p_23 <- summary(bart_with_x2_res)$ATE23[1,] %>%
  dplyr::bind_rows(summary(bart_without_x2_res)$ATE23[1,]) %>%
  dplyr::bind_rows(summary(SA_adjust_result)["ATE_RD23",]) %>%
  dplyr::mutate(group = c("With x2",
                   "Without x2",
                   "SA")) %>%
  dplyr::mutate(group = factor(group, levels = c("SA", "With x2", "Without x2"))) %>%
  ggplot2::ggplot(ggplot2::aes(y = EST, x = group))+
  ggplot2::geom_errorbar(ggplot2::aes(ymin = LOWER, ymax = UPPER), width = 0.2)+
  ggplot2::geom_point()+
  ggplot2::labs(x = "", y = bquote(ATE["2,3"]))+
  ggplot2::scale_x_discrete(labels = c("SA",expression(italic(x[2])~ "included"),expression(italic(x[2])~ "ignored")))+
  ggplot2::theme_bw()

p_13 <- summary(bart_with_x2_res)$ATE13[1,] %>%
  dplyr::bind_rows(summary(bart_without_x2_res)$ATE13[1,]) %>%
  dplyr::bind_rows(summary(SA_adjust_result)["ATE_RD13",]) %>%
  dplyr::mutate(group = c("With x2",
                   "Without x2",
                   "SA")) %>%
  dplyr::mutate(group = factor(group, levels = c("SA", "With x2", "Without x2"))) %>%
  ggplot2::ggplot(ggplot2::aes(y = EST, x = group))+
  ggplot2::geom_errorbar(ggplot2::aes(ymin = LOWER, ymax = UPPER), width = 0.2)+
  ggplot2::geom_point()+
  ggplot2::labs(x = "", y = bquote(ATE["1,3"]))+
  ggplot2::scale_x_discrete(labels = c("SA",expression(italic(x[2])~ "included"),expression(italic(x[2])~ "ignored")))+
  ggplot2::theme_bw()
# Figure  4  compares  the  estimates  of ATE1,2, ATE2,3 and ATE1,3 from  the  three  analyses. The sensitivity analysis estimators are similar to the results that could be achieved had the unmeasured confounder x2 been made available.
figure_4 <- cowplot::plot_grid(p_12, p_23, p_13, nrow = 1)
figure_4
# ggplot2::ggsave(figure_4, file = "../plot/p_SA_demo.png", width = 8, height = 3)
# We can also conduct the sensitivity analysis for the ATT effects by setting estimand = "ATT":
set.seed(11111)
sa_att_res <-
  sa(
    m1 = 50,
    m2 = 1,
    x = x1,
    y = data_SA$y,
    w = data_SA$w,
    ndpost = 100,
    estimand = "ATT",
    prior_c_function =  true_c_fun,
    nCores = 1,
    reference_trt = 1
  )

summary(sa_att_res)
# demonstrate the sa()function in a more complex data setting with 4 measured confounders and 2 unmeasured confounders
set.seed(1)
data_SA_2 <- data_sim(
  sample_size = 100,
  n_trt = 3,
  x = c(
    "rnorm(0, 0.5)",# x1
    "rbeta(2, .4)",   # x2
    "runif(0, 0.5)",# x3
    "rweibull(1,2)",  # x4 as one of the unmeasured confounders
    "rbinom(1, .4)"# x5 as one of the unmeasured confounders
  ),
  lp_y = rep(".2*x1 + .3*x2 - .1*x3 - 1.1*x4 - 1.2*x5", 3),
  nlp_y  = rep(".7*x1*x1  - .1*x2*x3", 3), # parallel response surfaces
  align = FALSE,
  lp_w =  c(".4*x1 + .1*x2  - 1.1*x4 + 1.1*x5",    # w = 1
            ".2 * x1 + .2 * x2  - 1.2 * x4 - 1.3 * x5"),  # w = 2,
  nlp_w = c("-.5*x1*x4  - .1*x2*x5", # w = 1
            "-.3*x1*x4 + .2*x2*x5"), # w = 2,
  tau = c(0.5,-0.5,0.5),
  delta = c(0.5,0.5),
  psi = 2
)

# We will use a range of point mass priors forc(1,3,x) and c(3,1,x),and use uniform distributions for the other confounding functions.
c_grid <- c(
  "runif(-0.6, 0)",# c(1,2)
  "runif(0, 0.6)",# c(2,1)
  "runif(-0.6, 0)", # c(2,3)
  "seq(-0.6, 0, by = 0.15)", # c(1,3)
  "seq(0, 0.6, by = 0.15)", # c(3,1)
  "runif(0, 0.6)" # c(3,2)
)
set.seed(1119)
SA_grid_res <-
  sa(
    y = data_SA_2$y,
    w = data_SA_2$w,
    x = data_SA_2$covariates[,-c(4,5)],
    prior_c_function = c_grid,
    m1 = 1,
    nCores = 3,
    estimand = "ATE",
  )
# The sensitivity analysis results can be visualized via a contour plot.
plot(SA_grid_res, ATE = "1,3")
# ggplot2::ggsave(file = "../plot/p_contour.png", width = 4, height = 4)
