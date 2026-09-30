##This R codes run the examples codes in ForLion manuscript by Lin, Huang and Yang
library(ForLion) # version 0.4.0
library(VGAM)   # version 1.1-12
library(psych)   # version 2.4.6.26
library(BB)      # version 2019.10-1
library(ggplot2) # version 4.0.2

#-------------------------Example 4.1 : An MLM example: Emergence of house files-------------------------
## ----------Finding a locally D-optimal approximate design----------
theta <- c(-1.935, -0.02642, 0.0003174, -9.159, 0.06386)
hfunc.temp <- function(x){
  matrix(data = c(1, x, x*x, 0, 0,
                  0, 0, 0, 1, x,
                  0, 0, 0, 0, 0), nrow = 3, ncol = 5, byrow = TRUE)
}

hprime.temp <- function(x){
  list(matrix_1 = matrix(data = c(0, 1, 2*x, 0, 0,
                                  0, 0, 0, 0, 1,
                                  0, 0, 0, 0, 0),
                         nrow = 3, ncol = 5, byrow = TRUE))}

set.seed(123)
forlion_MLM <- ForLion_MLM_Optimal(J = 3, n.factor = c(0),
                                   factor.level = list(c(0, 200)), hfunc = hfunc.temp,
                                   h.prime = hprime.temp, bvec = theta, link = "continuation",
                                   Fi.func = Fi_MLM_func, delta0 = 1e-6, epsilon = 1e-12,
                                   reltol = 1e-8, delta = 0.15, maxit = 1000, random = TRUE,
                                   nram = 3, random.initial = TRUE, nram.initial =3)
print(forlion_MLM)
# Design Output
# ===========================
# Count  X1        Allocation
# ---------------------------
# 1      103.5300  0.3981
# 2        0.0000  0.2027
# 3      149.2116  0.3992
# ===========================
#
# m:
#   [1] 3
#
# det:
#   [1] 54016299
#
# convergence:
#   [1] TRUE
#
# min.diff:
#   [1] 45.6816
#
# x.close:
#   [1] 103.5300 149.2116
#
# itmax:
#   [1] 23


## ----------Obtaining exact designs from the locally D-optimal approximate design----------
forlion_MLM_exact <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                      design_x = forlion_MLM$x.factor, design_p = forlion_MLM$p,
                                      det.design = forlion_MLM$det, p = 5, ForLion = TRUE,
                                      bvec = theta, delta2 = 1, L = 0.1, N = 3500,
                                      hfunc = hfunc.temp, link = "continuation")
print(forlion_MLM_exact)

# Design Output
# ===========================
# Count  X1        Allocation
# ---------------------------
# 1      103.5000  0.3981
# 2        0.0000  0.2027
# 3      149.2000  0.3992
# ===========================
#
# ni.design:
#   [1] 1393  710 1397
# det:
#   [1] 54016013
# rel.efficiency:
#   [1] 0.9999989


## ----------Table 1----------
######## L=0.1
forlion_MLM_exact_L0.1 <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                           design_x = forlion_MLM$x.factor, design_p = forlion_MLM$p,
                                           det.design = forlion_MLM$det,p = 5,ForLion = TRUE,bvec = theta,
                                           delta2 = 1, L = 0.1, N = 3500, hfunc = hfunc.temp, link = "continuation")

# Design Output
# =========================== 
# Count  X1        Allocation
# --------------------------- 
# 1      103.5000  0.3981
# 2        0.0000  0.2027
# 3      149.2000  0.3992
# =========================== 
#   
# ni.design:
#   [1] 1393  710 1397
# 
# det:
#   [1] 54016013
# 
# rel.efficiency:
#   [1] 0.9999989

######## L=1
forlion_MLM_exact_L1 <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                         design_x = forlion_MLM$x.factor,design_p = forlion_MLM$p,
                                         det.design = forlion_MLM$det, p = 5, ForLion = TRUE, bvec = theta,
                                         delta2 = 1, L = 1, N = 3500, hfunc = hfunc.temp, link = "continuation")

# Design Output
# =========================== 
# Count  X1        Allocation
# --------------------------- 
# 1      104.0000  0.3981
# 2        0.0000  0.2027
# 3      149.0000  0.3992
# =========================== 
#  
# ni.design:
#   [1] 1393  710 1397
# 
# det:
#   [1] 53974400
# 
# rel.efficiency:
#   [1] 0.9998448

######## L=5
forlion_MLM_exact_L5 <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                         design_x = forlion_MLM$x.factor, design_p = forlion_MLM$p,
                                         det.design = forlion_MLM$det, p = 5, ForLion = TRUE, bvec = theta,
                                         delta2 = 1, L = 5, N = 3500, hfunc = hfunc.temp, link = "continuation")

# Design Output
# =========================== 
# Count  X1        Allocation
# --------------------------- 
# 1      105.0000  0.3981
# 2        0.0000  0.2027
# 3      150.0000  0.3992
# =========================== 
#   
# ni.design:
#   [1] 1393  710 1397
# 
# det:
#   [1] 53838917
# 
# rel.efficiency:
#   [1] 0.9993424

######## L=10
forlion_MLM_exact_L10 <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                          design_x = forlion_MLM$x.factor, design_p = forlion_MLM$p,
                                          det.design = forlion_MLM$det, p = 5, ForLion = TRUE, bvec = theta,
                                          delta2 = 1, L = 10, N = 3500, hfunc = hfunc.temp, link = "continuation")
# Design Output
# =========================== 
# Count  X1        Allocation
# --------------------------- 
# 1      100.0000  0.3981
# 2        0.0000  0.2027
# 3      150.0000  0.3992
# =========================== 
#   
# ni.design:
#   [1] 1393  710 1397
# 
# det:
#   [1] 52650270
# 
# rel.efficiency:
#   [1] 0.9948902

######## L=20
forlion_MLM_exact_L20 <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                          design_x = forlion_MLM$x.factor, design_p = forlion_MLM$p,
                                          det.design = forlion_MLM$det, p = 5, ForLion = TRUE, bvec = theta,
                                          delta2 = 1, L = 20, N = 3500, hfunc = hfunc.temp, link = "continuation")

# Design Output
# =========================== 
# Count  X1        Allocation
# --------------------------- 
# 1      100.0000  0.3981
# 2        0.0000  0.2027
# 3      140.0000  0.3992
# =========================== 
#   
# ni.design:
#   [1] 1393  710 1397
# 
# det:
#   [1] 41048195
# 
# rel.efficiency:
#   [1] 0.9465724



## ----------Finding a sample-based EW D-optimal approximate design----------

## simulate multinomial counts using the observed proportions as probabilities
n  <- 1000   # number of simulated datasets
Ni <- 500    # multinomial sample size at each design point
set.seed(2024)
## 7 design points with covariates (x1,x2), where x2=x1^2
x1_vec <- seq(80, 200, by = 20)
x2_vec <- x1_vec^2
## Multinomial probabilities at each design point (rows sum to 1)
prob_mat <- rbind(c( 62,  5, 433),
                  c( 94, 24, 382),
                  c(179, 60, 261),
                  c(335, 80,  85),
                  c(432, 46,  22),
                  c(487, 11,   2),
                  c(498,  2,   0)) / Ni
## Step 1: generate n simulated datasets with the specified probabilities;
## each dataset has 7 rows (one per design point) and 3 categories multinomial count
## sim_data[ , , k] is the k-th simulated dataset (7 x 5 matrix)
## columns: x1, x2, y1, y2, y3
sim_data <- array(NA, dim = c(7, 5, n),
                  dimnames = list(NULL, c("x1", "x2", "y1", "y2", "y3"), NULL))
for (i in 1:7) {
  Y_mat <- t(rmultinom(n, size = Ni, prob = prob_mat[i, ]))    # n x 3
  Allsimdata_i <- cbind(x1 = x1_vec[i], x2 = x2_vec[i], Y_mat) # n x 5
  for(k in 1:n){
    sim_data[i , ,k] <- Allsimdata_i[k, ]
  }
}
## Step 2: fit models for each simulated dataset and store selected coefficients
theta_matrix <- matrix(0, nrow = n, ncol = 5)
for (k in 1:n) {
  data_k <- as.data.frame(sim_data[, , k])
  ## continuation-ratio model (VGAM: vglm, family = sratio)
  ## fit1: predictors x1 + x2; fit2: predictor x1 only
  fit1 <- vglm(cbind(y1, y2, y3) ~ x1 + x2, family = sratio, data = data_k)
  fit2 <- vglm(cbind(y1, y2, y3) ~ x1,      family = sratio, data = data_k)
  theta1 <- coef(fit1)
  theta2 <- coef(fit2)
  ## store selected coefficients
  ## The indices (1,3,5) and (2,4) follow coefficient ordering for family=sratio
  theta_matrix[k, ] <- c(theta1[c(1, 3, 5)], theta2[c(2, 4)])
}

##EW ForLion
ttemp=proc.time()
set.seed(123)
ew_forlion_MLM = EW_ForLion_MLM_Optimal(J = 3 ,n.factor = c(0),
                                        factor.level = list(c(0, 200)), hfunc = hfunc.temp,
                                        h.prime = hprime.temp, bvec_matrix = theta_matrix,
                                        link = "continuation", EW_Fi.func = EW_Fi_MLM_func,
                                        delta0 = 1e-6, epsilon = 1e-12, reltol = 1e-8, delta = 0.15,
                                        maxit = 1000, random = TRUE, nram = 1, random.initial = TRUE,
                                        nram.initial = 3)
proc.time()-ttemp

#> proc.time()-ttemp
#user  system elapsed 
#334.43    3.84  368.85
#> print(ew_forlion_MLM)
#Design Output
#===========================
#Count  X1        Allocation
#---------------------------
#1        0.0000  0.2029
#2      103.5039  0.3543
#3      103.2826  0.0436
#4      149.1144  0.3991
#===========================
#
# m:
#   [1] 4
#
# det:
#   [1] 58719194
#
# convergence:
#   [1] TRUE
#
# min.diff:
#   [1] 0.2213
#
# x.close:
#   [1] 103.5039 103.2826
#
# itmax:
#   [1] 20


## ----------Obtaining exact designs from the sample-based EW D-optimal approximate design----------
ew_forlion_MLM_exact <- MLM_Exact_Design(J = 3, k.continuous = 1,
                                         design_x = ew_forlion_MLM$x.factor,
                                         design_p = ew_forlion_MLM$p,
                                         det.design = ew_forlion_MLM$det, p = 5, ForLion = FALSE,
                                         bvec_matrix = theta_matrix, delta2 = 1, L = 0.1,
                                         N = 3500, hfunc = hfunc.temp, link = "continuation")
print(ew_forlion_MLM_exact)
#Design Output
#===========================
#Count  X1        Allocation
#---------------------------
#1        0.0000  0.2029
#2      149.1000  0.3991
#3      103.5000  0.3980
#===========================
#
# ni.design:
#   [1]  710 1397 1393
#
# det:
#   [1] 58718854
#
# rel.efficiency:
#   [1] 0.9999988


#-------------------------Example 4.2 A GLM example: Electrostatic Discharge Experiment in Lukemire et al. (2018)-------------------------
## ----------Finding a locally D-optimal approximate design----------
## x5--Voltage in [25,45], x1--LotA in {-1,1}, x2--LotB in {-1,1}, x3--ESD in {-1,1}, x4--Pulse in {-1,1}
## Note: Always put continuous factors ahead of discrete factors
## After reordering the components in x: x=(x5, x1, x2, x3, x4)
## x -> h(x)=(x5, x1, x2, x3, x4, x3*x4, 1)
hfunc.temp <- function(x) {c(x, x[4]*x[5], 1);};
beta.value <- c(0.35, 1.50, -0.2, -0.15, 0.25, 0.4, -7.5)
variable_names <- c("Vol.", "LotA", "LotB", "ESD", "Pul.")
###Using self defined function for the dh(x)/d(x)
hprime.temp <- function(x){
  matrix_1 = matrix(data = c(1, 0, 0, 0, 0, 0, 0),
                    nrow = 7, ncol = 1, byrow = TRUE)
}

forlion_start=proc.time()
set.seed(482)
forlion_GLM <- ForLion_GLM_Optimal(n.factor = c(0, 2, 2, 2, 2),
                                   factor.level =list(c(25, 45), c(-1, 1), c(-1, 1), c(-1, 1),
                                                      c(-1, 1)), var_names = variable_names, hfunc = hfunc.temp,
                                   h.prime = hprime.temp, bvec = beta.value, link = "logit",
                                   delta0 = 1e-5, epsilon = 1e-12, reltol = 1e-7, random = TRUE,
                                   nram = 1, random.initial = TRUE, nram.initial = 1, delta  = 0.01,
                                   maxit = 1000, logscale = TRUE)
forlion_end = proc.time()-forlion_start
print(forlion_GLM)

# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.1165
# 2      27.5443  -1.0000  -1.0000  -1.0000  -1.0000  0.0156
# 3      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0895
# 4      32.7748  -1.0000   1.0000   1.0000  -1.0000  0.1313
# 5      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0854
# 6      25.0000   1.0000   1.0000   1.0000  -1.0000  0.1331
# 7      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0922
# 8      25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0136
# 9      25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0341
# 10     29.0549  -1.0000   1.0000  -1.0000  -1.0000  0.0042
# 11     25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0367
# 12     25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0748
# 13     28.6912  -1.0000  -1.0000  -1.0000   1.0000  0.0722
# 14     25.0000  -1.0000   1.0000  -1.0000   1.0000  0.1008
# ==============================================================
#
# m:
#   [1] 14
#
# det:
#   [1] 1.268957e-05
#
# convergence:
#   [1] TRUE
#
# min.diff:
#   [1] 2
#
# x.close:
#   [,1] [,2] [,3] [,4] [,5]
# [1,]   25   -1   -1    1   -1
# [2,]   25   -1   -1    1    1
#
# itmax:
#   [1] 298

## ----------Obtaining exact designs based on the locally D-optimal approximate design----------
forlion_GLM_exact <- GLM_Exact_Design(k.continuous = 1,
                                      design_x = forlion_GLM$x.factor, design_p = forlion_GLM$p,
                                      var_names = variable_names, det.design = forlion_GLM$det,
                                      p = 7, ForLion = TRUE, bvec = beta.value, delta2 = 0.5,
                                      L = 0.1, N = 500, hfunc = hfunc.temp, link = "logit")

print(forlion_GLM_exact)
# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.1165
# 2      27.5000  -1.0000  -1.0000  -1.0000  -1.0000  0.0156
# 3      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0895
# 4      32.8000  -1.0000   1.0000   1.0000  -1.0000  0.1313
# 5      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0854
# 6      25.0000   1.0000   1.0000   1.0000  -1.0000  0.1331
# 7      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0922
# 8      25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0136
# 9      25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0341
# 10     29.1000  -1.0000   1.0000  -1.0000  -1.0000  0.0042
# 11     25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0367
# 12     25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0748
# 13     28.7000  -1.0000  -1.0000  -1.0000   1.0000  0.0722
# 14     25.0000  -1.0000   1.0000  -1.0000   1.0000  0.1008
# ==============================================================
#
# ni.design:
#   [1] 58  8 45 66 43 67 46  7 17  2 18 37 36 50
#
# det:
#   [1] 1.268788e-05
#
# rel.efficiency:
#   [1] 0.999981


## ----------Table 5----------
ForLion_exactL0.5_ESD <- GLM_Exact_Design(k.continuous = 1,
                                          design_x = forlion_GLM$x.factor, design_p = forlion_GLM$p,
                                          var_names = variable_names, det.design = forlion_GLM$det, p = 7,
                                          ForLion = TRUE, bvec = beta.value, delta2 = 0.5, L = 0.5, N = 100,
                                          hfunc = hfunc.temp, link = "logit")

ForLion_exactL0.5_ESD500 <- GLM_Exact_Design(k.continuous = 1,
                                             design_x = forlion_GLM$x.factor, design_p = forlion_GLM$p,
                                             var_names = variable_names, det.design = forlion_GLM$det, p = 7,
                                             ForLion = TRUE, bvec = beta.value, delta2 = 0.5, L = 0.5, N = 500,
                                             hfunc = hfunc.temp, link = "logit")


# > ForLion_exactL0.5_ESD
# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.1165
# 2      27.5000  -1.0000  -1.0000  -1.0000  -1.0000  0.0156
# 3      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0895
# 4      33.0000  -1.0000   1.0000   1.0000  -1.0000  0.1313
# 5      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0854
# 6      25.0000   1.0000   1.0000   1.0000  -1.0000  0.1331
# 7      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0922
# 8      25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0136
# 9      25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0341
# 10     29.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0042
# 11     25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0367
# 12     25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0748
# 13     28.5000  -1.0000  -1.0000  -1.0000   1.0000  0.0722
# 14     25.0000  -1.0000   1.0000  -1.0000   1.0000  0.1008
# ==============================================================
#
# ni.design:
#   [1] 12  2  9 13  9 13  9  1  3  0  4  8  7 10
#
# det:
#   [1] 1.262429e-05
#
# rel.efficiency:
#   [1] 0.9992635
# > ForLion_exactL0.5_ESD500
# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.1165
# 2      27.5000  -1.0000  -1.0000  -1.0000  -1.0000  0.0156
# 3      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0895
# 4      33.0000  -1.0000   1.0000   1.0000  -1.0000  0.1313
# 5      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0854
# 6      25.0000   1.0000   1.0000   1.0000  -1.0000  0.1331
# 7      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0922
# 8      25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0136
# 9      25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0341
# 10     29.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0042
# 11     25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0367
# 12     25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0748
# 13     28.5000  -1.0000  -1.0000  -1.0000   1.0000  0.0722
# 14     25.0000  -1.0000   1.0000  -1.0000   1.0000  0.1008
# ==============================================================
#
# ni.design:
#   [1] 58  8 45 66 43 67 46  7 17  2 18 37 36 50
#
# det:
#   [1] 1.267204e-05
#
# rel.efficiency:
#   [1] 0.9998025


## ----------Finding integral-based EW D-optimal approximate design----------
paras_lowerbound <- c(0.25, 1, -0.3, -0.3, 0.1, 0.35, -8.0)
paras_upperbound <- c(0.45, 2, -0.1,  0.0, 0.4, 0.45, -7.0)

##the prior distributions are follow uniform distribution
gjoint_b <- function(x) {
  Func_b = 1/(prod(paras_upperbound-paras_lowerbound))
  ##the prior distributions are follow uniform distribution
  return(Func_b)
}

ew_forlion_start = proc.time()
set.seed(482)
ew_forlion_GLM <- EW_ForLion_GLM_Optimal(n.factor = c(0, 2, 2, 2, 2),
                                         factor.level = list(c(25,45),c(-1,1),c(-1,1),c(-1,1),c(-1,1)),
                                         var_names = variable_names, hfunc = hfunc.temp, h.prime = hprime.temp,
                                         Integral_based = TRUE, joint_Func_b = gjoint_b,
                                         Lowerbounds = paras_lowerbound, Upperbounds = paras_upperbound,
                                         link = "logit", delta0 = 1e-5, epsilon = 1e-12, reltol = 1e-5, delta = 0.01,
                                         maxit = 500, random = TRUE, nram = 1,
                                         logscale = TRUE)
ew_forlion_end = proc.time()-ew_forlion_start
print(ew_forlion_GLM)

# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0875
# 2      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0845
# 3      25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0848
# 4      25.0000   1.0000   1.0000  -1.0000   1.0000  0.0621
# 5      38.9047  -1.0000   1.0000   1.0000  -1.0000  0.0214
# 6      25.0000   1.0000   1.0000  -1.0000  -1.0000  0.0356
# 7      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0856
# 8      25.0000  -1.0000   1.0000  -1.0000   1.0000  0.0515
# 9      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0690
# 10     33.1161  -1.0000   1.0000   1.0000   1.0000  0.0022
# 11     35.4140  -1.0000   1.0000  -1.0000   1.0000  0.0028
# 12     25.0000   1.0000   1.0000   1.0000  -1.0000  0.0443
# 13     25.0000   1.0000   1.0000   1.0000   1.0000  0.0090
# 14     35.3993  -1.0000   1.0000  -1.0000   1.0000  0.0352
# 15     25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0901
# 16     25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0743
# 17     34.0238  -1.0000   1.0000  -1.0000  -1.0000  0.0157
# 18     37.1975  -1.0000  -1.0000   1.0000  -1.0000  0.0455
# 19     25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.0410
# 20     38.9522  -1.0000   1.0000   1.0000  -1.0000  0.0580
# ==============================================================
#
# m:
#   [1] 20
#
# det:
#   [1] 4.552703e-06
#
# convergence:
#   [1] TRUE
#
# min.diff:
#   [1] 0.0147
#
# x.close:
#         [,1] [,2] [,3] [,4] [,5]
# [1,] 35.4140   -1    1   -1    1
# [2,] 35.3993   -1    1   -1    1

# itmax:
#   [1] 56
# user  system elapsed 
# 2606.68  112.49 2864.29 

## ----------Obtaining exact designs based on the integral-based EW D-optimal approximate design----------
ew_forlion_exact <- GLM_Exact_Design(k.continuous = 1,
                                     design_x = ew_forlion_GLM$x.factor,
                                     design_p = ew_forlion_GLM$p, var_names = variable_names,
                                     det.design = ew_forlion_GLM$det, p = 7, ForLion = FALSE,
                                     Integral_based = TRUE, joint_Func_b = gjoint_b,
                                     Lowerbounds = paras_lowerbound,
                                     Upperbounds = paras_upperbound, delta2 = 0.5, L = 0.1,
                                     N = 500, hfunc = hfunc.temp, link = "logit")
print(ew_forlion_exact)

# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0875
# 2      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0845
# 3      25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0848
# 4      25.0000   1.0000   1.0000  -1.0000   1.0000  0.0621
# 5      25.0000   1.0000   1.0000  -1.0000  -1.0000  0.0356
# 6      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0856
# 7      25.0000  -1.0000   1.0000  -1.0000   1.0000  0.0515
# 8      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0690
# 9      33.1000  -1.0000   1.0000   1.0000   1.0000  0.0022
# 10     25.0000   1.0000   1.0000   1.0000  -1.0000  0.0443
# 11     25.0000   1.0000   1.0000   1.0000   1.0000  0.0090
# 12     25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0901
# 13     25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0743
# 14     34.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0157
# 15     37.2000  -1.0000  -1.0000   1.0000  -1.0000  0.0455
# 16     25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.0410
# 17     35.4000  -1.0000   1.0000  -1.0000   1.0000  0.0380
# 18     38.9000  -1.0000   1.0000   1.0000  -1.0000  0.0794
# ==============================================================
#
# ni.design:
#   [1] 44 42 42 31 18 43 26 34  1 22  4 45 37  8 23 21 19 40
#
# det:
#   [1] 4.551996e-06
#
# rel.efficiency:
#   [1] 0.9999778


## ----------Finding a sample-based EW D-optimal approximate design----------
#### Generate Parameters ####
nrun <- 1000
set.seed(0713)
b_0 <- runif(nrun, -8, -7)
b_1 <- runif(nrun, 1, 2)
b_2 <- runif(nrun, -0.3, -0.1)
b_3 <- runif(nrun, -0.3, 0)
b_4 <- runif(nrun, 0.1, 0.4)
b_5 <- runif(nrun, 0.25, 0.45)
b_34 <- runif(nrun, 0.35, 0.45)

beta.matrix <- cbind(b_5,b_1,b_2,b_3,b_4,b_34,b_0)


sample_EW_forlion_start=proc.time()
set.seed(482)
sample_ew_forlion_GLM <- EW_ForLion_GLM_Optimal(n.factor = c(0, 2, 2, 2, 2),
                                                factor.level = list(c(25, 45), c(-1, 1), c(-1, 1),
                                                                    c(-1, 1), c(-1, 1)), var_names = variable_names,
                                                hfunc = hfunc.temp, h.prime = hprime.temp, Integral_based = FALSE,
                                                b_matrix = beta.matrix, link = "logit", delta0 = 1e-5,
                                                epsilon = 1e-12, reltol = 1e-6, delta = 0.01,
                                                maxit = 500, random = TRUE, nram = 1, logscale = TRUE)
sample_EW_forlion_end=proc.time()-sample_EW_forlion_start
print(sample_ew_forlion_GLM)

# Design Output
# ==============================================================
# Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
# --------------------------------------------------------------
# 1      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0851
# 2      25.0000  -1.0000   1.0000  -1.0000   1.0000  0.0723
# 3      33.5304  -1.0000   1.0000  -1.0000  -1.0000  0.0095
# 4      25.0000  -1.0000  -1.0000   1.0000  -1.0000  0.0640
# 5      25.0000   1.0000   1.0000   1.0000  -1.0000  0.0499
# 6      25.0000   1.0000   1.0000  -1.0000  -1.0000  0.0310
# 7      25.0000  -1.0000   1.0000   1.0000   1.0000  0.0882
# 8      25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0743
# 9      38.4919  -1.0000   1.0000   1.0000  -1.0000  0.1171
# 10     33.2875  -1.0000  -1.0000  -1.0000   1.0000  0.0403
# 11     25.0000   1.0000  -1.0000   1.0000  -1.0000  0.0702
# 12     25.0000  -1.0000  -1.0000  -1.0000  -1.0000  0.0843
# 13     25.0000  -1.0000   1.0000   1.0000  -1.0000  0.0738
# 14     25.0000   1.0000   1.0000  -1.0000   1.0000  0.0612
# 15     25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0660
# 16     36.7975  -1.0000  -1.0000   1.0000  -1.0000  0.0084
# 17     25.0000   1.0000   1.0000   1.0000   1.0000  0.0037
# 18     33.5593  -1.0000   1.0000  -1.0000  -1.0000  0.0008
# ==============================================================
#
# m:
#   [1] 18
#
# det:
#   [1] 4.229431e-06
#
# convergence:
#   [1] TRUE
#
# min.diff:
#   [1] 0.0289
#
# x.close:
#         [,1] [,2] [,3] [,4] [,5]
# [1,] 33.5304   -1    1   -1   -1
# [2,] 33.5593   -1    1   -1   -1
#
# itmax:
#   [1] 96


## ----------Comparing sample-based and integral-based EW D-optimal designs----------
EW_Xw_maineffects_int <- function(x,joint_Func_b,Lowerbounds, Upperbounds,link="logit", h.func=NULL) {
  if(is.null(h.func)) h.func = function(y) {c(1,y);}; # default: main-effects
  xrow = h.func(x);
  integrand_w<-function(b){
    eta = sum(b*xrow);
    w = NULL ;
    if(link=="probit") w = nu_probit_self(eta);
    if(link=="cloglog") w = nu_loglog_self(eta);
    if(link=="loglog") w = nu_loglog_self(eta);
    if(link=="cauchit") w = nu_cauchit_self(eta);
    if(link=="log") w = nu_log_self(eta);
    if(is.null(w)) { link="logit"; w=nu_logit_self(eta);};
    return(w * joint_Func_b(b))
  }
  result <- cubature::hcubature(f = integrand_w,lowerLimit = Lowerbounds,upperLimit = Upperbounds, tol = 1e-4,maxEval = 1e4)
  Ew<-result$integral
  
  list(X=xrow, E_w=Ew, link=link);
}

#x.design: the obtained design
#p.design: the corresponding weight
Integral_Fdet<-function(x.design,p.design,joint_Func_b,Lowerbounds, Upperbounds,link="logit", h.func=NULL){
  if(is.null(nrow(x.design))){m.design=length(x.design)} else {m.design=nrow(x.design);}# initial number of design points
  p.factor=length(Lowerbounds)
  X.mat = matrix(0, m.design, p.factor);  # initial model matrix X
  E_w.vec = rep(0, m.design);     # E_w vector
  for(i in 1:m.design) {
    if(is.null(nrow(x.design))) htemp=EW_Xw_maineffects_int(x=x.design[i],joint_Func_b=joint_Func_b,Lowerbounds=Lowerbounds, Upperbounds=Upperbounds,link=link, h.func=h.func) else {
      htemp=EW_Xw_maineffects_int(x=x.design[i,],joint_Func_b=joint_Func_b, Lowerbounds=Lowerbounds, Upperbounds=Upperbounds, link=link, h.func=h.func);
    };
    X.mat[i,]=htemp$X;
    E_w.vec[i]=htemp$E_w;
  };
  f.det = det(t(X.mat * (p.design*E_w.vec)) %*% X.mat)
  
  list(det=f.det)
}


# Sample 3 random integers between 1 and 10000
set.seed(2025)
seeds <- sample(1:10000, 3)
#932 1985  279

nsize<-c(100,1000)
difseeds<-c(932, 1985, 279)
rel.matrix = matrix(0, length(nsize), length(difseeds))
n.design=matrix(0, length(nsize), length(difseeds))
for(b in 1:length(nsize)){
  for(j in 1:length(difseeds)){
    runn = nsize[b]
    set.seed(difseeds[j])
    b_0 = runif(runn, -8, -7)
    b_1 = runif(runn, 1, 2)
    b_2 = runif(runn, -0.3, -0.1)
    b_3 = runif(runn, -0.3, 0)
    b_4 = runif(runn, 0.1, 0.4)
    b_5 = runif(runn, 0.25, 0.45)
    b_34= runif(runn, 0.35, 0.45)
    
    beta.matrix = cbind(b_5,b_1,b_2,b_3,b_4,b_34,b_0)
    
    set.seed(difseeds[j])
    sample_EW_ForLion_simu=EW_ForLion_GLM_Optimal(n.factor=c(0, 2, 2, 2, 2), factor.level=
                                                    list(c(25,45),c(-1,1),c(-1,1),c(-1,1),c(-1,1)),
                                                  hfunc=hfunc.temp,Integral_based=FALSE,b_matrix=beta.matrix, link="logit",
                                                  delta0 = 1e-5,
                                                  epsilon = 1e-12, reltol = 1e-6, delta = 0.01,
                                                  maxit = 500, random = TRUE, nram = 1, logscale = TRUE)
    
    
    fdets<-Integral_Fdet(x.design=as.matrix(sample_EW_ForLion_simu$x.factor),p.design=as.vector(sample_EW_ForLion_simu$p),
                         joint_Func_b=gjoint_b, Lowerbounds=paras_lowerbound,
                         Upperbounds=paras_upperbound,link="logit", h.func=hfunc.temp)
    rel.matrix[b,j]<-(fdets$det/ew_forlion_GLM$det)^(1/7)
    n.design[b,j]<-sample_EW_ForLion_simu$m
  }
}


# > rel.matrix
# [,1]      [,2]      [,3]
# [1,] 0.9982641 0.9975130 0.9989559
# [2,] 0.9997052 0.9997704 0.9994012
# n.design
# [,1] [,2] [,3]
# [1,]   17   21   20
# [2,]   20   19   18



#-------------------------Supplementary Material--------------------------------

#-------------------------------Section 2---------------------------------------
# The locally D-optimal design in Example 4.1
# delta = 0.15 and random seed 123
theta <- c(-1.935, -0.02642, 0.0003174, -9.159, 0.06386)
hfunc.temp <- function(x){
  matrix(data = c(1, x, x*x, 0, 0,
                  0, 0, 0, 1, x,
                  0, 0, 0, 0, 0), nrow = 3, ncol = 5, byrow = TRUE)
}

hprime.temp <- function(x){
  list(matrix_1 = matrix(data = c(0, 1, 2*x, 0, 0,
                                  0, 0, 0, 0, 1,
                                  0, 0, 0, 0, 0),
                         nrow = 3, ncol = 5, byrow = TRUE))}

set.seed(123)
forlion_MLM <- ForLion_MLM_Optimal(J = 3, n.factor = c(0),
                                   factor.level = list(c(0, 200)), hfunc = hfunc.temp,
                                   h.prime = hprime.temp, bvec = theta, link = "continuation",
                                   Fi.func = Fi_MLM_func, delta0 = 1e-6, epsilon = 1e-12,
                                   reltol = 1e-8, delta = 0.15, maxit = 1000, random = TRUE,
                                   nram = 3, random.initial = TRUE, nram.initial =3)

# We randomly generate 10 random seeds and find the corresponding designs.
# Sample 10 random integers between 1 and 1000
set.seed(2026)
seeds <- sample(1:1000, 10)
#733 633 993 294 557 623 859 108 164 176

difseeds<-c(733,633,993,294,557,623,859,108,164,176)
design_point2<-NULL
rel_eff2<-NULL
min_distance2<-NULL
convergence<-NULL
start2=proc.time()
for(i in 1:10){
  set.seed(difseeds[i])
  forlion.design345si2 = ForLion_MLM_Optimal(J=3, n.factor=c(0),
                                             factor.level=list(c(0, 200)), hfunc=hfunc.temp, h.prime=hprime.temp,
                                             bvec=theta, link="continuation", Fi.func=Fi_MLM_func, delta0=1e-6,
                                             epsilon=1e-12, reltol=1e-8, delta=0.15, maxit=1000, random=TRUE,
                                             nram=3, rowmax=NULL, Xini=NULL, random.initial=TRUE, nram.initial=3)
  design_point2[i]<-forlion.design345si2$m
  rel_eff2[i]<-(forlion.design345si2$det/forlion_MLM$det)^(1/5)
  min_distance2[i]<-forlion.design345si2$min.diff
  convergence[i]<-forlion.design345si2$convergence
}
end2=proc.time()-start2
##  user  system elapsed
##1034.86   28.08 1129.40

#> design_point2
#[1] 4 4 3 3 3 4 3 3 3 4
#> rel_eff2
#[1] 0.9999989 0.9999963 1.0000002 0.9999998 1.0000010 0.9999994 1.0000011 0.9999981 1.0000011
#[10] 0.9999980
#> min(rel_eff2)
#[1] 0.9999963
#> max(rel_eff2)
#[1] 1.000001


## ----------Figure S1---------- 
df2 <- data.frame(
  Random_seeds=c(1,2,3,4,5,6,7,8,9,10),
  num_points = design_point2,
  rel_efficiency =rel_eff2*100,
  min_distance = round(min_distance2,2)
)

tiff("Rpaper_examp4_1_b1.tiff", units="in", width=5, height=2.8,res=600)
ggplot(df2, aes(x = Random_seeds, y = num_points)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black", size = 2) +
  scale_x_continuous(breaks = df2$Random_seeds, labels = format(df2$Random_seeds, trim = TRUE)) +
  labs(x = "Random seeds", y = "Number of Design Points") +
  theme_bw()
dev.off() #save figure as tiff resolution 600

tiff("Rpaper_examp4_1_b2.tiff", units="in", width=5, height=2.8,res=600)
ggplot(df2, aes(x = Random_seeds)) +
  # bar：min_distance
  geom_col(aes(y = min_distance, fill = "Minimum distance"),
           alpha = 0.6, width = 0.6) +
  geom_text(aes(y = min_distance, label = format(min_distance, trim = TRUE)),
            vjust = -0.4, size = 3) +
  # line：rel_efficiency
  geom_line(aes(y = rel_efficiency, linetype = "Relative efficiency"),
            linewidth = 1, color = "black") +
  geom_point(aes(y = rel_efficiency),
             color = "black", size = 2) +
  scale_x_continuous(breaks = df2$Random_seeds, labels = format(df2$Random_seeds, trim = TRUE)) +
  scale_y_continuous(
    name = "Relative Efficiency (%)",
    limits = c(0, 105),
    sec.axis = sec_axis(~ ., name = "Minimum Distance")
  ) +
  scale_fill_manual(values = c("Minimum distance" = "grey50")) +
  scale_linetype_manual(values = c("Relative efficiency" = "dotted")) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  labs(x = "Random seeds") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = -8, l = 0),
    plot.margin = margin(t = 2, r = 6, b = 4, l = 6)
  )
dev.off() #save figure as tiff resolution 600


#-------------------------------Section 3---------------------------------------
## Note: Always put continuous factors ahead of discrete factors
## After reordering the components in x: x = (x5, x1, x2, x3, x4)
## Here, x5--Voltage in [25,45], x1--LotA in {-1,1}, x2--LotB in {-1,1}, 
## x3--ESD in {-1,1}, x4--Pulse in {-1, 0, 1}
## Suppose x4 (x[5]) is coded as two dummy variables: Pul1, Pul2
## x -> h(x) = (x5, x1, x2, x3, Pul1, Pul2, Pul1*x5, Pul2*x5, 1) 
hfunc.temp.int <- function(x) {
                     Pul1 <- ifelse(x[5] == -1, 1, 0)
                     Pul2 <- ifelse(x[5] == 0, 1, 0)  
                     c(x[1:4], Pul1, Pul2, x[1]*Pul1, x[1]*Pul2, 1)}
# Adjust beta vector length accordingly 
#(now 9 terms: 4 main + 2 dummies + 2 interactions + intercept)
beta.value.int <- c(0.35, 1.50, -0.2, -0.15, 0.25, 0.40, 0.10, -0.05, -7.5)
variable_names.int <- c("Vol.", "LotA", "LotB", "ESD", "Pul.")
## hprime: derivative wrt x (dh(x)/d(x))
hprime.temp.int <- function(x) {
                   Pul1 <- ifelse(x[5] == -1, 1, 0)
                   Pul2 <- ifelse(x[5] == 0, 1, 0)
                   matrix(
                     data = c(
                       1,   # derivative wrt Vol.
                       0,   # LotA
                       0,   # LotB
                       0,   # ESD
                       0,   # Pul1
                       0,   # Pul2
                       Pul1,# interaction Vol*Pul1 wrt Vol
                       Pul2,# interaction Vol*Pul2 wrt Vol
                       0    # intercept
                       ),
                      nrow = 9, ncol = 1, byrow = TRUE
                   )}


set.seed(482)
forlion_GLM_int <- ForLion_GLM_Optimal(n.factor = c(0, 2, 2, 2, 3),
                   factor.level = list(c(25,45),c(-1,1),c(-1,1),c(-1,1),
                   c(-1,0,1)), var_names = variable_names.int, 
                   hfunc = hfunc.temp.int, h.prime = hprime.temp.int, 
                   bvec = beta.value.int, link = "logit", delta0 = 0.01,
                   epsilon = 1e-8, reltol = 1e-4, random = TRUE, 
                   nram = 1, delta  = 0.08, maxit = 1200, logscale = TRUE)


#Design Output
#============================================================== 
#Count  Vol.     LotA     LotB     ESD      Pul.     Allocation
#-------------------------------------------------------------- 
#1      25.0000   1.0000  -1.0000   1.0000   0.0000  0.0308
#2      25.0000   1.0000   1.0000  -1.0000   0.0000  0.0386
#3      25.0000  -1.0000  -1.0000  -1.0000   1.0000  0.0869
#4      25.0000  -1.0000  -1.0000   1.0000   1.0000  0.0141
#5      25.0000  -1.0000   1.0000  -1.0000   1.0000  0.0134
#6      32.8659  -1.0000  -1.0000   1.0000   0.0000  0.0490
#7      34.3536  -1.0000   1.0000   1.0000   0.0000  0.0480
#8      25.0000   1.0000   1.0000   1.0000   0.0000  0.0635
#9      30.7937  -1.0000  -1.0000   1.0000   1.0000  0.0191
#10     25.0000  -1.0000   1.0000   1.0000   1.0000  0.0870
#11     31.1887  -1.0000   1.0000  -1.0000   1.0000  0.0280
#12     25.0000  -1.0000  -1.0000   1.0000   0.0000  0.0673
#13     33.2984  -1.0000   1.0000  -1.0000   0.0000  0.0527
#14     32.2390  -1.0000   1.0000   1.0000   1.0000  0.0787
#15     25.0000  -1.0000  -1.0000  -1.0000   0.0000  0.0304
#16     25.0000  -1.0000   1.0000  -1.0000  -1.0000  0.0110
#17     29.6334  -1.0000   1.0000   1.0000  -1.0000  0.1111
#18     25.0014  -1.0000   1.0000   1.0000  -1.0000  0.1027
#19     25.0602  -1.0000   1.0000  -1.0000   0.0000  0.0679
#============================================================== 
#m:
#  [1] 19
#det:
#  [1] 6.404087e-10
#convergence:
#  [1] TRUE
#min.diff:
#  [1] 1
#x.close:
#  [,1] [,2] [,3] [,4] [,5]
#  [1,]   25   -1   -1   -1    1
#  [2,]   25   -1   -1   -1    0
#itmax:
#  [1] 1160

