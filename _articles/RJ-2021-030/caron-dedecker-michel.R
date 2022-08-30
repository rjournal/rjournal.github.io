#Replication materials for the paper of E. Caron, J. Dedecker and B. Michel (2019),
#Linear Regression with Stationary Errors: the R Package slm.

#Package
library(slm)

#chosen seed
set.seed(42)

#Section "Introduction to linear regression with the slm package"
n = 500
eps = generative_process(n, "Nonmixing")
design = generative_model(n, "mod2")
design_sim = cbind(rep(1,n), as.matrix(design))
beta_vec = c(2,0.001,0.5)
Y = design_sim %*% beta_vec + eps

#Subsection "Linear regression via AR fitting on the residuals"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "fitAR", model_selec = 3)
regslm

regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "fitAR", model_selec = -1)
regslm@model_selec

#Subsection "Linear regression via kernel estimation of the error covariance"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "kernel", 
             model_selec = 5, kernel_fonc = triangle, plot = TRUE)
regslm
#the argument 'plot = TRUE' displays the Figure 1 (Actually, the function displays the ACF and the PACF of the residual process)

#"Order selection via bootstrap"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "kernel",
             model_selec = -1, kernel_fonc = triangle, model_max = 30,
             block_size = 100, block_n = 100, plot = TRUE)
regslm@model_selec
#the argument 'plot = TRUE' displays the Figure 2

#"Order selection by Efromovich’s method (rectangular kernel)"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "efromovich", model_selec = -1)
regslm

#"Order Selection by Andrews’s method"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "hac")
regslm

#Subsection "Linear regression via projection spectral estimation"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "spectralproj",
             model_selec = 10, plot = TRUE)
regslm
#the argument 'plot = TRUE' displays the Figure 3

regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "spectralproj",
             model_selec = -1, model_max = 50, plot = TRUE)
regslm@model_selec

#Subsection "Linear regression via masked covariance estimation"
regslm = slm(Y ~ X1 + X2, data = design, method_cov_st = "select", model_selec = c(1,2,4))
regslm

#Subsection "Linear regression via manual plugged covariance matrix"
v = rep(0,n)
v[1:10] = acf(eps, type = "covariance", lag.max = 9)$acf
regslm = slm(Y ~ X1 + X2, data = design, cov_st = v)
regslm

v = rep(0,n)
v[1:10] = acf(eps, type = "covariance", lag.max = 9)$acf
V = toeplitz(v)
regslm = slm(Y ~ X1 + X2, data = design, Cov_ST = V)
regslm

#Section "Numerical experiments and method comparisons"

#Subsection "Non-Seasonal errors", 

#Estimated levels of the tests, Table 2

#The function 'levels' is necessary to compute the levels of the tests in the tables 2, 3 and 4
#It uses the functions of the slm package:
#generative_process, to simulate an error process
#slm, for the linear regression
#cov_matrix_estimator, to compute the estimator of the asymptotic covariance matrix of the least squares estimator.

#Arguments
#n = samples size
#L = number of simulations
#process = the error process chosen ("iid", AR1", "AR12", "MA12", "Nonmixing" or "sysdyn")
#method = the method chosen ("fitAR", "spectralproj", "efromovich", "kernel" or "hac")
#phi = a numeric vector with AR parameters if the process is "AR1" or "AR12".
#theta = a numeric vector with MA parameters if the process is "MA12".

#In the paper, we have chosen the following values for the simulations:
#n = 200, 1000, 2000 or 5000
#L = 1000
#process = "iid", AR1", "AR12", "MA12", "Nonmixing" or "sysdyn"
#method = "fitAR", "spectralproj", "efromovich", "kernel" or "hac"
#phi = c(0.7) if process = "AR1"; c(0.5,0,0,0,0,0,0,0,0,0,0,0.2) if process = "AR12"
#theta = c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2) if process = "MA12"

#For the kernel method, we have chosen the triangle kernel and 100 blocks of size n/2 for the bootstrap.

#The function returns the mean level of the tests over the L simulations.

#The computation takes a few minutes for L = 1000
#The kernel method is particularly slow, a few minutes for n = 200, up to a few hours for large n.

levels = function(n, L=1000, process="AR1", method="fitAR", phi, theta)
{
  set.seed(42) #the seed chosen in the paper
  I <- rep(0,L) #the test values (0 or 1) for each simulation l
  for(l in seq(1,L)) #for each simulation
  {
    epsilon = generative_process(n, process, phi, theta) #the error process simulated
    X = generative_model(n,"mod2") #simulated design , without the intercept
    X = as.matrix(X)
    Y = 3 + epsilon #model under H0
    
    #regression with slm
    reg = slm(Y ~ X, method_cov_st = method, model_selec = -1, model_max = 50, kernel_fonc = triangle, block_size = n/2, block_n = 100)

    #estimation of the covariance matrix, from the residuals
    Cn = cov_matrix_estimator(reg)
    
    #test H_0: beta_2 = beta_3 = 0
    #compute Cn^{-1/2}
    Cn_p0 = Cn[2:3,2:3]
    Cn_p0_root = expm::sqrtm(Cn_p0)
    Cn_p0_root_inv = solve(Cn_p0_root)
    
    #Dn*hat(beta)
    dn_hat_beta = matrix(c(reg@norm_matrix[2,2]*reg$coefficients[2],reg@norm_matrix[3,3]*reg$coefficients[3]), nrow=2, ncol=1)
    
    #Z = Cn^{-1/2}*Dn*hat(beta)
    Z = Cn_p0_root_inv%*%dn_hat_beta
    
    #statistical test
    test_stat = Z[1]^2 + Z[2]^2

    #test result
    if (test_stat>qchisq(0.95,2)) {
      I[l] = 1
    } else {
      I[l] = 0
    }
  }
  print(process)
  print(mean(I)) #type I error
}

#Estimated levels of the tests, Table 2

#"fitAR" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR1","fitAR",c(0.7))
  levels(n,1000,"Nonmixing","fitAR")
  levels(n,1000,"sysdyn","fitAR")
}

#"spectralproj" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR1","spectralproj",c(0.7))
  levels(n,1000,"Nonmixing","spectralproj")
  levels(n,1000,"sysdyn","spectralproj")
}

#"efromovich" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR1","efromovich",c(0.7))
  levels(n,1000,"Nonmixing","efromovich")
  levels(n,1000,"sysdyn","efromovich")
}

#"kernel" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR1","kernel",c(0.7))
  levels(n,1000,"Nonmixing","kernel")
  levels(n,1000,"sysdyn","kernel")
}

#"hac" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR1","hac",c(0.7))
  levels(n,1000,"Nonmixing","hac")
  levels(n,1000,"sysdyn","hac")
}

#Subsection "Seasonal errors"

#Estimated levels of the tests, Table 3

#"fitAR" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR12","fitAR",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2))
  levels(n,1000,"MA12","fitAR",c(0),c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2))
}

#"spectralproj" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR12","spectralproj",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2))
  levels(n,1000,"MA12","spectralproj",c(0),c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2))
}

#"efromovich" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR12","efromovich",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2))
  levels(n,1000,"MA12","efromovich",c(0),c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2))
}

#"kernel" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR12","kernel",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2))
  levels(n,1000,"MA12","kernel",c(0),c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2))
}

#"hac" method
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  levels(n,1000,"AR12","hac",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2))
  levels(n,1000,"MA12","hac",c(0),c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2))
}

#Subsection "I.I.D. errors"

#Estimated levels of the tests, Table 4

#for the i.i.d. process, n = 150, 300 or 500

#"fitAR" method
for (n in c(150, 300, 500)) {
  print(n)
  levels(n,1000,"iid","fitAR")
}

#"spectralproj" method
for (n in c(150, 300, 500)) {
  print(n)
  levels(n,1000,"iid","spectralproj")
}

#"efromovich" method
for (n in c(150, 300, 500)) {
  print(n)
  levels(n,1000,"iid","efromovich")
}

#"kernel" method
for (n in c(150, 300, 500)) {
  print(n)
  levels(n,1000,"iid","kernel")
}

#"hac" method
for (n in c(150, 300, 500)) {
  print(n)
  levels(n,1000,"iid","hac")
}

#fisher_test function in Table 2, 3 and 4

#The following function computes the levels of the usual Fisher tests, assuming that the error process is i.i.d.
#This allows to find the results of the first column ("Fisher test") of Tables 2, 3 and 4.

#Arguments
#n = samples size
#L = number of simulations
#process = the error process chosen ("iid", AR1", "AR12", "MA12", "Nonmixing" or "sysdyn")
#phi = a numeric vector with AR parameters if the process is "AR1" or "AR12".
#theta = a numeric vector with MA parameters if the process is "MA12".

#For the simulations, we have chosen the following values:
#n = 200, 1000, 2000 or 5000 (n = 150, 300 or 500 for the i.i.d. process)
#L = 1000
#process = "iid", AR1", "AR12", "MA12", "Nonmixing" or "sysdyn"
#phi = c(0.7) if process = "AR1"; c(0.5,0,0,0,0,0,0,0,0,0,0,0.2) if process = "AR12"
#theta = c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2) if process = "MA12"

fisher_test = function(n, L = 1000, process = "AR1", phi, theta)
{
  set.seed(42) #the seed chosen in the paper
  I <- rep(0,L) #the test values (0 or 1) for each simulation l
  for(l in 1:L) #for each simulation
  {
    epsilon = generative_process(n,process,phi,theta) #the error process simulated
    X = generative_model(n,"mod2") #simulated design , without the intercept
    X = as.matrix(X)
    Y = 3 + epsilon #model under H0
    
    p = dim(X)[2] + 1 #dimension for the full model with the intercept
    p0 = 1 #dimension under H0

    #regression
    reg <- lm(Y ~ X)
    sumreg = summary(reg)

    #Fisher test statistic
    test_stat = sumreg$fstatistic[[1]]

    #test result
    if (test_stat > qf(0.95,p-p0,n-p)) {
      I[l] = 1
    } else {
      I[l] = 0
    }
  }
  print(process)
  print(mean(I)) #type I error
}

#Table 2
for (n in c(200, 1000, 2000, 5000)) {
  print(n)
  fisher_test(n,1000,"AR1",c(0.7))
  fisher_test(n,1000,"Nonmixing")
  fisher_test(n,1000,"sysdyn")
}

#Table 3
for (n in c(200, 1000, 2000, 5000)) {
  print(n) 
  fisher_test(n,1000,"AR12",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2))
  fisher_test(n,1000,"MA12",c(0),c(0,0.5,0.3,0,0,0,0,0,0,0,0,0.2))
}

#Tabel 4
for (n in c(150, 300, 500)) {
  print(n) 
  fisher_test(n,1000,"iid")
}

#Subsection "Automatic calibration of the tests", Figure 4

#the function 'level_curve' is necessary to obtain the Figure 4
level_curve = function(n, L=1000)
{
  set.seed(42) #the seed chosen in the paper
  test_result_tab = matrix(0,nrow=L,ncol=50)
  mean_level = rep(0,50) #the estimated levels
  
  for (l in 1:L) 
  {
    test_result = rep(0,50) #test results (0 or 1) for one simulation l
    
    epsilon = generative_process(n,"AR12",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2)) #the AR12 process
    X = generative_model(n,"mod2") #simulated design, without the intercept
    X = as.matrix(X)
    Y = 3 + epsilon #model under H0
    
    for (p in seq(1,50)) {
      #for each p, we fit an AR(p) process on the residuals
      reg = slm(Y~X, method_cov_st = "fitAR", model_selec = p, model_max = 50)
      Cn = cov_matrix_estimator(reg) #estimation of the covariance matrix, from the residuals
      
      #test H_0: beta_2 = beta_3 = 0
      #compute Cn^{-1/2}
      Cn_p0 = Cn[2:3,2:3]
      Cn_p0_root = expm::sqrtm(Cn_p0)
      Cn_p0_root_inv = solve(Cn_p0_root)
      
      #Dn*hat(beta)
      dn_hat_beta = matrix(c(reg@norm_matrix[2,2]*reg$coefficients[2],reg@norm_matrix[3,3]*reg$coefficients[3]), nrow=2, ncol=1)
      
      #Z = Cn^{-1/2}*Dn*hat(beta)
      Z = Cn_p0_root_inv%*%dn_hat_beta
      
      #statistical test
      test_stat = Z[1]^2 + Z[2]^2
      
      #test result
      if (test_stat>qchisq(0.95,2)) {
        test_result[p] = 1
      } else {
        test_result[p] = 0
      }
    }
    
    test_result_tab[l,] = test_result
  }
  
  for (d in seq(1,50)) {
    mean_level[d] = (1/L)*sum(test_result_tab[,d])
  }
  
  return(mean_level)
}

#To obtain the boxplot in Figure 4
order_select = function(n, L=1000)
{
  set.seed(42)
  order_sel = rep(0,L) #the vector of the selected orders
  
  for(l in seq(1,L)) #for each simulation
  {
    epsilon = generative_process(n,"AR12",c(0.5,0,0,0,0,0,0,0,0,0,0,0.2)) #the AR12 process
    X = generative_model(n,"mod2") #simulated design
    X = as.matrix(X)
    Y = 3 + epsilon #model under H0
    
    #regression with slm and the fitAR method, with automatic selection of the AR order
    reg = slm(Y ~ X, method_cov_st = "fitAR", model_selec = -1, model_max = 50)
    order_sel[l] = reg@model_selec #the selected order
  }
  return(order_sel)
}

#To display the Figure 4
level_curve_AR12 = level_curve(1000,1000)
order_select_AR12 = order_select(1000,1000)

x=seq(1,50)
levels_AR12 = data.frame(x,level_curve_AR12)

xmod=seq(1,1000)
order_AR12 = data.frame(xmod,order_select_AR12)

library(ggplot2)
#To obtain the level curve
g1=ggplot(levels_AR12, aes(x=x, y=level_curve_AR12)) + geom_point() + ylim(c(0.05,0.2)) + labs(title=" ", x="Order", y="Estimated Level") + theme_light()
g1

#To obtain the boxplot
g2=ggplot(order_AR12, aes(x="       ", y=order_select_AR12)) + geom_boxplot(width=0.9) + ylim(c(1,50)) + coord_flip() + labs(title=" ", x=" ", y= "Order") + theme_light()# + transparent_theme
g2

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )

#To obtain the Figure 4
library(gridExtra)
grid.arrange(g1, blankPlot, g2, ncol=2, nrow=2, widths=c(5,0.1), heights=c(4, 1.4))


#Section "Application to the PM2.5 pollution Shanghai Dataset"
#The dataset is available in the slm package
data("shan")
shan_complete = shan

reglm = lm(shan_complete$PM_Xuhui ~ . ,data = shan_complete)
summary.lm(reglm)

shan_lm = shan[,-6]
reglm = lm(shan_lm$PM_Xuhui ~ . ,data = shan_lm)
summary.lm(reglm)

regslm = slm(shan_complete$PM_Xuhui ~ . ,data = shan_complete, method_cov_st = "fitAR", model_selec = -1)
summary(regslm)

shan_slm = shan[,c(-6,-8,-9,-10)]
regslm = slm(shan_slm$PM_Xuhui ~ . , data = shan_slm, method_cov_st = "fitAR", model_selec = -1)
summary(regslm)








