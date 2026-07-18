#Estimation of parameters for Thomas process with complex inhomogeneities in all model components

require(binspp)
require(spatstat)

X <- trees_N4
x_left <- x_left_N4
x_right <- x_right_N4
y_bottom <- y_bottom_N4
y_top <- y_top_N4

z_beta <- list(refor=cov_refor, slope=cov_slope)
z_alpha <- list(tmi=cov_tmi, td=cov_tdensity)
z_omega <- list(slope=cov_slope, reserv=cov_reserv)


W <- owin(c(x_left[1],x_right[1]),c(y_bottom[1],y_top[1]))
if(length(x_left)>=2){
  for(i in 2:length(x_left)){
    W2 <- owin(c(x_left[i],x_right[i]),c(y_bottom[i],y_top[i]))
    W <- union.owin(W,W2)
  }
}

W_dil <- dilation.owin(W,100)

#with 250 000 iterations it runs approximatelly a hour

control <- list(NStep=250000, BurnIn=150000, SamplingFreq=10, Prior_alpha_mean=3,
                Prior_alpha_SD=2, Prior_omega_mean=5.5, Prior_omega_SD=5,
                Prior_alphavec_SD=c(4.25,0.012), Prior_omegavec_SD=c(0.18,0.009))

set.seed(12345)
Output <- estintp(X=X, control=control, x_left=x_left, x_right=x_right,
                  y_bottom=y_bottom, y_top=y_top, W_dil=W_dil,
                  z_beta=z_beta, z_alpha=z_alpha, z_omega=z_omega)

print(Output)

plot(Output)

Y <- simulate(Output)
plot(Y)

#Simulation of Thomas process with complex inhomogeneities in all model components

W <- square(1)
W_dil <- dilation.owin(W,0.1)
cov1 <- as.im(function(x,y){x}, W=W_dil)
cov2 <- as.im(function(x,y){y}, W=W_dil)
cov3 <- as.im(function(x,y){1 - (y - 0.5) ^ 2}, W=W_dil)
Y=rThomasInhom(kappa=10, betavec=c(1), z_beta=list(cov1),
               alpha=log(10), alphavec = c(1), z_alpha=list(cov2),
               omega=log(0.01), omegavec=c(1), z_omega=list(cov3),
               W=W, W_dil=W_dil)
plot(Y)


#Simulation and estimation of Generalised Neyman-Scott point process

kappa <- 10; omega <- .1; lambda <- .5; theta <- 10
X <- rgtp(kappa, omega, lambda, theta, win = owin(c(0, 1), c(0, 1)))
plot(X$X)
plot(X$C)

#Prior for parameter kappa
a_kappa <- 4
b_kappa <- 1
x <- seq(0, 100, length = 100)
hx <- dlnorm(x, a_kappa, b_kappa)
plot(x, hx, type = "l", lty = 1, xlab = "x value",
     ylab = "Density", main = "Prior")
#Prior for parameter omega
a_omega <- -3
b_omega <- 1
x <- seq(0, 1, length = 100)
hx <- dlnorm(x, a_omega, b_omega)
plot(x, hx, type = "l", lty = 1, xlab = "x value",
     ylab = "Density", main = "Prior")
#Prior for parameter lambda
l_lambda <- -1
u_lambda <- 0.99
x <- seq(-1, 1, length = 100)
hx <- dunif(x, l_lambda, u_lambda)
plot(x, hx, type = "l", lty = 1, xlab = "x value",
     ylab = "Density", main = "Prior")
#Prior for parameter theta
a_theta <- 4
b_theta <- 1
x <- seq(0, 100, length = 100)
hx <- dlnorm(x, a_theta, b_theta)
plot(x, hx, type = "l", lty = 1, xlab = "x value",
     ylab = "Density", main = "Prior")

est <- estgtp(X$X,
              skappa = exp(a_kappa + ((b_kappa ^ 2) / 2)) / 100,
              somega = exp(a_omega + ((b_omega ^ 2) / 2)) / 100, dlambda = 0.01,
              stheta = exp(a_theta + ((b_theta ^ 2) / 2)) / 100, smove = 0.1,
              a_kappa = a_kappa, b_kappa = b_kappa,
              a_omega = a_omega, b_omega = b_omega,
              l_lambda = l_lambda, u_lambda = u_lambda,
              a_theta = a_theta, b_theta = b_theta,
              iter = 1000, plot.step = 1000, save.step = 1e9,
              filename = "")
#plotting the results and estimationg of the parameters from refined MCMC chain
discard <- 100
step <- 10
result <- estgtpr(est, discard, step)
result
