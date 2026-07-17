#########################
# Reproducibility Check #
#########################
install.packages("nimblewomble")
# please setwd("path to submission folder")
require(nimble)
require(nimblewomble)
require(ggplot2)
require(cowplot)
require(MBA)
require(metR)
require(patchwork)
require(coda) # for tracking convergence

set.seed(1)
# Generated Simulated Data
N = 1e2
tau = 1
coords = matrix(runif(2 * N, -10, 10), ncol = 2); colnames(coords) = c("x", "y")
y = rnorm(N, mean = 20 * sin(sqrt(coords[, 1]^2  + coords[, 2]^2)), sd = tau)

# Create equally spaced grid of points
xsplit = ysplit = seq(-10, 10, by = 1)[-c(1, 21)]
grid = as.matrix(expand.grid(xsplit, ysplit), ncol = 2)
colnames(grid) = c("x", "y")

####################################
# Process for True Rates of Change #
####################################
# Gradient along x
true_sx = round(20 * cos(sqrt(grid[,1]^2 + grid[,2]^2)) *
                  grid[,1]/sqrt(grid[,1]^2 + grid[,2]^2), 3)
# Gradient along y
true_sy = round(20 * cos(sqrt(grid[,1]^2 + grid[,2]^2)) *
                  grid[,2]/sqrt(grid[,1]^2 + grid[,2]^2), 3)
# Curvature along x
true_sxx = round(20 * cos(sqrt(grid[,1]^2 + grid[,2]^2))/sqrt(grid[,1]^2 + grid[,2]^2) -
                   20 * cos(sqrt(grid[,1]^2 + grid[,2]^2)) * grid[,1]^2/(grid[,1]^2 + grid[,2]^2)^(3/2) -
                   20 * sin(sqrt(grid[,1]^2 + grid[,2]^2)) * grid[,1]^2/(grid[,1]^2 + grid[,2]^2), 3)
# Mixed Curvature
true_sxy = round(-20 * (cos(sqrt(grid[,1]^2 + grid[,2]^2)) -
                          sin(sqrt(grid[,1]^2 + grid[,2]^2))) * grid[,1] * grid[,2]/(grid[,1]^2 + grid[,2]^2), 3)
# Curvature along y
true_syy = round(20 * cos(sqrt(grid[,1]^2 + grid[,2]^2))/sqrt(grid[,1]^2 + grid[,2]^2) -
                   20 * cos(sqrt(grid[,1]^2 + grid[,2]^2)) * grid[,2]^2/(grid[,1]^2 + grid[,2]^2)^(3/2) -
                   20 * sin(sqrt(grid[,1]^2 + grid[,2]^2)) * grid[,2]^2/(grid[,1]^2 + grid[,2]^2), 3)
# Create the plots
p1 = sp_ggplot(data_frame = data.frame(coords, z = y))
p2 = sp_ggplot(data_frame = data.frame(grid[-which(is.nan(true_sx)),],
                                       z = true_sx[-which(is.nan(true_sx))]))
p3 = sp_ggplot(data_frame = data.frame(grid[-which(is.nan(true_sy)),], z = true_sy[-which(is.nan(true_sy))]))
p4 = sp_ggplot(data_frame = data.frame(grid[-which(is.nan(true_sxx)),], z = true_sxx[-which(is.nan(true_sxx))]))
p5 = sp_ggplot(data_frame = data.frame(grid[-which(is.nan(true_sxy)),], z = true_sxy[-which(is.nan(true_sxy))]))
p6 = sp_ggplot(data_frame = data.frame(grid[-which(is.nan(true_syy)),], z = true_syy[-which(is.nan(true_syy))]))

# Plot (to be compared with plots from line 112)
((p1 + p2 + p3)/(p4 + p5 + p6))

##########################
# Fit a Gaussian Process #
##########################
# Posterior samples for theta
mc_sp = gp_fit(coords = coords, y = y, kernel = "matern2")
par(mfcol=c(1,3))
traceplot(mc_sp$mcmc) # tracking convergence
acf(mc_sp$mcmc)
# Posterior samples for Z(s) and beta
model = zbeta_samples(y = y, coords = coords,
                      model = mc_sp$mcmc,
                      kernel = "matern2")
# Estimates for parameters
estimates = t(round(apply(model, 2, quantile, probs = c(0.5, 0.025, 0.975)), 3))
# Fitted Process
yfit = estimates[paste0("z[", 1:N, "]"), "50%"] + estimates["beta[0]", "50%"]
# 95% Credible Band
ylow = estimates[paste0("z[", 1:N, "]"), "2.5%"] + estimates["beta[0]", "2.5%"]
yhigh = estimates[paste0("z[", 1:N, "]"), "97.5%"] + estimates["beta[0]", "97.5%"]

fit_frame = data.frame(true = round(y, 3),
                       est = yfit, `2.5%` = ylow, `97.5%` = yhigh)
fit_frame$sig = significance(data_frame = data.frame(fit_frame[,-1]))
colnames(fit_frame) = c("true", "est", "2.5%", "97.5%", "sig")
# Plot
p7 = sp_ggplot(data_frame = data.frame(coords, z = yfit, sig = fit_frame$sig))

###################
# Rates of Change #
###################
# this takes some time to complete ~10mins
# gradients = sprates(grid = grid,
#                     coords = coords,
#                     model = model,
#                     kernel = "matern2")
# pre-saved output
load("code/data/gradients.RData")
# Gradient along x-axis
p8 = sp_ggplot(data_frame = data.frame(grid,
                                       z = gradients$estimate.sx[,"50%"],
                                       sig = gradients$estimate.sx$sig))
# Gradient along y-axis
p9 = sp_ggplot(data_frame = data.frame(grid,
                                       z = gradients$estimate.sy[,"50%"],
                                       sig = gradients$estimate.sy$sig))
# Curvature along x-axis
p10 = sp_ggplot(data_frame = data.frame(grid,
                                        z = gradients$estimate.sxx[,"50%"],
                                        sig = gradients$estimate.sxx$sig))
# Mixed Curvature
p11 = sp_ggplot(data_frame = data.frame(grid,
                                        z = gradients$estimate.sxy[,"50%"],
                                        sig = gradients$estimate.sxy$sig))
# Curvature along y-axis
p12 = sp_ggplot(data_frame = data.frame(grid,
                                        z = gradients$estimate.syy[,"50%"],
                                        sig = gradients$estimate.syy$sig))
# Plot
((p7 + p8 + p9)/(p10 + p11 + p12))

################################
# Wombling (Boundary Analysis) #
################################
load("code/curves1.RData")
curve = curves.1[[1]]
tvec = sapply(1:(nrow(curve) - 1), function(x) sqrt(sum((curve[(x + 1),] - curve[x,])^2)))
umat = as.matrix(t(sapply(1:(nrow(curve) - 1), function(x) (curve[(x + 1),] - curve[x,])))/tvec)

# takes about 30--35mins
# wm = spwombling(coords = coords,
#                 curve = curve,
#                 model = model,
#                 kernel = "matern2")
# pre-saved output
load("code/data/wombling.RData")

###############
# True Values #
###############
truth = matrix(0, nrow = nrow(curve) - 1, ncol = 2)
rule = seq(0, 1, by = 0.01)

for(i in 1:(nrow(curve) - 1)){
  u.perp = c(umat[i, 2], - umat[i, 1])
  s0 = curve[i,]
  
  truth.lsegment = sapply(rule * tvec[i], function(x){
    s.t = s0 + x * umat[i,]
    true_sx = 20 * cos(sqrt(s.t[1]^2 + s.t[2]^2)) * s.t[1]/sqrt(s.t[1]^2 + s.t[2]^2)
    true_sy = 20 * cos(sqrt(s.t[1]^2 + s.t[2]^2)) * s.t[2]/sqrt(s.t[1]^2 + s.t[2]^2)
    true_sx * u.perp[1] + true_sy * u.perp[2]
  })
  truth[i, 1] = sum(truth.lsegment * (tvec[i]/101))
  
  truth.lsegment = sapply(rule * tvec[i], function(x){
    s.t = s0 + x * umat[i,]
    true_sxx = 20 * cos(sqrt(s.t[1]^2 + s.t[2]^2))/sqrt(s.t[1]^2 + s.t[2]^2) -
      20 * cos(sqrt(s.t[1]^2 + s.t[2]^2)) * s.t[1]^2/(s.t[1]^2 + s.t[2]^2)^(3/2) -
      20 * sin(sqrt(s.t[1]^2 + s.t[2]^2)) * s.t[1]^2/(s.t[1]^2 + s.t[2]^2)
    true_sxy = -20 * (cos(sqrt(s.t[1]^2 + s.t[2]^2)) - sin(sqrt(s.t[1]^2 + s.t[2]^2))) * s.t[1] * s.t[2]/(s.t[1]^2 + s.t[2]^2)
    true_syy = 20 * cos(sqrt(s.t[1]^2 + s.t[2]^2))/sqrt(s.t[1]^2 + s.t[2]^2) -
      20 * cos(sqrt(s.t[1]^2 + s.t[2]^2)) * s.t[2]^2/(s.t[1]^2 + s.t[2]^2)^(3/2) -
      20 * sin(sqrt(s.t[1]^2 + s.t[2]^2)) * s.t[2]^2/(s.t[1]^2 + s.t[2]^2)
    true_sxx * u.perp[1]^2 + 2 * true_sxy * u.perp[1] * u.perp[2] + true_syy * u.perp[2]^2
  })
  truth[i, 2] = sum(truth.lsegment * (tvec[i]/101))
}
# Estimated total wombling measure for gradient
colSums(wm$estimate.wm.1[,-4])
# Estimated average wombling measure for gradient
colSums(wm$estimate.wm.1[,-4])/sum(tvec)
# Estimated total wombling measure for curvature
colSums(wm$estimate.wm.2[,-4])
# Estimated average wombling measure for curvature
colSums(wm$estimate.wm.2[,-4])/sum(tvec)

# True values for total gradient and curvature wombling measure
true.total = colSums(truth); true.total
# True values for average gradient and curvature wombling measure
true.avg.total = true.total/sum(tvec); true.avg.total

########################
# Plot Wombling Curves #
########################
# Color code points based on significance
col.pts.1 = sapply(wm$estimate.wm.1$sig, function(x){
  if(x == 1) return("green")
  else if(x == -1) return("cyan")
  else return(NA)
})

col.pts.2 = sapply(wm$estimate.wm.2$sig, function(x){
  if(x == 1) return("green")
  else if(x == -1) return("cyan")
  else return(NA)
})

p13 = sp_ggplot(data_frame = data.frame(coords, y))
p14 = p13 + geom_path(curve, mapping = aes(x, y), linewidth = 2)
p15 = p13 + geom_path(curve, mapping = aes(x, y), linewidth = 2) +
  geom_path(curve, mapping = aes(x, y),
            colour = c(col.pts.1, NA), linewidth = 1, na.rm = TRUE)
p16 = p13 + geom_path(curve, mapping = aes(x, y), linewidth = 2) +
  geom_path(curve, mapping = aes(x, y),
            colour = c(col.pts.2, NA), linewidth = 1, na.rm = TRUE)

p14 + (p15/p16)

######################
# Observed vs Fitted #
######################
p15 = ggplot(data = fit_frame,
             mapping = aes(x = true, y = est)) +
  labs(x = "Observed", y = "Fitted") +
  geom_line(cbind(x = seq(-25, 25, by = 0.1),
                  y = seq(-25, 25, by = 0.1)),
            mapping = aes(x, y),
            col = "blue") +
  geom_point() + geom_ribbon(mapping = aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  theme_bw() + coord_equal()

gradients.sx = cbind(true = true_sx, gradients$estimate.sx[,-4])
p16 = ggplot(data = gradients.sx,
             mapping = aes(x = true, y = `50%`)) +
  labs(x = "Observed", y = "Fitted") +
  geom_line(cbind(x = seq(-25, 25, by = 0.1),
                  y = seq(-25, 25, by = 0.1)),
            mapping = aes(x, y),
            col = "blue") +
  geom_point() + geom_ribbon(mapping = aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  theme_bw() + coord_equal()

gradients.sy = cbind(true = true_sy, gradients$estimate.sy[,-4])
p17 = ggplot(data = gradients.sy,
             mapping = aes(x = true, y = `50%`)) +
  labs(x = "Observed", y = "Fitted") +
  geom_line(cbind(x = seq(-25, 25, by = 0.1),
                  y = seq(-25, 25, by = 0.1)),
            mapping = aes(x, y),
            col = "blue") +
  geom_point() + geom_ribbon(mapping = aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  theme_bw() + coord_equal()

gradients.sxx = cbind(true = true_sxx, gradients$estimate.sxx[,-4])
p18 = ggplot(data = gradients.sxx,
             mapping = aes(x = true, y = `50%`)) +
  labs(x = "Observed", y = "Fitted") +
  geom_line(cbind(x = seq(-25, 25, by = 0.1),
                  y = seq(-25, 25, by = 0.1)),
            mapping = aes(x, y),
            col = "blue") +
  geom_point() + geom_ribbon(mapping = aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  theme_bw() + coord_equal()

gradients.sxy = cbind(true = true_sxy, gradients$estimate.sxy[,-4])
p19 = ggplot(data = gradients.sxy,
             mapping = aes(x = true, y = `50%`)) +
  labs(x = "Observed", y = "Fitted") +
  geom_line(cbind(x = seq(-25, 25, by = 0.1),
                  y = seq(-25, 25, by = 0.1)),
            mapping = aes(x, y),
            col = "blue") +
  geom_point() + geom_ribbon(mapping = aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  theme_bw()

gradients.syy = cbind(true = true_syy, gradients$estimate.syy[,-4])
p20 = ggplot(data = gradients.syy,
             mapping = aes(x = true, y = `50%`)) +
  labs(x = "Observed", y = "Fitted") +
  geom_line(cbind(x = seq(-25, 25, by = 0.1),
                  y = seq(-25, 25, by = 0.1)),
            mapping = aes(x, y),
            col = "blue") +
  geom_point() + geom_ribbon(mapping = aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  theme_bw()

(p15 + p16 + p17)/(p18 + p19 + p20)
