

plot_densities <- function(res) {
  beta_inds = res$beta_inds
  for (i in 1:length(beta_inds)) {
    plot(res$ldens_beta[[i]], main = beta_inds[i])
  }
  plot(res$dens_sigma2)
  plot(res$dens_lambda2)
}
  
plot_density_pairs <- function(res1,res2) 
{
  beta_inds = res1$beta_inds
  for (i in 1:length(beta_inds)) {
    xlim = range(c(res1$ldens_beta[[i]]$x, res2$ldens_beta[[i]]$x))
    ylim = range(c(res1$ldens_beta[[i]]$y, res2$ldens_beta[[i]]$y))
    plot(NA, type="n", xlim=xlim, ylim=ylim, main = beta_inds[i])
    lines(res1$ldens_beta[[i]], col="black", lwd=2)
    lines(res2$ldens_beta[[i]], col="blue", lwd=2)
  }
  
  xlim = range(c(res1$dens_sigma2$x, res2$dens_sigma2$x))
  ylim = range(c(res1$dens_sigma2$y, res2$dens_sigma2$y))
  plot(NA, type="n", xlim=xlim, ylim=ylim, main = "sigma2")
  lines(res1$dens_sigma2, col="black", lwd=2)
  lines(res2$dens_sigma2, col="blue", lwd=2)
  
  xlim = range(c(res1$dens_lambda2$x, res2$dens_lambda2$x))
  ylim = range(c(res1$dens_lambda2$y, res2$dens_lambda2$y))
  plot(NA, type="n", xlim=xlim, ylim=ylim, main = "lambda2")
  lines(res1$dens_lambda2, col="black", lwd=2)
  lines(res2$dens_lambda2, col="blue", lwd=2)
  
}