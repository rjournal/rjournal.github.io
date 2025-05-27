deb.hdi=function(X, y, loading, model = "linear")
{
  if(model == "linear"){
    family = "gaussian"
  }else{
    family = "binomial"
  }
  fit.lasso <- hdi:::lasso.proj(x = X, y = y, standardize = F, family = family, return.Z = T)
  pdata <- hdi:::prepare.data(x = X, y = y, standardize = F, family = family)
  X <- pdata$x
  y <- pdata$y
  
  Z <- hdi:::score.rescale(Z = fit.lasso$Z, x = X)$Z
  Cov.est <- crossprod(Z)/(nrow(X)^2) 
  se_linear <- sqrt(t(loading)%*%Cov.est%*%loading)
  
  debias.est <- sum(loading*fit.lasso$bhat)
  CI <- c(debias.est - qnorm(1-alpha/2)*se_linear, debias.est + qnorm(1-alpha/2)*se_linear)
  returnList <- list("hdi.est" = debias.est,
                     "hdi.se" = se_linear,
                     "hdi.CI" = CI)
  return(returnList)
}