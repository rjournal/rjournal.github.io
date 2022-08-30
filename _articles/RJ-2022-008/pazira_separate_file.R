
## The codes in this file sould be run in R version 2.10 !!!

## simulation study with dglars v.1.0.5
detach(name = "package:dglars", unload = TRUE)
remove.packages(pkgs = "dglars")
Old_dglars <- "https://cran.r-project.org/src/contrib/Archive/dglars/dglars_1.0.5.tar.gz"
install.packages(Old_dglars, repos = NULL, type = "source")

library(dglars)

set.seed(11235)
for(h in seq_len(length(n))) {
  nh <- n[h]
  for(k in seq_len(length(p))) {
    pk <- p[k]
    for(j in seq_len(length(rho))) {
      rhoj <- rho[j]
      mu <- rep(0, pk)
      Sigma <- outer(seq_len(pk), seq_len(pk), function(i, j) rhoj^abs(i - j))
      X <- mvrnorm(n = nh, mu = mu, Sigma = Sigma)
      eta <- b0 + drop(X[, A] %*% b)
      prob <- binomial()$linkinv(eta)
      for(i in seq_len(nsim)) {
        y <- rbinom(n = nh, size = 1L, prob = prob)
        out[h, k, j, i, "pc", "time"] <- system.time(out.dglars <- dglars.fit(X = X, y = y, family = "binomial", control = list(g0 = 0.2)))[3L]
        out[h, k, j, i, "pc", "q"] <- out.dglars$np
        cat("simulation study with h =", h, "k =", k, "j =", j, "i =", i,"completed\n")
      }
    }
  }
}
save.image(file = "output_simulation.RData")

# and then run the remaining lines in the simulation section.