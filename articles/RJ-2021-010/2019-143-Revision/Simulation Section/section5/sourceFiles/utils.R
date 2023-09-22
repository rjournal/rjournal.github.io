makeDesignMatrix <- function(n, p, method = "cs", rho = 0.5) {

  #- Create Correlation Structure:
  if(method == "cs") {
    V <- matrix(rho, nrow = p, ncol = p)
    diag(V) <- 1
  } else if (method == "ar") {
    V <- matrix(0, ncol = p, nrow = p)
    for(index in 1:p){
      V[index, ] <-
        c(rev(cumprod(rep(rho, index - 1))),
          1,
          cumprod(rep(rho, p - index)))
    }; rm(index)
  } else if (method == "ind") {
    V <- diag(p)
  }

  X <- matrix(rnorm(p*n, 0, 1), ncol = p)
  X <- X %*% chol(V)

  out <- list()
  out$X <- X
  out$Sigma <- V
  return(out)
}

makeCorrelationMatrix <- function(p, method = "cs", rho = 0.5) {
  #- Create Correlation Structure:
  if(method == "cs") {
    V <- matrix(rho, nrow = p, ncol = p)
    diag(V) <- 1
  } else if (method == "ar") {
    V <- matrix(0, ncol = p, nrow = p)
    for(index in 1:p){
      V[index, ] <-
        c(rev(cumprod(rep(rho, index - 1))),
          1,
          cumprod(rep(rho, p - index)))
    }; rm(index)
  } else if (method == "ind") {
    V< - diag(p)
  }
  return(V)
}
