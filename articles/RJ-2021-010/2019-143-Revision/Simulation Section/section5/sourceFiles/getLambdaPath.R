getLambdaPath <- function(ftime, fstatus, X, cencode = 0, failcode = 1, nlambda = 25, lambda.min = 0.001) {
  n <- length(ftime)
  p <- ncol(X)
  d <- data.frame(time = ftime, fstatus = fstatus)
  if (!missing(X)) 
    d$X <- as.matrix(X)
  d <- d[order(d$time), ]
  ftime <- d$time
  cenind <- ifelse(d$fstatus == cencode, 1, 0)
  fstatus <- ifelse(d$fstatus == failcode, 1, 2 * (1 - cenind))
  X <- d$X
  u <- do.call("survfit", list(formula = Surv(ftime, cenind) ~ 
                                 1, data = data.frame(ftime, cenind)))
  u <- approx(c(0, u$time, max(u$time) * (1 + 10 * .Machine$double.eps)), 
              c(1, u$surv, 0), xout = ftime * (1 - 100 * .Machine$double.eps), 
              method = "constant", f = 0, rule = 2)
  uuu <- u$y
  std <- .Call("standardize", X, PACKAGE = "crrp")
  XX <- std[[1]]
  center <- std[[2]]
  scale <- std[[3]]
  eta0 <- rep(0, n)
  sw <- .C("scorehessian", as.double(ftime), as.integer(fstatus), 
             as.double(XX), as.integer(p), as.integer(n), as.double(uuu), 
             as.double(eta0), double(n), double(n), double(1), 
             PACKAGE = "crrp")
    score0 <- sw[[8]]
    w0 <- sw[[9]]
    r0 <- ifelse(w0 == 0, 0, score0/w0)
    z <- eta0 + r0
    l.max <- max(t(w0 * z) %*% XX)/n
    l.min <- lambda.min
    lambda = exp(seq(log(l.max), log(l.min * l.max), len = nlambda))
    return(lambda)
}