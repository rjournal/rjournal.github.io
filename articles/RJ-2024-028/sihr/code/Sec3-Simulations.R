library(SIHR)
set.seed(0)

# Example 1 ---------------------------------------------------------------

# Data Preparation
n <- 100
p <- 120
mu <- rep(0, p)
Cov <- diag(p)
beta <- rep(0, p)
beta[c(1, 2)] <- c(0.5, 1)
X <- MASS::mvrnorm(n, mu, Cov)
y <- a0 + X %*% beta + rnorm(n)

# Two observations (loadings)
loading1 <- c(1, 1, rep(0, p - 2))
loading2 <- c(-0.5, -1, rep(0, p - 2))
loading.mat <- cbind(loading1, loading2)

# Linear Functional
Est <- LF(X, y, loading.mat, model = "linear")

# Print results
ci(Est)
summary(Est)


# Example 2 ---------------------------------------------------------------

# Data Preparation
n <- 200
p <- 150
mu <- rep(0, p)
Cov <- matrix(0, p, p)
for (j in 1:p) {
  for (k in 1:p) {
    Cov[j, k] <- 0.5^(abs(j - k))
  }
}
beta <- rep(0, p)
beta[25:50] <- 0.2
X <- MASS::mvrnorm(n, mu, Cov)
y <- X %*% beta + rnorm(n)

# Specify G
test.set <- c(40:60)

# Quadratic Functional
Est <- QF(X, y, G = test.set, A = NULL, model = "linear", split = FALSE)

# Print results
ci(Est)
summary(Est)


# Example 3 ---------------------------------------------------------------

# Data Preparation
n1 <- 100
n2 <- 180
p <- 120
mu1 <- mu2 <- rep(0, p)
Cov1 <- diag(p)
Cov2 <- matrix(0, p, p)
for (j in 1:p) {
  for (k in 1:p) {
    Cov2[j, k] <- 0.5^(abs(j - k))
  }
}
beta1 <- rep(0, p)
beta1[c(1, 2)] <- c(0.5, 0.5)
beta2 <- rep(0, p)
beta2[c(1, 2)] <- c(1.8, 1.8)
X1 <- MASS::mvrnorm(n1, mu1, Cov1)
val1 <- X1 %*% beta1
X2 <- MASS::mvrnorm(n2, mu2, Cov2)
val2 <- X2 %*% beta2
y1 <- rbinom(n1, 1, exp(val1) / (1 + exp(val1)))
y2 <- rbinom(n2, 1, exp(val2) / (1 + exp(val2)))

# Specify one observation (loading)
loading.mat <- c(1, 1, rep(0, p - 2))

# Conditional average treatment effect (CATE)
Est <- CATE(X1, y1, X2, y2, loading.mat, model = "logistic_alter")

# Print results
ci(Est)
ci(Est, probability = TRUE)


# Example 4 ---------------------------------------------------------------

# Data Preparation
n1 <- 200
n2 <- 260
p <- 120
mu1 <- mu2 <- rep(0, p)
Cov1 <- diag(p)
Cov2 <- matrix(0, p, p)
for (j in 1:p) {
  for (k in 1:p) {
    Cov2[j, k] <- 0.5^(abs(j - k))
  }
}
beta1 <- rep(0, p)
beta1[1:10] <- 0.5
beta2 <- rep(0, p)
beta2[3:12] <- 0.4
X1 <- MASS::mvrnorm(n1, mu1, Cov1)
X2 <- MASS::mvrnorm(n2, mu2, Cov2)
y1 <- X1 %*% beta1 + rnorm(n1)
y2 <- X2 %*% beta2 + rnorm(n2)

# Specify G and A
test.set <- c(1:20)
A <- diag(length(test.set))

# Inner Product
Est <- InnProd(X1, y1, X2, y2, G = test.set, A, model = "linear")

# Print results
ci(Est)


# Example 5 ---------------------------------------------------------------

# Data Preparation
n1 <- 220
n2 <- 180
p <- 100
mu <- rep(0, p)
Cov <- diag(p)
beta1 <- rep(0, p)
beta1[1:2] <- c(0.5, 1)
beta2 <- rep(0, p)
beta2[1:10] <- c(0.3, 1.5, rep(0.08, 8))
X1 <- MASS::mvrnorm(n1, mu, Cov)
X2 <- MASS::mvrnorm(n2, mu, Cov)
y1 <- X1 %*% beta1 + rnorm(n1)
y2 <- X2 %*% beta2 + rnorm(n2)

# Specify G
test.set <- c(1:10)

# Distance when A is not specified
Est <- Dist(X1, y1, X2, y2, G = test.set, A = NULL, model = "linear", split = FALSE)

# Print results
ci(Est)
summary(Est)
