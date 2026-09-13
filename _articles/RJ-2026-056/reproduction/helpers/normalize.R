#' Normalize Response and Covariates
#'
#' This function centers and (optionally) scales the response vector and each column
#' of the design matrix using the population variance. It is used to prepare data
#' for Bayesian Lasso regression.
#'
#' @param y A numeric response vector.
#' @param X A numeric matrix or data frame of covariates (design matrix).
#' @param scale Logical; if \code{TRUE}, variables are scaled to have unit population variance (default is \code{TRUE}).
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{vy}: Normalized response vector.
#'   \item \code{mX}: Normalized design matrix.
#'   \item \code{mu.y}: Mean of the response vector.
#'   \item \code{sigma2.y}: Population variance of the response vector.
#'   \item \code{mu.x}: Vector of column means of \code{X}.
#'   \item \code{sigma2.x}: Vector of population variances for columns of \code{X}.
#' }
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(100 * 10), 100, 10)
#' beta <- c(2, -3, rep(0, 8))
#' y <- as.vector(X %*% beta + rnorm(100))
#' norm_result <- normalize(y, X)
#'
#' @importFrom stats var
#' @export
normalize <- function(y, X, scale=TRUE)
{
  n <- length(y)
  p <- ncol(X)

  mu.y <- mean(y)
  sigma2.y <- (n - 1) * var(y) / n

  if (scale) {
    vy <- (y - mu.y) / sqrt(sigma2.y)
  } else {
    vy = y - mu.y
  }

  # Normalise covariates
  mX <- matrix(0, n, p)
  mu.x <- c()
  sigma2.x <- c()
  for (j in 1:p)
  {
    mu.x[j] <- mean(X[, j])
    sigma2.x[j] <- (n - 1) * var(X[, j]) / n
    if (scale) {
      mX[, j] <- (X[, j] - mu.x[j]) / sqrt(sigma2.x[j])
    } else {
      mX[, j] <- X[, j] - mu.x[j]
    }
  }

  return(list(vy = vy, mX = mX, mu.y = mu.y, sigma2.y = sigma2.y, mu.x = mu.x, sigma2.x = sigma2.x))
}
