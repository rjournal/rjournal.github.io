# Create multivariate structure
#
# Examples:
#   m <- matrix(1:9, ncol = 3)
#   create_multivariate_data(m)
#
#   create_multivariate_data(1:10, 5:15, (-5):5)
#

#...: Either a matrix (with variable sper column) or vectors with the data. 
create_multivariate_data <- function(...) {

  d <- list(...)
  l <-length(d)


  if(l == 1) {
  # One argument, then it must be a matrix with data.
    if(!(is.matrix(d[[1]]) | is.data.frame(d[[1]])))
      stop("When a single object is passed it must be a matrix or a data.frame.")

    d <- lapply(1:ncol(d[[1]]), 
      function(X) d[[1]][, X]
    )
    l <-length(d)
  } else {
    aux <- lapply(d, 
      function(X) is.vector(X)
    )

    if(sum(unlist(aux)) < l ) 
      stop("All argumenst must be of type vector.")
  }

  n <- unlist(lapply(d, length))
  csumn <- cumsum(n)

  # Results
  res <- matrix(NA, ncol = length(d), nrow = sum(n))
  for(i in 1:l) {
    res[(csumn[i]-n[i]+1):(csumn[i]), i] <- d[[i]]

  }

  return(res)
} 


