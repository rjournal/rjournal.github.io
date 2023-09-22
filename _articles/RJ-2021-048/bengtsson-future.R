## -------------------------------------------------------------------
## Introduction
## -------------------------------------------------------------------

slow_fcn <- function(x) {
  Sys.sleep(0.5)
  x^2
}

xs <- 1:10
y <- lapply(xs, function(x) {
  slow_fcn(x)
})

library(parallel)
xs <- 1:10
y <- mclapply(xs, function(x) {
  slow_fcn(x)
}, mc.cores = 2)

library(parallel)
workers <- makeCluster(2)
clusterExport(workers, "slow_fcn")
xs <- 1:10
y <- parLapply(workers, xs, function(x) {
  slow_fcn(x)
})



## -------------------------------------------------------------------
## Map-reduce parallelization with more control for the end-user
## -------------------------------------------------------------------

library(foreach)
library(doParallel)
workers <- parallel::makeCluster(2)
registerDoParallel(workers)
xs <- 1:10
y <- foreach(x = xs) %dopar% {
  slow_fcn(x)
}


## -------------------------------------------------------------------
## The future framework
## -------------------------------------------------------------------

library(future)

x <- 1
f <- future({
  slow_fcn(x)
})
x <- 2
v <- value(f)



library(future)
plan(multisession, workers = 2)

xs <- 1:10

f1 <- future({
  slow_fcn(xs[1])
})

f2 <- future({
  slow_fcn(xs[2])
})

f3 <- future({
  slow_fcn(xs[3])
})

v1 <- value(f1)
v2 <- value(f2)
v3 <- value(f3)


xs <- 1:10
fs <- list()
for (i in seq_along(xs)) {
  fs[[i]] <- future(slow_fcn(xs[i]))
}
vs <- lapply(fs, value)



xs <- 1:10
fs <- lapply(xs, function(x) {
  future(slow_fcn(x))
})
vs <- lapply(fs, value)



## -------------------------------------------------------------------
## Exception handling
## -------------------------------------------------------------------

x <- "24"
f <- future(log(x))
try(v <- value(f))
# Error in log(x) : non-numeric argument to mathematical function

x <- "24"
try(v <- log(x))
# Error in log(x) : non-numeric argument to mathematical function

v <- tryCatch({
  value(f)
}, error = function(e) {
  NA_real_
})



## -------------------------------------------------------------------
## Relaying of standard output and conditions (e.g. messages and warnings)
## -------------------------------------------------------------------

x <- c(1:10, NA)
f <- future({
  cat("Hello world\n")
  y <- sum(x, na.rm = TRUE)
  message("The sum of 'x' is ", y)
  if (anyNA(x)) warning("Missing values were omitted", call. = FALSE)
  cat("Bye bye\n")
  y
})
v <- value(f)
# Hello world
# Bye bye
# The sum of 'x' is 55
# Warning message:
# Missing values were omitted

stdout <- capture.output({
  v <- value(f)
})
# The sum of 'x' is 55
# Warning message:
# Missing values were omitted

stdout
# [1] "Hello world" "Bye bye"


## -------------------------------------------------------------------
## Globals and packages
## -------------------------------------------------------------------

plan(multisession)
k <- 42
f <- future({
  get("k")
})
try(v <- value(f))
# Error in get("k") : object 'k' not found

f <- future({
  k
  get("k")
})

f <- future({
  get("k")
}, globals = "k")


## -------------------------------------------------------------------
## Proper parallel random number generation
## -------------------------------------------------------------------

f <- future(rnorm(3), seed = TRUE)
value(f)
#  [1] -0.02648871 -1.73240257  0.78139056


## -------------------------------------------------------------------
## Future assignment construct
## -------------------------------------------------------------------

xs <- 1:10
v1 %<-% slow_fcn(xs[1])
v2 %<-% slow_fcn(xs[2])
v3 %<-% slow_fcn(xs[3])
print(v1)
print(v2)
print(v3)


v %<-% rnorm(3) %seed% TRUE
print(v)


xs <- 1:10
vs <- listenv::listenv()
for (i in seq_along(xs)) {
  vs[[i]] %<-% slow_fcn(xs[i])
}
vs <- as.list(vs)


plan(list(
  tweak(multisession, workers = 2),
  tweak(multisession, workers = 3)
))
