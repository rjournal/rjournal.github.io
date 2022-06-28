source("packages.R")
iris.pattern.nc <- list(
  "X",
  before=".*", as.integer,
  "[.]",
  column=".*",
  "[.]",
  dim=".*")
iris.pattern.args <- nc::var_args_list(iris.pattern.nc)
iris.reshape.cols <- iris[, 1:4]

names_to <- names(iris.pattern.args$fun.list)
names_to[names_to=="column"] <- ".value"
N.rep.vec <- as.integer(c(0, 10^seq(0, 5, by=0.5)))
timing.dt.list <- list()
for(N.rep in N.rep.vec){
  print(N.rep)
  i.vec <- 1:N.rep
  L <- lapply(i.vec, function(i)iris.reshape.cols)
  names(L) <- i.vec
  L[["0"]] <- iris
  some.iris <- do.call(data.frame, L)
  N.col <- ncol(some.iris)
  result.list <- list()
  timing.dt.list[[paste(N.rep)]] <- data.table(N.rep, N.col, microbenchmark(
    control=list(order="block"),
    "nc::capture_melt_multiple"={
      result.list[["nc"]] <- nc::capture_melt_multiple(
        some.iris, iris.pattern.nc)
    },
    "tidyr::pivot_longer"={
      result.list[["pivot"]] <- transform(tidyr::pivot_longer(
        some.iris,
        grep(iris.pattern.args$pattern, names(some.iris)),
        names_to=names_to,
        names_pattern=iris.pattern.args$pattern),
        before=as.integer(before))
    },
    times=10))
  convert <- function(df){
    data.table(df)[
      order(X0.Species, before, dim, Petal, Sepal),
      .(X0.Species, before, dim, Petal, Sepal)]
  }
  ref.dt <- convert(result.list[[1]])
  for(other.df in result.list){
    other.dt <- convert(other.df)
    stopifnot(identical(ref.dt, other.dt))
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-iris-cols-convert-data.rds")
