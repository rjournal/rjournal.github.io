source("packages.R")
iris.pattern.nc <- list(
  nc::field("day", "", "[0-9]+", as.integer),
  "[.]",
  column=".*",
  "[.]",
  dim=".*")
iris.pattern.args <- nc::var_args_list(iris.pattern.nc)
iris.reshape.rows <- iris[, 1:4]
iris.days <- data.frame(
  Species=iris$Species, day1=iris.reshape.rows, day2=iris.reshape.rows)
names_to <- names(iris.pattern.args$fun.list)
names_to[names_to=="column"] <- ".value"

N.row.vec <- as.integer(10^seq(1, 6, by=0.5))
timing.dt.list <- list()
for(N.row in N.row.vec){
  print(N.row)
  i.vec <- ((0:(N.row-1)) %% nrow(iris)) + 1
  some.iris <- data.frame(iris.days[i.vec,])
  result.list <- list()
  m.args <- list(
    times=10,
    control=list(order="block"),
    "nc::capture_melt_multiple"=quote({
      result.list[["nc"]] <- nc::capture_melt_multiple(
        some.iris, iris.pattern.nc)
    }),
    "tidyr::pivot_longer"=quote({
      result.list[["pivot"]] <- tidyr::pivot_longer(
        some.iris,
        grep(iris.pattern.args$pattern, names(some.iris)),
        names_to=names_to,
        names_ptypes=list(day=integer()),
        names_pattern=iris.pattern.args$pattern)
    }))
  m.result <- do.call(microbenchmark, m.args)
  timing.dt.list[[paste(N.row)]] <- data.table(N.row, m.result)
  first <- result.list[[1]]
  convert <- function(df){
    data.table(df, key=names(first))[, names(first), with=FALSE]
  }
  ref.dt <- convert(first)
  for(other.df in result.list){
    other.dt <- convert(other.df)
    stopifnot(identical(ref.dt, other.dt))
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-iris-rows-convert-data.rds")
