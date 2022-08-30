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
  day1=iris.reshape.rows, day2=iris.reshape.rows, Species=iris$Species)
names_to <- names(iris.pattern.args$fun.list)
names_to[names_to=="column"] <- ".value"

N.row.vec <- as.integer(10^seq(1, 6, by=0.5))
timing.dt.list <- list()
for(N.row in N.row.vec){
  print(N.row)
  i.vec <- ((0:(N.row-1)) %% nrow(iris)) + 1
  some.iris <- data.frame(iris.days[i.vec,], obs=1:N.row)
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
        names_pattern=iris.pattern.args$pattern)
    }),
    "data.table::melt"=quote({
      result.list$dt <- data.table::melt.data.table(
        data.table(some.iris),
        measure.vars=patterns(Sepal="Sepal", Petal="Petal"))
    }))
  if(N.row <= Inf){
    m.args[["stats::reshape"]] <- quote({
      new.names <- sub("(.*?)[.](.*)", "\\2_\\1", names(some.iris))
      result.list$stats <- stats::reshape(
        structure(some.iris, names=new.names),
        direction="long",
        varying=1:(ncol(some.iris)-2))
    })
    m.args[["cdata::rowrecs_to_blocks"]] <- quote({
      part.list <- list()
      for(part in c("Petal", "Sepal")){
        part.list[[part]] <- grep(part, names(some.iris), value=TRUE)
      }
      controlTable.args <- c(list(
        stringsAsFactors=FALSE,
        dim=sub(".Sepal.", "", part.list$Sepal, fixed=TRUE)),
        part.list)
      controlTable <- do.call(data.frame, controlTable.args)
      result.list$cdata <- cdata::rowrecs_to_blocks(
        some.iris, controlTable=controlTable, columnsToCopy=c("Species", "obs"))
    })
  }
  m.result <- do.call(microbenchmark, m.args)
  timing.dt.list[[paste(N.row)]] <- data.table(N.row, m.result)
  result.row.vec <- sapply(result.list, nrow)
  stopifnot(result.row.vec[1] == result.row.vec)
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-iris-rows-data.rds")
