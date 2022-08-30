source("packages-new.R")
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

do.sub <- function(...){
  mcall <- match.call()
  L <- as.list(mcall[-1])
  for(arg.name in names(L)){
    maybe.lang <- L[[arg.name]]
    if(is.language(maybe.lang)){
      L[[arg.name]] <- substitute(
        result.list[[NAME]] <- EXPR,
        list(NAME=arg.name, EXPR=maybe.lang))
    }
  }
  L
}
timing.dt.list <- list()

N.row.vec <- as.integer(10^seq(1, 6, by=0.5))
N.row.new <- N.row.vec[! N.row.vec %in% names(timing.dt.list)]
for(N.row in N.row.new){
  print(N.row)
  i.vec <- ((0:(N.row-1)) %% nrow(iris)) + 1
  some.iris <- data.table(iris.days[i.vec,], obs=1:N.row)
  result.list <- list()
  m.args <- c(do.sub(
    times=10,
    "nc::capture_melt_multiple"={
      nc::capture_melt_multiple(
        some.iris, iris.pattern.nc)
    },
    "tidyr::pivot_longer"={
      tidyr::pivot_longer(
        some.iris,
        grep(iris.pattern.args$pattern, names(some.iris)),
        names_to=names_to,
        names_transform=iris.pattern.args$fun.list,
        names_pattern=iris.pattern.args$pattern)
    },
    "data.table::melt"={
      dt <- data.table::melt(
        some.iris,
        measure=patterns(Petal="Petal", Sepal="Sepal"))
      pnames <- grep("Petal", names(some.iris), value=TRUE)
      dt[, day := as.integer(gsub("[^0-9]", "", pnames))[variable] ]
      dt[, dim := sub(".*[.]", "", pnames)[variable] ]
      dt
    }
  ), if(N.row < 1e6)do.sub(
    "cdata::rowrecs_to_blocks"={
      part.list <- list()
      for(part in c("Petal", "Sepal")){
        part.list[[part]] <- grep(part, names(some.iris), value=TRUE) 
      }
      controlTable.args <- c(list(
        stringsAsFactors=FALSE,
        day.chr=gsub("[^0-9]", "", part.list$Sepal),
        dim=sub(".*[.]", "", part.list$Sepal)),
        part.list)
      controlTable <- do.call(data.frame, controlTable.args)
      df <- cdata::rowrecs_to_blocks(
        some.iris, controlTable=controlTable,
        columnsToCopy=c("Species", "obs"),
        controlTableKeys=c("day.chr", "dim"))
      df$day <- as.integer(df$day.chr)
      df
    },
    "stats::reshape"={
      DF <- stats::reshape(
        some.iris,
        direction="long",
        v.names=c("Petal","Sepal"),
        varying=list(
          grep("Petal", names(some.iris)),
          grep("Sepal", names(some.iris))))
      Petal <- grep("Petal", names(some.iris), value=TRUE)
      DF$day <- as.integer(gsub("[^0-9]", "", Petal))[DF$time]
      DF$dim <- sub(".*[.]", "", Petal)[DF$time]
      DF
    })
  )
  m.result <- do.call(microbenchmark, m.args)
  lapply(result.list, names)
  timing.dt.list[[paste(N.row)]] <- data.table(N.row, m.result)
  ref.name <- "nc::capture_melt_multiple"
  name.vec <- names(result.list[[ref.name]])
  ord.dt.list <- list()
  for(compare.name in names(result.list)){
    compare.dt <- data.table(result.list[[compare.name]])
    ord.args <- lapply(name.vec, function(N)compare.dt[[N]])
    ord.result <- do.call(order, ord.args)
    ord.dt.list[[compare.name]] <- compare.dt[ord.result, ..name.vec]
  }
  ref.dt <- ord.dt.list[[ref.name]]
  for(compare.name in names(result.list)){
    ord.dt <- ord.dt.list[[compare.name]]
    stopifnot(all.equal(ord.dt, ref.dt))
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
fwrite(timing.dt, "figure-iris-rows-new-data.csv")



