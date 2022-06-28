source("packages-dt.R")

iris.pattern.nc <- list(
  column=".*",
  "[.]",
  dim=".*")
iris.pattern.args <- nc::var_args_list(iris.pattern.nc)

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

N.rows.vec <- as.integer(10^seq(1, 6, by=0.5))
var.tab <- data.table(dim=c("Length", "Width"))
dim.dt <- var.tab[, data.table(.SD, variable = factor(1:.N)) ]
measure.list <- list()
for(part in c("Sepal", "Petal")){
  measure.list[[part]] <- grep(part, names(iris))
}
measure.vars <- structure(measure.list, variable_table=var.tab)
timing.dt.list <- list()
for(N.rows in N.rows.vec){
  print(N.rows)
  i.vec <- ((0:(N.rows-1)) %% nrow(iris)) + 1
  some.iris <- data.table::data.table(iris[i.vec, ])
  result.list <- list()
  m.args <- do.sub(
    times=10,
    "tidyr::pivot_longer"={
      tidyr::pivot_longer(
        some.iris,
        grep(iris.pattern.args$pattern, names(some.iris)),
        names_to=c(".value", "dim"),
        names_pattern=iris.pattern.args$pattern)
    },
    "data.table::melt.old.join"={
      some.tall <- data.table::melt.data.table(
        some.iris,
        measure.vars=measure.list)
      dim.dt[some.tall, on="variable"]
    },
    "data.table::melt.old.set"={
      some.tall <- data.table::melt.data.table(
        some.iris,
        measure.vars=measure.list)
      some.tall[, dim := dim.dt$dim[some.tall$variable] ]
      some.tall
    },
    "data.table::melt.new.pattern"={
      data.table::melt.data.table(
        some.iris,
        measure.vars=measure(
          value.name, dim, pattern=iris.pattern.args$pattern))
    },
    "data.table::melt.new.sep"={
      data.table::melt.data.table(
        some.iris,
        measure.vars=measure(
          value.name, dim, sep="."))
    },
    "data.table::melt.new.var_tab"={
      data.table::melt.data.table(
        some.iris,
        na.rm=TRUE,
        measure.vars=measure.vars)
    })
  m.result <- do.call(microbenchmark::microbenchmark, m.args)
  ref.name <- "tidyr::pivot_longer"
  name.vec <- names(result.list[[ref.name]])
  result.dts <- lapply(result.list, function(df){
    DT <- data.table::data.table(df)
    ord.args <- lapply(name.vec, function(N)DT[[N]])
    ord.result <- do.call(order, ord.args)
    DT[ord.result, ..name.vec]
  })
  ref.dt <- result.dts[[ref.name]]
  for(compare.name in names(result.list)){
    (compare.dt <- result.dts[[compare.name]])
    stopifnot(all.equal(ref.dt, compare.dt))
  }
  timing.dt.list[[paste(N.rows)]] <- data.table(N.rows, m.result)
}

timing.dt <- do.call(rbind, timing.dt.list)
fwrite(timing.dt, "figure-iris-rows-dt-data.csv")


