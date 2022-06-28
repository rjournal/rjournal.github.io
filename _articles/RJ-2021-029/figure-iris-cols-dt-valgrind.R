source("packages-dt.R")

iris.pattern.nc <- list(
  before=".*",
  "[.]",
  column=".*",
  "[.]",
  dim=".*")
iris.pattern.args <- nc::var_args_list(iris.pattern.nc)
iris.reshape.cols <- iris[, 1:4]
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

N.rep.vec <- 1
timing.dt.list <- list()
for(N.rep in N.rep.vec){
  print(N.rep)
  i.vec <- N.rep:1
  L <- list()
  L[paste(i.vec)] <- list(iris.reshape.cols)
  L[["0"]] <- iris
  some.iris <- do.call(data.table::data.table, L)
  N.col <- ncol(some.iris)
  result.list <- list()
  var.tab <- data.table(
    ##Sepal=grep("Sepal", names(some.iris), value=TRUE),
    dim=c("Length", "Width"),
    before=rep(N.rep:0, each=2))
  dim.dt <- var.tab[, data.table(.SD, variable = factor(1:.N)) ]
  measure.list <- list()
  for(part in c("Sepal", "Petal")){
    measure.list[[part]] <- grep(part, names(some.iris))
  }
  measure.vars <- structure(measure.list, variable_table=var.tab)
  m.args <- do.sub(
    times=1,
    "tidyr::pivot_longer"={
      tidyr::pivot_longer(
        some.iris,
        grep(iris.pattern.args$pattern, names(some.iris)),
        names_to=names_to,
        names_transform=list(before=as.integer),
        names_pattern=iris.pattern.args$pattern)
    },
    "data.table::melt.old.set"={
      some.tall <- data.table::melt.data.table(
        some.iris,
        measure.vars=patterns(Sepal="Sepal", Petal="Petal"))
      some.tall[, dim := dim.dt$dim[some.tall$variable] ]
      some.tall[, before := dim.dt$before[some.tall$variable] ]
      some.tall
    },
    "data.table::melt.old.join"={
      some.tall <- data.table::melt.data.table(
        some.iris,
        measure.vars=patterns(Sepal="Sepal", Petal="Petal"))
      some.tall[dim.dt, on="variable"]
    },
    "data.table::melt.new.pattern"={
      data.table::melt.data.table(
        some.iris,
        measure.vars=measure(
          before=as.integer, value.name, dim, pattern=iris.pattern.args$pattern))
    },
    "data.table::melt.new.var_tab"={
      data.table::melt.data.table(
        some.iris,
        measure.vars=measure.vars)
    },      
    "data.table::melt.new.sep"={
      data.table::melt.data.table(
        some.iris,
        measure.vars=measure(
          before=as.integer, value.name, dim, sep="."))
    })
  m.result <- do.call(microbenchmark::microbenchmark, m.args)
  result.dts <- lapply(result.list, function(df){
    DT <- data.table::data.table(df)
    name.vec <- c("0.Species", "before", "dim", "Sepal", "Petal")
    ord.args <- lapply(name.vec, function(N)DT[[N]])
    ord.result <- do.call(order, ord.args)
    DT[ord.result, ..name.vec]
  })
  ref.name <- "tidyr::pivot_longer"
  ref.dt <- result.dts[[ref.name]]
  for(compare.name in names(result.list)){
    (compare.dt <- result.dts[[compare.name]])
    stopifnot(all.equal(ref.dt, compare.dt))
  }
  timing.dt.list[[paste(N.rep)]] <- data.table(N.rep, N.col, m.result)
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-iris-cols-dt-data.rds")


