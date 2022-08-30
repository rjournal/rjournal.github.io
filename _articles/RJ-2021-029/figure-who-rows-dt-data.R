source("packages-dt.R")

data(who, package="tidyr")
who.pattern.nc <- list(
  "new_?",
  diagnosis=".*",
  "_",
  gender=".",
  ages=list(
    min.years="0|[0-9]{2}", as.numeric,
    max.years="[0-9]{0,2}", function(x)ifelse(x=="", Inf, as.numeric(x))))
who.pattern.args <- nc::var_args_list(who.pattern.nc)

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

N.rows.vec <- as.integer(10^seq(1, 5, by=0.5))
nc.dt <- nc::capture_first_vec(
  names(who), who.pattern.nc, nomatch.error=FALSE)
measure.vec <- which(!is.na(nc.dt$ages))
length(measure.vec)
var.tab <- data.table(nc.dt[measure.vec])
dim.dt <- var.tab[, data.table(.SD, variable = names(who)[measure.vec]) ]
measure.vars <- structure(measure.vec, variable_table=var.tab)
timing.dt.list <- list()
for(N.rows in N.rows.vec){
  print(N.rows)
  i.vec <- ((0:(N.rows-1)) %% nrow(who)) + 1
  some.who <- data.table::data.table(who[i.vec, ])
  result.list <- list()
  m.args <- do.sub(
    times=10,
    "tidyr::pivot_longer"={
      tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        names_to=names(who.pattern.args$fun.list),
        names_transform=who.pattern.args$fun.list,
        names_pattern=who.pattern.args$pattern,
        values_drop_na = TRUE)
    },
    "data.table::melt.old.join"={
      some.tall <- data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=patterns(who.pattern.args$pattern))
      dim.dt[some.tall, on="variable"]
    },
    "data.table::melt.new.pattern"={
      data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=measure(
          diagnosis, gender, ages,
          min.years=who.pattern.args$fun.list$min.years,
          max.years=who.pattern.args$fun.list$max.years,
          pattern=who.pattern.args$pattern))
    },
    "data.table::melt.new.var_tab"={
      data.table::melt.data.table(
        some.who,
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
fwrite(timing.dt, "figure-who-rows-dt-data.csv")


