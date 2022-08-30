source("packages-dt.R")

data(who, package="tidyr")
who.pattern.nc <- list(
  before=".*", as.integer,
  "new_?",
  diagnosis=".*",
  "_",
  gender=".",
  ages="[0-9]+")
who.pattern.args <- nc::var_args_list(who.pattern.nc)
not.na.vec <- rowSums(!is.na(who))
some.i <- which(not.na.vec==max(not.na.vec))[1:11]
who.rows.some <- who[some.i,]
who.rows <- who.rows.some[, !apply(is.na(who.rows.some), 2, all)]
who.reshape.names <- grep(who.pattern.args$pattern, names(who.rows), value=TRUE)
who.reshape.cols <- who.rows[, who.reshape.names]
dim(who.reshape.cols)

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

N.rep.vec <- unique(as.integer(10^seq(0, 3, by=0.5)))
timing.dt.list <- list()
for(N.rep in N.rep.vec){
  print(N.rep)
  i.vec <- 1:N.rep
  L <- list("0"=who.rows)
  L[paste(i.vec)] <- list(who.reshape.cols)
  some.who <- do.call(data.table::data.table, L)
  N.col <- ncol(some.who)
  result.list <- list()
  nc.dt <- nc::capture_first_vec(
    names(some.who), who.pattern.nc, nomatch.error=FALSE)
  measure.vec <- which(!is.na(nc.dt$before))
  var.tab <- data.table(nc.dt[measure.vec])
  dim.dt <- var.tab[, data.table(.SD, variable = names(some.who)[measure.vec]) ]
  measure.vars <- structure(measure.vec, variable_table=var.tab)
  m.args <- do.sub(
    times=10,
    "tidyr::pivot_longer"={
      tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        names_to=names(who.pattern.args$fun.list),
        names_transform=list(before=as.integer),
        names_pattern=who.pattern.args$pattern,
        values_drop_na = TRUE)
    },
    "data.table::melt.old.set"={
      some.tall <- data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=patterns(who.pattern.args$pattern))
      for(new.name in names(var.tab)){
        set(some.tall, j=new.name, value=var.tab[[new.name]][some.tall$variable])
      }
      some.tall
    },
    "data.table::melt.old.join"={
      some.tall <- data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=patterns(who.pattern.args$pattern))
      some.tall[dim.dt, on="variable"]
    },
    "data.table::melt.new.pattern"={
      data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=measure(
          before=as.integer, diagnosis, gender, ages,
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
  timing.dt.list[[paste(N.rep)]] <- data.table(N.rep, N.col, m.result)
}

timing.dt <- do.call(rbind, timing.dt.list)
fwrite(timing.dt, "figure-who-cols-dt-data.csv")


