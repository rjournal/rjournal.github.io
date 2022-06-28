source("packages-new.R")

data(who, package="tidyr")
who.pattern.nc <- list(
  before=".*", as.integer,
  "new_?",
  diagnosis=".*",
  "_",
  gender=".",
  ages="[0-9]+")
who.pattern.no.groups <- paste(
  who.pattern.nc[sapply(who.pattern.nc, is.character)], collapse="")
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
      L[[arg.name]] <- substitute({
        result <- EXPR
        if(is.null(result))stop(MSG)
        result.list[[NAME]] <- result
      }, list(
        NAME=arg.name,
        EXPR=maybe.lang,
        MSG=paste("result for", arg.name, "is NULL")))
    }
  }
  L
}

## this seems to be the fastest way to add capture columns to the
## melted data table. it is important to run the regex function (sub)
## on the unique values in the variable column, instead of running it
## on the variable column (which contains repeated variable names).
dt.add.vars <- function(DT){
  name.vec <- paste(unique(DT$variable))
  for(group.i in seq_along(who.pattern.args$fun.list)){
    group.name <- names(who.pattern.args$fun.list)[[group.i]]
    group.fun <- who.pattern.args$fun.list[[group.i]]
    value <- group.fun(sub(
      who.pattern.args$pattern, paste0("\\", group.i), name.vec))
    names(value) <- name.vec
    set(DT, j=group.name, value=value[DT$variable])
  }
  DT
} 

df.add.vars <- function(g.result, variable.name="variable"){
  for(group.i in seq_along(who.pattern.args$fun.list)){
    group.name <- names(who.pattern.args$fun.list)[[group.i]]
    group.fun <- who.pattern.args$fun.list[[group.i]]
    g.result[[group.name]] <- group.fun(sub(
      who.pattern.args$pattern,
      paste0("\\", group.i),
      g.result[[variable.name]]))
  }
  g.result
}  

N.rep.vec <- unique(as.integer(10^seq(0, 3, by=0.5)))
timing.dt.list <- list()
for(N.rep in N.rep.vec){
  print(N.rep)
  i.vec <- 1:N.rep
  L <- list("0"=who.rows, check.names=FALSE)
  L[paste(i.vec)] <- list(who.reshape.cols)
  ##data.type.vec <- c("data.table", "data.frame")
  data.type.vec <- "data.table"
  for(data.type in data.type.vec){
    some.who <- do.call(data.type, L)
    if(FALSE){
      microbenchmark(
        setDT(some.who),
        data.table(some.who),
        data.frame(some.who),
        as.data.table(some.who),
        as.data.frame(some.who),
        times=2)
    }
    N.col <- ncol(some.who)
    result.list <- list()
    m.args <- c(do.sub(
      times=10,
      "tidyr::pivot_longer-0"={
        tidyr::pivot_longer(
          some.who,
          grep(who.pattern.no.groups, names(some.who)),
          names_to="variable")
      },
      "tidyr::pivot_longer-4"={
        tidyr::pivot_longer(
          some.who,
          grep(who.pattern.args$pattern, names(some.who)),
          names_to=names(who.pattern.args$fun.list),
          names_transform=list(before=as.integer),
          names_pattern=who.pattern.args$pattern)
      },
      "tidyr::gather-4"={
        df.add.vars(tidyr::gather(
          some.who,
          "variable",
          "value",
          grep(who.pattern.args$pattern, names(some.who))))
      },
      "tidyr::gather-0"={
        tidyr::gather(
          some.who,
          "variable",
          "value",
          grep(who.pattern.args$pattern, names(some.who)))
      },
      "reshape2::melt-4"={
        df.add.vars(reshape2:::melt.data.frame(
          some.who,
          measure.vars=grep(who.pattern.args$pattern, names(some.who))))
      },
      "reshape2::melt-0"={
        reshape2:::melt.data.frame(
          some.who,
          measure.vars=grep(who.pattern.args$pattern, names(some.who)))
      },
      "utils::stack-4"={
        to.stack <- grep(who.pattern.args$pattern, names(some.who))
        s.res <- utils::stack(some.who, to.stack)
        ids.only <- data.frame(some.who, check.names=FALSE)[, -to.stack]
        with.ids <- data.frame(ids.only, s.res, check.names=FALSE)
        with.ids$value <- with.ids$values
        df.add.vars(with.ids, "ind")
      },
      "utils::stack-0"={
        to.stack <- grep(who.pattern.args$pattern, names(some.who))
        utils::stack(some.who, to.stack)
      },
      "nc::capture_melt_single-4"={
        nc::capture_melt_single(
          some.who, who.pattern.nc)
      },
      "nc::capture_melt_single-0"={
        nc::capture_melt_single(
          some.who, variable=who.pattern.no.groups)
      },
      "tidyfast::dt_pivot_longer-4"={
        dt.add.vars(tidyfast::dt_pivot_longer(
          some.who,
          grep(who.pattern.args$pattern, names(some.who)),
          names_to="variable"))
      },
      "tidyfast::dt_pivot_longer-0"={
        tidyfast::dt_pivot_longer(
          some.who,
          grep(who.pattern.args$pattern, names(some.who)))
      },
      "data.table::melt-0"={
        data.table::melt.data.table(
          some.who,
          measure.vars=patterns(who.pattern.args$pattern))
      },
      "data.table::melt-4"={
        dt.add.vars(data.table::melt.data.table(
          some.who,
          measure.vars=patterns(who.pattern.args$pattern)))
      }
    ),
    if(N.rep < Inf)do.sub(
      "cdata::unpivot_to_blocks-4"={
        df.add.vars(cdata::unpivot_to_blocks(
          some.who, "variable", "value",
          grep(who.pattern.args$pattern, names(some.who), value=TRUE)))
      },
      "cdata::unpivot_to_blocks-0"={
        cdata::unpivot_to_blocks(
          some.who, "variable", "value",
          grep(who.pattern.args$pattern, names(some.who), value=TRUE))
      }
    ),
    if(N.rep < 100)do.sub(
      "stats::reshape-4"={
        times <- grep(who.pattern.args$pattern, names(some.who), value=TRUE)
        df.add.vars(stats::reshape(
          some.who,
          direction="long",
          v.names="value",
          times=times,
          timevar="variable",
          varying=times))
      },
      "stats::reshape-0"={
        times <- grep(who.pattern.args$pattern, names(some.who), value=TRUE)
        stats::reshape(
          some.who,
          direction="long",
          v.names="value",
          times=times,
          timevar="variable",
          varying=times)
      })
    )
    m.result <- do.call(microbenchmark::microbenchmark, m.args)
    ref.name <- "tidyr::pivot_longer-4"
    name.vec <- names(result.list[[ref.name]])
    not.alone <- grep(
      "0$", names(result.list), invert=TRUE, value=TRUE)
    result.dts <- lapply(not.alone, function(df.name){
      DT <- data.table::data.table(result.list[[df.name]])
      ord.args <- lapply(name.vec, function(N)DT[[N]])
      bad <- sapply(ord.args, is.null)
      if(any(bad)){
        stop(
          paste(name.vec[bad], collapse=","),
          " columns not found for ", df.name)
      }
      ord.result <- do.call(order, ord.args)
      DT[ord.result, ..name.vec]
    })
    names(result.dts) <- not.alone
    ref.dt <- result.dts[[ref.name]]
    compare.vec <- names(result.dts)
    if(is.null(compare.vec))stop("compare.vec is NULL")
    for(compare.name in compare.vec){
      (compare.dt <- result.dts[[compare.name]])
      stopifnot(all.equal(ref.dt, compare.dt))
    }
    nrow.vec <- sapply(result.list, nrow)
    nrow.bad <- nrow.vec[1] != nrow.vec
    if(any(nrow.bad)){
      print(nrow.vec[nrow.bad])
      stop("some output not the same size")
    }
    timing.dt.list[[paste(N.rep, data.type)]] <- data.table(
      N.rep, data.type, N.col, m.result)
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
fwrite(timing.dt, "figure-who-cols-new-data.csv")
