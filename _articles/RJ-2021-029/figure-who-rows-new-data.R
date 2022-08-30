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

N.rows.vec <- as.integer(10^seq(1, 5, by=0.5))
timing.dt.list <- list()
for(N.rows in N.rows.vec){
  print(N.rows)
  i.vec <- ((0:(N.rows-1)) %% nrow(who)) + 1
  some.who <- data.table(who[i.vec,])
  result.list <- list()
  m.args <- c(do.sub(
    times=10
    ,"tidyr::pivot_longer-0"={
      tidyr::pivot_longer(
        some.who,
        grep(who.pattern.no.groups, names(some.who)),
        names_to="variable",
        values_drop_na = TRUE)
    }
    ,"tidyr::pivot_longer-4"={
      tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        names_to=names(who.pattern.args$fun.list),
        values_drop_na = TRUE,
        names_transform=list(before=as.integer),
        names_pattern=who.pattern.args$pattern)
    }
    ,"tidyr::gather-4"={
      df.add.vars(tidyr::gather(
        some.who,
        "variable",
        "value",
        grep(who.pattern.args$pattern, names(some.who)),
        na.rm = TRUE))
    }
    ,"tidyr::gather-0"={
      tidyr::gather(
        some.who,
        "variable",
        "value",
        grep(who.pattern.args$pattern, names(some.who)),
        na.rm = TRUE)
    }
    ,"reshape2::melt-4"={
      df.add.vars(reshape2:::melt.data.frame(
        some.who,
        na.rm=TRUE,
        measure.vars=grep(who.pattern.args$pattern, names(some.who))))
    }
    ,"reshape2::melt-0"={
      reshape2:::melt.data.frame(
        some.who,
        na.rm=TRUE,
        measure.vars=grep(who.pattern.args$pattern, names(some.who)))
    }
    ,"nc::capture_melt_single-4"={
      nc::capture_melt_single(
        some.who, who.pattern.nc)
    }
    ,"nc::capture_melt_single-0"={
      nc::capture_melt_single(
        some.who, variable=who.pattern.no.groups)
    }
    ,"tidyfast::dt_pivot_longer-4"={
      dt.add.vars(tidyfast::dt_pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        names_to = "variable",
        values_drop_na = TRUE))
    }
    ,"tidyfast::dt_pivot_longer-0"={
      tidyfast::dt_pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        values_drop_na = TRUE)
    }
    ,"data.table::melt-0"={
      data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=patterns(who.pattern.args$pattern))
    }
    ,"data.table::melt-4"={
      dt.add.vars(data.table::melt.data.table(
        some.who,
        na.rm=TRUE,
        measure.vars=patterns(who.pattern.args$pattern)))
    }
    ,"utils::stack-0"={
      to.stack <- grep(who.pattern.args$pattern, names(some.who))
      subset(utils::stack(some.who, to.stack), !is.na(values))
    }
  )
  ,if(N.rows <= 1e4)do.sub(
    "cdata::unpivot_to_blocks-0"={
      subset(cdata::unpivot_to_blocks(
        some.who, "variable", "value",
        grep(who.pattern.args$pattern, names(some.who), value=TRUE)),
        !is.na(value))
    },
    "stats::reshape-0"={
      times <- grep(who.pattern.args$pattern, names(some.who), value=TRUE)
      subset(stats::reshape(
        some.who,
        direction="long",
        v.names="value",
        times=times,
        timevar="variable",
        varying=times),
        !is.na(value))
    }
  )
  ,if(N.rows < 1e4)do.sub(
    "utils::stack-4"={
      to.stack <- grep(who.pattern.args$pattern, names(some.who))
      s.res <- utils::stack(some.who, to.stack)
      ids.only <- data.frame(some.who, check.names=FALSE)[, -to.stack]
      with.ids <- data.frame(ids.only, s.res, check.names=FALSE)
      with.ids$value <- with.ids$values
      subset(df.add.vars(with.ids, "ind"), !is.na(value))
    },
    "cdata::unpivot_to_blocks-4"={
      DF <- df.add.vars(cdata::unpivot_to_blocks(
        some.who, "variable", "value",
        grep(who.pattern.args$pattern, names(some.who), value=TRUE)))
      subset(DF, !is.na(value))
    },
    "stats::reshape-4"={
      times <- grep(who.pattern.args$pattern, names(some.who), value=TRUE)
      subset(df.add.vars(stats::reshape(
        some.who,
        direction="long",
        v.names="value",
        times=times,
        timevar="variable",
        varying=times)),
        !is.na(value))
    })
  )
  (m.result <- do.call(microbenchmark::microbenchmark, m.args))
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
  timing.dt.list[[paste(N.rows)]] <- data.table(
    N.rows, m.result)
}

timing.dt <- do.call(rbind, timing.dt.list)
fwrite(timing.dt, "figure-who-rows-new-data.csv")


