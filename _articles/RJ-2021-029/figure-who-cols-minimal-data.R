source("packages.R")

data(who, package="tidyr")
who.pattern.nc <- list(
  before=".*",
  "new_?",
  diagnosis=".*",
  "_",
  gender=".",
  ages="[0-9]+")
who.pattern.args <- nc::var_args_list(who.pattern.nc)
who.reshape.names <- grep(who.pattern.args$pattern, names(who), value=TRUE)
who.reshape.cols <- who[1, who.reshape.names]

N.rep.vec <- as.integer(c(0, 10^seq(0, 2.5, by=0.5)))
timing.dt.list <- list()
for(N.rep in N.rep.vec){
  print(N.rep)
  i.vec <- 1:N.rep
  L <- lapply(i.vec, function(i)who.reshape.cols)
  names(L) <- i.vec
  L$last <- who
  some.who <- do.call(data.frame, L)
  N.col <- ncol(some.who)
  result.list <- list()
  timing.dt.list[[paste(N.rep)]] <- data.table(N.rep, N.col, microbenchmark(
    control=list(order="block"),
    "nc::capture_melt_single"={
      result.list[["nc"]] <- nc::capture_melt_single(
        some.who, who.pattern.nc,
        na.rm=FALSE)
    },
    "tidyr::pivot_longer"={
      result.list[["pivot"]] <- tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        names_to=names(who.pattern.args$fun.list),
        names_pattern=who.pattern.args$pattern)
    },
    "tidyr::gather"={
      result.list[["gather"]] <- tidyr::gather(
        some.who,
        "variable",
        "value",
        grep(who.pattern.args$pattern, names(some.who)))
    },
    "reshape2::melt"={
      result.list$reshape2 <- reshape2:::melt.data.frame(
        some.who,
        measure.vars=grep(who.pattern.args$pattern, names(some.who)))
    },
    "data.table::melt"={
      result.list$dt <- data.table::melt.data.table(
        data.table(some.who),
        measure.vars=patterns(who.pattern.args$pattern))
    },
    "utils::stack"={
      result.list$utils <- utils::stack(
        some.who, grep(who.pattern.args$pattern, names(some.who)))
    },
    "cdata::unpivot_to_blocks"={
      result.list$cdata <- cdata::unpivot_to_blocks(
        some.who, "variable", "value",
        grep(who.pattern.args$pattern, names(some.who), value=TRUE))
    },
    times=10))
  result.row.vec <- sapply(result.list, nrow)
  stopifnot(result.row.vec[1] == result.row.vec)
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-who-cols-minimal-data.rds")
