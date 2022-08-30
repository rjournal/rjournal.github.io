source("packages.R")

data(who, package="tidyr")
who.pattern.simple <- "new_?(.*)_(.)(.*)"
who.pattern.complex <- "new_?(.*)_(.)((0|[0-9]{2})([0-9]{0,2}))"
N.rows.vec <- as.integer(10^seq(1, 5, by=0.5))
timing.dt.list <- list()
for(N.rows in N.rows.vec){
  print(N.rows)
  i.vec <- ((0:(N.rows-1)) %% nrow(who)) + 1
  some.who <- data.frame(who[i.vec,])
  result.list <- list()
  timing.dt.list[[paste(N.rows)]] <- data.table(N.rows, microbenchmark(
    control=list(order="block"),
    "nc::capture_melt_single"={
      result.list[["nc"]] <- nc::capture_melt_single(
        some.who,
        "new_?",
        diagnosis=".*",
        "_",
        gender=".",
        ages=".*",
        na.rm=FALSE)
    },
    "tidyr::pivot_longer"={
      result.list[["pivot"]] <- tidyr::pivot_longer(
        some.who,
        grep(who.pattern.simple, names(some.who)),
        names_to=c("diagnosis", "gender", "ages"),
        names_pattern=who.pattern.simple)
    },
    "tidyr::gather"={
      result.list[["gather"]] <- tidyr::gather(
        some.who,
        "variable",
        "value",
        grep(who.pattern.simple, names(some.who)))
    },
    "reshape2::melt"={
      result.list$reshape2 <- reshape2:::melt.data.frame(
        some.who,
        measure.vars=grep(who.pattern.simple, names(some.who)))
    },
    "data.table::melt"={
      result.list$dt <- data.table::melt.data.table(
        data.table(some.who),
        measure.vars=patterns(who.pattern.simple))
    },
    "stats::reshape"={
      times <- grep(who.pattern.simple, names(some.who), value=TRUE)
      result.list$stats <- stats::reshape(
        some.who,
        direction="long",
        v.names="value",
        times=times,
        timevar="variable",
        varying=times)
    },
    "utils::stack"={
      result.list$utils <- utils::stack(
        some.who, grep(who.pattern.simple, names(some.who)))
    },
    "cdata::unpivot_to_blocks"={
      result.list$cdata <- cdata::unpivot_to_blocks(
        some.who, "variable", "value",
        grep(who.pattern.simple, names(some.who), value=TRUE))
    },
    times=10))
  result.row.vec <- sapply(result.list, nrow)
  stopifnot(result.row.vec[1] == result.row.vec)
}

timing.dt <- do.call(rbind, timing.dt.list)

saveRDS(timing.dt, "figure-who-rows-data.rds")
