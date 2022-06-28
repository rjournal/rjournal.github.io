source("packages.R")

data(who, package="tidyr")
who.pattern.nc <- list(
  before=".*",
  "new_?",
  diagnosis=".*",
  "_",
  gender=".", function(mf)factor(mf, c("m", "f")),
  ages=list(
    ymin="0|[0-9]{2}", as.numeric,
    ymax="[0-9]{0,2}", function(y)as.numeric(ifelse(y=="", Inf, y))))
who.pattern.args <- nc::var_args_list(who.pattern.nc)
who.reshape.names <- grep(who.pattern.args$pattern, names(who), value=TRUE)
who.reshape.cols <- who[, who.reshape.names]

N.rep.vec <- as.integer(c(0, 10^seq(0, 2, by=0.5)))
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
  timing.dt.list[[paste(N.col)]] <- data.table(N.rep, N.col, microbenchmark(
    control=list(order="block"),
    "nc::capture_melt_single"={
      result.list[["nc"]] <- nc::capture_melt_single(
        some.who,
        who.pattern.nc)
    },
    "tidyr::pivot_longer"={
      result.list[["transform"]] <- transform(tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        values_drop_na = TRUE,
        names_to=names(who.pattern.args$fun.list),
        names_pattern=who.pattern.args$pattern,
        names_ptypes=lapply(
          who.pattern.args$fun.list[c("gender", "ymin")],
          function(f)f(character()))),
        ymax=who.pattern.args$fun.list$ymax(ymax))
    },
    times=10))
  convert <- function(df){
    data.table(df)[order(last.country, last.year, diagnosis, gender, ages)]
  }
  ref.dt <- convert(result.list[[1]])
  for(other.df in result.list){
    other.dt <- convert(other.df)
    stopifnot(identical(ref.dt, other.dt))
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-who-complex-cols-data.rds")
