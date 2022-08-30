source("packages.R")

data(who, package="tidyr")
who.pattern.nc <- list(
  ##before=".*",
  "new_?",
  diagnosis=".*",
  "_",
  gender=".", function(mf)factor(mf, c("m", "f")),
  ages=list(
    ymin="0|[0-9]{2}", as.numeric,
    ymax="[0-9]{0,2}", function(y)as.numeric(ifelse(y=="", Inf, y))))
who.pattern.args <- nc::var_args_list(who.pattern.nc)
who.reshape.names <- grep(who.pattern.args$pattern, names(who), value=TRUE)

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
    "tidyr::pivot_longer+transform"={
      result.list[["transform"]] <- transform(tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        values_drop_na = TRUE,
        names_to=names(who.pattern.args$fun.list),
        names_pattern=who.pattern.args$pattern),
        ymax=who.pattern.args$fun.list$ymax(ymax),
        ymin=who.pattern.args$fun.list$ymin(ymin),
        gender=who.pattern.args$fun.list$gender(gender))
    },
    "tidyr::pivot_longer+mutate"={
      result.list[["mutate"]] <- dplyr::mutate(tidyr::pivot_longer(
        some.who,
        grep(who.pattern.args$pattern, names(some.who)),
        values_drop_na = TRUE,
        names_to=names(who.pattern.args$fun.list),
        names_pattern=who.pattern.args$pattern),
        ymax=who.pattern.args$fun.list$ymax(ymax),
        ymin=who.pattern.args$fun.list$ymin(ymin),
        gender=who.pattern.args$fun.list$gender(gender))
    },
    times=10))
  convert <- function(df){
    data.table(df)[order(country, year, diagnosis, gender, ages)]
  }
  ref.dt <- convert(result.list[[1]])
  for(other.df in result.list){
    other.dt <- convert(other.df)
    stopifnot(identical(ref.dt, other.dt))
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
saveRDS(timing.dt, "figure-who-complex-rows-data.rds")
