source("packages-new.R")
iris.pattern.nc <- list(
  before="[0-9]+", as.integer,
  "[.]",
  column=".*",
  "[.]",
  dim=".*")
iris.pattern.args <- nc::var_args_list(iris.pattern.nc)
iris.reshape.cols <- iris[, 1:4]

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

names_to <- names(iris.pattern.args$fun.list)
names_to[names_to=="column"] <- ".value"
N.rep.vec <- as.integer(c(0, 10^seq(0, 4, by=0.5)))
timing.dt.list <- list()
for(N.rep in N.rep.vec){
  print(N.rep)
  i.vec <- 1:N.rep
  L <- lapply(i.vec, function(i)iris.reshape.cols)
  names(L) <- i.vec
  L[["0"]] <- iris
  some.iris <- do.call(data.table, L)
  N.col <- ncol(some.iris)
  result.list <- list()
  m.args <- c(do.sub(
    times=10,
    "nc::capture_melt_multiple"={
      nc::capture_melt_multiple(
        some.iris, iris.pattern.nc)
    },
    "tidyr::pivot_longer"={
      tidyr::pivot_longer(
        some.iris,
        grep(iris.pattern.args$pattern, names(some.iris)),
        names_to=names_to,
        names_transform=iris.pattern.args$fun.list,
        names_pattern=iris.pattern.args$pattern)
    },
    "data.table::melt"={
      dt <- data.table::melt(
        some.iris,
        measure=patterns(Petal="Petal", Sepal="Sepal"))
      pnames <- grep("Petal", names(some.iris), value=TRUE)
      dt[, before := as.integer(gsub("[^0-9]", "", pnames))[variable] ]
      dt[, dim := sub(".*[.]", "", pnames)[variable] ]
      dt
    },
    "cdata::rowrecs_to_blocks"={
      part.list <- list()
      for(part in c("Petal", "Sepal")){
        part.list[[part]] <- grep(part, names(some.iris), value=TRUE) 
      }
      controlTable.args <- c(list(
        stringsAsFactors=FALSE,
        before.chr=sub("[.].*", "", part.list$Sepal),
        dim=sub(".*[.]", "", part.list$Sepal)),
        part.list)
      controlTable <- do.call(data.frame, controlTable.args)
      df <- cdata::rowrecs_to_blocks(
        some.iris, controlTable=controlTable,
        columnsToCopy="0.Species",
        controlTableKeys=c("before.chr", "dim"))
      df$before <- as.integer(df$before.chr)
      df
    }    
  ), if(N.rep < 1e3)do.sub(
    "stats::reshape"={
      new.names <- sub("(.*?)[.](.*)", "\\2_\\1", names(some.iris))
      transform(stats::reshape(
        structure(some.iris, names=new.names),
        direction="long",
        varying=1:(ncol(some.iris)-1)),
        dim=sub("_.*", "", time),
        before=as.integer(sub(".*_", "", time)),
        "0.Species"=Species_0)
    })
  )
  m.result <- do.call(microbenchmark, m.args)
  lapply(result.list, names)
  timing.dt.list[[paste(N.rep)]] <- data.table(N.rep, N.col, m.result)
  ref.name <- "nc::capture_melt_multiple"
  name.vec <- names(result.list[[ref.name]])
  ord.dt.list <- list()
  for(compare.name in names(result.list)){
    compare.dt <- data.table(result.list[[compare.name]])
    ord.args <- lapply(name.vec, function(N)compare.dt[[N]])
    ord.result <- do.call(order, ord.args)
    ord.dt.list[[compare.name]] <- compare.dt[ord.result, ..name.vec]
  }
  ref.dt <- ord.dt.list[[ref.name]]
  for(compare.name in names(result.list)){
    ord.dt <- ord.dt.list[[compare.name]]
    stopifnot(all.equal(ord.dt, ref.dt))
  }
}

timing.dt <- do.call(rbind, timing.dt.list)
fwrite(timing.dt, "figure-iris-cols-new-data.csv")



