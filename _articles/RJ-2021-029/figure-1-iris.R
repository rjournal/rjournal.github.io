library(data.table)
frows <- c(1, 51)
(some.iris <- data.table(iris, flower=1:nrow(iris))[frows])
o <- function(...){
  L <- list(...)
  vals <- L[[1]]
  dt <- data.table(
    flower=rep(frows, each=length(vals)))
  dt[[names(L)]] <- vals
  dt
}

nc::capture_melt_single(
  some.iris,
  part=".*",
  "[.]",
  dim=".*", 
  value.name="cm"
)[o(variable=names(iris)[1:4]), .(
  cm, Species, flower, part, dim
), on=.(flower, variable)]

nc::capture_melt_multiple(
  some.iris,
  column=".*",
  "[.]",
  dim=".*"
)[o(dim=c("Length", "Width")), .(
  Sepal, Petal, Species, flower, dim),
  on=.(flower, dim)]

nc::capture_melt_multiple(
  some.iris,
  part=".*",
  "[.]",
  column=".*"
)[o(part=c("Sepal", "Petal")), .(
  Length, Width, Species, flower, part
), on=.(flower, part)]
