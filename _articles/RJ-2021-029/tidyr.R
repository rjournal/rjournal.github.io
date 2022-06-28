N.rep <- 10000
i.vec <- 1:N.rep
L <- lapply(i.vec, function(i)iris[, 1:4])
names(L) <- i.vec
L[["0"]] <- iris
some.iris <- do.call(data.frame, L)
pattern <- "X(.*)[.](.*)[.](.*)"
system.time({
  result <- tidyr::pivot_longer(
    some.iris,
    grep(pattern, names(some.iris)),
    names_to=c("before", ".value", "dim"),
    names_pattern=pattern)
})
