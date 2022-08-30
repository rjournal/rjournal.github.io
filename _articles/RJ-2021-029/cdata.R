N.rep <- 1000
i.vec <- 1:N.rep
L <- lapply(i.vec, function(i)iris[,1:4])
names(L) <- i.vec
L[["0"]] <- iris
some.iris <- do.call(data.frame, L)
part.list <- list()
for(part in c("Petal", "Sepal")){
  part.list[[part]] <- grep(part, names(some.iris), value=TRUE)
}
controlTable.args <- c(list(
  stringsAsFactors=FALSE,
  dim=sub(".Sepal.", "", part.list$Sepal, fixed=TRUE)),
  part.list)
controlTable <- do.call(data.frame, controlTable.args)
system.time({
  result <- cdata::rowrecs_to_blocks(
    some.iris, controlTable=controlTable, columnsToCopy="X0.Species")
})
