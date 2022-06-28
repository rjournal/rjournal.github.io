hocking.lines <- readLines("hocking.Rnw")
str(hocking.lines)
space.i <- which(hocking.lines=="")
at.i <- grep("^@", hocking.lines)
rm.i <- space.i[space.i %in% (at.i-1)]
cat(hocking.lines[-rm.i], file="hocking-edited.Rnw", sep="\n")
