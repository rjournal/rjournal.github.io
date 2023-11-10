a=readLines("filename.tex")
grep("\\\\section.*Introduction", a)
a[grep("\\\\section.*Introduction", a)]
a[grep("\\\\bibliography", a)]
grep("\\\\bibliography", a)
a[390:365]
cat(a[390:365])
savehistory("foo.R")
