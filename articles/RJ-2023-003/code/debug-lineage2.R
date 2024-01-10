> debug.lineage("x", forward=T)
$x
  scriptNum startLine                                                                code
1         1         1                                                                 x <- 1
2         1         4                                                             x <- x + y
3         1         5 if (x == 2) {\n  print ("x is 2")\n} else {\n  print ("x is not 2")\n}