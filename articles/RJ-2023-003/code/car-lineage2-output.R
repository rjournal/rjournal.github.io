Line   Code 
2      allCars.df <- read.csv("mtcars.csv")
5      cars4Cyl.df <- allCars.df[allCars.df$cyl == 4, ]