Line   Code 
5      cars4Cyl.df <- allCars.df[allCars.df$cyl == 4, ]
11     mpg <- c(mean(cars4Cyl.df$mpg), mean(cars6Cyl.df$mpg), mean(cars8Cyl.df$mpg))
12     cyl.vs.mpg.df <- data.frame (cylinders, mpg)
15     plot(cylinders, mpg)