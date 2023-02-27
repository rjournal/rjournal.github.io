data <- read.csv ("examples/mydata.csv")
data$z <- data$x + data$y
write.csv (data, "examples/mydata2.csv")
