library (rdtLite)
prov.init ()
data <- read.csv ("mydata.csv")
plot (data$x, data$y)
dev.off()
prov.quit ()