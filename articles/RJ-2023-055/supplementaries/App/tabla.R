tab <- st_drop_geometry(d[[1]][[1]][, c("mean.excess", "median.excess", "mean.excess.deaths", "median.excess.deaths")])

res <- apply(tab, 2, function(X) {
  c(mean(X), median(X), quantile(X, c(0.025, 0.975)))

})

row.names(res) <- c("Mean", "Median", "0.025 q.", "0.975 q.")
colnames(res)<- c("RLE (mean)", "RLE (median)", "NED (mean)", "NED (median)")


