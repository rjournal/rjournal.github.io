# Write images to www/temp_plots so that they are not loaded into memory
# Images are written into www/temp_plots
library(png)
library(parallel)
options(mc.cores = 3)

countries <- c("England", "Greece", "Italy", "Spain", "Switzerland")

for(cc in countries) {
  ff <- paste0("data/", cc, "_p_list.RData")
  load(ff)

  mclapply(1:length(p_list_png), function(i) {
    outfile <- paste0("www/temp_plots/", cc,"_",i,".png")

    writePNG(p_list_png[[i]], outfile)
  })

}

