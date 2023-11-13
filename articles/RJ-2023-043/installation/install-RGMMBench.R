# .libPaths(c(temp_lib))

# CRAN packages
temp_lib <- "./temp_lib"; dir.create("temp_lib", showWarnings = F)
pkgs <- c("tibble", "MixSim", "dplyr", "microbenchmark", 
         "purrr", "bgmm", "EMCluster", "mclust", 
         "rebmix", "glue", "Rmixmod", "flexmix", "mixtools", "GMKMcharlie", 
         "clustvarsel", "HDclassif", "magrittr", "pgmm", "EMMIXmfa", "otrimle", 
         "mvtnorm", "tidyr", "stringr", "rlang", "ggtext", "egg", "ggnewscale", 
         "ggforce", "viridis", "ade4", "adegraphics", "factoextra", "GGally", 
         "cowplot", "DCEM", "ggplot2", "knitr", 
         "rmarkdown", "testthat", "vdiffr")
install.packages(pkgs, 
                 lib = temp_lib)

# Bioconductor packages
install.packages("BiocManager", 
                 lib = temp_lib)
BiocManager::install("ComplexHeatmap", 
                     lib =temp_lib)

# proper installation of RGMMBench
install.packages("RGMMBench_0.0.0.9000.tar.gz", 
                 lib = temp_lib)