#####PREPARATION#####
library("bench")
library("magrittr")
library("tdaunif")
set.seed(42)

# input and function setup
curr_max_mem <- function() {
  bench_process_memory() %>%
    extract(2) %>%              # load maximum memory used in current session
    as.numeric() %>%            # remove unnecessary attributes
    divide_by(1024 ^ 2)         # convert from bytes to megabytes
}

params <- commandArgs(trailingOnly = TRUE)
data_type <- params[1]
curr_dim <- as.integer(params[2])
num_pts <- as.integer(params[3])
engine <- params[4]

pt_cloud <- switch(data_type,
                   `circle` = {
                     sample_sphere(n = num_pts,
                                   dim = curr_dim - 1)
                   },
                   `annulus` = {
                     sample_sphere(n = num_pts,
                                   dim = curr_dim - 1,
                                   sd = 0.15)
                   },
                   `uniform` = {
                     runif(n = num_pts * curr_dim) %>%
                       matrix(ncol = curr_dim)
                   },
                   `torus` = {
                     sample_torus_flat(n = num_pts)
                   })

# measure max memory prior to calculating homology
before_mem <- curr_max_mem()

#####WORKHORSE#####
# calculate homology
switch(engine,
       `TDAstats` = {
         TDAstats::calculate_homology(pt_cloud,
                                      dim = curr_dim - 1,
                                      threshold = 4) %>%
           invisible()
       },
       `Dionysus` = {
         TDA::ripsDiag(pt_cloud,
                       maxdimension = curr_dim - 1,
                       maxscale = 4,
                       location = FALSE,
                       library = "Dionysus") %>%
           invisible()
       },
       `GUDHI` = {
         TDA::ripsDiag(pt_cloud,
                       maxdimension = curr_dim - 1,
                       maxscale = 4,
                       location = FALSE,
                       library = "GUDHI") %>%
           invisible()
       })

#####RESULTS#####
# measure max memory after calculating homology
after_mem <- curr_max_mem()
diff_mem <- after_mem - before_mem

# append results to file
if (!file.exists("memory-data.csv")) {
  cat(paste("DataType,NumPoints,Dimension,Engine,Memory\n"),
      file = "memory-data.csv")
}
cat(paste(data_type, num_pts, curr_dim, engine, diff_mem, sep = ","),
    "\n",
    file = "memory-data.csv",
    append = TRUE)
cat(paste("DONE WITH",
          data_type, num_pts, curr_dim, engine,
          "\n"))
