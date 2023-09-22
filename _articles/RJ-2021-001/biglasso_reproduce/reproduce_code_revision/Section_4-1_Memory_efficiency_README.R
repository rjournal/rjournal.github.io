## -----------------------------------------------------------------------------
## Replication code accompanying the paper "The biglasso Package: A Memory-
## and Computation-Efficient Solver for Lasso Model Fitting with Big Data in R"
## Authors: Yaohui Zeng and Patrick Breheny
##
## benchmarking platform:
##    MacBook Pro with Intel Core i7 @ 2.3 GHz and 16 GB RAM.
##
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Section 4.1: memory efficiency
#
# Replicating Table 2
#
# To replicate results in Section 4.1:
# 1. the Python script "Syrupy" needs to be installed first.
# (See details in https://github.com/jeetsukumaran/Syrupy)
# 
# 2. A feature matrix X and response y are simulated on Mac terminal using
# C file: Section_4-1_dataGenerate_linear.c
# This generates two files: x_e3_e5.txt and y_e3_e5.txt
# 
# 3. Generate memory-mapped file for X using following command:
library(biglasso)
X.bm <- setupX(filename = "x_e3_e5.txt")
# this gives two files: x_e3_e5.bin and x_e3_e5.desc
#
# Also need to save the feature matrix as a Rdata file
x <- as.matrix(read.table("x_e3_e5.txt"))
save(x, file = "x_e3_e5.RData")

# 4. To accurately measure memory usage via Syrupy, each model fit needs to be
# a standalone file. So I have four files for single fit, and 3 files for CV.
#   - Section_4-1_biglasso_memory.R
#   - Section_4-1_glmnet_memory.R
#   - Section_4-1_ncvreg_memory.R
#   - Section_4-1_picasso_memory.R
#   - Section_4-1_cv_biglasso_serial.R
#   - Section_4-1_cv_glmnet_serial.R
#   - Section_4-1_cv_ncvreg_serial.R
#
# All these files should be put in the same folder as data files and generated
# x_e3_e5.bin and x_e3_e5.desc.
#
# 5. Here is how the maximum RSS is measured and reported. 
# 
# 1) For a single fit (for example biglasso model), cd to the folder that contains
# the standalone R script Section_4-1_biglasso_memory.R.
# Then run following command at terminal:
#   
#   syrupy.py Rscript Section_4-1_biglasso_memory.R
# 
# this generates four files:
#   
#   syrupy_*.ps.log
#   syrupy_*.ps.raw
#   syrupy_*.err.log
#   syrupy_*.out.log
# 
# The .ps.log file contains memory profiles during model fitting.
# 
# 2) Open file "syrupy_*.ps.log", identify the maximum value in Column RSS.
# This is the maximum RSS (in KB) during the model fitting.
# 
# 3) This is the same procedure that applies to measure memory usage by other
# packages for single fit as well as for cross-validation. Just run the command
# on each of the standalone R script file.
