#--------------------------------------
# This script runs the analysis for the
# theft paper
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 12 July 2025
#--------------------------------------

# Load packages

library(dplyr)
library(ggplot2)
library(tsibble)
library(theft) # NOTE: This paper requires v0.8.1 of theft
library(theftdlc) # NOTE: This paper requires v0.2.0 of theftdlc

# Read in data

temp <- tempfile()
download.file("https://github.com/hendersontrent/bonn-eeg-data/raw/refs/heads/main/INP_Bonn_EEG.txt.zip",temp)
bonn_eeg <- read.table(unz(temp, "INP_Bonn_EEG.txt"))
unlink(temp)
bonn_eeg <- as_tsibble(bonn_eeg, key = c("id", "group"), index = "timepoint")

# Create a virtual environment and install Python libraries

install_python_pkgs(venv = "theft-eco-py", python = "/usr/local/bin/python3.10")
init_theft("theft-eco-py")

# OPTIONAL: Load pre-computed features and classifiers for speed, otherwise the
# raw code to produce these files is below in the calculate_features() and classify()
# functions

files <- c("all_features", "mf_results", "feature_classifiers")

for(f in files){
  temp <- tempfile()
  download.file(paste0("https://github.com/hendersontrent/bonn-eeg-data/raw/refs/heads/main/", f, ".Rda"), temp)
  load(temp)
  unlink(temp)
}

# Calculate features (NOTE: This takes a very long time!)

all_features <- calculate_features(
  data = bonn_eeg, 
  feature_set = c("catch22", "feasts", "tsfeatures", 
                  "tsfresh", "tsfel", "kats"),
  use_compengine = FALSE, catch24 = TRUE)

# Plot feature matrix

plot(all_features, 
     type = "matrix", 
     norm_method = "RobustSigmoid", 
     clust_method = "average")

# Plot low-dimensional projection

low_dim_calc <- project(all_features, 
                        method = "MinMax", 
                        low_dim_method = "tSNE", 
                        perplexity = 15)

plot(low_dim_calc)

# Fit feature set classifiers

mf_results <- classify(
  data = all_features, 
  by_set = TRUE, 
  train_size = 0.8,
  n_resamples = 100,
  use_null = TRUE)

# Compute and visualise set classifier intervals

set_intervals <- interval(mf_results, 
                          metric = "accuracy",
                          by_set = TRUE,
                          type = "sd",
                          model_type = "main")

plot(set_intervals)

# Compare each feature set to its empiricall null distribution

compare_features(mf_results,
                 metric = "accuracy",
                 by_set = TRUE,
                 hypothesis = "null",
                 p_adj = "none")

# Compare each feature set pairwise

compare_features(mf_results,
                 metric = "accuracy",
                 by_set = TRUE,
                 hypothesis = "pairwise",
                 p_adj = "none")

# Fit individual feature classifiers (NOTE: This takes a very long time!)

feature_classifiers <- classify(data = all_features, 
                                by_set = FALSE, 
                                train_size = 0.8,
                                n_resamples = 100,
                                use_null = TRUE)

# Compare each feature's performance to its empirical null distribution

feature_vs_null <- compare_features(feature_classifiers,
                                    by_set = FALSE,
                                    hypothesis = "null",
                                    n_workers = 6)

# Find the top 40 features by mean classification accuracy

top_40 <- feature_vs_null |>
  dplyr::slice_max(feature_mean, n = 40)

# Plot correlation matrix of the top 40 features

feature_matrix_filt <- all_features |>
  dplyr::filter(feature_set %in% top_40$feature_set & 
                  names %in% top_40$original_names) |>
  structure(class = c("feature_calculations", "data.frame"))

plot(feature_matrix_filt, type = "cor") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                 axis.text.y = ggplot2::element_text(size = 7))

# Draw violin plot of two sample top features

plot(feature_matrix_filt,
     type = "violin", 
     feature_names = c("values__autocorrelation__lag_6", 
                       "0_Signal distance"))
