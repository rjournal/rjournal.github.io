# NMAforest_RJournal.R
# Reproducibility script for the R Journal submission

# Load required package
library(NMAforest)

# Binary Outcome Example

# Load example data 
data(example_data)

# Run NMAforest for the x vs y comparison to generate expanded version
NMAforest(
  data = example_data,
  sm = "OR",
  reference = "x",
  model = "random",
  comparison = c("x", "y"),
  study = "study",
  treat = "t",
  event = "r",
  N = "n",
  study_id = "id",
  study_path = TRUE
)

# Optionally run with study_path = FALSE to generate summary version
NMAforest(
  data = example_data,
  sm = "OR",
  reference = "x",
  model = "random",
  comparison = c("x", "y"),
  study = "study",
  treat = "t",
  event = "r",
  N = "n",
  study_id = "id",
  study_path = FALSE
)

# Continuous Outcome Example

# Load example data 
library(pcnetmeta) 
data(parkinson)

# Run NMAforest for the 1 vs 3 comparison to generate expanded version
NMAforest(
  data = parkinson,
  sm = "MD",
  reference = "1",
  model = "random",
  comparison = c("1", "3"),
  study = "s.id",
  treat = "t.id",
  mean = "mean",
  sd = "sd",
  N = "n",
  study_path = TRUE
)

# Optionally run with study_path = FALSE to generate summary version
NMAforest(
  data = parkinson,
  sm = "MD",
  reference = "1",
  model = "random",
  comparison = c("1", "3"),
  study = "s.id",
  treat = "t.id",
  mean = "mean",
  sd = "sd",
  N = "n",
  study_path = FALSE
)


