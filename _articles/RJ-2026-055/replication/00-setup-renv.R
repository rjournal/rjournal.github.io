# ============================================================
# 00-setup-renv.R — Create isolated R virtual environment
# ============================================================
# Run this ONCE to set up the renv-based virtual environment.
# From RStudio or R console:
#   setwd("<path-to>/replication")
#   source("00-setup-renv.R")
# ============================================================

cat("=== Setting up renv virtual environment ===\n\n")

# Initialize renv in this directory
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::init(bare = TRUE)

# Install required packages into the isolated library
cat("Installing packages into isolated renv library...\n")
# The replication requires metafrontier >= 0.3.0. Install from CRAN;
# fall back to the tagged GitHub release if CRAN does not yet carry it.
try(renv::install("metafrontier"), silent = TRUE)
ok <- requireNamespace("metafrontier", quietly = TRUE) &&
  packageVersion("metafrontier") >= "0.3.0"
if (!ok) {
  cat("CRAN version unavailable or < 0.3.0; installing v0.3.0 from GitHub...\n")
  renv::install("iik1/metafrontier@v0.3.0")
}

# Install sfaR for the real-data example (§4.6)
renv::install("sfaR")

# Install ggplot2 for autoplot verification
renv::install("ggplot2")

# Install quadprog for the QP objective (LP vs QP comparison, Target 9)
renv::install("quadprog")

# Snapshot the environment for reproducibility
renv::snapshot(prompt = FALSE)

cat("\n=== renv setup complete ===\n")
cat("Installed packages:\n")
cat("  metafrontier:", as.character(packageVersion("metafrontier")), "\n")
cat("  sfaR:", as.character(packageVersion("sfaR")), "\n")
cat("  ggplot2:", as.character(packageVersion("ggplot2")), "\n")
cat("\nNow run: source('01-replicate.R')\n")
