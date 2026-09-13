========================================================
  Reproduction Materials
  "The Lasso Distribution: Properties, Sampling Methods,
   and Applications in Bayesian Lasso Regression"
  Mohammad Javad Davoudabadi, Jonathon Tidswell,
  Samuel Muller, Garth Tarr and John T. Ormerod
========================================================

OVERVIEW
--------
This folder contains all scripts, pre-computed results,
and data files needed to reproduce the tables in the paper.
The main entry point is create_tables.R, which reproduces
Tables 2 through 6 without needing to re-run the full MCMC
computations (which can take hours).


REQUIREMENTS
------------
R version >= 4.4.0

Required R packages:
  install.packages(c("knitr", "kableExtra", "BayesianLasso",
                     "posterior"))

BayesianLasso is available on CRAN:
  install.packages("BayesianLasso")

Or from GitHub:
  remotes::install_github("garthtarr/BayesianLasso")


FOLDER STRUCTURE
----------------
reproduction/
  create_tables.R              <- MAIN SCRIPT: run this to
                                  reproduce Tables 2-6
  README_reproduction.txt      <- this file

  results/
    simulated_output_Hans.RData  <- pre-computed MCMC output
                                    for Table 3 (Hans sampler)
    simulated_output_PC.RData    <- pre-computed MCMC output
                                    for Table 3 (PC sampler)
    pre-computed results for different samplers and different datasets

  tables/
    Table_2_Simulated.Rdata      <- pre-computed stats for Table 2
    Table_5_Diabeties2.Rdata     <- pre-computed stats for Table 5
    Table_5_Kakadu2.Rdata        <- pre-computed stats for Table 5
    Table_5_Crime.Rdata          <- pre-computed stats for Table 5
    Table_6_Eye.Rdata            <- pre-computed stats for Table 6
    Table_6_Riboflavin.Rdata     <- pre-computed stats for Table 6

  extdata/
    comData.Rdata                <- Communities and Crime dataset


  Functions/
    Benchmarks_ngtp.R            <- script used to generate the
                                    pre-computed MCMC results
    Credible_Intervals.R         <- script used to generate the
                                    credible intervals in Table 3
    reproduce_all.R              <- master script to re-run
                                    everything from scratch
    (other helper scripts)

  helpers/
    (benchmark helper functions sourced by Benchmarks_ngtp.R)


HOW TO REPRODUCE TABLES 2-6 (RECOMMENDED)
------------------------------------------
This is the fastest option. Pre-computed MCMC results are
provided so you do not need to re-run the samplers.

Step 1: Open RStudio (or any R session).

Step 2: Source the main script:
  source("path/to/reproduction/create_tables.R")

  The script sets its own working directory automatically,
  so it can be sourced from any location.

Step 3: Tables 2 through 6 will be printed to the console.

Expected run time: approximately 4 minutes.
(Table 4 runs 100 x 2 MCMC chains live; all other tables
load pre-computed results instantly.)

Note on Table 3: The credible intervals are computed from
the pre-saved MCMC draws in results/simulated_output_Hans.RData
and results/simulated_output_PC.RData. The simulated dataset
(X, y) is regenerated using set.seed(123) to match exactly
what was used when those draws were produced.


HOW TO RE-RUN EVERYTHING FROM SCRATCH (OPTIONAL)
--------------------------------------------------
If you wish to re-run the full MCMC computations rather than
using the pre-computed results, use the master script:

  source("path/to/reproduction/Functions/reproduce_all.R")

WARNING: This will take several hours to complete, particularly
for the high-dimensional datasets (Riboflavin: n=71, p=4088).
The Riboflavin dataset using the PC sampler is computationally
infeasible and is therefore reported as NA in Table 6, consistent
with the paper.

Computations were originally performed on an Apple M1 Pro with
12 cores and 16 GB RAM. Runtimes on other hardware will differ.


NOTES FOR REVIEWERS
--------------------
- All Gibbs samplers are implemented in R (version 4.4.2)
  using the BayesianLasso package (version on CRAN).
- Small numerical differences from the paper's tables are
  expected due to MCMC sampling variability. The results
  should be close but not necessarily identical to the
  last decimal place.
- Table 4 reports mean +/- standard deviation across 100
  simulation replicates and will show minor variation across
  runs due to Monte Carlo variability.


CONTACT
-------
For questions about the reproduction materials, please contact:
  Mohammad Javad Davoudabadi
  mohammad.davoudabadi@sydney.edu.au
  School of Mathematics and Statistics
  University of Sydney
========================================================
