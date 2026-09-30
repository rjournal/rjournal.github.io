# Script to run the runtime experiment
# NOTE: R jobs and Python jobs must be submitted in separate R sessions.
# Loading R torch and Python PyTorch in the same session causes conflicts
# due to incompatible libtorch versions.
library(batchtools)

# Load registry
source("config-runtime.R")
reg = loadRegistry(conf$reg_path, writeable = TRUE)
tab = unwrap(getJobPars())
getStatus()

# Cluster functions are configured via batchtools.conf.R in the project root.
# If no config file exists, fall back to SSH on localhost.
if (!fs::file_exists("batchtools.conf.R")) {
	cli::cli_alert_warning("No {.file batchtools.conf.R} found, falling back to SSH on localhost")
	reg$cluster.functions = makeClusterFunctionsSSH(
		list(Worker$new("localhost", ncpus = 4, max.load = 10)),
		fs.latency = 0
	)
}

# Split R and Python jobs — these must not run in the same session
tab[, python := grepl("python", tags)]
tab_py = tab[(python)]
tab_r = tab[!(python)]

# --- Step 1: Submit first replication as a smoke test ---
# Run ONE of the following blocks per session (not both!)

# R jobs (run in one session):
tab_first_r = ijon(findExperiments(repls = 1), tab_r)
tab_first_py = ijon(findExperiments(repls = 1), tab_py)

submitJobs(tab_first_r)

# Python jobs (run in a separate session):
# submitJobs(tab_first_py)

# --- Step 2: After verifying repl 1 results, submit remaining replications ---
# Adapt resources to your compute environment (walltime, memory).

# R jobs:
# ijoin(tab_r[repl > 1], findNotSubmitted()) |>
# 	submitJobs(resources = list(walltime = 24 * 3600, memory = 3 * 1024))

# Python jobs (separate session):
# ijoin(tab_py[repl > 1], findNotSubmitted()) |>
# 	submitJobs(resources = list(walltime = 24 * 3600, memory = 3 * 1024))
