# Script to run the importance experiment
# NOTE: R jobs and Python jobs must be submitted in separate R sessions.
# Loading R torch and Python PyTorch in the same session causes conflicts
# due to incompatible libtorch versions.
library(batchtools)

# Load registry
source("config-importance.R")
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
ids_py = findTagged("python")
tab_py = ijoin(tab, ids_py)
tab_r = ajoin(tab, ids_py)

# --- Step 1: Submit first replication as a smoke test ---
# Run ONE of the following blocks per session (not both!)

# R jobs (run in one session):
tab_first_r = ijon(findExperiments(repls = 1), tab_r)
tab_first_py = ijon(findExperiments(repls = 1), tab_py)

submitJobs(tab_first_r)

# Python jobs (run in a separate session):
# submitJobs(tab_first_py)

# --- Step 2: After verifying repl 1 results, submit remaining replications ---

# R jobs:
# ijoin(tab_r[repl > 1], findNotSubmitted()) |> submitJobs()

# Python jobs (separate session):
# ijoin(tab_py[repl > 1], findNotSubmitted()) |> submitJobs()
