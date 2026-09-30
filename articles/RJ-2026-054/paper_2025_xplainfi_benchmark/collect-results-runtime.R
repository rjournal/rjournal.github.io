# Collect and process runtime benchmark results from batchtools registry.
# Produces the RDS files consumed by A02-results-runtime.Rmd.
library(batchtools)
library(data.table)
source(here::here("R", "clean-results.R"))
source(here::here("config-runtime.R"))

results_dir = here::here("results", "runtime")
fs::dir_create(results_dir)

file_results = fs::path(results_dir, "results", ext = "rds")
file_runtime = fs::path(results_dir, "runtime", ext = "rds")
file_job_pars = fs::path(results_dir, "jobs", ext = "rds")

# Step 1: Load registry and reduce raw results
if (!fs::file_exists(file_results)) {
	cli::cli_alert_info("Reducing results from registry at {.path {conf$reg_path}}")
	reg = suppressWarnings(loadRegistry(conf$reg_path, writeable = FALSE, work.dir = here::here()))
	tab = unwrap(getJobTable())
	saveRDS(tab, file_job_pars)

	results = reduceResultsDataTable(ids = findDone(tab))
	saveRDS(results, file_results)
	cli::cli_alert_success("Saved raw results to {.path {file_results}}")
} else {
	cli::cli_alert_info("Raw results already exist at {.path {file_results}}, skipping reduction")
}

# Step 2: Clean and process into runtime table
if (!fs::file_exists(file_runtime)) {
	cli::cli_alert_info("Processing runtime data")
	runtimes = clean_results_runtime(
		results = readRDS(file_results),
		job_pars = readRDS(file_job_pars)
	)
	saveRDS(runtimes, file_runtime)
	cli::cli_alert_success("Saved runtime data to {.path {file_runtime}}")
} else {
	cli::cli_alert_info("Runtime data already exists at {.path {file_runtime}}, skipping")
}
