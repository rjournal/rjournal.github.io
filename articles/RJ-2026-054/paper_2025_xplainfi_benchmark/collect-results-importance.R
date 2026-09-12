# Collect and process importance benchmark results from batchtools registry.
# Produces the RDS files consumed by A01-results-importance.Rmd.
library(batchtools)
library(data.table)
source(here::here("R", "clean-results.R"))
source(here::here("config-importance.R"))

results_dir = here::here("results", "importance")
fs::dir_create(results_dir)

file_results = fs::path(results_dir, "results", ext = "rds")
file_importance = fs::path(results_dir, "importances", ext = "rds")
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

# Step 2: Clean and process into importance table
if (!fs::file_exists(file_importance)) {
	cli::cli_alert_info("Processing importance scores")
	importances = clean_results_importance(
		results = readRDS(file_results),
		job_pars = readRDS(file_job_pars)
	)
	# Don't store individual scores after all — bloats file size for little benefit
	saveRDS(copy(importances)[, scores := NULL], file_importance)
	cli::cli_alert_success("Saved importances to {.path {file_importance}}")
} else {
	cli::cli_alert_info("Importances already exist at {.path {file_importance}}, skipping")
}
