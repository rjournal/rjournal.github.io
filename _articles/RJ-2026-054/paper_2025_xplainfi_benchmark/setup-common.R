# Package dependencies, will be checked for installation
local({
	packages = c(
		"xplainfi",
		"mlr3",
		"mlr3learners",
		"mlr3pipelines",
		"mlr3fselect",
		"mlr3torch",
		"reticulate",
		"batchtools",
		"mlbench",
		"mlr3data",
		"batchtools",
		"data.table",
		"checkmate",
		"digest",
		"iml",
		"vip",
		"ranger",
		"xgboost",
		"arf",
		"mvtnorm",
		"fs"
	)

	missing_pks = setdiff(packages, rownames(installed.packages()))

	if (length(missing_pks) > 0) {
		cli::cli_warn(c(
			"!" = "Not all required packages are installed, missing {.val {missing_pks}}",
			i = "Run {.code rv sync} in the terminal to sync dependencies.",
			"See README.md"
		))
	}
})


fs::dir_create(here::here("registries"))

library(batchtools)
library(mlr3)
library(data.table)
