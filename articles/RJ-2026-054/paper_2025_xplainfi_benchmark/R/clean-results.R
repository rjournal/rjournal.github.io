# Importance -------------------------------------------------------------

#' Keep only the most recent run for each experiment cell.
#'
#' Some experiments were run more than once across registry batches: the SAGE methods
#' were re-run after the `min_permutations` parameter was introduced, so those cells
#' carry two `job.id`s (an earlier one with `min_permutations = NA` and the current one
#' with `min_permutations = 20`). This keeps the newest run (largest `job.id`) per cell;
#' cells with a single run -- i.e. all other methods/packages -- are returned unchanged.
#' Safe to call on already-deduplicated data (it is then a no-op).
#' @param importances data.table with a `job.id` column and experiment identifiers.
dedup_latest_run = function(importances) {
	importances = data.table::as.data.table(importances)
	cell_keys = intersect(
		c("problem", "method", "package", "learner_type", "feature", "repl", "correlation"),
		names(importances)
	)
	if (!("job.id" %in% names(importances)) || length(cell_keys) == 0L) {
		return(importances)
	}
	data.table::setorderv(importances, "job.id", order = -1L)
	unique(importances, by = cell_keys)
}

#' Aggregate results from batchtools registry
#' @param results Result table as returned by reduceResultsDataTable(). Read from `here::here("results", "importance", "results.rds")` if NULL.
clean_results_importance = function(results, job_pars) {
	tmpres = data.table::rbindlist(results$result, fill = TRUE)
	tmpres = cbind(results[, .(job.id)], tmpres)
	res = ijoin(
		tmpres,
		job_pars[, .(
			job.id,
			repl,
			problem,
			algorithm,
			learner_type,
			# n_samples,
			# n_features,
			correlation,
			n_repeats,
			sampler,
			n_permutations,
			sage_n_samples,
			# SAGE-only parameter; the registry holds two SAGE runs (an earlier one
			# without `min_permutations` (NA) and the current one with `min_permutations = 20`).
			# Omitting it collapsed these distinct jobs into apparent duplicates.
			min_permutations
		)],
		by = "job.id"
	)

	# Minor issue where learner_performance var was incorrectly labelled in vip and iml
	if ("learner_performance.regr.rsq" %in% names(res)) {
		res[,
			learner_performance := fifelse(
				is.na(learner_performance),
				learner_performance.regr.rsq, # in vip, iml
				learner_performance # Regular variable name otherwise
			)
		]
		res[, learner_performance.regr.rsq := NULL]
	}

	# Metadata for grouping
	res[, let(
		method = fcase(
			startsWith(algorithm, "PFI")             , "PFI"   ,
			startsWith(algorithm, "CFI")             , "CFI"   ,
			startsWith(algorithm, "MarginalSAGE")    , "mSAGE" ,
			startsWith(algorithm, "ConditionalSAGE") , "cSAGE"
		),
		package = fcase(
			endsWith(algorithm, "_iml")   , "iml"   ,
			endsWith(algorithm, "_vip")   , "vip"   ,
			endsWith(algorithm, "_fippy") , "fippy" ,
			endsWith(algorithm, "_sage")  , "sage"  ,
			default = "xplainfi"
		)
	)]
	res[, package := factor(package, levels = c("xplainfi", "fippy", "vip", "iml", "sage"))]
	res[, method := factor(method, levels = c("PFI", "CFI", "mSAGE", "cSAGE"))]
	res[, learner_type := factor(learner_type, levels = c("linear", "rf", "boosting", "mlp"))]
	res[, algorithm_lab := sprintf("%s (%s)", method, package)]
	res[,
		problem_clean := fcase(
			problem == "correlated"   , sprintf("correlated (r=%.2f)", correlation) ,
			problem == "bike_sharing" , "bike sharing"                              ,
			default = problem
		)
	]
	res[,
		algorithm_lab := factor(
			algorithm_lab,
			levels = c(
				"PFI (xplainfi)",
				"PFI (iml)",
				"PFI (vip)",
				"CFI (xplainfi)",
				"CFI (fippy)",
				"PFI (fippy)",
				"mSAGE (xplainfi)",
				"mSAGE (sage)",
				"mSAGE (fippy)",
				"cSAGE (xplainfi)",
				"cSAGE (fippy)"
			)
		)
	]

	# res |> dplyr::count(algorithm, method, package, sampler)

	# Extract importances
	importances = rbindlist(
		lapply(results$job.id, \(x) {
			importances = results[job.id == x, result[[1]]$importance]
			importances[, job.id := x]
		}),
		fill = TRUE
	)
	# Add job parameters (algorithm, problem parameters, ...)
	importances = merge(res[, -"importance"], importances, by = "job.id")

	# The registry contains an earlier SAGE run (min_permutations = NA) superseded by
	# the current one (min_permutations = 20), so those cells have two jobs. Keep the
	# newest run (largest job.id) per experiment cell; cells with a single job (all
	# other methods/packages) are unaffected.
	importances = dedup_latest_run(importances)

	importances[, language := fifelse(package %in% c("fippy", "sage"), "Python", "R")]
	importances[,
		# Scaled to unit interval, within each job
		importance_scaled := (importance - min(importance)) / (max(importance) - min(importance)),
		by = .(job.id, algorithm, learner_type, problem, correlation)
	]
	# Ranks are also computed within each job
	importances[,
		# Rank such that highest importance -> rank 1
		importance_rank := frank(-importance, ties.method = "average"),
		by = .(job.id, algorithm, learner_type, problem, correlation)
	]

	checkmate::assert_numeric(importances$importance_scaled, lower = 0, upper = 1)
	# ranks should be 1 to p, bike sharing is problem with highest feature count, 12
	checkmate::assert_numeric(
		importances$importance_rank,
		lower = 1,
		upper = max(importances[, .(p = uniqueN(feature)), by = .(problem)][, p])
	)

	importances[]
}


# Runtime ----------------------------------------------------------------

clean_results_runtime = function(results, job_pars) {
	tmpres = data.table::rbindlist(results$result, fill = TRUE)
	tmpres = cbind(results[, .(job.id)], tmpres)
	tmpres[, scores := NULL]

	res = ijoin(
		tmpres,
		job_pars[, .(
			job.id,
			algorithm,
			learner_type,
			n_repeats,
			sampler,
			n_permutations,
			sage_n_samples,
			repl
		)],
		by = "job.id"
	)

	# Metadata for grouping
	res[, let(
		method = fcase(
			startsWith(algorithm, "PFI")             , "PFI"   ,
			startsWith(algorithm, "CFI")             , "CFI"   ,
			startsWith(algorithm, "MarginalSAGE")    , "mSAGE" ,
			startsWith(algorithm, "ConditionalSAGE") , "cSAGE"
		),
		package = fcase(
			endsWith(algorithm, "_iml")   , "iml"   ,
			endsWith(algorithm, "_vip")   , "vip"   ,
			endsWith(algorithm, "_fippy") , "fippy" ,
			endsWith(algorithm, "_sage")  , "sage"  ,
			default = "xplainfi"
		)
	)]
	res[, package := factor(package, levels = c("xplainfi", "fippy", "vip", "iml", "sage"))]
	res[, method := factor(method, levels = c("PFI", "CFI", "mSAGE", "cSAGE"))]
	# res[, learner_type := factor(learner_type, levels = c("linear", "featureless"))]
	res[, algorithm_lab := sprintf("%s (%s)", method, package)]
	res[, language := fifelse(package %in% c("fippy", "sage"), "Python", "R")]
	res[,
		algorithm_lab := factor(
			algorithm_lab,
			levels = c(
				"PFI (xplainfi)",
				"PFI (iml)",
				"PFI (vip)",
				"CFI (xplainfi)",
				"CFI (fippy)",
				"PFI (fippy)",
				"mSAGE (xplainfi)",
				"mSAGE (sage)",
				"mSAGE (fippy)",
				"cSAGE (xplainfi)",
				"cSAGE (fippy)"
			)
		)
	]

	res[]
}
