# https://coolors.co/1e3888-ef476f-f5e663-ffad69-9c3848
pal_package = c(
	xplainfi = "#1e3888",
	fippy = "#8CD867",
	vip = "#A31621",
	iml = "#ffad69",
	sage = "#EF476F"
)

# Base-R stand-in for forcats::fct_rev, avoiding a dependency for one operation
fct_rev = function(x) {
	x = factor(x)
	factor(x, levels = rev(levels(x)))
}

#' Plot importances as box plots with feature on the y-axis and importance on the x-axis.
#'
#' @param importances ta.table of importance, produced by `clean_results_importance()`
#' @param type Type of importance, one of `"raw"` importances, `"scaled"` to [0, 1] (default), or `"rank"` (1 = most important).
#' @param problem,method,learner_type,feature character() specification of experiment parameter to select (one or more).
#'   If `NULL`, all available are used simulatenously.
#' @param color character(1): `"package"` Variable name to set as `color` (and `fill`) aesthetic in `ggplot2::aes()`
#' @param facets character(): `learner_type` Variable name(s) to facet by via `ggplot2::facet_wrap()`.
#' @param ncol,nrow integer(1) passed to `facet_wrap()`.
#' @param subtitle,caption logical(1): `TRUE` Toggle subtitle (showing method label) or caption (showing learner_type).
plot_importance = function(
	importances,
	type = c("scaled", "raw", "rank"),
	y_var = "feature",
	problem = NULL,
	method = NULL,
	learner_type = NULL,
	feature = NULL,
	color = "package",
	facets = "learner_type",
	ncol = NULL,
	nrow = NULL,
	title = FALSE,
	subtitle = FALSE,
	caption = FALSE,
	feature_sort = c("importance", "name"),
	multi_line = TRUE,
	base_size = 14
) {
	checkmate::assert_subset(problem, as.character(unique(importances$problem)))
	checkmate::assert_subset(method, as.character(unique(importances$method)))
	checkmate::assert_subset(
		learner_type,
		as.character(unique(importances$learner_type))
	)
	checkmate::assert_subset(color, names(importances))
	checkmate::assert_subset(facets, names(importances))
	type = match.arg(type)
	feature_sort = match.arg(feature_sort)

	target_var = switch(
		type,
		raw = "importance",
		scaled = "importance_scaled",
		rank = "importance_rank"
	)

	problem = problem %||% unique(importances$problem)
	method = method %||% unique(importances$method)
	learner_type = learner_type %||% unique(importances$learner_type)
	feature = feature %||% unique(importances$feature)

	importance_subset = importances |>
		dplyr::filter(
			.data[["problem"]] %in% .env[["problem"]],
			.data[["method"]] %in% .env[["method"]],
			.data[["learner_type"]] %in% .env[["learner_type"]],
			.data[["feature"]] %in% .env[["feature"]]
		)

	if ("correlated" %in% problem) {
		problem[problem == "correlated"] = sprintf(
			"%s (r=%s)",
			problem,
			paste0(unique(importances$correlation), collapse = ", ")
		)
	}

	problem_lab = glue::glue_collapse(problem, sep = ", ", last = ", and ")
	method_lab = glue::glue_collapse(method, sep = ", ", last = ", and ")
	learner_type_lab = glue::glue_collapse(
		learner_type,
		sep = ", ",
		last = ", and "
	)

	# cli::cli_alert_info("Problem = {.val {problem}}")
	# cli::cli_alert_info("Method = {.val {method}}")
	# cli::cli_alert_info("Learner = {.val {learner_type}}")

	title_lab = NULL
	if (title && !is.null(problem)) {
		title_lab = glue::glue("Problem: {problem_lab}")
	}

	subtitle_lab = NULL
	if (subtitle && !is.null(method)) {
		subtitle_lab = glue::glue("Method: {method_lab}")
	}
	caption_lab = NULL
	if (caption && !is.null(learner_type)) {
		glue::glue("Learner: {learner_type_lab}")
	}

	x_lab = "Importance"
	x_lab = switch(
		type,
		scaled = paste0(x_lab, " (scaled, %)"),
		rank = paste0(x_lab, " (ranks)"),
		x_lab
	)

	if (feature_sort == "importance") {
		importance_subset = importance_subset |>
			dplyr::mutate(feature = stats::reorder(feature, importance, FUN = median))
	} else if (feature_sort == "name") {
		importance_subset = importance_subset |>
			dplyr::mutate(feature = fct_rev(feature))
	}

	p = importance_subset |>
		ggplot(aes(
			x = .data[[target_var]],
			y = .data[[y_var]],
			color = .data[[color]],
			fill = .data[[color]]
		)) +
		geom_boxplot(alpha = 3 / 4, outlier.size = 1, outlier.alpha = .5) +
		labs(
			title = title_lab,
			subtitle = subtitle_lab,
			x = x_lab,
			y = "Feature",
			color = NULL,
			fill = NULL,
			caption = caption_lab
		) +
		theme_minimal(base_size = base_size) +
		theme(legend.position = "top", plot.title.position = "plot")

	if (type == "scaled") {
		# p <- p + scale_x_continuous(labels = scales::label_percent())
		p = p + scale_x_continuous(labels = scales::label_percent(suffix = ""))
	}

	if (!is.null(color)) {
		if (color == "package") {
			p = p +
				scale_fill_manual(values = pal_package, aesthetics = c("color", "fill"))
		} else {
			if (length(unique(importances[[color]])) <= 5) {
				p = p +
					scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color"))
			} else {
				p = p + scale_fill_viridis_d(aesthetics = c("fill", "color"))
			}
		}
	}

	if (length(facets) > 0) {
		p = p +
			facet_wrap(
				facets = facets,
				dir = "h",
				ncol = ncol,
				nrow = nrow,
				labeller = label_wrap_gen(multi_line = multi_line)
			)
	}

	p
}

#' Plot pairwise differences in importance between a baseline package and each
#' reference implementation, paired within replication (i.e., on identical datasets).
#'
#' For each experimental cell (problem, method, learner, feature, replication, and
#' correlation where applicable), the importance of `baseline` is subtracted from
#' each reference package's importance. Boxplots show the distribution of these
#' paired differences across replications; values centered on zero indicate agreement.
#'
#' @param importances data.table of importances, produced by `clean_results_importance()`.
#' @param type One of `"scaled"` (differences of importances scaled to [0, 1] per job,
#'   i.e. percentage points; default) or `"raw"` (differences of raw importance scores).
#' @param problem,method,learner_type,feature character() specification of experiment
#'   parameter to select (one or more). If `NULL`, all available are used.
#' @param baseline character(1) Package used as the minuend (default `"xplainfi"`).
#' @param exclude_packages character() Packages dropped before pairing (default `"vip"`,
#'   whose RMSE-based PFI is not directly comparable on the MSE scale).
#' @param facets character() Variable name(s) to facet by via `ggplot2::facet_wrap()`.
#' @param ncol,nrow integer(1) passed to `facet_wrap()`.
#' @param title logical(1) Toggle title (showing problem label).
#' @param feature_sort One of `"importance"` (order by spread of differences) or `"name"`.
#' @param multi_line logical(1) passed to the facet labeller.
#' @param base_size numeric(1) Base font size for theme.
plot_importance_diff = function(
	importances,
	type = c("scaled", "raw"),
	problem = NULL,
	method = NULL,
	learner_type = NULL,
	feature = NULL,
	baseline = "xplainfi",
	exclude_packages = "vip",
	facets = c("method", "learner_type"),
	ncol = NULL,
	nrow = NULL,
	title = FALSE,
	feature_sort = c("importance", "name"),
	multi_line = TRUE,
	base_size = 14
) {
	checkmate::assert_subset(problem, as.character(unique(importances$problem)))
	checkmate::assert_subset(method, as.character(unique(importances$method)))
	checkmate::assert_subset(
		learner_type,
		as.character(unique(importances$learner_type))
	)
	type = match.arg(type)
	feature_sort = match.arg(feature_sort)
	value_col = switch(type, scaled = "importance_scaled", raw = "importance")

	dat = data.table::as.data.table(importances)

	sel_problem = problem %||% unique(as.character(dat$problem))
	sel_method = method %||% unique(as.character(dat$method))
	sel_learner = learner_type %||% unique(as.character(dat$learner_type))
	sel_feature = feature %||% unique(as.character(dat$feature))
	sel_exclude = exclude_packages

	dat = dat[
		as.character(problem) %in%
			sel_problem &
			as.character(method) %in% sel_method &
			as.character(learner_type) %in% sel_learner &
			as.character(feature) %in% sel_feature &
			!(as.character(package) %in% sel_exclude)
	]

	key = intersect(
		c("problem", "method", "learner_type", "feature", "repl", "correlation"),
		names(dat)
	)

	# Collapse any duplicate measurements per cell + package to a single value
	dat = dat[,
		.(value = mean(get(value_col), na.rm = TRUE)),
		by = c(key, "package")
	]

	wide = data.table::dcast(
		dat,
		stats::as.formula(paste(paste(key, collapse = " + "), "~ package")),
		value.var = "value"
	)

	if (!baseline %in% names(wide)) {
		stop("Baseline package '", baseline, "' not present in the data.")
	}
	refs = setdiff(unique(as.character(dat$package)), baseline)

	diff_long = data.table::rbindlist(lapply(refs, function(pkg) {
		w = wide[!is.na(get(baseline)) & !is.na(get(pkg))]
		if (!nrow(w)) {
			return(NULL)
		}
		out = w[, ..key]
		out[, package := pkg]
		out[, diff := w[[baseline]] - w[[pkg]]]
		out[]
	}))

	if (is.null(diff_long) || !nrow(diff_long)) {
		stop("No paired observations found for the requested selection.")
	}

	# Keep package colouring consistent with the rest of the report
	diff_long[, package := factor(package, levels = intersect(names(pal_package), unique(package)))]

	if (feature_sort == "importance") {
		diff_long[, feature := stats::reorder(feature, abs(diff), FUN = median)]
	} else {
		diff_long[, feature := fct_rev(feature)]
	}

	title_lab = NULL
	if (title) {
		problem_lab = glue::glue_collapse(problem, sep = ", ", last = ", and ")
		title_lab = glue::glue("Problem: {problem_lab}")
	}

	x_lab = switch(
		type,
		scaled = glue::glue("Scaled importance difference ({baseline} − reference, %)"),
		raw = glue::glue("Raw importance difference ({baseline} − reference)")
	)

	p = ggplot(
		diff_long,
		aes(x = diff, y = feature, color = package, fill = package)
	) +
		geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
		geom_boxplot(alpha = 3 / 4, outlier.size = 1, outlier.alpha = .5) +
		labs(
			title = title_lab,
			x = x_lab,
			y = "Feature",
			color = NULL,
			fill = NULL
		) +
		scale_fill_manual(values = pal_package, aesthetics = c("color", "fill")) +
		theme_minimal(base_size = base_size) +
		theme(legend.position = "top", plot.title.position = "plot")

	if (type == "scaled") {
		# Scaled importances live in [0, 1]; show differences as percentage points
		p = p + scale_x_continuous(labels = scales::label_percent(suffix = ""))
	}

	if (length(facets) > 0) {
		p = p +
			facet_wrap(
				facets = facets,
				dir = "h",
				ncol = ncol,
				nrow = nrow,
				# Scaled differences share the [-100, 100] pp range, so a common axis
				# aids comparison; raw magnitudes differ across methods, so free them.
				scales = if (type == "scaled") "fixed" else "free_x",
				labeller = label_wrap_gen(multi_line = multi_line)
			)
	}

	p
}

#' Plot runtime as box plots with algorithm on the y-axis and runtime on the x-axis.
#'
#' @param runtimes data.table of runtime data, produced by `clean_results_runtime()`
#' @param scale character(1) Scale type: "seconds", "log10 seconds", "relative", or "log10 relative".
#'   Relative scales compute runtime relative to xplainfi within each replication.
#' @param method,package,learner_type,sampler character() Filter by these variables. NULL uses all.
#' @param n_samples,n_features,n_permutations,sage_n_samples Filter by these numeric variables. NULL uses all.
#' @param color character(1) Variable name to color by.
#' @param facets character() Variable names to facet by.
#' @param ncol,nrow integer(1) Passed to `facet_wrap()`.
#' @param logscale logical(1) Whether to use log scale on x-axis (runtime).
#' @param show_legend logical(1) Whether to show the legend.
#' @param base_size numeric(1) Base font size for theme.
#' @return A ggplot2 object.
plot_runtime = function(
	runtimes,
	scale = c("seconds", "relative"),
	method = NULL,
	package = NULL,
	learner_type = NULL,
	sampler = NULL,
	n_samples = NULL,
	n_features = NULL,
	n_permutations = NULL,
	sage_n_samples = NULL,
	color = "package",
	facets = c("n_samples", "n_features"),
	ncol = 2,
	nrow = NULL,
	title = FALSE,
	logscale = FALSE,
	show_legend = TRUE,
	base_size = 16
) {
	scale = match.arg(scale)
	data = data.table::copy(runtimes)

	# Apply filters using dplyr::filter with .data/.env pronouns (like plot_importance)
	method = method %||% unique(data$method)
	package = package %||% unique(data$package)
	learner_type = learner_type %||% unique(data$learner_type)
	sampler = sampler %||% unique(data$sampler)
	n_samples = n_samples %||% unique(data$n_samples)
	n_features = n_features %||% unique(data$n_features)
	n_permutations = n_permutations %||% unique(data$n_permutations)
	sage_n_samples = sage_n_samples %||% unique(data$sage_n_samples)

	data = data |>
		dplyr::filter(
			.data[["method"]] %in% .env[["method"]],
			.data[["package"]] %in% .env[["package"]],
			.data[["learner_type"]] %in% .env[["learner_type"]],
			is.na(.data[["sampler"]]) | .data[["sampler"]] %in% .env[["sampler"]],
			.data[["n_samples"]] %in% .env[["n_samples"]],
			.data[["n_features"]] %in% .env[["n_features"]],
			is.na(.data[["n_permutations"]]) |
				.data[["n_permutations"]] %in% .env[["n_permutations"]],
			is.na(.data[["sage_n_samples"]]) |
				.data[["sage_n_samples"]] %in% .env[["sage_n_samples"]]
		) |>
		data.table::as.data.table()

	# Log filter selections
	cli::cli_alert_info("Methods: {.val {unique(data$method)}}")
	cli::cli_alert_info("Packages: {.val {unique(data$package)}}")
	cli::cli_alert_info("Learners: {.val {unique(data$learner_type)}}")
	cli::cli_alert_info("n_samples: {.val {sort(unique(data$n_samples))}}")
	cli::cli_alert_info("n_features: {.val {sort(unique(data$n_features))}}")

	# Set plot variable and axis label based on scale type
	plot_var = "runtime"
	x_lab = if (scale == "relative") {
		if (logscale) {
			"Runtime (relative to xplainfi, log10)"
		} else {
			"Runtime (relative to xplainfi)"
		}
	} else {
		if (logscale) "Runtime (seconds, log10)" else "Runtime (seconds)"
	}

	# Compute relative runtime if needed
	if (scale %in% c("relative", "log10 relative")) {
		group_cols = c(
			"method",
			"learner_type",
			"n_samples",
			"n_features",
			"sampler",
			"n_permutations",
			"sage_n_samples",
			"repl"
		)
		group_cols = intersect(group_cols, names(data))

		baseline = data[
			package == "xplainfi",
			.(baseline_runtime = runtime[1]),
			by = group_cols
		]

		data = merge(data, baseline, by = group_cols, all.x = TRUE)
		data[, runtime_relative := runtime / baseline_runtime]
		plot_var = "runtime_relative"
	}

	# Convert numeric columns to factors for proper faceting/coloring
	data[, n_samples := factor(n_samples)]
	data[, n_features := factor(n_features)]
	data[, n_permutations := factor(n_permutations)]
	data[, sage_n_samples := factor(sage_n_samples)]

	# Create algorithm label for y-axis
	data[, algo_label := sprintf("%s (%s)", method, package)]

	# Order by median runtime
	algo_order = data[,
		.(med_rt = median(get(plot_var), na.rm = TRUE)),
		by = algo_label
	]
	algo_order = algo_order[order(med_rt)]
	data[, algo_label := factor(algo_label, levels = algo_order$algo_label)]

	# Build plot
	p = ggplot(
		data,
		aes(
			x = .data[[plot_var]],
			y = algo_label,
			fill = .data[[color]],
			color = .data[[color]]
		)
	) +
		geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
		labs(
			title = if (title) "Runtime Comparison" else NULL,
			x = x_lab,
			y = NULL,
			fill = NULL,
			color = NULL
		) +
		theme_minimal(base_size = base_size) +
		theme(
			legend.position = if (show_legend) "top" else "none",
			plot.title.position = "plot"
		)

	# Color palette
	if (color == "package") {
		p = p +
			scale_fill_manual(values = pal_package, aesthetics = c("fill", "color"))
	} else {
		p = p +
			scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color"))
	}

	# Add faceting
	if (length(facets) > 0) {
		p = p +
			facet_wrap(
				facets = facets,
				ncol = ncol,
				nrow = nrow,
				labeller = label_both
			)
	}

	# X-axis scaling with pretty labels
	if (scale == "relative") {
		p = p + geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5)
		if (logscale) {
			p = p + scale_x_log10(labels = scales::label_number(suffix = "x"))
		} else {
			p = p + scale_x_continuous(labels = scales::label_number(suffix = "x"))
		}
	} else {
		if (logscale) {
			p = p + scale_x_log10(labels = scales::label_number())
		} else {
			p = p + scale_x_continuous(labels = scales::label_number())
		}
	}

	p
}

#' Plot runtime scaling as a function of a scaling variable (e.g., n_features or n_samples).
#'
#' @param runtimes data.table of runtime data, produced by `clean_results_runtime()`
#' @param x_var character(1) Variable to plot on x-axis, typically "n_features" or "n_samples".
#' @param geom character(1) Geom type: "boxplot" or "pointrange" (mean + quantile-based error bars).
#' @param method,package,learner_type,sampler character() Filter by these variables. NULL uses all.
#' @param n_samples,n_features,n_permutations,sage_n_samples Filter by these numeric variables. NULL uses all.
#' @param color character(1) Variable name to color by.
#' @param facets character() Variable names to facet by.
#' @param ncol,nrow integer(1) Passed to `facet_wrap()`.
#' @param logscale logical(1) Whether to use log scale on y-axis.
#' @param show_legend logical(1) Whether to show the legend.
#' @param labeller function, Defaults to `label_value`. Passed to `facet_wrap()`.
#' @param base_size numeric(1) Base font size for theme.
#' @return A ggplot2 object.
plot_runtime_scaling = function(
	runtimes,
	x_var = c("n_features", "n_samples"),
	geom = c("boxplot", "pointrange"),
	method = NULL,
	package = NULL,
	learner_type = NULL,
	sampler = NULL,
	n_samples = NULL,
	n_features = NULL,
	n_permutations = NULL,
	sage_n_samples = NULL,
	color = "package",
	facets = "method",
	ncol = NULL,
	nrow = NULL,
	logscale = FALSE,
	title = FALSE,
	show_legend = TRUE,
	labeller = label_value,
	base_size = 14
) {
	x_var = match.arg(x_var)
	geom = match.arg(geom)
	data = data.table::copy(runtimes)

	# Apply filters using dplyr::filter with .data/.env pronouns
	method = method %||% unique(data$method)
	package = package %||% unique(data$package)
	learner_type = learner_type %||% unique(data$learner_type)
	sampler = sampler %||% unique(data$sampler)
	n_samples = n_samples %||% unique(data$n_samples)
	n_features = n_features %||% unique(data$n_features)
	n_permutations = n_permutations %||% unique(data$n_permutations)
	sage_n_samples = sage_n_samples %||% unique(data$sage_n_samples)

	data = data |>
		dplyr::filter(
			.data[["method"]] %in% .env[["method"]],
			.data[["package"]] %in% .env[["package"]],
			.data[["learner_type"]] %in% .env[["learner_type"]],
			is.na(.data[["sampler"]]) | .data[["sampler"]] %in% .env[["sampler"]],
			.data[["n_samples"]] %in% .env[["n_samples"]],
			.data[["n_features"]] %in% .env[["n_features"]],
			is.na(.data[["n_permutations"]]) |
				.data[["n_permutations"]] %in% .env[["n_permutations"]],
			is.na(.data[["sage_n_samples"]]) |
				.data[["sage_n_samples"]] %in% .env[["sage_n_samples"]]
		) |>
		data.table::as.data.table()

	# Log filter selections
	# cli::cli_alert_info("Methods: {.val {unique(data$method)}}")
	# cli::cli_alert_info("Packages: {.val {unique(data$package)}}")
	# cli::cli_alert_info("Learners: {.val {unique(data$learner_type)}}")
	# cli::cli_alert_info("n_samples: {.val {sort(unique(data$n_samples))}}")
	# cli::cli_alert_info("n_features: {.val {sort(unique(data$n_features))}}")
	# cli::cli_alert_info("n_repeats: {.val {sort(unique(data$n_repeats))}}")

	# Convert x_var to factor for proper ordering
	data[, (x_var) := factor(get(x_var))]

	# x-axis label
	x_lab = switch(
		x_var,
		n_features = "Number of features",
		n_samples = "Number of samples"
	)

	y_lab = if (logscale) "Runtime (seconds, log10)" else "Runtime (seconds)"

	# Build plot
	p = ggplot(
		data,
		aes(
			x = .data[[x_var]],
			y = runtime,
			fill = .data[[color]],
			color = .data[[color]]
		)
	)

	if (geom == "boxplot") {
		p = p +
			geom_boxplot(
				alpha = 0.7,
				outlier.size = 0.8,
				position = position_dodge(width = 0.5)
			)
	} else if (geom == "pointrange") {
		# Compute summary statistics
		summary_data = data[,
			.(
				mean_runtime = median(runtime, na.rm = TRUE),
				lower = quantile(runtime, 0.25, na.rm = TRUE),
				upper = quantile(runtime, 0.75, na.rm = TRUE)
			),
			by = c(x_var, color, facets)
		]

		p = ggplot(
			summary_data,
			aes(
				x = .data[[x_var]],
				y = mean_runtime,
				ymin = lower,
				ymax = upper,
				color = .data[[color]],
				group = .data[[color]]
			)
		) +
			geom_pointrange(position = position_dodge(width = 0.2), size = 0.5) +
			geom_line(position = position_dodge(width = 0.2), alpha = 0.5)
	}

	p = p +
		labs(
			title = if (title) sprintf("Runtime scaling by %s", x_lab) else NULL,
			x = x_lab,
			y = y_lab,
			fill = NULL,
			color = NULL
		) +
		theme_minimal(base_size = base_size) +
		theme(
			legend.position = if (show_legend) "top" else "none",
			plot.title.position = "plot"
		)

	# Color palette
	if (color == "package") {
		p = p +
			scale_fill_manual(values = pal_package, aesthetics = c("fill", "color"))
	} else {
		p = p +
			scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color"))
	}

	# Y-axis scaling
	if (logscale) {
		p = p + scale_y_log10(labels = scales::label_number())
	} else {
		p = p + scale_y_continuous(labels = scales::label_number())
	}

	# Add faceting
	if (length(facets) > 0) {
		p = p +
			facet_wrap(
				facets = facets,
				ncol = ncol,
				nrow = nrow,
				labeller = labeller,
				scales = "free_y"
			)
	}

	p
}
