# Algorithm definitions for batchtools experiment

# ============================================================================
# PFI - Permutation Feature Importance
# ============================================================================

algo_PFI = function(data = NULL, job = NULL, instance, n_repeats = 1) {
	# Create learner for this algorithm
	learner = create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	method = PFI$new(
		task = instance$task,
		learner = learner,
		measure = instance$measure,
		resampling = instance$resampling,
		n_repeats = n_repeats
	)

	start_time = Sys.time()
	method$compute()
	end_time = Sys.time()

	data.table::data.table(
		importance = list(method$importance()),
		scores = list(method$scores()),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = method$resample_result$aggregate(instance$measure_eval),
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# CFI - Conditional Feature Importance
# ============================================================================

algo_CFI = function(
	data = NULL,
	job = NULL,
	instance,
	n_repeats = 1,
	sampler = "arf"
) {
	# Create learner for this algorithm
	learner = create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	# Create sampler instance
	sampler_instance = create_sampler(sampler = sampler, task = instance$task)

	method = CFI$new(
		task = instance$task,
		learner = learner,
		measure = instance$measure,
		resampling = instance$resampling,
		sampler = sampler_instance,
		n_repeats = n_repeats
	)

	start_time = Sys.time()
	method$compute()
	end_time = Sys.time()

	data.table::data.table(
		importance = list(method$importance()),
		scores = list(method$scores()),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = method$resample_result$aggregate(instance$measure_eval),
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}

# ============================================================================
# MarginalSAGE - Marginal SAGE
# ============================================================================

algo_MarginalSAGE = function(
	data = NULL,
	job = NULL,
	instance,
	n_permutations = 10,
	sage_n_samples = 200,
	batch_size = 10000,
	early_stopping = TRUE,
	min_permutations = 20
) {
	# Create learner for this algorithm
	learner = create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	method = MarginalSAGE$new(
		task = instance$task,
		learner = learner,
		measure = instance$measure,
		resampling = instance$resampling,
		n_permutations = n_permutations,
		n_samples = sage_n_samples,
		batch_size = batch_size,
		early_stopping = early_stopping,
		min_permutations = min_permutations
	)

	start_time = Sys.time()
	method$compute()
	end_time = Sys.time()

	data.table::data.table(
		importance = list(method$importance()),
		scores = list(method$scores()),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = method$resample_result$aggregate(instance$measure_eval),
		n_permutations_used = method$n_permutations_used,
		converged = method$converged,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# ConditionalSAGE - Conditional SAGE
# ============================================================================

algo_ConditionalSAGE = function(
	data = NULL,
	job = NULL,
	instance,
	n_permutations = 10,
	sage_n_samples = 200,
	sampler = "arf",
	batch_size = 10000,
	early_stopping = TRUE,
	min_permutations = 20
) {
	# Create learner for this algorithm
	learner = create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	# Create sampler instance
	sampler_instance = create_sampler(sampler = sampler, task = instance$task)

	method = ConditionalSAGE$new(
		task = instance$task,
		learner = learner,
		measure = instance$measure,
		resampling = instance$resampling,
		sampler = sampler_instance,
		n_permutations = n_permutations,
		n_samples = sage_n_samples,
		batch_size = batch_size,
		early_stopping = early_stopping,
		min_permutations = min_permutations
	)

	start_time = Sys.time()
	method$compute()
	end_time = Sys.time()

	data.table::data.table(
		importance = list(method$importance()),
		scores = list(method$scores()),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = method$resample_result$aggregate(instance$measure_eval),
		n_permutations_used = method$n_permutations_used,
		converged = method$converged,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}

# ============================================================================
# PFI_iml - Reference implementation from iml package
# ============================================================================

algo_PFI_iml = function(data = NULL, job = NULL, instance, n_repeats = 1) {
	require(iml)

	# iml requires a trained model, so we need to train first
	# Use the first resampling iteration (train/test split)
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Create learner for this algorithm
	learner = create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	# Clone learner to avoid modifying the instance
	learner_clone = learner$clone(deep = TRUE)

	# Use resample() with the existing resampling to properly handle XGBoost early stopping
	# This ensures early stopping uses the test set for validation
	resample_result = resample(
		instance$task,
		learner_clone,
		instance$resampling,
		store_models = TRUE
	)

	# Extract the trained learner from first iteration (early stopping applied)
	learner_clone = resample_result$learners[[1]]

	# Create iml Predictor object
	# iml expects a predict function that returns predictions
	predictor = Predictor$new(
		model = learner_clone,
		data = instance$task$data(
			rows = test_ids,
			cols = instance$task$feature_names
		),
		y = instance$task$data(
			rows = test_ids,
			cols = instance$task$target_names
		)[[1]],
		predict.function = function(model, newdata) {
			# Create temporary task for prediction
			temp_task = instance$task$clone()
			temp_task$select(colnames(newdata))
			# Predict and return as vector
			preds = model$predict_newdata(newdata, task = temp_task)
			if (instance$task_type == "classif") {
				# For classification, iml expects probabilities for the positive class
				# or just the response for binary
				preds$response
			} else {
				preds$response
			}
		}
	)

	perf = resample_result$score(instance$measure_eval)[,
		.SD,
		.SDcols = mlr3misc::ids(c(instance$measure_eval))
	]

	start_time = Sys.time()
	# Create FeatureImp object
	# iml computes importance on initialization
	imp = FeatureImp$new(
		predictor = predictor,
		loss = ifelse(instance$task_type == "regr", "mse", "ce"),
		n.repetitions = n_repeats,
		compare = "difference" # Difference between permuted and original loss (matches xplainfi)
	)
	end_time = Sys.time()

	imp_results = imp$results

	# Convert iml results to standard format
	# iml returns: feature, importance (difference), importance.05, importance.95
	importance_dt = data.table::data.table(
		feature = imp_results$feature,
		importance = imp_results$importance
	)

	data.table::data.table(
		importance = list(importance_dt),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = perf[[1]],
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# PFI_vip - Reference implementation from vip package
# ============================================================================

algo_PFI_vip = function(data = NULL, job = NULL, instance, n_repeats = 1) {
	require(vip)

	# vip requires a trained model, so we need to train first
	# Use the first resampling iteration (train/test split)
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Create learner for this algorithm
	learner = create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	# Clone learner to avoid modifying the instance
	learner_clone = learner$clone(deep = TRUE)

	# Use resample() with the existing resampling to properly handle XGBoost early stopping
	# This ensures early stopping uses the test set for validation
	resample_result = resample(
		instance$task,
		learner_clone,
		instance$resampling,
		store_models = TRUE
	)

	# Extract the trained learner from first iteration (early stopping applied)
	learner_clone = resample_result$learners[[1]]

	# Prepare data for vip
	test_data = instance$task$data(rows = test_ids)
	target_name = instance$task$target_names

	# Determine metric based on task type
	# Doesn't support MSE accoridng to vip::list_metrics()
	metric = if (instance$task_type == "regr") "rmse" else "accuracy"

	# Create wrapper predict function for vip
	# vip expects a function(object, newdata) that returns predictions
	pred_wrapper = function(object, newdata) {
		# Create temporary task for prediction
		temp_task = instance$task$clone()

		# Predict using predict_newdata_fast if available
		if (is.function(object$predict_newdata_fast)) {
			preds = object$predict_newdata_fast(newdata, task = temp_task)
		} else {
			preds = object$predict_newdata(newdata, task = temp_task)
		}

		# For regression, return numeric predictions
		# class predictions for classif
		preds$response
	}

	perf = resample_result$score(instance$measure_eval)[,
		.SD,
		.SDcols = mlr3misc::ids(c(instance$measure_eval))
	]

	start_time = Sys.time()

	# Compute permutation importance using vip
	# vip uses nsim for number of permutations
	imp_results = vip::vi(
		object = learner_clone,
		method = "permute",
		train = test_data,
		target = target_name,
		metric = metric,
		nsim = n_repeats,
		pred_wrapper = pred_wrapper
	)

	end_time = Sys.time()

	# Convert vip results to standard format
	# vip returns: Variable, Importance
	importance_dt = data.table::as.data.table(imp_results)
	data.table::setnames(
		importance_dt,
		c("Variable", "Importance"),
		c("feature", "importance")
	)

	data.table::data.table(
		# if n_repeats > 1, there's a StDev column we don't keep
		importance = list(importance_dt[, .(feature, importance)]),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = perf[[1]],
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# PFI_fippy - Reference implementation from fippy package (Python)
# ============================================================================

algo_PFI_fippy = function(data = NULL, job = NULL, instance, n_repeats = 1, sampler = "simple") {
	# Use first resampling iteration
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
	sklearn_data = task_to_sklearn(
		instance$task,
		train_ids,
		test_ids,
		as_pandas = TRUE
	)

	# Create and train sklearn learner (with encoding if categoricals present)
	sklearn_learner = create_sklearn_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		encode = instance$has_categoricals,
		random_state = job$seed
	)

	fit_sklearn_learner(
		sklearn_learner,
		sklearn_data$X_train,
		sklearn_data$y_train,
		sklearn_data$X_test,
		sklearn_data$y_test
	)

	# Import fippy and create sampler using helper function
	fippy = reticulate::import("fippy")
	sklearn_metrics = reticulate::import("sklearn.metrics")

	# Calculate learner performance on test set
	test_predictions = sklearn_learner$predict(sklearn_data$X_test)
	learner_performance = if (instance$task_type == "regr") {
		as.numeric(sklearn_metrics$r2_score(sklearn_data$y_test, test_predictions))
	} else {
		as.numeric(sklearn_metrics$accuracy_score(sklearn_data$y_test, test_predictions))
	}

	sampler_obj = create_fippy_sampler(
		task = instance$task,
		X_train_pandas = sklearn_data$X_train,
		sampler = sampler
	)

	# For regression use MSE, for classification use zero-one loss (classification error)
	# Note: Using predict (labels) for both, not predict_proba
	loss_fn = if (instance$task_type == "regr") {
		sklearn_metrics$mean_squared_error
	} else {
		sklearn_metrics$zero_one_loss
	}

	# Create a wrapper function for the predict method to handle potential conversion issues
	predict_wrapper = function(X) {
		sklearn_learner$predict(X)
	}

	explainer = fippy$Explainer(
		predict = predict_wrapper,
		X_train = sklearn_data$X_train,
		loss = loss_fn,
		sampler = sampler_obj
	)

	start_time = Sys.time()

	# Compute PFI using fippy
	pfi_result = explainer$pfi(
		X_eval = sklearn_data$X_test,
		y_eval = sklearn_data$y_test,
		nr_runs = as.integer(n_repeats)
	)

	end_time = Sys.time()

	# Convert to standard format
	# fippy returns an explanation object with fi_means_stds() method
	fi_series = pfi_result$fi_means_stds()
	importance_dt = data.table::data.table(
		feature = instance$task$feature_names,
		importance = sapply(instance$task$feature_names, function(f) {
			as.numeric(fi_series[[f]])
		})
	)

	data.table::data.table(
		importance = list(importance_dt),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = learner_performance,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# CFI_fippy - Conditional Feature Importance from fippy (Python, Gaussian sampler)
# ============================================================================

algo_CFI_fippy = function(data = NULL, job = NULL, instance, n_repeats = 1, sampler = "gaussian") {
	# Use first resampling iteration
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
	sklearn_data = task_to_sklearn(
		instance$task,
		train_ids,
		test_ids,
		as_pandas = TRUE
	)

	# Create and train sklearn learner (with encoding if categoricals present)
	sklearn_learner = create_sklearn_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		encode = instance$has_categoricals,
		random_state = job$seed
	)

	fit_sklearn_learner(
		sklearn_learner,
		sklearn_data$X_train,
		sklearn_data$y_train,
		sklearn_data$X_test,
		sklearn_data$y_test
	)

	# Import fippy and create sampler using helper function
	fippy = reticulate::import("fippy")
	sklearn_metrics = reticulate::import("sklearn.metrics")

	# Calculate learner performance on test set
	test_predictions = sklearn_learner$predict(sklearn_data$X_test)
	learner_performance = if (instance$task_type == "regr") {
		as.numeric(sklearn_metrics$r2_score(sklearn_data$y_test, test_predictions))
	} else {
		as.numeric(sklearn_metrics$accuracy_score(sklearn_data$y_test, test_predictions))
	}

	sampler_obj = create_fippy_sampler(
		task = instance$task,
		X_train_pandas = sklearn_data$X_train,
		sampler = sampler
	)

	# For regression use MSE, for classification use zero-one loss (classification error)
	# Note: Using predict (labels) for both, not predict_proba
	loss_fn = if (instance$task_type == "regr") {
		sklearn_metrics$mean_squared_error
	} else {
		sklearn_metrics$zero_one_loss
	}

	# Create a wrapper function for the predict method to handle potential conversion issues
	predict_wrapper = function(X) {
		sklearn_learner$predict(X)
	}

	explainer = fippy$Explainer(
		predict = predict_wrapper,
		X_train = sklearn_data$X_train,
		loss = loss_fn,
		sampler = sampler_obj
	)

	start_time = Sys.time()

	# Compute CFI using fippy
	cfi_result = explainer$cfi(
		X_eval = sklearn_data$X_test,
		y_eval = sklearn_data$y_test,
		nr_runs = as.integer(n_repeats)
	)

	end_time = Sys.time()

	# Convert to standard format
	# fippy returns an explanation object with fi_means_stds() method
	fi_series = cfi_result$fi_means_stds()
	importance_dt = data.table::data.table(
		feature = instance$task$feature_names,
		importance = sapply(instance$task$feature_names, function(f) {
			as.numeric(fi_series[[f]])
		})
	)

	data.table::data.table(
		importance = list(importance_dt),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = learner_performance,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# MarginalSAGE_fippy - Marginal SAGE from fippy package (Python)
# ============================================================================

algo_MarginalSAGE_fippy = function(
	data = NULL,
	job = NULL,
	instance,
	n_permutations = 10,
	sage_n_samples = 10,
	sampler = "simple",
	early_stopping = TRUE,
	min_permutations = 20
) {
	# Use first resampling iteration
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Ensure Python packages (including pandas) are available before data conversion
	.ensure_python_packages()

	# Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
	sklearn_data = task_to_sklearn(
		instance$task,
		train_ids,
		test_ids,
		as_pandas = TRUE
	)

	# Create and train sklearn learner (with encoding if categoricals present)
	sklearn_learner = create_sklearn_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		encode = instance$has_categoricals,
		random_state = job$seed
	)

	fit_sklearn_learner(
		sklearn_learner,
		sklearn_data$X_train,
		sklearn_data$y_train,
		sklearn_data$X_test,
		sklearn_data$y_test
	)

	# Import fippy and create sampler using helper function
	fippy = reticulate::import("fippy")
	sklearn_metrics = reticulate::import("sklearn.metrics")

	# Calculate learner performance on test set
	test_predictions = sklearn_learner$predict(sklearn_data$X_test)
	learner_performance = if (instance$task_type == "regr") {
		as.numeric(sklearn_metrics$r2_score(sklearn_data$y_test, test_predictions))
	} else {
		as.numeric(sklearn_metrics$accuracy_score(sklearn_data$y_test, test_predictions))
	}

	sampler_obj = create_fippy_sampler(
		task = instance$task,
		X_train_pandas = sklearn_data$X_train,
		sampler = sampler
	)
	# For regression use MSE, for classification use zero-one loss (classification error)
	# Note: Using predict (labels) for both, not predict_proba
	loss_fn = if (instance$task_type == "regr") {
		sklearn_metrics$mean_squared_error
	} else {
		sklearn_metrics$zero_one_loss
	}

	# Create a wrapper function for the predict method to handle potential conversion issues
	predict_wrapper = function(X) {
		sklearn_learner$predict(X)
	}

	explainer = fippy$Explainer(
		predict = predict_wrapper,
		X_train = sklearn_data$X_train,
		loss = loss_fn,
		sampler = sampler_obj
	)

	start_time = Sys.time()

	# Compute Marginal SAGE using fippy
	# nr_orderings: number of permutation orderings (like n_permutations in xplainfi)
	# nr_runs: how often each value function is computed (no xplainfi equivalent, use default 1)
	# nr_resample_marginalize: number of samples for Monte Carlo integration (like n_samples in xplainfi)
	# detect_convergence: use convergence detection for optimal results (matches KernelSAGE approach)
	# Returns tuple (explanation, orderings) - we only need explanation
	sage_result = explainer$msage(
		X_eval = sklearn_data$X_test,
		y_eval = sklearn_data$y_test,
		nr_orderings = as.integer(n_permutations),
		nr_runs = 1L,
		nr_resample_marginalize = as.integer(sage_n_samples),
		detect_convergence = early_stopping,
		# For consistency with xplainfi: Ensure at least this number of permutations (orderings) is evaluated
		# fippy adds `extra_orderings` after convergence detection, xplainfi explicitly checks if min_permutations are checked before convergence is declared
		extra_orderings = min_permutations
	)

	end_time = Sys.time()

	# Extract explanation object (first element of tuple) and orderings (second element)
	explanation = sage_result[[1]]
	orderings = sage_result[[2]]
	n_permutations_used = nrow(orderings)

	fi_series = explanation$fi_means_stds()
	importance_dt = data.table::data.table(
		feature = instance$task$feature_names,
		importance = sapply(instance$task$feature_names, function(f) {
			as.numeric(fi_series[[f]])
		})
	)

	data.table::data.table(
		importance = list(importance_dt),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = learner_performance,
		n_permutations_used = n_permutations_used,
		converged = n_permutations_used < n_permutations,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# ConditionalSAGE_fippy - Conditional SAGE from fippy package (Python)
# ============================================================================

algo_ConditionalSAGE_fippy = function(
	data = NULL,
	job = NULL,
	instance,
	n_permutations = 10,
	sage_n_samples = 10,
	sampler = "gaussian",
	early_stopping = TRUE,
	min_permutations = 20
) {
	# Use first resampling iteration
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Ensure Python packages (including pandas) are available before data conversion
	.ensure_python_packages()

	# Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
	sklearn_data = task_to_sklearn(
		instance$task,
		train_ids,
		test_ids,
		as_pandas = TRUE
	)

	# Create and train sklearn learner (with encoding if categoricals present)
	sklearn_learner = create_sklearn_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		encode = instance$has_categoricals,
		random_state = job$seed
	)

	fit_sklearn_learner(
		sklearn_learner,
		sklearn_data$X_train,
		sklearn_data$y_train,
		sklearn_data$X_test,
		sklearn_data$y_test
	)

	# Import fippy and create sampler using helper function
	fippy = reticulate::import("fippy")
	sklearn_metrics = reticulate::import("sklearn.metrics")

	# Calculate learner performance on test set
	test_predictions = sklearn_learner$predict(sklearn_data$X_test)
	learner_performance = if (instance$task_type == "regr") {
		as.numeric(sklearn_metrics$r2_score(sklearn_data$y_test, test_predictions))
	} else {
		as.numeric(sklearn_metrics$accuracy_score(sklearn_data$y_test, test_predictions))
	}

	sampler_obj = create_fippy_sampler(
		task = instance$task,
		X_train_pandas = sklearn_data$X_train,
		sampler = sampler
	)

	# For regression use MSE, for classification use zero-one loss (classification error)
	# Note: Using predict (labels) for both, not predict_proba
	loss_fn = if (instance$task_type == "regr") {
		sklearn_metrics$mean_squared_error
	} else {
		sklearn_metrics$zero_one_loss
	}

	# Create a wrapper function for the predict method to handle potential conversion issues
	predict_wrapper = function(X) {
		sklearn_learner$predict(X)
	}

	explainer = fippy$Explainer(
		predict = predict_wrapper,
		X_train = sklearn_data$X_train,
		loss = loss_fn,
		sampler = sampler_obj
	)

	start_time = Sys.time()

	# Compute Conditional SAGE using fippy
	# nr_orderings: number of permutation orderings (like n_permutations in xplainfi)
	# nr_runs: how often each value function is computed (no xplainfi equivalent, use default 1)
	# nr_resample_marginalize: number of samples for Monte Carlo integration (like n_samples in xplainfi)
	# detect_convergence: use convergence detection for optimal results (matches KernelSAGE approach)
	# Returns tuple (explanation, orderings)
	# orderings is a table
	sage_result = explainer$csage(
		X_eval = sklearn_data$X_test,
		y_eval = sklearn_data$y_test,
		nr_orderings = as.integer(n_permutations),
		nr_runs = 1L,
		nr_resample_marginalize = as.integer(sage_n_samples),
		detect_convergence = early_stopping,
		# For consistency with xplainfi: Ensure at least this number of permutations (orderings) is evaluated
		# fippy adds `extra_orderings` after convergence detection, xplainfi explicitly checks if min_permutations are checked before convergence is declared
		extra_orderings = min_permutations
	)

	end_time = Sys.time()

	# Extract explanation object (first element of tuple) and orderings (second element)
	explanation = sage_result[[1]]
	orderings = sage_result[[2]]
	n_permutations_used = nrow(orderings)

	fi_series = explanation$fi_means_stds()
	importance_dt = data.table::data.table(
		feature = instance$task$feature_names,
		importance = sapply(instance$task$feature_names, function(f) {
			as.numeric(fi_series[[f]])
		})
	)

	data.table::data.table(
		importance = list(importance_dt),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = learner_performance,
		n_permutations_used = n_permutations_used,
		converged = n_permutations_used < n_permutations,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}


# ============================================================================
# MarginalSAGE_sage - Official SAGE implementation with kernel estimator -> KernelSAGE
# ============================================================================

algo_MarginalSAGE_sage = function(
	data = NULL,
	job = NULL,
	instance,
	sage_n_samples = 200, # Background data size for marginalization
	early_stopping = TRUE,
	min_permutations = 20
) {
	# Use first resampling iteration
	train_ids = instance$resampling$train_set(1)
	test_ids = instance$resampling$test_set(1)

	# Convert to sklearn format (no special handling needed with numeric-only features)
	sklearn_data = task_to_sklearn(
		instance$task,
		train_ids,
		test_ids,
		as_pandas = FALSE # Use numpy arrays for simplicity
	)

	# Create and train sklearn learner
	sklearn_learner = create_sklearn_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		encode = instance$has_categoricals, # Will be FALSE with convert_to_numeric = TRUE
		random_state = job$seed
	)

	fit_sklearn_learner(
		sklearn_learner,
		sklearn_data$X_train,
		sklearn_data$y_train,
		sklearn_data$X_test,
		sklearn_data$y_test
	)

	# Calculate learner performance on test set
	sklearn_metrics = reticulate::import("sklearn.metrics")
	test_predictions = sklearn_learner$predict(sklearn_data$X_test)
	learner_performance = if (instance$task_type == "regr") {
		as.numeric(sklearn_metrics$r2_score(sklearn_data$y_test, test_predictions))
	} else {
		as.numeric(sklearn_metrics$accuracy_score(sklearn_data$y_test, test_predictions))
	}

	# Import sage and create MarginalImputer + KernelEstimator
	sage = reticulate::import("sage")

	# Use training data as background data for marginalization
	# Limit to sage_n_samples to control computation
	n_background = min(sage_n_samples, nrow(sklearn_data$X_train))
	background_data = sklearn_data$X_train[1:n_background, , drop = FALSE]

	# Create imputer (simple since no encoding pipeline needed)
	imputer = sage$MarginalImputer(
		model = sklearn_learner,
		data = background_data
	)

	# Create KernelEstimator
	loss = if (instance$task_type == "regr") "mse" else "cross entropy"

	# Handle random_state
	random_state = job$seed
	if (is.null(random_state)) {
		cli::cli_alert_info("{.code random_state} not specified, using constant seed")
		random_state = 2093564L
	}

	estimator = sage$KernelEstimator(
		imputer = imputer,
		loss = loss,
		random_state = as.integer(random_state)
	)

	start_time = Sys.time()

	# Compute SAGE values with convergence detection
	# Note: Featureless learners are excluded from KernelSAGE in experiment setup
	# to avoid indefinite convergence loops
	# Convert to numpy arrays explicitly to avoid shape attribute errors
	np = reticulate::import("numpy", convert = FALSE)

	explanation = estimator(
		X = np$array(sklearn_data$X_test),
		Y = np$array(sklearn_data$y_test),
		detect_convergence = early_stopping,
		verbose = FALSE,
		bar = FALSE
	)

	end_time = Sys.time()

	# Extract SAGE values from explanation object
	# explanation$values is a numpy array with shape (n_features,)
	importance_dt = data.table::data.table(
		feature = instance$task$feature_names,
		importance = as.numeric(explanation$values)
	)

	data.table::data.table(
		importance = list(importance_dt),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = learner_performance,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}
