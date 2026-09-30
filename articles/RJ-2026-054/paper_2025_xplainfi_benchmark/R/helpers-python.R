# Python/fippy integration helpers

# Python environment is managed via uv with a local .venv
# See pyproject.toml for dependency specification and uv.lock for exact versions
# Setup instructions are in README.md

# Initialize Python environment from local .venv
# The .venv must be created beforehand using: uv sync
.ensure_python_packages = function() {
	if (!reticulate::py_available()) {
		venv_path = here::here(".venv")
		if (!dir.exists(venv_path)) {
			cli::cli_abort(c(
				"x" = "Python virtual environment not found at {.path {venv_path}}",
				"i" = "Run {.code uv sync} in the project directory to create it.",
				"i" = "See README.md for detailed setup instructions."
			))
		}
		reticulate::use_virtualenv(venv_path, required = TRUE)
	}
}

# Helper function to convert mlr3 task to scikit-learn format
# Returns list with X_train, X_test, y_train, y_test
# Does NOT encode categorical features - that is now handled by:
#   - Learner: via create_sklearn_learner(..., encode = TRUE)
#   - Sampler: via create_fippy_sampler() using SequentialSampler with RFSamplers
# For classification tasks, encodes target labels as integers for fippy compatibility
# If as_pandas=TRUE, returns pandas DataFrames (needed for fippy samplers)
# If pre_encode_factors=TRUE, converts factor columns to integers (for MarginalSAGE_sage)
task_to_sklearn = function(
	task,
	train_ids,
	test_ids,
	as_pandas = FALSE,
	pre_encode_factors = FALSE
) {
	# Get training data
	train_data = task$data(rows = train_ids)
	X_train = train_data[, task$feature_names, with = FALSE]
	y_train = train_data[[task$target_names]]

	# Get test data
	test_data = task$data(rows = test_ids)
	X_test = test_data[, task$feature_names, with = FALSE]
	y_test = test_data[[task$target_names]]

	# Pre-encode factors if requested (for MarginalSAGE_sage compatibility)
	if (pre_encode_factors) {
		factor_cols = names(X_train)[sapply(X_train, is.factor)]
		if (length(factor_cols) > 0) {
			for (col in factor_cols) {
				X_train[[col]] = as.integer(X_train[[col]])
				X_test[[col]] = as.integer(X_test[[col]])
			}
		}
	}

	# For classification, encode target labels as integers
	# This is needed for fippy to work properly (numeric predictions can be averaged)
	label_encoder = NULL
	if (task$task_type == "classif") {
		.ensure_python_packages()
		sklearn_preprocessing = reticulate::import("sklearn.preprocessing")
		label_encoder = sklearn_preprocessing$LabelEncoder()

		# Convert factors to characters first, then encode
		y_train_chr = as.character(y_train)
		y_test_chr = as.character(y_test)

		y_train = label_encoder$fit_transform(y_train_chr)
		y_test = label_encoder$transform(y_test_chr)
	}

	if (as_pandas) {
		# Ensure Python packages (including pandas) are available before data conversion
		.ensure_python_packages()

		# Convert to pandas DataFrames/Series for fippy
		# Both X and y need pandas objects (DataFrames have .columns, Series have .to_numpy())
		# Note: Categorical features remain as-is (factor/character columns)
		pd = reticulate::import("pandas", convert = FALSE)

		# Convert factors to strings for pandas compatibility
		X_train_converted = data.table::copy(X_train)
		X_test_converted = data.table::copy(X_test)
		factor_cols = names(X_train_converted)[sapply(X_train_converted, is.factor)]
		if (length(factor_cols) > 0) {
			X_train_converted[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
			X_test_converted[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
		}

		list(
			X_train = pd$DataFrame(X_train_converted),
			X_test = pd$DataFrame(X_test_converted),
			y_train = pd$Series(as.vector(y_train)),
			y_test = pd$Series(as.vector(y_test))
		)
	} else {
		# Convert to matrices/vectors for sage and sklearn
		# Note: Matrices don't support categorical, so this assumes numeric data only
		list(
			X_train = as.matrix(X_train),
			X_test = as.matrix(X_test),
			y_train = as.vector(y_train),
			y_test = as.vector(y_test)
		)
	}
}

# Helper function to create scikit-learn learner
create_sklearn_learner = function(
	learner_type,
	task_type,
	encode = FALSE,
	n_trees = 500L,
	n_units = 20L,
	random_state = NULL
) {
	# Ensure random_state is integer
	if (is.null(random_state)) {
		# Easier for debugging but should not occur in actual benchmark
		cli::cli_alert_info("{.code random_state} not specified, using constant seed")
		random_state = 2093564
	}
	random_state = as.integer(random_state)

	.ensure_python_packages()
	sklearn = reticulate::import("sklearn")
	xgb = reticulate::import("xgboost")
	ce = reticulate::import("category_encoders")

	if (learner_type == "linear") {
		if (task_type == "regr") {
			learner = sklearn$linear_model$LinearRegression()
		} else {
			learner = sklearn$linear_model$LogisticRegression(
				random_state = random_state,
				max_iter = 200L
			)
		}
	} else if (learner_type == "rf") {
		if (task_type == "regr") {
			learner = sklearn$ensemble$RandomForestRegressor(
				n_estimators = as.integer(n_trees),
				random_state = random_state,
				n_jobs = 1L
			)
		} else {
			learner = sklearn$ensemble$RandomForestClassifier(
				n_estimators = as.integer(n_trees),
				random_state = random_state,
				n_jobs = 1L
			)
		}
	} else if (learner_type == "mlp") {
		if (task_type == "regr") {
			learner = sklearn$neural_network$MLPRegressor(
				hidden_layer_sizes = reticulate::tuple(n_units),
				max_iter = 500L,
				early_stopping = TRUE,
				shuffle = TRUE,
				batch_size = 18000L,
				n_iter_no_change = 50L,
				learning_rate_init = 0.1,
				random_state = random_state
			)
		} else {
			learner = sklearn$neural_network$MLPClassifier(
				hidden_layer_sizes = reticulate::tuple(n_units),
				max_iter = 500L,
				early_stopping = TRUE,
				shuffle = TRUE,
				batch_size = 18000L,
				n_iter_no_change = 50L,
				learning_rate_init = 0.1,
				random_state = random_state
			)
		}
	} else if (learner_type == "boosting") {
		# XGBoost with parameters matching R implementation
		if (task_type == "regr") {
			learner = xgb$XGBRegressor(
				n_estimators = 1000L,
				learning_rate = 0.1,
				booster = "gbtree",
				tree_method = "hist",
				early_stopping_rounds = 50L,
				random_state = random_state,
				n_jobs = 1L
			)
		} else {
			learner = xgb$XGBClassifier(
				n_estimators = 1000L,
				learning_rate = 0.1,
				booster = "gbtree",
				tree_method = "hist",
				early_stopping_rounds = 50L,
				random_state = random_state,
				n_jobs = 1L
			)
		}
	} else {
		stop("Unknown learner_type: ", learner_type)
	}
	if (encode) {
		# For simplicity with mixed types, use encoders from category_encoders
		# These handle both numeric and categorical seamlessly
		# cols=NULL means auto-detect categorical columns (object dtype)
		if (learner_type == "rf") {
			# RF: Can handle categoricals directly with OrdinalEncoder
			encoder = ce$OrdinalEncoder(handle_unknown = "value")
		} else if (learner_type == "linear") {
			# Linear: Use target encoding or one-hot with drop_first
			encoder = ce$OneHotEncoder(
				handle_unknown = "value",
				use_cat_names = TRUE,
				drop_invariant = TRUE
			)
		} else {
			# MLP and XGBoost: Need one-hot encoding
			encoder = ce$OneHotEncoder(handle_unknown = "value", use_cat_names = TRUE)
		}

		# Create pipeline with encoder
		learner = sklearn$pipeline$make_pipeline(encoder, learner)
	}
	learner
}

# Helper function to fit sklearn learner with proper XGBoost early stopping
fit_sklearn_learner = function(learner, X_train, y_train, X_test, y_test) {
	# Check if it's an XGBoost model that needs validation data for early stopping
	learner_class = class(learner)[1]
	is_xgboost = grepl("XGB", learner_class)

	# Check if it's a pipeline containing XGBoost
	is_pipeline = "steps" %in% names(learner)
	if (!is_xgboost && is_pipeline) {
		# Get the last step of the pipeline (the actual learner)
		last_step = learner$steps[[length(learner$steps)]]
		is_xgboost = grepl("XGB", class(last_step[[2]])[1])
	}

	if (is_xgboost && !is_pipeline) {
		# Direct XGBoost model: provide validation data for early stopping
		learner$fit(X_train, y_train, eval_set = list(list(X_test, y_test)), verbose = FALSE)
	} else if (is_xgboost && is_pipeline) {
		# XGBoost in pipeline: need to transform validation data through encoder first
		# Fit the encoder on training data and transform both sets
		encoder = learner$steps[[1]][[2]]
		encoder$fit(X_train, y_train)
		X_train_encoded = encoder$transform(X_train)
		X_test_encoded = encoder$transform(X_test)

		# Now fit XGBoost with encoded validation data
		xgb_model = learner$steps[[2]][[2]]
		xgb_model$fit(
			X_train_encoded,
			y_train,
			eval_set = list(list(X_test_encoded, y_test)),
			verbose = FALSE
		)

		# Store the fitted components back in the pipeline
		learner$steps[[1]][[2]] = encoder
		learner$steps[[2]][[2]] = xgb_model
	} else {
		# For other learners (including pipelines), use standard fit
		learner$fit(X_train, y_train)
	}

	invisible(learner)
}

# Helper function to create fippy sampler based on original task data
# sampler: "simple" (bootstrap resample), "gaussian" (parametric), or "rf" (RF-based)
# - SimpleSampler: Resamples from observed data (no assumptions, works with any data)
# - GaussianSampler: Assumes multivariate Gaussian (continuous features only)
# - SequentialSampler with RF: Semi-parametric, handles mixed data
create_fippy_sampler = function(task, X_train_pandas, sampler = "gaussian") {
	.ensure_python_packages()
	fippy = reticulate::import("fippy")

	# Validate sampler
	checkmate::assert_choice(sampler, choices = c("simple", "gaussian", "rf"))

	# SimpleSampler: bootstrap resampling, no distributional assumptions
	if (sampler == "simple") {
		cli::cli_inform("Using SimpleSampler (bootstrap resampling)")
		return(fippy$samplers$SimpleSampler(X_train_pandas))
	}

	# Check if task has categorical features (from the task, not the pandas data)
	# The sampler receives unprocessed data, so we check the original feature types
	feature_types = task$feature_types$type
	has_categoricals = any(feature_types %in% c("factor", "character"))

	# For fippy, also check the actual pandas DataFrame for object columns
	pd = reticulate::import("pandas")
	dtypes = X_train_pandas$dtypes
	object_cols = names(dtypes[dtypes == "object"])

	# Validate sampler choice
	if (has_categoricals && sampler == "gaussian") {
		cli::cli_abort(c(
			"x" = "GaussianSampler cannot be used with categorical features",
			"i" = "Task has categorical features",
			"i" = "Use {.code sampler = 'rf'} or {.code sampler = 'simple'} for tasks with categorical features"
		))
	}

	if (sampler == "gaussian") {
		# Use GaussianSampler (only valid for continuous features)
		cli::cli_inform("Using GaussianSampler")
		fippy$samplers$GaussianSampler(X_train_pandas)
	} else if (sampler == "rf") {
		# Use SequentialSampler with RF-based samplers for mixed data
		# This follows the fippy example for handling categorical features

		if (length(object_cols) > 0) {
			# Have categorical columns - use SequentialSampler like in fippy example
			cli::cli_inform("Using SequentialSampler with RF samplers for mixed data")

			# Create RF-based samplers
			cat_sampler = fippy$samplers$UnivRFSampler(X_train_pandas, cat_inputs = object_cols)
			cont_sampler = fippy$samplers$ContUnivRFSampler(X_train_pandas, cat_inputs = object_cols)

			# Combine into SequentialSampler
			fippy$samplers$SequentialSampler(
				X_train_pandas,
				categorical_fs = object_cols,
				cat_sampler = cat_sampler,
				cont_sampler = cont_sampler
			)
		} else {
			# All features are numeric - use ContUnivRFSampler
			cli::cli_inform("Using ContUnivRFSampler (all features numeric)")
			fippy$samplers$ContUnivRFSampler(X_train_pandas, cat_inputs = character(0))
		}
	}
}
