# Configuration file for batchtools experiment / importance sub-benchmark
conf = list(
	# General batchtools settings
	reg_path = here::here("registries", "importance"),
	seed = 2025,
	repls = 25,
	# Samples to generate
	n_samples = 5000,
	# Affects correlation task
	correlation = c(0.25, 0.75),
	# Affects PFI, CFI iterations
	n_repeats = 50,
	# For SAGE permutations: large n_permutations with convergence detection (across all implementations)
	n_permutations = 100,
	min_permutations = 20,
	sage_early_stopping = TRUE,
	# Size of sampled data used for Monte Carlo integration in SAGE methods, 200 was usually sufficient
	# increases RAM usage a lot if set too high, and returns are diminishing somewhat quickly
	sage_n_samples = 100,
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c("linear", "rf", "mlp", "boosting"),
	# Conditional samplers for CFI, and ConditionalSAGE
	samplers = "gaussian"
)
