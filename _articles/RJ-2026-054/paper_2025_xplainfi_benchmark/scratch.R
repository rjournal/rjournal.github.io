importances = readRDS("results/importance/importances.rds")

single_feature_test = importances[
	algorithm == "MarginalSAGE_fippy" &
		learner_type == "rf" &
		problem_clean == "mediated" &
		feature == "direct"
]

length(single_feature_test$job.id)
data.table::uniqueN(single_feature_test$job.id)


single_feature_pfi = importances[
	algorithm == "PFI_fippy" &
		learner_type == "rf" &
		problem_clean == "mediated" &
		feature == "direct"
]

# ----

problem = prob_ewald()

algo_PFI_vip(instance = problem)
