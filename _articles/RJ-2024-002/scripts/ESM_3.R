## SURVIVAL ANALYSIS WORKFLOW

# (R script for section 5 of the paper Costa, R J and Gerstung, M,
# 'ebmstate: an R package for disease progression analysis
# under empirical Bayes Cox models.'
 
library(ebmstate)
data(mstate_data)

## SECTION 5.1

# Table 1
print(mstate_data[mstate_data$id %in% c(77, 78), ])


## SECTION 5.2

# Argument 'Z' of CoxRFX for a model assuming that the 
# impact of each covariate is the same for all transitions
# (one coefficient per covariate).
Z <- mstate_data[
  !names(mstate_data) %in% c(
    "id",
    "from",
    "to",
    "Tstart",
    "Tstop",
    "time",
    "status"
  )
]

# Create transition matrix with mstate::transMat()
tmat <- mstate::transMat(
  x = list(c(2, 3), c(4), c(), c()),
  names = c("MDS", "AML", "death", "death_after_AML")
)

# To expand covariates by transition using mstate::expand.covs,
# first set the class of 'mstate_data' as
class(mstate_data) <- c("data.frame", "msdata")

# then add the transition matrix as attribute: 
attr(mstate_data, "trans") <- tmat

# expand covariates by transition:
outcome_covs <- c("id", "from", "to", "trans",
                      "Tstart", "Tstop",
                      "time", "status",
                      "strata"
)
covariates_expanded_123 <- mstate::expand.covs(
  mstate_data,
  covs = names(mstate_data)[
    !names(mstate_data) %in% outcome_covs
  ],
  append = FALSE
)

# Columns `id ' and ` trans ' from ` mstate_data ' together with the first
# two expanded covariates (patients 77 and 78):
print(
  cbind(
    mstate_data,
    covariates_expanded_123
  )[
    mstate_data$id %in% c(77, 78),
    c("id", "trans", names(covariates_expanded_123))
  ]
)


# remove all covariates for transition 3 from 'covariates_expanded_123'
# to fit a fully non-parametric model on this transition:
covariates_expanded_12 <- covariates_expanded_123[
  ! grepl(".3", names(covariates_expanded_123), fixed = TRUE)
]

# argument 'Z' of coxrfx
Z_12 <- data.frame(
  covariates_expanded_12,
  strata = mstate_data$trans,
  trans = mstate_data$trans
)

# argument 'surv' for a clock-forward model
surv <- survival::Surv(
  mstate_data$Tstart,
  mstate_data$Tstop,
  mstate_data$status
)

# argument 'surv' for a clock-reset model
surv <- survival::Surv(
  mstate_data$time,
  mstate_data$status
)

# argument 'groups' of coxrfx
groups_12 <- paste0(rep("group", ncol(Z_12) - 2), c("_1", "_2"))


# fit random effects model
model_12 <- CoxRFX(Z = Z_12, surv = surv, groups = groups_12)

## SECTION 5.3

# Build 'patient_data' data frame with the covariate values for which
# cumulative hazards are to be computed (covariate values of patient 78)
patient_data <- mstate_data[
  mstate_data$id == 78,
  ,
  drop = FALSE][rep(1, 3), ]

patient_data$strata <- patient_data$trans <- 1:3

patient_data <- mstate::expand.covs(
  patient_data,
  covs = names(patient_data)[
    ! names(patient_data) %in% outcome_covs
  ],
  append = TRUE
)

patient_data <- patient_data[!grepl(".3", names(patient_data), fixed = TRUE)]

# The 'patient_data' data frame has only 3 rows ( one for each transition ).
# The output below shows its 'id' and 'trans' columns
# and expanded covariates ASXL1 and DNMT3A:
print(
  patient_data[
    ,
    names(patient_data) %in% c(
      "id",
      "trans",
      "ASXL1.1",
      "ASXL1.2",
      "DNMT3A.1",
      "DNMT3A.2"
    )
  ]
)

# compute cumulative hazards
msfit_object <- msfit_generic(model_12, patient_data, tmat)


## SECTION 5.4

# compute state occupation probabilities for patient 78:
probtrans_object <- probtrans_ebmstate(
  "MDS",
  msfit_object,
  "clockreset",
  max_time = 4000
)

# generate plot of state occupation probabilities:
plot(probtrans_object)


# compute state occupation probabilities for patient 78 using
# the fast Fourier transform:
probtrans_object <- probtrans_fft(
  "MDS",
  msfit_object,
  max_time = 4000
)

# remake plot of state occupation probabilities:
plot(probtrans_object)


## INTERVAL ESTIMATES AND LEAVE-ONE-OUT PREDICTIONS

# Creating the object arguments for boot_ebmstate()

# 'groups' argument was already created, but we
# need to add names to it
names(groups_12) <- names(covariates_expanded_12)

# 'mstate_data_expanded' argument
# (similar to 'covariates_expanded_12' 
# but including outcome variables)
mstate_data_expanded <- cbind(
  mstate_data[names(mstate_data) %in% outcome_covs],
  covariates_expanded_12
)

# create the non-parametric bootstrap confidence intervals
boot_ebmstate_object <- boot_ebmstate(
  mstate_data = mstate_data_expanded,
  which_group = groups_12,
  min_nr_samples = 100,
  patient_data = patient_data,
  tmat = tmat,
  initial_state = "MDS",
  time_model = "clockreset",
  input_file = NULL,
  coxrfx_args = list(max.iter = 200),
  probtrans_args = list(max_time = 4000)
)


# #leave-one-out outcome predictions
# patient_IDs <- sample(unique(mstate_data$id), 14 * 14)
# loo_ebmstate_object <- loo_ebmstate(
#   mstate_data,
#   mstate_data_expanded,
#   which_group = groups_12,
#   patient_IDs = patient_IDs,
#   initial_state = "MDS",
#   tmat = tmat,
#   input_file = NULL,
#   time_model = "clockreset",
#   coxrfx_args = list(max.iter = 200),
#   probtrans_args = list(max_time = 4000)
# )

## SECTION 5.5

# All transitions semi-parametric

# arguments 'groups' and 'Z' for fitting a Cox regression model on all transitions
Z_123 <- data.frame(
  covariates_expanded_123,
  strata = mstate_data$trans,
  trans = mstate_data$trans
)
groups_123 <- paste0(rep("group", ncol(Z_123) - 2), c("_1", "_2", "_3"))

# Fit a Cox regression model for all transitions
model_123 <- CoxRFX(Z = Z_123, surv = surv, groups = groups_123)

# Concordance statistic for each model
concordance(model_12)
concordance(model_123)

# BIC
model_12$BIC
model_123$BIC


