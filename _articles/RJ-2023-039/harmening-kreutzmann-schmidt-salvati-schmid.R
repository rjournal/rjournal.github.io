######################################################################
# Replication code for "A Framework for Producing Small Area Estimates
# Based on Area-Levels in R"
######################################################################

# Install the package
install.packages("emdi", dependencies = TRUE)

####################
# Code in Section "Estimation procedure for the standard
# Fay-Herriot model"
####################

# Load package and population and sample data
library("emdi")
data("eusilcA_popAgg")
data("eusilcA_smpAgg")

## Combine input data
combined_data <- combine_data(pop_data = eusilcA_popAgg,
                              pop_domains = "Domain",
                              smp_data = eusilcA_smpAgg,
                              smp_domains = "Domain")

## Identify spatial structures
# Create proximity matrix
load_shapeaustria()
shape_austria_dis <- shape_austria_dis[order(shape_austria_dis$PB),]
austria_shape <- sp::merge(shape_austria_dis, eusilcA_smpAgg, by.x = "PB",
                       by.y = "Domain", all.x = F)
rel <- spdep::poly2nb(austria_shape, row.names = austria_shape$PB)
eusilcA_prox <- spdep::nb2mat(rel, style = "W", zero.policy = TRUE)

# Spatial correlation tests
spatialcor.tests(direct = combined_data$Mean,
                 corMatrix = eusilcA_prox)

## Perform model selection
# Generate initial object of class "fh", "emdi"
fh_std <- fh(fixed = Mean ~ cash + self_empl + unempl_ben,
             vardir = "Var_Mean", combined_data = combined_data,
             domains = "Domain", method = "ml", B = c(0,50))
# Perform stepwise variable selection
step(fh_std, criteria = "KICb2")

## Estimate EBLUPs and MSEs
# Generate object of class "fh","emdi" with MSE estimation
fh_std <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
             combined_data = combined_data, domains = "Domain",
             method = "ml", MSE = TRUE, B = c(0,50))

## Assess the estimated model
summary(fh_std)
plot(fh_std)

## Compare results with direct estimates
compare_plot(fh_std, CV = TRUE, label = "no_title")
compare(fh_std)

## Benchmarking for consistent estimates
# The benchmark value is calculated by taking the mean of the variable
# eqIncome of the eusilcA_smp data frame of package emdi.
data("eusilcA_smp")
mean(eusilcA_smp$eqIncome)
fh_bench <- benchmark(fh_std, benchmark = 20140.09,
                      share = eusilcA_popAgg$ratio_n, type = "ratio")
# Showing the first 6 rows of the results with head command
head(fh_bench)

# Visualization of results on maps
# Load shape file
load_shapeaustria()

map_plot(object = fh_std, MSE = TRUE,
         map_obj = shape_austria_dis, map_dom_id = "PB",
         scale_points = list(Direct = list(ind = c(8000, 60000),
                                           MSE = c(200000, 10000000)),
                             FH = list(ind = c(8000, 60000),
                                       MSE = c(200000, 10000000))))

####################
# Code in Section "Estimation of the extended area-level models"
####################

## FH model with transformation
# Generate object of class "fh","emdi" with arcsin
# transformation, bc backtransformation and bootstrap MSE estimation
fh_arcsin <- fh(fixed = MTMED ~ cash + age_ben + rent + house_allow,
                vardir = "Var_MTMED", combined_data = combined_data,
                domains = "Domain", transformation = "arcsin",
                backtransformation = "bc", eff_smpsize = "n",
                MSE = TRUE, mse_type = "boot")


## Spatial FH model
# Generate object of class "fh","emdi" with spatial
# correlation and analytical MSE estimation
fh_spatial <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                 combined_data = combined_data, domains = "Domain",
                 correlation = "spatial", corMatrix = eusilcA_prox,
                 MSE = TRUE)

## Robust FH model
# Generate object of class "fh","emdi" with robust estimation
# method and pseudolinear MSE estimation
fh_robust <- fh(fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
                combined_data = combined_data, domains = "Domain",
                method = "reblup", MSE = TRUE, mse_type = "pseudo")

## Measurement error (ME) model
# Create variance-covariance array
# Number of covariates
P <- 1
# Number of areas
M <- 94

Ci_array <- array(data = 0, dim = c(P + 1, P + 1, M))

for(i in 1:M){
    Ci_array[2,2,i] <- eusilcA_smpAgg$Var_Cash[i]
}

# Generate object of class "fh","emdi" with ME method and
# Jackknife MSE estimation
fh_yl <- fh(fixed = Mean ~ Cash, vardir = "Var_Mean",
            combined_data = eusilcA_smpAgg, domains = "Domain",
            method = "me", Ci = Ci_array, MSE = TRUE,
            mse_type = "jackknife")
