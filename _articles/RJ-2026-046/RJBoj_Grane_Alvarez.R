### ============================================================================
### Example 1 — Simulated data (Moderate correlation with 5% contamination) 
### ============================================================================

# Load required libraries
library(dbrobust)
library(dbstats)
library(tidyverse)
library(countrycode)

# -----------------------------------------------------------------------------
# 1. Load dataset
# -----------------------------------------------------------------------------
data("Data_MC_contamination", package = "dbrobust")

# -----------------------------------------------------------------------------
# 2. Assign standardized variable names
# -----------------------------------------------------------------------------
names(Data_MC_contamination) <- c(
  "cont1", "cont2", "cont3", "cont4",
  "cat1", "cat2", "cat3",
  "bin1", "bin2", "weights"
)

# -----------------------------------------------------------------------------
# 3. Convert categorical variables to factors
# -----------------------------------------------------------------------------
Data_MC_contamination$cat1 <- as.factor(Data_MC_contamination$cat1)  # set factor
Data_MC_contamination$cat2 <- as.factor(Data_MC_contamination$cat2)  # set factor
Data_MC_contamination$cat3 <- as.factor(Data_MC_contamination$cat3)  # set factor

# -----------------------------------------------------------------------------
# 4. Select predictors in a specific order
# -----------------------------------------------------------------------------
Predictors <- Data_MC_contamination[, c(
  "cont1","cont2","cont3","cont4",
  "bin1","bin2",
  "cat1","cat2","cat3"
)]

# -----------------------------------------------------------------------------
# 5. Convert predictors to matrix format
# -----------------------------------------------------------------------------
Predictors <- as.matrix(Predictors)  # predictors to matrix

# -----------------------------------------------------------------------------
# 6. Force numeric class for matrix entries
# -----------------------------------------------------------------------------
class(Predictors) <- "numeric"  # set class to numeric

# -----------------------------------------------------------------------------
# 7. Assign uniform weights to observations
# -----------------------------------------------------------------------------
w <- Data_MC_contamination$weights

# -----------------------------------------------------------------------------
# 8. Classical Gower distance
# -----------------------------------------------------------------------------

# dbrobust Euclidean correction
Dgower_dist <- calculate_distances(
  Predictors,
  method = "gower",
  type = list(symm = c("bin1", "bin2"))
)

# Squared distance matrix
Dgower_D2 <- as.matrix(Dgower_dist)^2

# Lingoes correction
Dgower_eucl <- make_euclidean(Dgower_D2, w)

# Corrected distance object
Dgower_corrected <- D2toDist(as.D2(Dgower_eucl$D_euc))


# -----------------------------------------------------------------------------
# 9. Robust distances (G-Gower and RelMS)
# -----------------------------------------------------------------------------

# Alpha trimming
alpha <- 0.10

## -------------------------
## Robust G-Gower
## -------------------------

# Squared distance matrix
Dggower_D2 <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(4, 2, 3),
  method = "ggower"
)

# Lingoes correction
Dggower_eucl <- make_euclidean(Dggower_D2, w)

# Euclidean-corrected distance object
Dggower_corrected <- D2toDist(as.D2(Dggower_eucl$D_euc))

# Distance object (without Euclidean correction)
Dggower_dist <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(4, 2, 3),
  method = "ggower",
  return_dist = TRUE
)


## -------------------------
## Robust RelMS
## -------------------------

# Squared distance matrix
DRelMS_D2 <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(4, 2, 3),
  method = "relms"
)

# Lingoes correction
DRelMS_eucl <- make_euclidean(DRelMS_D2, w)

# Euclidean-corrected distance object
DRelMS_corrected <- D2toDist(as.D2(DRelMS_eucl$D_euc))

# Distance object (without Euclidean correction)
DRelMS_dist <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(4, 2, 3),
  method = "relms",
  return_dist = TRUE
)

# -----------------------------------------------------------------------------
# 10. Visualization
# -----------------------------------------------------------------------------

## Identify trimming outliers
n_obs <- nrow(Dggower_D2)

group_vec <- rep("Norm", n_obs)
group_vec[attr(Dggower_D2, "outlier_idx")] <- "Out"

group_factor <- factor(group_vec, levels = c("Norm", "Out"))

# -----------------------------------------------------------------------------
# 11. MDS
# -----------------------------------------------------------------------------

## G-Gower (detected outliers)
# Subfigure 3 (d)
visualize_distances(
  Dggower_corrected,
  method = "mds_classic",
  group = group_factor
)

## RelMS (detected outliers)
# Subfigure 3 (e)
visualize_distances(
  DRelMS_corrected,
  method = "mds_classic",
  group = group_factor
)

## Classical Gower (true outliers)
# Subfigure 3 (a)
Data_MC_contamination$outliers <- factor(
  c(rep("Norm", 500), rep("Out", 25)),
  levels = c("Norm", "Out")
)

visualize_distances(
  Dgower_corrected,
  method = "mds_classic",
  group = Data_MC_contamination$outliers
)

### Comparison using true outliers

## G-Gower
# Subfigure 3 (b)
visualize_distances(
  Dggower_corrected,
  method = "mds_classic",
  group = Data_MC_contamination$outliers
)

## RelMS
# Subfigure 3 (c)
visualize_distances(
  DRelMS_corrected,
  method = "mds_classic",
  group = Data_MC_contamination$outliers
)

# -----------------------------------------------------------------------------
# 11. Qgraphs
# -----------------------------------------------------------------------------

## G-Gower
# Subfigure 4 (b)
visualize_distances(
  Dggower_dist,
  method = "qgraph",
  group = group_factor,
  show_legend = FALSE
)

## RelMS
# Subfigure 4 (c)
visualize_distances(
  DRelMS_dist,
  method = "qgraph",
  group = group_factor,
  show_legend = FALSE
)

## Gower
# Subfigure 4 (a)
visualize_distances(
  Dgower_dist,
  method = "qgraph",
  group = Data_MC_contamination$outliers,
  show_legend = FALSE
)

### Comparison True Outliers

## G-Gower
# Subfigure 4 (d)
visualize_distances(
  Dggower_dist,
  method = "qgraph",
  group = Data_MC_contamination$outliers,
  show_legend = FALSE
)

## RelMS
# Subfigure 4 (e)
visualize_distances(
  DRelMS_dist,
  method = "qgraph",
  group = Data_MC_contamination$outliers,
  show_legend = FALSE
)

### ============================================================================
### Example 2 — World Development Multivariate Analysis
### ============================================================================

# -----------------------------------------------------------------------------
# 1. Load dataset
# -----------------------------------------------------------------------------
# User-friendly: local CSV expected in working directory
World_Development_Multivariate_Data <- read.csv(
  "Datos_Desarrollo_Mundial_Multivariante.csv",header=TRUE
)

# -----------------------------------------------------------------------------
# 2. Standardize variable names for clarity and consistency
# -----------------------------------------------------------------------------
colnames(World_Development_Multivariate_Data) <- c(
  "Country", "Homicides", "Access_To_Electricity", "Health_Expenditure",
  "Doctors", "Nurses", "Below3.20", "Life_Expectancy", "Infant_Mortality",
  "Insufficient_Nutrition", "Government_Education_Expenditure", "Arable_Land",
  "CO2_Emissions", "Health_System", "Education_And_Health_Investment",
  "Poverty", "Polution", "Insecurity"
)

# -----------------------------------------------------------------------------
# 3. Exclude variables not used in the analysis
# -----------------------------------------------------------------------------
Data <- World_Development_Multivariate_Data %>% 
  select(-Doctors, -Nurses)

# -----------------------------------------------------------------------------
# 4. Rename variables using standard abbreviations
# -----------------------------------------------------------------------------
Data <- Data %>%
  rename(
    Hom = Homicides,
    Ele = Access_To_Electricity,
    HEx = Health_Expenditure,
    B320 = Below3.20,
    LEx = Life_Expectancy,
    IMo = Infant_Mortality,
    INu = Insufficient_Nutrition,
    GEE = Government_Education_Expenditure,
    ArL = Arable_Land,
    CO2 = CO2_Emissions,
    HSy = Health_System,
    EHI = Education_And_Health_Investment,
    Pov = Poverty,
    Pol = Polution,
    Ins = Insecurity
  )

# -----------------------------------------------------------------------------
# 5. Recode categorical variables into English equivalents
# -----------------------------------------------------------------------------
Data$HSy <- ifelse(Data$HSy == "Adecuado", "Adequate", "Inadequate")
Data$EHI <- ifelse(Data$EHI == "Alta", "High", "Low")
Data$Pov <- recode(Data$Pov, "Alta" = "High", "Media" = "Medium", "Baja" = "Low")
Data$Pol <- recode(Data$Pol, "Alta" = "High", "Media" = "Medium", "Baja" = "Low")
Data$Ins <- recode(Data$Ins, "Alta" = "High", "Media" = "Medium", "Baja" = "Low")

# -----------------------------------------------------------------------------
# 6. Threshold electricity access (≥95% treated as “High”)
# -----------------------------------------------------------------------------
Data$Ele <- ifelse(Data$Ele >= 95, "High", "Low")

# -----------------------------------------------------------------------------
# 7. Add continent information from country names
# -----------------------------------------------------------------------------
Data$Con <- countrycode(Data$Country, "country.name", "continent")

# -----------------------------------------------------------------------------
# 8. Select predictors used in the multivariate analysis
# -----------------------------------------------------------------------------
Data <- Data %>% 
  select(Country, Hom, B320, LEx, IMo, INu, GEE, ArL, HSy, Ele, Con, Pol)

# -----------------------------------------------------------------------------
# 9. Separate numerical and categorical variables
# -----------------------------------------------------------------------------
Categorical_Variables <- Data %>% 
  select(Country, HSy, Ele, Con, Pol)

Numerical_Variables <- Data %>% 
  select(-names(Categorical_Variables))

# -----------------------------------------------------------------------------
# 10. Log-transform skewed numerical variables
# -----------------------------------------------------------------------------
Transformed_Numerical_Variables <- Numerical_Variables
Transformed_Numerical_Variables$Hom  <- log(Numerical_Variables$Hom)
Transformed_Numerical_Variables$B320 <- log(Numerical_Variables$B320 + 0.1)
Transformed_Numerical_Variables$IMo  <- log(Numerical_Variables$IMo)
Transformed_Numerical_Variables$INu  <- log(Numerical_Variables$INu)
Transformed_Numerical_Variables$GEE  <- log(Numerical_Variables$GEE)

# -----------------------------------------------------------------------------
# 11. Encode categorical variables numerically
#      - Ordered factors are encoded manually
#      - Nominal factors retain general coding
# -----------------------------------------------------------------------------
Categorical_Variables$HSy <- factor(Categorical_Variables$HSy, labels = c("1", "0"))
Categorical_Variables$Ele <- factor(Categorical_Variables$Ele, labels = c("1", "0"))
Categorical_Variables$Con <- factor(Categorical_Variables$Con)
Categorical_Variables$Pol <- factor(Categorical_Variables$Pol, labels = c("2", "0", "1"))

# -----------------------------------------------------------------------------
# 12. Construct population weights
# -----------------------------------------------------------------------------
pop_data <- read.csv("https://datahub.io/core/population/r/population.csv", header = TRUE)

pop_recent <- pop_data %>% 
  filter(Year == max(Year, na.rm = TRUE)) %>% 
  select(Country.Name, Value) %>% 
  rename(Country = Country.Name, PopTotal = Value)

Data2 <- Data %>% 
  left_join(pop_recent, by = "Country")

# Manual corrections for missing values
Data2$PopTotal[92] <- 84120000
Data2$PopTotal[19] <- 10840000

Data2 <- Data2 %>% 
  mutate(w_pop = PopTotal / mean(PopTotal, na.rm = TRUE))

# Add external Poverty classification
Data2$Poverty <- factor(c(1,1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2,1,2,1,1,1,2,1,1,1,2,2,1,2,1,2,1,1,1,1,2,2,1,1,2,2,2,1,1,1,1,2,1,2,1,1,2,2,1,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,2,2,2,2,1,1,1,2,1,2,2,1,2,1,1,2,2,1,1,1,1,2,2,2,2,1,1))

# Use the weights
v_obs <- Data2$w_pop

# -----------------------------------------------------------------------------
# 13. Final predictor matrix and p-vector definition
# -----------------------------------------------------------------------------
Transformed_Data <- cbind(Transformed_Numerical_Variables, Categorical_Variables)
head(Transformed_Data)

# p-vector: considering 7 continuous, 0 binary and 3 categorical multiclass
p <- c(7,0,3)

Predictors <- Transformed_Data %>%
  select(Hom, B320, LEx, IMo, INu, GEE, ArL,   # continuous
         HSy, Ele, Pol)                        # categorical

# Convert factors to numeric
Predictors <- as.data.frame(lapply(Predictors, function(x) {
  if (is.factor(x)) as.numeric(as.character(x)) else x
}))

Predictors <- as.matrix(Predictors)
class(Predictors) <- "numeric"

# Observational weights
w <- v_obs

# -----------------------------------------------------------------------------
# 14. Classical Gower distance
# -----------------------------------------------------------------------------

# dbrobust Euclidean correction
Dgower_dist_WDD <- calculate_distances(
  Predictors,
  method = "gower",
  type = list(symm = c(8, 9))
)

# Squared distance matrix
Dgower_D2_WDD <- as.matrix(Dgower_dist_WDD)^2

# Lingoes correction
Dgower_eucl_WDD <- make_euclidean(Dgower_D2_WDD, w)

# Corrected distance object
Dgower_corrected_WDD <- D2toDist(as.D2(Dgower_eucl_WDD$D_euc))


# -----------------------------------------------------------------------------
# 15. Robust distances (G-Gower and RelMS)
# -----------------------------------------------------------------------------

alpha <- 0.10

## -------------------------
## Robust G-Gower
## -------------------------

# Squared distance matrix
Dggower_D2_WDD <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(7, 0, 3),
  method = "ggower"
)

# Euclidean correction
Dggower_eucl_WDD <- make_euclidean(Dggower_D2_WDD, w)

# Euclidean-corrected distance object
Dggower_corrected_WDD <- D2toDist(as.D2(Dggower_eucl_WDD$D_euc))

# Original distance object
Dggower_dist_WDD <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(7, 0, 3),
  method = "ggower",
  return_dist = TRUE
)


## -------------------------
## Robust RelMS
## -------------------------

# Squared distance matrix
DRelMS_D2_WDD <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(7, 0, 3),
  method = "relms"
)

# Euclidean correction
DRelMS_eucl_WDD <- make_euclidean(DRelMS_D2_WDD, w)

# Euclidean-corrected distance object
DRelMS_corrected_WDD <- D2toDist(as.D2(DRelMS_eucl_WDD$D_euc))

# Original distance object
DRelMS_dist_WDD <- robust_distances(
  data = Predictors,
  w = w,
  alpha = alpha,
  p = c(7, 0, 3),
  method = "relms",
  return_dist = TRUE
)

# -----------------------------------------------------------------------------
# 16. Visualization (MDS, Heatmaps, Qgraphs)
# -----------------------------------------------------------------------------
## Visualize classical Gower
# Subfigure 6 (a)
visualize_distances(Dgower_corrected_WDD,
                    method = "mds_classic",
                    group = Data2$Poverty,
                    k = 3
                    # main_title = "MDS for World Development Data with classical Gower (Poverty)"
)

# ==============================================================================
# COLOR CUSTOMIZATION NOTE:
# If the user wishes to change the group colors, they simply need to pass a 
# character vector to the 'group_colors' argument. This can be done either as:
#   1. A standard vector of HEX codes or color names matching the number of levels 
#      in the grouping variable: c("#1B9E77", "#D95F02")
#   2. A named vector mapping explicit factor levels to colors:
#      c("Level_A" = "#1B9E77", "Level_B" = "#D95F02")
# ==============================================================================

# Visualize classical MDS for World Development Data
# visualize_distances(
#   dist_mat     = Dgower_corrected_WDD,
#   method       = "mds_classic",
#   group        = Data2$Poverty,
#   group_colors = c("#1B9E77", "#D95F02"), # Green and Orange (ColorBrewer palette)
#   k            = 3
#   # main_title = "MDS for World Development Data with classical Gower (Poverty)"
# )

# Subfigure 7 (a)
visualize_distances(Dgower_corrected_WDD,
                    method = "mds_classic",
                    group = Transformed_Data$Con,
                    k = 3
                    # main_title = "MDS for World Development Data with classical Gower (Continent)"
)

# Subfigure 8 (a)
visualize_distances(Dgower_dist_WDD,
                    method = "qgraph",
                    group = Data2$Poverty,
                    show_legend = FALSE
                    # main_title = "Qgraph for World Development Data with classical Gower (Poverty)"
)

# Subfigure 9 (a)
visualize_distances(Dgower_dist_WDD,
                    method = "qgraph",
                    group = Transformed_Data$Con,
                    show_legend = FALSE
                    # main_title = "Qgraph for World Development Data with classical Gower (Continent)"
)

## Visualize Robust G-Gower
# Subfigure 6 (b)
visualize_distances(Dggower_corrected_WDD,
                    method = "mds_classic",
                    group = Data2$Poverty,
                    k = 3
                    # main_title = "MDS for World Development Data with robust G-Gower (Poverty)"
)

# Subfigure 7 (b)
visualize_distances(Dggower_corrected_WDD,
                    method = "mds_classic",
                    group = Transformed_Data$Con,
                    k = 3
                    # main_title = "MDS for World Development Data with robust G-Gower (Continent)"
)

# Subfigure 8 (b)
visualize_distances(Dggower_dist_WDD,
                    method = "qgraph",
                    group = Data2$Poverty,
                    show_legend = FALSE
                    # main_title = "Qgraph for World Development Data with robust G-Gower (Poverty)"
)

# Subfigure 9 (b)
visualize_distances(Dggower_dist_WDD,
                    method = "qgraph",
                    group = Transformed_Data$Con,
                    show_legend = FALSE
                    # main_title = "Qgraph for World Development Data with robust G-Gower (Continent)"
)

## Visualize Robust RelMS
# Subfigure 6 (c)
visualize_distances(DRelMS_corrected_WDD,
                    method = "mds_classic",
                    group = Data2$Poverty,
                    k = 3
                    # main_title = "MDS for World Development Data with robust RelMS (Poverty)"
)

# Subfigure 5
visualize_distances(Dggower_corrected_WDD,
                    method = "heatmap",
                    max_n = 97,
                    group = Transformed_Data$Con,
                    main_title = "Heatmap for World Development Data with robust G-Gower (Continent)")

# Subfigure 7 (c)
visualize_distances(DRelMS_corrected_WDD,
                    method = "mds_classic",
                    group = Transformed_Data$Con,
                    k = 3
                    # main_title = "MDS for World Development Data with robust RelMS (Continent)"
)

# Subfigure 8 (c)
visualize_distances(DRelMS_dist_WDD,
                    method = "qgraph",
                    group = Data2$Poverty,
                    show_legend = FALSE
                    # main_title = "Qgraph for World Development Data with robust RelMS (Poverty)"
)

# Subfigure 9 (c)
visualize_distances(DRelMS_dist_WDD,
                    method = "qgraph",
                    group = Transformed_Data$Con,
                    show_legend = FALSE
                    # main_title = "Qgraph for World Development Data with robust RelMS (Continent)"
)
