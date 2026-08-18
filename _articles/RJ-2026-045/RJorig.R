library("rSRD")

#Preprocessing and other utility functions

#Sample input
SRD_input <- data.frame(
  A=c(2, 5, 7, 8),
  B=c(5, 1, 6, 10),
  C=c(6, 3, 2, 3))


#The next function calculates the SRD values assuming that the last column (C) is the reference
#and that columns A and B are the methods
calculateSRDValues(SRD_input)


#The package offers methods to create a new reference based on the existing data
utilsCreateReference(SRD_input, method = "mean")

#Note that the function does not change the input, only returns with a new matrix
View(SRD_input)

#Another way of creating new reference
#We can choose a different aggregation methods for each row if the objects are not of the same type
ref <- c("max","min","mean","mean")
SRD_input = utilsCreateReference(SRD_input, method = "mixed", ref)
View(SRD_input)

#Detailed SRD computation that shows how the test statistic is computed
#The function outputs the whole computation table with raw (non-normalized) SRD scores in the last row
utilsDetailedSRD(SRD_input)

#Calculates the normalization factor
#This is the number that the raw SRD scores needs to be divided to obtain normalized values
#The only parameter the function needs is the number of rows
utilsMaxSRD(4)

#Calculates the tie probability for a vector
#This function might come handy if the user wants to specify an SRD distribution with a fixed tie probability
solution <- c(1,3,3,3,2,2,4,3)
utilsTieProbability(solution)


#Case studies

#MEP profiles data

#Reading the input from file
profiles_df <- read.csv("Data/mep_profiles.csv", row.names = 1, sep=";")

#Alternatively, we can load the MEP dataset from the package's extdata directory
path <- system.file("extdata", "mep_profiles.csv", package = "rSRD")
profiles_df <- read.csv(path, header = TRUE, sep = ";", row.names = 1)

View(profiles_df)

#Calculating the (normalized) SRD scores of the columns (MEPs)
#The last column (MEP Rego) is the reference
calculateSRDValues(profiles_df)

#Computation of the distribution and significance thresholds
dist_f <- calculateSRDDistribution(profiles_df, option = 'f', output_to_file = FALSE, seed=42)
plotPermTest(profiles_df, dist_f)

#Cross-validation combined with Wilcoxon-test
#This helps resolving ambiguity in the ranking
cv_Wilc <- calculateCrossValidation(profiles_df, seed = 42)
plotCrossValidation(cv_Wilc)

#Computing the pairwise differences and plotting on a heatmap
plotHeatmapSRD(profiles_df, output_to_file = TRUE, color = utilsColorPalette)

#Changing the color palette for the heatmap
myPalette <- c("#eb9c34", "#ebba34", "#ebd634", "#ebe534", "#d9eb34",
               "#b7eb34", "#99eb34", "#6beb34")
plotHeatmapSRD(profiles_df, color = myPalette)

#Cross-validation combined with Dietterich-test
cv_Diet <- calculateCrossValidation(profiles_df, method = "Dietterich", number_of_folds = 10, seed = 137)
plotCrossValidation(cv_Diet)

#Small sample size
#We remove rows (topics) from profiles_df where the topic was mentioned less than 400 times across all MEPs
small_df <- profiles_df[rowSums(profiles_df) >= 400, ]
View(small_df)

#The SRD scores are naturally different
calculateSRDValues(small_df)

#Changing the underlying SRD distribution will affect the significance thresholds
dist_r <- calculateSRDDistribution(small_df, option = 'r', output_to_file = TRUE, seed = 137)
plotPermTest(small_df, dist_r)

dist_d <- calculateSRDDistribution(small_df, option = 'd', output_to_file = TRUE, seed = 137)
plotPermTest(small_df, dist_d)



#Bundesliga data

#Again, we can either read the data from file
bundesliga_df <- read.csv("Data/bundesliga20_21.csv", row.names = 1, sep=";")

#Or load it from the extdata directory
path <- system.file("extdata", "bundesliga20_21.csv", package = "rSRD")
bundesliga_df <- read.csv(path, header = TRUE, sep = ";", row.names = 1, check.names = FALSE)

View(bundesliga_df)

#Calculating the (normalized) SRD scores of the columns (Game statistics)
calculateSRDValues(bundesliga_df)

#Computation of the distribution and significance thresholds
calculateSRDDistribution(bundesliga_df, option = "f", seed = 42)

#Cross-validation combined with Wilcoxon-test
cv_Wilc <- calculateCrossValidation(bundesliga_df, seed = 137)
plotCrossValidation(cv_Wilc)

#Cross-validation combined with Alpaydin-test
cv_Alp <- calculateCrossValidation(bundesliga_df, method = "Alpaydin", number_of_folds = 10, seed = 33)
plotCrossValidation(cv_Alp)

#Plotting the permutation-test, distribution is represented by the CDF 
dist_r <- calculateSRDDistribution(bundesliga_df, option = 'r')
plotPermTest(bundesliga_df, dist_r, densityToDistr = TRUE)

