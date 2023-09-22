# -------------------------------------------------------------------------------------------------
# Dataset to reproduce the results on paper:
# Testing the equality of normal distributed groups' means under unequal variances by doex package

# Created by: Mustafa Cavus in 09/11/2020
# Contact: mustafacavus@eskisehir.edu.tr
# -------------------------------------------------------------------------------------------------

# Example 1 #######################################################################################

# Call the doex package
library(doex)

# print hybrid data in Weerahandi (1995)
hybrid

# observations of the hybrid data
hybrid$data

# group labels of the hybrid data
hybrid$species

# The ggplot2 package can be used to plot the box plot of the data in Figure 1.
ggplot(hybrid, aes(x = species, y = data)) + 
  geom_boxplot() +
  ylab("Yield") +
  xlab("Corn Species")

# Look at the summary statistics of the data before using the tests.
# Use psych package to obtain the descriptive statistics of the hybrid data
library(psych)

# Describe the hybrid data by species using describe.by(.) function
describe.by(hybrid$data, hybrid$species)

# It is seen that the variances of the species are unequal
# Thus we need to use the tests for equality of the group means under unequal variances
#
# Examples of the use of the AF and GF tests on the hybrid data are given in the follows.
# The following code performs the Approximate F-test on the hybrid data.
AF(hybrid$data,hybrid$species)

# Following code performs the Generalized F-test.
GF(hybrid$data,hybrid$species)


# Example 2 #######################################################################################

# Dataset in Westfall and Young (1993) Resampling-Based Multiple Testing: Examples and Methods for P-Value Adjustment
# Example: Litter Weight Dose Response
# A data set analyzed by Westfall and Rom (1990) involces
# litter weights of mice born from mothers assigned to three 
# different dosage groups and a control. For the low dose
# group the dose metameter is 5, for the medium dose 
# group it is 50, and for the high dose group it is 500.

# average litter weight data
weight_data <- c(22.69, 26.59, 28.85, 28.03, 29.05,
                 23.61, 22.21, 26.81, 26.01, 25.98,
                 24.75, 26.60, 24.10, 23.09, 26.56,
                 26.58, 23.65, 26.19, 25.11, 28.18,
                 27.84, 21.45, 19.85, 30.95, 22.40,
                 26.95, 20.23, 26.46, 28.64, 21.48,
                 25.04, 24.18, 21.74, 25.64, 26.86,
                 17.39, 20.73, 16.34, 22.75, 24.80,
                 28.25, 22.33, 26.43, 24.50, 24.04,
                 21.71, 25.43, 29.21, 22.84, 17.54,
                 24.69, 24.44, 22.18, 18.79, 23.58,
                 24.18, 23.30, 19.55, 26.90, 26.38,
                 20.53, 24.10, 16.13, 21.11, 23.03,
                 16.26, 26.19, 20.99, 26.33, 26.31, 
                 30.61, 26.48, 24.31, 27.98)

# dose 
dose <- as.factor(c(rep("0", 20), rep("5", 19),
                    rep("50", 18), rep("500", 17)))

born_weight_data <- data.frame(weight_data, dose) 

# The ggplot2 package can be used to plot the box plot of the data in Figure 2.
ggplot(born_weight_data, aes(x = dose, y = weight_data)) + 
  geom_boxplot() +
  ylab("Born Weight (gr)") +
  xlab("Dose Treatment")

# Describe the born weight data by species using describe.by(.) function
describe.by(born_weight_data$weight_data, born_weight_data$dose)

# It is seen that the variances of the dose groups may be unequal
# To conclude whether the variance homogenity assumption is valid, 
# Levene test is used. 
car::LeveneTest(weight_data ~ dose)

# The GF, AF, and PB are used to conclude there is a significance difference between 
# the mean born weight of mice according to used dose group.
doex::GF(weight_data, dose)
doex::AF(weight_data, dose)
doex::PB(weight_data, dose)