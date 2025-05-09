### ConcordanceTestRJ R code

# The ConcordanceTest package can be installed from CRAN:

install.packages("ConcordanceTest")

# After installation, the package must be loaded:

library("ConcordanceTest")

# Code for Figure 1
par(mfrow = c(1,2))
Sample_Sizes <- c(2,2,2)
ProbDistr <- CT_Distribution(Sample_Sizes, Num_Sim = 0, H = 1)
CT_Probability_Plot(C_freq = ProbDistr$C_freq, H_freq = ProbDistr$H_freq)

# Example 1
# Using the function CT_Distribution() as follows,
# we can approximate the probability distributions of 
# Example 1 of the paper by simulating, for example, 
# 25,000 permutations of 3 treatments with 2 patients 
# each. Note that, for reproducibility, we always initialize 
# the generator for pseudo-random numbers when the results 
# rely on simulation.

set.seed(12)
Sample_Sizes <- c(2,2,2)
CT_Distribution(Sample_Sizes, Num_Sim = 25000, H = 1)

# Example 2
# Using the function CT_Hypothesis_Test() as follows,
# we obtain de Concordance and Kruskal-Wallis tests of 
# Example 2 of the paper (application of three treatments,
# A, B and C, to 18 patients, measuring the number of hours 
# it takes these patients to recover). 
# We use 25,000 simulations.

set.seed(12)
A <- c(12,13,15,20,23,28,30,32,40,48)
B <- c(29,31,49,52,54)
C <- c(24,26,44)
Sample_List <- list(A, B, C)
CT_Hypothesis_Test(Sample_List, Num_Sim = 25000, H = 1)


# Using the function CT_Critical_Values() as follows,
# we can compare the test statistics with different 
# significance levels with sample sizes N = (10, 5, 3), 
# as in Example 2.

set.seed(12)
Sample_Sizes <- c(10,5,3)
CT_Critical_Values(Sample_Sizes, Num_Sim = 25000, H = 1)


# Using the function CT_Coefficient() as follows,
# we obtain the Concordance coefficient and the Kruskal-Wallis 
# statistic from the result of an experiment. This function
# is useful when we only want to obtain the value of 
# the statistic to check its significance using statistical tables.
# We use this function for the data in Example 2 of the paper.

A <- c(12,13,15,20,23,28,30,32,40,48)
B <- c(29,31,49,52,54)
C <- c(24,26,44)
Sample_List <- list(A, B, C)
CT_Coefficient(Sample_List, H = 1)


# Code for Figure 2
# The ConcordanceTest package can perform the graphical 
# visualization of the probability distributions (function 
# CT_Probability_Plot()) and the density distributions 
# (function CT_Density_Plot()) of the Concordance coefficient
# and the Kruskal-Wallis statistic. The following figure 
# compares the probability distributions (first row) and 
# the density distributions (second row) of Example 2 
# generated by simulation.

set.seed(12)
Sample_Sizes <- c(10,5,3)
ProbDistr <- CT_Distribution(Sample_Sizes, Num_Sim = 25000, H = 1)
layout(matrix(c(1,3,2,3), ncol=2))
CT_Probability_Plot(C_freq = ProbDistr$C_freq, H_freq = ProbDistr$H_freq)
CT_Density_Plot(C_freq = ProbDistr$C_freq, H_freq = ProbDistr$H_freq)


# The ConcordanceTest package also contains the function LOP(), 
# which solves the Linear Ordering Problem from a square 
# data matrix. This function allows to calculate the disorder 
# of a permutation of elements from the preference matrix 
# induced by that permutation.
# We apply the function LOP() on the matrix of preferences 
# between treatments observed in Example 2.

mat_LOP <- matrix(c(0,7,11,43,0,13,19,2,0), nrow=3)
LOP(mat_LOP)

# Example 4
# The well-known stats package contains, among many other functions, 
# the function kruskal.test() that performs a Kruskal-Wallis rank 
# sum test. In this example, we compare the results obtained with 
# the ConcordanceTest package and the function kruskal.test(), 
# making use of the Hollander & Wolfe (1973) dataset referenced 
# in the kruskal.test() examples. In the paper we discuss why the 
# test statistics are the same but the p-values differ.

## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.

x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
Sample_List <- list(x, y, z)

kruskal.test(Sample_List)

set.seed(12)
CT_Hypothesis_Test(Sample_List, Num_Sim = 25000, H = 1)


