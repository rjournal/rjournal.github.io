
## Loading the packages and dataset
library(onewaytests)
library(palmerpenguins) 

## Obtaining the descriptive statistics 
describe(body_mass_g ~ species, data = penguins)

## Assessing the variance homogeneity assumption
homog.test(body_mass_g ~ species, data = penguins, method = "Levene")

## Assessing the normality assumption
nor.test(body_mass_g ~ species, data = penguins, method = "SW")

## Conducting some of the one-way independent groups comparison tests
out <- onewaytests(body_mass_g ~ species, data = penguins, method = "af")
summary(out)

out <- onewaytests(body_mass_g ~ species, data = penguins, method = "ss", verbose = FALSE)
summary(out)

out <- onewaytests(body_mass_g ~ species, data = penguins, method = "wgf", verbose = FALSE)
summary(out)

out <- onewaytests(body_mass_g ~ species, data = penguins, method = "james", verbose = FALSE)
summary(out)

## Performing pairwise comparisons 
out <- onewaytests(body_mass_g ~ species, data = penguins, method = "af", verbose = FALSE)
paircomp(out, adjust.method = "bonferroni")

## Graphical approaches
# Box-and-whisker plot with violin lines and dots
gplot(body_mass_g ~ species, data = penguins, type = "boxplot-violin", 
         width = c(0.4, 0.95, NA), binwidth = 50, ylab = "body mass (grams)")

# Box-and-whisker plot with violin lines
gplot(body_mass_g ~ species, data = penguins, type = "boxplot-violin", 
         width = c(0.4, 0.95, NA), dots = FALSE, ylab = "body mass (grams)")

# Box-and-whisker plot
gplot(body_mass_g ~ species, data = penguins, type = "boxplot", 
         width = c(0.4, NA, NA), dots = FALSE, ylab = "body mass (grams)")

# Violin plot
gplot(body_mass_g ~ species, data = penguins, type = "violin",
         width = c(NA, 0.95, NA), dots = FALSE, ylab = "body mass (grams)")

# Error bar (mean +- standard deviation)
gplot(body_mass_g ~ species, data = penguins, type = "errorbar", 
         width = c(NA, NA, 0.3), binwidth = 50, ylab = "body mass (grams)", 
         option = "sd")

# Error bar (mean +- standard deviation) with mean bar
gplot(body_mass_g ~ species, data = penguins, type = "errorbar",
         width = c(NA, NA, 0.3), binwidth = 50, ylab = "body mass (grams)", 
         option = "sd", bar = TRUE)
