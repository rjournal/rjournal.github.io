##
## Replication file for htestClust package
## R Journal submission
##

##
## Section 3.2 - Simulated example data set

## import htestClust and look at first few observations in screen8 data
library(htestClust)
data(screen8)
head(screen8)

## tabulate number of participants from each school and see summary of cluster sizes
(tab <- table(screen8$sch.id))
summary(as.vector(tab))


## 
## Section 4.1 - Evaluating informative cluster size
##

## Test for ICS
## This test is slow due to bootstrap resampling. Set print.it = TRUE to see progression
set.seed(100)
ics.math <- icstestClust(screen8$math, screen8$sch.id, B = 1000, print.it = FALSE)
ics.math

### Figure 1
#windows(7,4)
par(mfrow = c(1,2))
icsPlot(x = screen8$math, id = screen8$sch.id, FUN = "mean", pch = 20)
icsPlot(x = screen8$read, id = screen8$sch.id, FUN = "mean", pch = 20)

### Figure 2
#windows(9, 5)
layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
       heights = c(1, 2), # Heights of the two rows
       widths = c(2, 2.5)) 
par(mar = c(5, 4, 1, 0))
icsPlot(x = screen8$gender, id = screen8$sch.id, FUN = "prop", ylab = "P(Female)", pch = 20)
par(mar = c(5, 4, 1, 5))
icsPlot(x = screen8$activity, id = screen8$sch.id, FUN = "prop", legend = TRUE,
        args.legend = list(x = "topright", bty = "n", inset=c(-0.32, 0)))

## 
## Section 4.2 - Testing a marginal proportion
## 
screen8$math.p <- 1*(screen8$math >= 65)
proptestClust(screen8$math.p, screen8$sch.id, p = .75, alternative = "great")
## illustrating general correspondence between proptestClust and prop.test when 
## cluster sizes are uniformly 1
set.seed(123)
x <- rbinom(100, size = 1, p = 0.7)
id <- 1:100
proptestClust(x, id)
prop.test(sum(x), length(x))

## 
## Section 4.3 - Test of independence
##
tab <- table(screen8$gender, screen8$activity, screen8$sch.id)
ptab <- prop.table(tab, c(1,3))
apply(ptab, c(1,2), mean)
chisqtestClust(screen8$gender, screen8$activity, screen8$sch.id)

## Standard chi-squared test, ignoring clustering
prop.table(table(screen8$gender, screen8$activity), 1)
chisq.test(screen8$gender, screen8$activity)

## 
## Section 4.4 - Tests of quantitative variables for two or more groups
##
ttestClust(math ~ gender, id = sch.id, data = screen8)

wilcoxtestClust(math ~ gender, id = sch.id, data = screen8, method = "group")

onewaytestClust(read ~ activity, id = sch.id, data = screen8)
