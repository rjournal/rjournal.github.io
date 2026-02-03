library(coneproj)
library(csurvey)
library(survey)
library(data.table)
library(tidyverse)
library(MASS)
library(rlang)
library(zeallot)
library(patchwork)
library(knitr)
library(tinytex)
#options(csurvey.multicore = TRUE)

#------------------------------------------------
#NHANES Example with monotonic domain means
#------------------------------------------------
data(nhdat2, package = "csurvey")
#use ?nhdat2 to see details of this data set
#specify the design:
dstrat <- svydesign(ids = ~id,  strata = ~str, data = nhdat2,  weight = ~wt)
#fit the model
set.seed(1)
ans <- csvy(chol ~ incr(age), design = dstrat, n.mix = 100)

cat("CIC (constrained):", ans$CIC, "\n")
cat("CIC (unconstrained):", ans$CIC.un, "\n")
cat(svycontrast(ans, list(avg = c(rep(-1, 13)/13, rep(1, 12)/12))), "\n")

#Figure 1
#see ?plot_csvy_control to find out how to adjust aesthetics
ctl = list(unconstrained_color = "grey80")
png("nhanes1.png")
plot(ans, type = 'both', control = ctl)
dev.off()

#Figure 2
data(nhdat2, package = 'csurvey')
#specify the design:
dstrat <- svydesign(ids = ~id, strata = ~str, data = nhdat2, weight = ~wt)
#use parallel computing:
#options(csurvey.multicore=TRUE)
set.seed(1)
ans <- csvy(chol ~ incr(age)*incr(wcat)*icat, design = dstrat)

domains <- data.frame(age = c(24, 35), wcat = c(2, 4), icat = c(2, 3))
pans <- predict(ans, newdata = domains, se.fit = TRUE)
cat("Predicted values, confidence intervals and standard errors for specified domains:\n")
print (pans)

ctl <- list(x1lab = 'waist', x2lab = 'income', subtitle.size = 8)

png('nhanes_grid3.png')
plot(ans, x1 = "wcat", x2 = "icat", control = ctl, domains = domains)
dev.off()

#Figure 3
png('nhanes_grid3_un.png')
plot(ans, x1 = "wcat", x2 = "icat", control = ctl, type="unconstrained")
dev.off()

#----------------------------------------------------------------------
#Example: Constrained domain means with a block ordering
#----------------------------------------------------------------------
load('../nscg19.rda')
rds <- svrepdesign(data = nscg, repweights = dplyr::select(nscg, "RW0001":"RW0320"),
                   weights = ~w, combined.weights = TRUE, mse = TRUE, type = "other", scale = 1, rscale = 0.05)

#options(csurvey.multicore = TRUE)
ans <- csvy(logSalary~decr(hd_year_grouped)*incr(hd_type)*block.Ord(field, order = c(2, 1, 2, 1, 2))*region, design = rds)

#Figure 4
ctl <- list(categ = "region", categnm = c("New England", "Middle Atlantic", "East North Central", "West North Central",
                                          "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific and US Territories"),
            NCOL = 3, th = 60, xlab = "years since degree", ylab = "field of study", zlab = "log(salary)")

png('new_surfaces9.png')
plotpersp(ans, x1="hd_year_grouped", x2="field", control = ctl) 
dev.off()

#Figure 5
#use regions: 3, 9, 6 
#use the patchwork package to stack three plots into one
ctl1 <- list(x1lab = " ",x2lab = "Field of study", x3lab = "Year of award of highest degree",
  x1_labels = FALSE,  x2_labels = FALSE, x3_labels = c(1995, 2000, 2005, 2010, 2015),
  x4_labels =  c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central",
                 "Mountain", "Pacific and US Territories"), ynm = "log(Salary)", ci = TRUE, legend = FALSE,  ylab=FALSE, unconstrained_color = 'grey80', x4_vals = 3, subtitle.size = 10)

p1 <- plot(ans, x1 = 'hd_type', x2 = 'field', x3 = 'hd_year_grouped',
           control = ctl1, type = 'both') 

ctl2 <- list(x1lab = " ", x2lab = " ", x3lab = " ", x1_labels = FALSE,  x2_labels = FALSE, x3_labels = FALSE,
  x4_labels =  c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central",
   "Mountain", "Pacific and US Territories"), ynm = 'log(Salary)', ci = TRUE, legend = FALSE,  ylab = TRUE, unconstrained_color = 'grey80', x4_vals = 9)

p2 <- plot(ans, x1 = 'hd_type', x2 = 'field', x3 = 'hd_year_grouped',
           control = ctl2, type = 'both') 

ctl3 <- list(x1lab = 'Highest degree type', x2lab = " ", x3lab = " ",
  x1_labels = c('BS', 'MS', 'PHD'), x2_labels = FALSE, x3_labels = FALSE,
  x4_labels =  c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central",
  "Mountain", "Pacific and US Territories"), ynm = 'log(Salary)', ci = TRUE, legend = FALSE, ylab = FALSE, x1size = 2.5,
  unconstrained_color = 'grey80', x4_vals = 6, angle = 0, hjust = .5)

p3 <- plot(ans, x1 = 'hd_type', x2 = 'field', x3 = 'hd_year_grouped', control = ctl3, type = 'both') 

#use the patchwork package to stack three plots into one
library(patchwork)

png('newplot4.png')
(p1 / p2 / p3)
dev.off()

#--------------------------
#Example:One-sided testing
#--------------------------
load('../nscg19_2.rda')

#code to generate Table 1
pvals.table = data.frame(csvy_onesided = numeric(6), svyglm_twosided = numeric(6))
rownames(pvals.table) = c('2008-09', '2010-11', '2012-13', '2014-15', '2016-17', '2018-19')

#we need to set a random seed because the p-value for csvy is simulated
set.seed(1)
#svyglm may throw an error message when there is an empty domain, and we use tryCatch to address this issue
#we include the failing example for svyglm on purpose because we want to show that the proposed method csvy will work in some cases when svyglm fails
for(i in 1:6){
  print (i)
  years <- c(2008, 2009) + 2 * (i - 1)
  #select the data for specific years
  data <- nscg2 |> dplyr::filter(hd_year %in% years)
  rds <- svrepdesign(data = data, repweights = dplyr::select(data, "RW0001":"RW0320"), weights = ~w,
                    combined.weights = TRUE, mse = TRUE, type = "other", scale = 1, rscale = 0.05)
  
  ans <- csvy(logSalary ~ incr(daded) * field * region, design = rds, test = TRUE)
  #p-value for the constrained estimate
  pvals.table[i, 1] = ans$pval
  
  #when there is an empty domain, svyglm may not work and will throw an error message, we use tryCatch for the 
  #objects fitted by svyglm in this for loop 
  ansu1 <- tryCatch({svyglm(formula = logSalary ~ factor(field) * factor(daded) * factor(region), design = rds, data = data)}, error = function(e) {e})
  ansu2 <- tryCatch({svyglm(formula = logSalary ~ factor(field) * factor(region), design = rds, data = data)}, error = function(e) {e})
  #if ansu1 will be an error, then the anova function will not work and throw an error message, we use tryCatch for the
  #object fitted by anova in this for loop
  punc <- tryCatch({as.numeric(anova(ansu1, ansu2)[6])}, error = function(e) {e})
  if (inherits(punc, "error")) {
    #p-value for the unconstrained estimate
    pvals.table[i, 2] = NA
  } else {
    pvals.table[i, 2] = punc
  }
}

comppv = data.frame(year = c(rep("2008-09", 2), rep("2010-11", 2), rep("2012-13", 2), 
                             rep("2014-15", 2), rep("2016-17", 2), rep("2018-19", 2)), 
                    test = rep(c("one", "two"), 6), pvals = as.vector(t(round(pvals.table, 3))))

#print out results in Table 1
#cat("Table 1:", "\n")
comppv = kable(t(comppv)) 
print (comppv)

#compile the Latex file, which saves the result, into a pdf file using the pdflatex function
tinytex::pdflatex("../tables/comppv.tex")


#Figure 6
data <- nscg2 |> dplyr::filter(hd_year %in% c(2016, 2017))
#specify a survey design with replicate weights
rds <- svrepdesign(data = data, repweights = dplyr::select(data, "RW0001":"RW0320"), weights = ~w,
                  combined.weights = TRUE, mse = TRUE, type = "other", scale = 1, rscale = 0.05)
#fit the model
ans <- csvy(logSalary ~ incr(daded) * field * region, design = rds)

#create the plot
ctl <- list(x1lab = 'Field', x2lab = 'Region', subtitle.size=10,  x1size=3, x2size=3, x3lab = "father's education",
            ynm = "log(salary)", unconstrained_color = "red", ci = FALSE,
            x2_labels = c('Northeast', 'North Central', 'Southeast', 'West', 'Pacific & Territories'))

png('daded.png')
plot(ans, x1='field', x2='region', control = ctl, type = 'both')
dev.off()

#show the one-sided test result for 2008-2009
load("../nscg19_2.rda")
data <- nscg2 |> dplyr::filter(hd_year %in% c(2008, 2009))
rds <- svrepdesign(data = data, repweights = dplyr::select(data, "RW0001":"RW0320"), weights = ~w,
                   combined.weights = TRUE, mse = TRUE, type = "other",
                   scale = 1, rscale = 0.05)
set.seed(1)
ans <- csvy(logSalary ~ incr(daded) * field * region, design = rds, test = TRUE)
summary(ans)

#------------------------
#Example: Binary outcome
#------------------------
#use ?nhdat to check details of this data set
data(nhdat, package = 'csurvey')
#specify the design
dstrat <- svydesign(ids = ~ id, strata = ~ str, data = nhdat, weight = ~ wt)

#fit the model
set.seed(1)
ans <- csvy(chol ~ incr(age) * incr(wcat) * gender, design = dstrat, family = binomial(link = "logit"), test = TRUE)

#show the one-sided test result for 2008-2009
summary(ans)

#Figure 7
#see ?plot_csvy_control to get more information about how to adjust aesthetics
ctl <- list(x1lab = 'waist', x2lab = 'gender', ynm = 'high cholesterol level', x2_labels = c('male', 'female'), ci = TRUE, subtitle.size = 8)
png('nhanes_bin.png')
plot(ans, x1='wcat', x2='gender', type="both", control = ctl)
dev.off()






























