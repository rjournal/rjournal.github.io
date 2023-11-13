
global dir "/Users/mauriciosarrias/Library/CloudStorage/Dropbox/work/Packages/Discrete-JSS/R-Journal/StataFiles"
log using "$dir/het_stata.log", replace

************************************
*** Heterokedastic binary models ***
************************************

*=========================================
*** Example 1: Promotion of scientists ***
*=========================================

import delimited "$dir/tenure.csv", clear 

** Logit models for men and women
quietly eststo logit_m: logit tenure year c.year#c.year select articles prestige if (year <= 10 & female == 0)
quietly eststo logit_w: logit tenure year c.year#c.year select articles prestige if (year <= 10 & female == 1)

esttab logit_m logit_w, b(3) se(3)

** Heterokedastic logit model
quietly ssc install oglm
quietly eststo het_logit: oglm tenure i.female year c.year#c.year select articles prestige if (year <= 10), hetero(i.female) link(logit)
esttab logit_m logit_w het_logit, b(3) se(3)

** Testing how much the disturbance standard deviation differ by gender
margins, expression((1 - exp([lnsigma]_b[1.female])) / exp([lnsigma]_b[1.female]))

** Heterokedastic logit model 2
eststo het_logit2: oglm tenure i.female year c.year#c.year select articles prestige i.female#c.articles if (year <= 10), hetero(i.female) link(logit)

** Plot predicted probability and sigma
predict phat, pr outcome(1)
predict sigmahat, sigma
hist phat
hist sigmahat

** Average Marginal Effects for logit and probit heterokedastic models
quietly oglm tenure i.female year c.year#c.year select articles prestige i.female#c.articles if (year <= 10), hetero(i.female) link(probit)
eststo eff_probit: margins, dydx(*) predict(outcome(1)) post
quietly oglm tenure i.female year c.year#c.year select articles prestige i.female#c.articles if (year <= 10), hetero(i.female) link(logit)
eststo eff_logit: margins, dydx(*) predict(outcome(1)) post
esttab eff_probit eff_logit, b(3) se(3)


*=========================================
*** Example 2: Labor Participation ***
*=========================================

* Open dataset and create variables
webuse mroz, clear
gen kids = (kidslt6 + kidsge6) > 0
gen finc = faminc/10000

* Hetekedastic binary probit model
quietly eststo labor_hom: probit inlf age c.age#c.age finc educ kids
quietly eststo labor_het: oglm inlf age c.age#c.age finc educ i.kids, hetero(finc i.kids) link(probit)
quietly eststo eff_labor_het: margins, dydx(*) predict(outcome(1)) post
esttab labor_hom labor_het eff_labor_het, b(3) se(3)

* Wald test
estimates restore labor_het
quietly oglm
test [lnsigma]: finc 1.kids

log close
