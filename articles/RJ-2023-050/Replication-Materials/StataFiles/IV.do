
global dir "/Users/mauriciosarrias/Library/CloudStorage/Dropbox/work/Packages/Discrete-JSS/R-Journal/StataFiles"
log using "$dir/ivprobit_stata.log", replace

************************************
*** IV Probit ***
************************************

*=========================================
*** Control function approach ***
*=========================================

import delimited "$dir/mroz.csv", clear 


* Probit estimates and marginal effect 
probit inlf educ exper c.exper#c.exper age kidslt6 kidsge6 nwifeinc
margins, dydx(nwifeinc)

* Control function approach
eststo fstep: reg nwifeinc educ exper c.exper#c.exper age kidslt6 kidsge6 huseduc
predict res_hat, resi

test huseduc

eststo sstep: probit inlf educ exper c.exper#c.exper age kidslt6 kidsge6 nwifeinc res_hat

* Two-step IV-probit
ivprobit inlf educ exper c.exper#c.exper age kidslt6 kidsge6 (nwifeinc = huseduc), twostep

*=========================================
*** MLE ***
*=========================================

ivprobit inlf educ exper c.exper#c.exper age kidslt6 kidsge6 (nwifeinc = huseduc)
eststo me1: margins, dydx(*) predict(pr) post
qui ivprobit inlf educ exper c.exper#c.exper age kidslt6 kidsge6 (nwifeinc = huseduc)
eststo me2: margins, dydx(*) predict(pr fix(nwifeinc)) post
esttab  me1 me2, b(3) se(3)

log close
