#---------------------------------------------------------------------------
# R code used in the paper in the RJournal:
# “Two-stage Sampling Design and Sample Selection with the R package R2BEAT”
# Authors: Giulio Barcaroli, Andrea Fasulo, Alessio Guandalini, Marco D. Terribili
# Corresponding author: Giulio Barcaroli
# email: gbarcaroli@gmail.com
#---------------------------------------------------------------------------



## --------------------------------------------------------------------
if (!"R2BEAT" %in% rownames(installed.packages())) {
  install.packages("R2BEAT")
}
library("R2BEAT")


## --------------------------------------------------------------------
## Sampling frame
if (!file.exists("pop_lfs.csv")) {
  url <- "https://zenodo.org/records/10172873/files/pop_lfs.csv?download=1"
  download.file(url, destfile = "pop_lfs.csv")
}
pop <-read.csv("pop_lfs.csv")
pop$region <- as.factor(pop$region)
pop$province <- as.factor(pop$province)
pop$id_hh <- as.factor(pop$id_hh) 
pop$stratum <- as.factor(pop$stratum)
pop$cl_age <- as.factor(pop$cl_age)
pop$active <- as.numeric(pop$active)
pop$unemployed <- as.numeric(pop$unemployed)
pop$inactive <- as.numeric(pop$inactive)

str(pop)


## --------------------------------------------------------------------
cv <- as.data.frame(list(DOM=c("DOM1","DOM2"),
                         CV1=c(0.02,0.03),
                         CV2=c(0.03,0.06),
                         CV3=c(0.03,0.06),
                         CV4=c(0.05,0.08)))
cv


## --------------------------------------------------------------------
sens_min_SSU <- sensitivity_min_SSU (
             samp_frame=pop,
             errors=cv,
             id_PSU="municipality",
             id_SSU="id_ind",
             strata_var="stratum",
             target_vars=c("income_hh","active","inactive","unemployed"),
             deff_var="stratum",
             domain_var="region",
             delta=1,
             deff_sugg=1,
             min=30,
             max=80,
             plot=TRUE)


## --------------------------------------------------------------------
## Preparation of inputs for allocation steps
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "stratum"   
target_vars <- c("income_hh","active","inactive","unemployed")   
deff_var <- "stratum"     
domain_var <- "region"  
delta =  1       # households = survey units
minimum <- 50    # minimum number of SSUs to be interviewed in each selected PSU
deff_sugg <- 1.5 # suggestion for the deff value
 
inp1 <- prepareInputToAllocation1(samp_frame,
                                id_PSU,
                                id_SSU,
                                strata_var,
                                target_vars,
                                deff_var,
                                domain_var,
                                minimum,
                                delta,
                                deff_sugg)


## --------------------------------------------------------------------
head(inp1$deff)


## --------------------------------------------------------------------
head(inp1$effst)


## --------------------------------------------------------------------
head(inp1$rho)


## --------------------------------------------------------------------
head(inp1$psu_file)


## --------------------------------------------------------------------
head(inp1$des_file)


## --------------------------------------------------------------------
inp1$desfile$MINIMUM <- 50
alloc1 <- beat.2st(stratif = inp1$strata, 
                  errors = cv, 
                  des_file = inp1$des_file, 
                  psu_file = inp1$psu_file, 
                  rho = inp1$rho, 
                  deft_start = NULL,
                  effst = inp1$effst, 
                  minPSUstrat = 2,
                  minnumstrat = 50
                  )


## --------------------------------------------------------------------
set.seed(1234)
sample_1st <- select_PSU(alloc1, type="ALLOC", pps=TRUE)


## --------------------------------------------------------------------
sample_1st$PSU_stats


## --------------------------------------------------------------------
samp <- select_SSU(df=pop,
                   PSU_code="municipality",
                   SSU_code="id_ind",
                   PSU_sampled=sample_1st$sample_PSU)


## --------------------------------------------------------------------
## Plot of weights distribution
par(mfrow=c(1, 2))
boxplot(samp$weight,col="orange")
title("Weights distribution (total sample)",cex.main=0.7)
boxplot(weight ~ region, data=samp,col="orange")
title("Weights distribution by region",cex.main=0.7)
par(mfrow=c(1, 2))
boxplot(weight ~ province, data=samp,col="orange")
title("Weights distribution by province",cex.main=0.7)
boxplot(weight ~ stratum, data=samp,col="orange")
title("Weights distribution by stratum",cex.main=0.7)


## --------------------------------------------------------------------
df=pop
df$one <- 1
PSU_code="municipality"
SSU_code="id_ind"
target_vars <- c("income_hh",
                 "active",
                 "inactive",
                 "unemployed")  


## --------------------------------------------------------------------
# Domain level = national
domain_var <- "one"
set.seed(1234)
eval11 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    sample_1st$sample_PSU,
                    nsampl=100, 
                    writeFiles=FALSE) 
eval11$coeff_var


## --------------------------------------------------------------------
# Domain level = regional
domain_var <- "region"
set.seed(1234)
set.seed(1234)
eval12 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    sample_1st$sample_PSU,
                    nsampl=100, 
                    writeFiles=FALSE) 
eval12$coeff_var


## --------------------------------------------------------------------
alloc1$sensitivity


## --------------------------------------------------------------------
save(samp,file="sample.RData")

## ----message=FALSE---------------------------------------------------
if (!"ReGenesees" %in% rownames(installed.packages())) {
  install.packages("ReGenesees_2.3.tar.gz", repos = NULL, type = "source")
}
library(ReGenesees)


## --------------------------------------------------------------------
## Sample design description
samp$stratum_2 <- as.factor(samp$stratum_2)
sample.des <- e.svydesign(samp, 
                          ids= ~ municipality + id_hh, 
                          strata = ~ stratum_2, 
                          weights = ~ weight,
                          self.rep.str = ~ SR,
                          check.data = TRUE)


## --------------------------------------------------------------------
## Find and collapse lonely strata
ls <- find.lon.strata(sample.des)
if (!is.null(ls)) sample.des <- collapse.strata(sample.des)


## --------------------------------------------------------------------
## Calibration with known totals
totals <- pop.template(sample.des,
             calmodel = ~ sex : cl_age, 
             partition = ~ region)
totals <- fill.template(pop, totals, mem.frac = 10)
sample.cal <- e.calibrate(sample.des, 
                          totals,
                          calmodel = ~ sex : cl_age, 
                          partition = ~ region,
                          calfun = "logit",
                          bounds = c(0.676, 1.279), 
                          aggregate.stage = 2,
                          force = FALSE)


## --------------------------------------------------------------------
samp_frame <- pop
RGdes <- sample.des
RGcal <- sample.cal
strata_var <- c("stratum")      
target_vars <- c("income_hh",
                 "active",
                 "inactive",
                 "unemployed")   
weight_var <- "weight"
deff_var <- "stratum"            
id_PSU <- c("municipality")      
id_SSU <- c("id_hh")             
domain_var <- c("region") 
delta <- 1                   
minimum <- 50                
inp2 <- prepareInputToAllocation2(
        samp_frame,  # sampling frame
        RGdes,       # ReGenesees design object
        RGcal,       # ReGenesees calibrated object
        id_PSU,      # identification variable of PSUs
        id_SSU,      # identification variable of SSUs
        strata_var,  # strata variable
        target_vars, # target variables
        deff_var,    # deff variable
        domain_var,  # domain variable
        delta,       # Average number of SSUs for each selection unit
        minimum      # Minimum number of SSUs to be selected in each PSU
      )


## --------------------------------------------------------------------
head(inp2$strata)


## --------------------------------------------------------------------
head(inp2$deff)


## --------------------------------------------------------------------
head(inp2$effst)


## --------------------------------------------------------------------
head(inp2$rho)


## --------------------------------------------------------------------
head(inp2$psu_file)


## --------------------------------------------------------------------
head(inp2$des_file)


## --------------------------------------------------------------------
set.seed(1234)
inp2$des_file$MINIMUM <- 50
alloc2 <- beat.2st(stratif = inp2$strata, 
                  errors = cv, 
                  des_file = inp2$des_file, 
                  psu_file = inp2$psu_file, 
                  rho = inp2$rho, 
                  deft_start = NULL, 
                  effst = inp2$effst,
                  minnumstrat = 2, 
                  minPSUstrat = 2)


## --------------------------------------------------------------------
set.seed(1234)
sample_1st <- select_PSU(alloc2, type="ALLOC", pps=TRUE)


## --------------------------------------------------------------------
sample_1st$PSU_stats


## --------------------------------------------------------------------
set.seed(1234)
samp <- select_SSU(df=pop,
                   PSU_code="municipality",
                   SSU_code="id_ind",
                   PSU_sampled=sample_1st$sample_PSU)


## --------------------------------------------------------------------
nrow(samp)
sum(alloc2$alloc$ALLOC[-nrow(alloc2$alloc)])


## --------------------------------------------------------------------
nrow(pop)
sum(samp$weight)


## --------------------------------------------------------------------
## Plot of weights distribution
par(mfrow=c(1, 2))
boxplot(samp$weight,col="orange")
title("Weights distribution (total sample)",cex.main=0.7)
boxplot(weight ~ region, data=samp,col="orange")
title("Weights distribution by region",cex.main=0.7)
par(mfrow=c(1, 2))
boxplot(weight ~ province, data=samp,col="orange")
title("Weights distribution by province",cex.main=0.7)
boxplot(weight ~ stratum, data=samp,col="orange")
title("Weights distribution by stratum",cex.main=0.7)


## --------------------------------------------------------------------
df=pop
df$one <- 1
PSU_code="municipality"
SSU_code="id_ind"
target_vars <- c("income_hh",
                 "active",
                 "inactive",
                 "unemployed")  


## --------------------------------------------------------------------
# Domain level = national
domain_var <- "one"
set.seed(1234)
eval21 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    PSU_sampled=sample_1st$sample_PSU,
                    nsampl=100, 
                    writeFiles=FALSE) 
eval21$coeff_var


## --------------------------------------------------------------------
# Domain level = regional
domain_var <- "region"
set.seed(1234)
eval22 <- eval_2stage(df,
                    PSU_code,
                    SSU_code,
                    domain_var,
                    target_vars,
                    PSU_sampled=sample_1st$sample_PSU,
                    nsampl=100, 
                    writeFiles=FALSE) 
eval22$coeff_var


## --------------------------------------------------------------------
alloc2$sensitivity


## --------------------------------------------------------------------
results <- as.data.frame(list(Variable = rep(NA,24),
                              Package = rep(NA,24),
                              Unit = rep(NA,24),
                              Units = rep(NA,24)))


## --------------------------------------------------------------------
if (!"PracTools" %in% rownames(installed.packages())) {
  install.packages("PracTools")
}
library(PracTools)


## --------------------------------------------------------------------
# Probabilities of inclusion I stage
pp <- as.numeric(table(pop$municipality))/nrow(pop)


## --------------------------------------------------------------------
bw <- BW2stagePPS(pop$income_hh, 
                  pp,
                  psuID=pop$municipality)
bw
des1 <- clusOpt2(C1=130,
                 C2=1,
                 delta=bw[6],
                 unit.rv=bw[3],
                 k=bw[5],
                 CV0=0.02,
                 tot.cost=NULL,
                 cal.sw=2)
des1
sample_size <- des1$m.opt*des1$n.opt
sample_size
results[1,1] <- "income_hh"
results[1,2] <- "PractTools"
results[1,3] <- "PSU" 
results[1,4] <- round(des1$m.opt)
results[2,1] <- "income_hh"
results[2,2] <- "PractTools"
results[2,3] <- "SSU"
results[2,4] <- round(des1$m.opt*des1$n.opt)


## --------------------------------------------------------------------
bw <- BW2stagePPS(pop$active, 
                  pp,
                  psuID=pop$municipality)
bw
des2 <- clusOpt2(C1=180,
                 C2=1,
                 delta=bw[6],
                 unit.rv=bw[3],
                 k=bw[5],
                 CV0=0.03,
                 tot.cost=NULL,
                 cal.sw=2)
des2
sample_size <- des2$m.opt*des2$n.opt
sample_size
results[3,1] <- "active"
results[3,2] <- "PractTools"
results[3,3] <- "PSU" 
results[3,4] <- round(des2$m.opt)
results[4,1] <- "active"
results[4,2] <- "PractTools"
results[4,3] <- "SSU" 
results[4,4] <- round(des2$m.opt*des2$n.opt)


## --------------------------------------------------------------------
# Third variable (inactive)
bw <- BW2stagePPS(pop$inactive, 
                  pp,
                  psuID=pop$municipality)
bw
des3 <- clusOpt2(C1=5,
                 C2=1,
                 delta=bw[6],
                 unit.rv=bw[3],
                 k=bw[5],
                 CV0=0.03,
                 tot.cost=NULL,
                 cal.sw=2)
des3
sample_size <- des3$m.opt*des3$n.opt
sample_size
results[5,1] <- "inactive"
results[5,2] <- "PractTools"
results[5,3] <- "PSU" 
results[5,4] <- round(des3$m.opt)
results[6,1] <- "inactive"
results[6,2] <- "PractTools"
results[6,3] <- "SSU" 
results[6,4] <- round(des3$m.opt*des3$n.opt)


## --------------------------------------------------------------------
# Fourth variable (unemployed)
bw <- BW2stagePPS(pop$unemployed, 
                  pp,
                  psuID=pop$municipality)
bw
des4 <- clusOpt2(C1=350, # whatever value, m.opt always exceed number of municipalities
                 C2=1,
                 delta=bw[6],
                 unit.rv=bw[3],
                 k=bw[5],
                 CV0=0.05,
                 tot.cost=NULL,
                 cal.sw=2)
des4
sample_size <- des4$m.opt*des4$n.opt
sample_size
results[7,1] <- "unemployed"
results[7,2] <- "PractTools"
results[7,3] <- "PSU" 
results[7,4] <- round(des4$m.opt)
results[8,1] <- "unemployed"
results[8,2] <- "PractTools"
results[8,3] <- "SSU"
results[8,4] <- round(des4$m.opt*des4$n.opt)


## --------------------------------------------------------------------
# FIRST VARIABLE : income_hh
## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.02)))
## -----------------------------------------------------------
## Prepare inputs for allocation
samp_frame <- pop
samp_frame$one <- 1
id_PSU <- "municipality"  
id_SSU <- "id_ind"        
strata_var <- "one"   
target_vars <- c("income_hh") 
deff_var <- "stratum"     
domain_var <- "one"  
delta =  1       # households = survey units
minimum <- 50    # minimum number of SSUs to be interviewed in each selected PSU
deff_sugg <- 1.5 # suggestion for the deff value
inp1 <- prepareInputToAllocation1(samp_frame,
                                  id_PSU,
                                  id_SSU,
                                  strata_var,
                                  target_vars,
                                  deff_var,
                                  domain_var,
                                  minimum,
                                  delta,
                                  deff_sugg)
inp1$rho
## -----------------------------------------------------------
## Allocation
alloc1 <- beat.2st(stratif = inp1$strata, 
                   errors = cv, 
                   des_file = inp1$des_file, 
                   psu_file = inp1$psu_file, 
                   rho = inp1$rho, 
                   deft_start = NULL, 
                   effst = inp1$effst,
                   epsilon1 = 5, 
                   mmdiff_deft = 1,maxi = 15, 
                   epsilon = 10^(-11), 
                   minPSUstrat = 2,
                   minnumstrat = 2, 
                   maxiter = 200, 
                   maxiter1 = 25)
PSUs <- select_PSU(alloc1,plot=F)
results[9,1] <- "income_hh"
results[9,2] <- "R2BEAT"
results[9,3] <- "PSU" 
results[9,4] <- PSUs$PSU_stats$PSU[nrow(PSUs$PSU_stats)]
results[10,1] <- "income_hh"
results[10,2] <- "R2BEAT"
results[10,3] <- "SSU" 
results[10,4] <- PSUs$PSU_stats$SSU[nrow(PSUs$PSU_stats)]


## --------------------------------------------------------------------
# SECOND VARIABLE : active
## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.03)))
## -----------------------------------------------------------
## Prepare inputs for allocation
target_vars <- c("active") 
inp2 <- prepareInputToAllocation1(samp_frame,
                                  id_PSU,
                                  id_SSU,
                                  strata_var,
                                  target_vars,
                                  deff_var,
                                  domain_var,
                                  minimum,
                                  delta,
                                  deff_sugg)
inp2$rho
## -----------------------------------------------------------
## Allocation
alloc2 <- beat.2st(stratif = inp2$strata, 
                   errors = cv, 
                   des_file = inp2$des_file, 
                   psu_file = inp2$psu_file, 
                   rho = inp2$rho, 
                   deft_start = NULL, 
                   effst = inp2$effst,
                   epsilon1 = 5, 
                   mmdiff_deft = 1,
                   maxi = 15, 
                   epsilon = 10^(-11), 
                   minPSUstrat = 2,
                   minnumstrat = 2, 
                   maxiter = 200, 
                   maxiter1 = 25)
PSUs <- select_PSU(alloc2,plot=F)
results[11,1] <- "active"
results[11,2] <- "R2BEAT"
results[11,3] <- "PSU" 
results[11,4] <- PSUs$PSU_stats$PSU[nrow(PSUs$PSU_stats)]
results[12,1] <- "active"
results[12,2] <- "R2BEAT"
results[12,3] <- "SSU" 
results[12,4] <- PSUs$PSU_stats$SSU[nrow(PSUs$PSU_stats)]


## --------------------------------------------------------------------
## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.03)))
## -----------------------------------------------------------
## Prepare inputs for allocation
target_vars <- c("inactive") 
inp3 <- prepareInputToAllocation1(samp_frame,
                                  id_PSU,
                                  id_SSU,
                                  strata_var,
                                  target_vars,
                                  deff_var,
                                  domain_var,
                                  minimum,
                                  delta,
                                  deff_sugg)
inp3$rho
## -----------------------------------------------------------
## Allocation
alloc3 <- beat.2st(stratif = inp3$strata, 
                   errors = cv, 
                   des_file = inp3$des_file, 
                   psu_file = inp3$psu_file, 
                   rho = inp3$rho, 
                   deft_start = NULL, 
                   effst = inp3$effst,
                   epsilon1 = 5, 
                   mmdiff_deft = 1,maxi = 15, 
                   epsilon = 10^(-11), 
                   minPSUstrat = 2,
                   minnumstrat = 2, 
                   maxiter = 200, 
                   maxiter1 = 25)
PSUs <- select_PSU(alloc3,plot=F)
results[13,1] <- "inactive"
results[13,2] <- "R2BEAT"
results[13,3] <- "PSU" 
results[13,4] <- PSUs$PSU_stats$PSU[nrow(PSUs$PSU_stats)]
results[14,1] <- "inactive"
results[14,2] <- "R2BEAT"
results[14,3] <- "SSU" 
results[14,4] <- PSUs$PSU_stats$SSU[nrow(PSUs$PSU_stats)]


## --------------------------------------------------------------------
## Precision constraints
cv <- as.data.frame(list(DOM=c("DOM1"),
                         CV1=c(0.05)))
## -----------------------------------------------------------
## Prepare inputs for allocation
target_vars <- c("unemployed") 
inp4 <- prepareInputToAllocation1(samp_frame,
                                  id_PSU,
                                  id_SSU,
                                  strata_var,
                                  target_vars,
                                  deff_var,
                                  domain_var,
                                  minimum,
                                  delta,
                                  deff_sugg)
inp4$rho
## -----------------------------------------------------------
## Allocation
alloc4 <- beat.2st(stratif = inp4$strata, 
                   errors = cv, 
                   des_file = inp4$des_file, 
                   psu_file = inp4$psu_file, 
                   rho = inp4$rho, 
                   deft_start = NULL, 
                   effst = inp4$effst,
                   epsilon1 = 5, 
                   mmdiff_deft = 1,maxi = 15, 
                   epsilon = 10^(-11), 
                   minPSUstrat = 2,
                   minnumstrat = 2, 
                   maxiter = 200, 
                   maxiter1 = 25)
PSUs <- select_PSU(alloc4,plot=F)
results[15,1] <- "unemployed"
results[15,2] <- "R2BEAT"
results[15,3] <- "PSU" 
results[15,4] <- PSUs$PSU_stats$PSU[nrow(PSUs$PSU_stats)]
results[16,1] <- "unemployed"
results[16,2] <- "R2BEAT"
results[16,3] <- "SSU" 
results[16,4] <- PSUs$PSU_stats$SSU[nrow(PSUs$PSU_stats)]


## --------------------------------------------------------------------
if (!"samplesize4surveys" %in% rownames(installed.packages())) {
  install.packages("samplesize4surveys")
}
library("samplesize4surveys")


## --------------------------------------------------------------------
pop_strata <- as.numeric(table(pop$stratum))
PSU <- length(unique(pop$municipality))
rho_income_hh <- inp1$rho$RHO_NAR1
rho_active <- inp2$rho$RHO_NAR1
rho_inactive<- inp3$rho$RHO_NAR1
rho_unmployed <- inp4$rho$RHO_NAR1


## --------------------------------------------------------------------
# First variable (income_hh)
a1 <- ss2s4m(N = nrow(pop), 
             mu = mean(pop$income_hh), 
             sigma = sd(pop$income_hh),
             # conf = 0.95,
             delta = 0.02 * 1.96,
             M = PSU, 
             to = 50, 
             rho = rho_income_hh)
a1[50,]
results[16,4] <- alloc4$iterations[nrow(alloc4$iterations),5]
results[17,1] <- "income_hh"
results[17,2] <- "samplesize4surveys"
results[17,3] <- "PSU" 
results[17,4] <- a1[50,2]
results[18,1] <- "income_hh"
results[18,2] <- "samplesize4surveys"
results[18,3] <- "SSU" 
results[18,4] <- a1[50,4]


## --------------------------------------------------------------------
# Second variable (active)
a2 <- ss2s4p(N = nrow(pop), 
             P = as.numeric(table(pop$active))[2]/nrow(pop), 
             # conf = 0.95, 
             delta = 0.03 * 1.96, 
             M = PSU, 
             to = 50, 
             rho = rho_active)
a2[50,]
results[19,1] <- "active"
results[19,2] <- "samplesize4surveys"
results[19,3] <- "PSU" 
results[19,4] <- a2[50,2]
results[20,1] <- "active"
results[20,2] <- "samplesize4surveys"
results[20,3] <- "SSU" 
results[20,4] <- a2[50,4]


## --------------------------------------------------------------------
a3 <- ss2s4p(N = nrow(pop), 
             P = as.numeric(table(pop$inactive))[2]/nrow(pop), 
             # conf = 0.95, 
             delta = 0.03 * 1.96, 
             M = PSU, 
             to = 50, 
             rho = rho_inactive)
a3[50,]
results[21,1] <- "inactive"
results[21,2] <- "samplesize4surveys"
results[21,3] <- "PSU" 
results[21,4] <- a3[50,2]
results[22,1] <- "inactive"
results[22,2] <- "samplesize4surveys"
results[22,3] <- "SSU" 
results[22,4] <- a3[50,4]


## --------------------------------------------------------------------
# Fourth variable (unemployed)
a4 <- ss2s4p(N = nrow(pop), 
             P = as.numeric(table(pop$unemployed))[2]/nrow(pop), 
             # conf = 0.95, 
             delta = 0.05 * 1.96, 
             M = PSU, 
             to = 50, 
             rho = rho_unmployed)
a4[50,]
results[23,1] <- "unemployed"
results[23,2] <- "samplesize4surveys"
results[23,3] <- "PSU" 
results[23,4] <- a4[50,2]
results[24,1] <- "unemployed"
results[24,2] <- "samplesize4surveys"
results[24,3] <- "SSU" 
results[24,4] <- a4[50,4]


## --------------------------------------------------------------------
counts <- aggregate(Units ~ Package + Variable, data=results[results$Unit == "PSU",], FUN=sum)
par(mfrow=c(1,1))
barplot(Units ~ Package + Variable,
        data = counts,
        main = "PSUs by variable and package",
        xlab = "Variable",
        col = c("red","green","blue"), beside=TRUE
)
legend("topleft",
       c("PractTools","R2BEAT","samplesize4surveys"),
       fill = c("red","green","blue")
)

counts <- aggregate(Units ~ Package + Variable, data=results[results$Unit == "SSU",], FUN=sum)
barplot(Units ~ Package + Variable,
        data = counts,
        main = "SSUs by variable and package",
        xlab = "Variable",
        col = c("red","green","blue"), beside=TRUE
)
legend("topleft",
       c("PractTools","R2BEAT","samplesize4surveys"),
       fill = c("red","green","blue")
)
