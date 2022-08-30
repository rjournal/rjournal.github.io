#JMC function

library(JMcmprsk)
set.seed(123)
data(lung)
yread <- lung[, c(1,2:11)]
cread <- unique(lung[, c(1, 12, 13, 6:10)])

jmcfit <- jmc(long_data = yread, surv_data = cread, out = "FVC",
           FE = c("time", "FVC0", "FIB0", "CYC", "FVC0.CYC",
                  "FIB0.CYC", "time.CYC"),
           RE = "linear", ID = "ID",cate = NULL, intcpt = 0,
           quad.points = 20, quiet = TRUE, do.trace=TRUE)
jmcfit
beta <- coef(jmcfit, coeff = "beta")

gamma <- coef(jmcfit, coeff = "gamma")

summary(jmcfit, coeff = "longitudinal", digits = 4)

summary(jmcfit, coeff = "survival", digits = 4)

linearTest(jmcfit, coeff="beta")

linearTest(jmcfit, coeff="gamma")

Lb <- matrix(c(1, -1, 0, 0, 0, 0, 0), ncol = length(beta)-1, nrow = 1)
linearTest(jmcfit, coeff="beta", Lb = Lb)

Lg <- matrix(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0), ncol = length(gamma), nrow = 1)
linearTest(jmcfit, coeff="gamma", Lg = Lg)

#make up two categorical variables and add them into yread
set.seed(123)
sex <- sample(c("Female", "Male"), nrow(cread), replace = TRUE)
race <- sample(c("White", "Black", "Asian", "Hispanic"),
               nrow(cread), replace = TRUE)
ID <- cread$ID
cate_var <- data.frame(ID, sex, race)
if (require(dplyr)) {
  yread <- dplyr::left_join(yread, cate_var, by = "ID")
}


# run jmc function again for yread file with two added categorical variables
res2 <- jmc(long_data = yread, surv_data = cread,
            out = "FVC", cate = c("sex", "race"),
            FE = c("time", "FVC0", "FIB0", "CYC", "FVC0.CYC",
                   "FIB0.CYC", "time.CYC"),
            RE = "time", ID = "ID", intcpt = 0,
            quad.points = 20, quiet = FALSE)
res2
coef(res2, coeff = "beta")



#JMO function

library(JMcmprsk)
set.seed(123)
data(ninds)
yread <- ninds[, c(1, 2:14)]
cread <- ninds[, c(1, 15, 16, 6, 10:14)]
cread <- unique(cread)

jmofit <- jmo(yread, cread, out = "Y",
              FE = c("group", "time3", "time6", "time12", "mrkprior",
                     "smlves", "lvORcs", "smlves.group", "lvORcs.group"),
              cate = NULL,RE = "intercept", NP = c("smlves", "lvORcs"),
              ID = "ID",intcpt = 1, quad.points = 6,
              max.iter = 1000, quiet = FALSE, do.trace = FALSE)

jmofit

alpha <- coef(jmofit, coeff = "alpha")
theta <- coef(jmofit, coeff = "theta")
summary(jmofit, coeff = "longitudinal")
linearTest(jmofit,coeff="beta")
linearTest(jmofit,coeff="gamma")
linearTest(jmofit,coeff="alpha")

La <- matrix(c(1, 0, -1, 0), ncol = length(alpha), nrow = 1)
linearTest(jmofit, coeff = "alpha", La = La)

#Create two categorical variables and add them into yread
ID <- cread$ID
set.seed(123)
sex <- sample(c("Female", "Male"), nrow(cread), replace = TRUE)
race <- sample(c("White", "Black", "Asian", "Hispanic"), nrow(cread), replace = TRUE)
cate_var <- data.frame(ID, sex, race)
if (require(dplyr)) {
   yread <- dplyr::left_join(yread, cate_var, by = "ID")
   }

res2 <- jmo(yread, cread, out = "Y",
            FE = c("group", "time3", "time6", "time12", "mrkprior",
                   	"smlves", "lvORcs", "smlves.group", "lvORcs.group"), cate = c("race", "sex"),
            RE = "intercept", NP = c("smlves", "lvORcs", "race", "sex"), ID = "ID",intcpt = 1,
            quad.points = 20, max.iter = 10000, quiet = FALSE, do.trace = FALSE)
res2
coef(res2, coeff = "beta")

#jmc_0() function
library(JMcmprsk)
yfile=system.file("extdata", "jmcsimy.txt", package = "JMcmprsk")
cfile=system.file("extdata", "jmcsimc.txt", package = "JMcmprsk")
mfile=system.file("extdata", "jmcsimm.txt", package = "JMcmprsk")
jmc_0fit = jmc_0(p=4, yfile, cfile, mfile, point=20, do.trace = FALSE)

library(JMcmprsk)
data(lung)
lungY <- lung[, c(2:11)]
lungC <- unique(lung[, c(1, 12, 13, 6:10)])
lungC <- lungC[, -1]
## return a vector file with the number of repeated measurements as lungM
lungM <- data.frame(table(lung$ID))
lungM <- as.data.frame(lungM[, 2])
jmc_0fit2=jmc_0(p=8, lungY, lungC, lungM, point=20, do.trace = FALSE, type_file = FALSE)




#Data Simulation

require(JMcmprsk)
set.seed(123)
yfn="jmcsimy1.txt";
cfn="jmcsimc1.txt";
mfn="jmcsimm1.txt";

k_val=200;p1_val=4;p1a_val=1; p2_val=2;g_val=2;
truebeta=c(10,-1,1.5,0.6);truegamma=c(0.8,-1,0.5,-1); randeffect=c(5,0.5,0.5,0.5);
#writing files
SimDataC(k_val, p1_val, p1a_val, p2_val, g_val,truebeta,truegamma, randeffect, yfn,
cfn,mfn)

require(JMcmprsk)
set.seed(123)
yfn="jmosimy1.txt";
cfn="jmosimc1.txt";
mfn="jmosimm1.txt";
k_val=500;p1_val=3;p1a_val=1; p2_val=2;g_val=2;
truebeta=c(-1,1.5,0.8);truetheta=c(-0.5,1);truegamma=c(0.8,-1,0.5,-1); randeffect=c(1,0.5,0.5);
#writing files
SimDataO(k_val, p1_val, p1a_val, p2_val, g_val,truebeta, truetheta, truegamma, randeffect, yfn,cfn,mfn)

