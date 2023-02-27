## ---- echo = FALSE, message = FALSE, warning = FALSE, note = FALSE------------
library(formatR)
library(knitr)
ref <- function(label){
    knitr::asis_output(ifelse(knitr::is_html_output(), 
            paste0("[\\@ref(fig:", label, ")]"),
            paste0("\\ref{fig:", label, "}")
            ))
}
reft <- function(label){
    knitr::asis_output(ifelse(knitr::is_html_output(), 
            paste0("[\\@ref(tab:", label, ")]"),
            paste0("\\ref{tab:", label, "}")
            ))
}
ctv <- function(label){
    knitr::asis_output(ifelse(knitr::is_html_output(), 
            paste0("[", label, "]", "(https://cran.r-project.org/web/views/", label, ".html)"),
            paste0("\\ctv{", label, "}")
            ))
}

options(scipen=1, digits=2)
knitr::opts_chunk$set(fig.width=9, fig.height=5, out.width = '100%', message = FALSE, warning = FALSE, note = FALSE, cache = TRUE) 
knitr::opts_chunk$set(tidy=FALSE, eval = TRUE, echo = TRUE)
knitr::opts_chunk$set(fig.pos = "!h", out.extra = "", fig.align = "center")

options(formatR.arrow=TRUE, width=90)
knitr::opts_chunk$set(dev = 'png', dpi = 400)
read_chunk('codes/Codes.R')


## ----install------------------------------------------------------------------
set.seed(12345)
library(openVA)
openVA_status()


## ----data-1-------------------------------------------------------------------
PHMRC_adult <- read.csv(getPHMRC_url("adult"))


## ----data-5-------------------------------------------------------------------
data(RandomVA1)
dim(RandomVA1)
head(RandomVA1[, 1:10])


## ----fit-1, messages=FALSE, results='hide'------------------------------------
fit_inter_who <- codeVA(data = RandomVA1, data.type = "WHO2012", 
                        model = "InterVA", version = "4.03", 
                        HIV = "h", Malaria = "h")

## ----fit-2, messages=FALSE----------------------------------------------------
summary(fit_inter_who) 


## ----fit-3, messages=FALSE, results='hide'------------------------------------
fit_ins_who <- codeVA(RandomVA1, data.type = "WHO2012", model = "InSilicoVA",
                    Nsim = 10000, auto.length = FALSE)

## ----fit-4, messages=FALSE----------------------------------------------------
summary(fit_ins_who) 


## ----phmrc-1------------------------------------------------------------------
PHMRC_adult <- read.csv(getPHMRC_url("adult"))
is.test <- which(PHMRC_adult$site == "AP")
test <- PHMRC_adult[is.test, ]
train <- PHMRC_adult[-is.test, ]
dim(test)
dim(train)


## ----phmrc-inter, messages=FALSE, results='hide'------------------------------
fit_inter <- codeVA(data = test, data.type = "PHMRC", model = "InterVA", 
                     data.train = train, causes.train = "gs_text34", 
                     phmrc.type = "adult", convert.type = "fixed")

## ----phmrc-ins, messages=FALSE, results='hide'--------------------------------
fit_ins <- codeVA(data = test, data.type = "PHMRC", model = "InSilicoVA",
                    data.train = train, causes.train = "gs_text34", 
                    phmrc.type = "adult", convert.type = "fixed", 
                    Nsim=10000, auto.length = FALSE)


## ----phmrc-nbc, messages=FALSE, results='hide'--------------------------------
fit_nbc <- codeVA(data = test, data.type = "PHMRC", model = "NBC", 
                   data.train = train, causes.train = "gs_text34", 
                   phmrc.type = "adult")

## ----phmrc-tar, messages=FALSE, results='hide'--------------------------------
fit_tariff <- codeVA(data = test, data.type = "PHMRC", model = "Tariff",
                     data.train = train, causes.train = "gs_text34", 
                     phmrc.type = "adult")


## ----phmrc-csmf, messages=FALSE, results='hide'-------------------------------
csmf_inter <- getCSMF(fit_inter)
csmf_ins <- getCSMF(fit_ins)
csmf_nbc <- getCSMF(fit_nbc)
csmf_tariff <- getCSMF(fit_tariff)


## ----phmrc-csmf-1, messages=FALSE---------------------------------------------
csmf_true <- table(c(test$gs_text34, unique(PHMRC_adult$gs_text34))) - 1
csmf_true <- csmf_true / sum(csmf_true)
c(getCSMF_accuracy(csmf_inter, csmf_true, undet = "Undetermined"), 
  getCSMF_accuracy(csmf_ins[, "Mean"], csmf_true), 
  getCSMF_accuracy(csmf_nbc, csmf_true), 
  getCSMF_accuracy(csmf_tariff, csmf_true))


## ----phmrc-cod, messages=FALSE------------------------------------------------
cod_inter <- getTopCOD(fit_inter)
cod_ins <- getTopCOD(fit_ins)
cod_nbc <- getTopCOD(fit_nbc)
cod_tariff <- getTopCOD(fit_tariff)


## ----phmrc-dist1, messages=FALSE----------------------------------------------
summary(fit_inter, id = "6288")


## ----phmrc-dist2, messages=FALSE----------------------------------------------
fit_ins <- updateIndiv(fit_ins, CI = 0.95)
summary(fit_ins, id = "6288")


## ----phmrc-dist3, messages=FALSE----------------------------------------------
fit_prob <- getIndivProb(fit_inter)
dim(fit_prob)


## ----vis-1, fig.show="hold", out.width="80%", fig.width = 6, fig.height = 3, fig.cap='Top 10 CSMFs estimated by InterVA.'----
plotVA(fit_inter, title = "InterVA")


## ----vis-2, out.width='.7\\linewidth', fig.width = 6, fig.height = 4, fig.cap='Estimated aggregated CSMFs for InterVA-4 and InSilicoVA, adding undetermined category.'----
data(SampleCategory)
grouping <- SampleCategory
grouping[,1] <- as.character(grouping[,1])
grouping <- rbind(grouping, c("Undetermined", "Undetermined"))
compare <- list(InterVA4 = fit_inter_who,
                InSilicoVA = fit_ins_who)
stackplotVA(compare, xlab = "", angle = 0,  grouping = grouping)


## ----vis-3, out.width='.7\\linewidth', fig.width = 6, fig.height = 4, fig.cap='Estimated aggregated CSMFs for InterVA-4 and InSilicoVA, with the causes reordered.'----
group_order <- c("TB/AIDS",  "Communicable", "NCD", "External", "Maternal",
            "causes specific to infancy", "Undetermined") 
stackplotVA(compare, xlab = "", angle = 0, grouping = grouping, 
            group_order = group_order)


## ----ins1, messages=FALSE-----------------------------------------------------
data(RandomVA2)
head(RandomVA2[, 244:248])


## ----ins2, messages=FALSE-----------------------------------------------------
fit_sub <- codeVA(RandomVA2, model = "InSilicoVA", 
              subpop = list("sex", "age"),  indiv.CI = 0.95,
              Nsim = 10000, auto.length = FALSE)


## ----ins3, out.width="90%", fig.width = 6, fig.height = 3, fig.cap='Estimated CSMFs for different sub-populations. The points indicate posterior means of the CSMF and the error bars indicate 95\\% credible intervals of the CSMF.'----
plotVA(fit_sub, type = "compare", title = "Comparing CSMFs", top = 3)


## ----ins4, fig.show="hold", out.width="80%", fig.width = 5.8, fig.height = 2.8, fig.cap=' Estimated fraction of deaths due to selected CODs for different sub-populations. The points indicate posterior means of the CSMF and the error bars indicate 95\\% credible intervals of the CSMF.'----
plotVA(fit_sub, type = "compare", title = "Comparing CSMFs",
                causelist = c("HIV/AIDS related death", 
                              "Pulmonary tuberculosis", 
                              "Other and unspecified infect dis", 
                              "Other and unspecified NCD"))


## ----ins5, fig.show="hold", out.width="80%", fig.width = 6, fig.height = 3, fig.cap='Top 10 CSMFs for a specified sub-population (women under $60$ years old).'----
plotVA(fit_sub, which.sub = "Women 60-", title = "Women 60-")


## ----ins6, fig.show="hold", out.width="49%", fig.width = 5.5, fig.height = 4, fig.cap='Aggregated CSMFs for four different sub-populations. Left: comparison using the stacked bar chart. Right: comparison using the bar chart arranged side to side. The height of the bars indicate posterior means of the CSMF and the error bars indicate 95\\% credible intervals of the CSMF.'----
stackplotVA(fit_sub)
stackplotVA(fit_sub, type = "dodge")


## ----phy0, messages=FALSE-----------------------------------------------------
data(SampleCategory)
data(RandomPhysician)
head(RandomPhysician[, 245:250])

## ----phy1, messages=FALSE, results="hide"-------------------------------------
doctors <- paste0("doc", c(1:15))
causelist <- c("Communicable", "TB/AIDS", "Maternal",
              "NCD", "External", "Unknown")
phydebias <- physician_debias(RandomPhysician, 
          phy.id = c("rev1", "rev2"), phy.code = c("code1", "code2"), 
          phylist = doctors, causelist = causelist, tol = 0.0001, max.itr = 100)


## ----phy2, messages=FALSE-----------------------------------------------------
fit_ins_phy <- codeVA(RandomVA1, model = "InSilicoVA",
             phy.debias = phydebias, phy.cat = SampleCategory, 
             phy.external = "External", phy.unknown = "Unknown",
             Nsim = 10000, auto.length = FALSE) 


## ----phy3, fig.show="hold", out.width="100%", fig.width = 5.7, fig.height = 2.5, fig.cap="Comparing fitted CSMF with and without physicians", fig.pos="!h"----
plotVA(fit_ins_who, title = "Without physician coding")
plotVA(fit_ins_phy, title = "With physician coding")

