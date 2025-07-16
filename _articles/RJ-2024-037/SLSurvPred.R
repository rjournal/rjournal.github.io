## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(plotly)
library(survivalSL)


## ----echo=FALSE---------------------------------------------------------------
load("graphique_A2C21.RData")
load("graphique_A2C22.RData")


## ----ibs-scea-plotly, echo = FALSE, fig.height = 4, fig.width = 5.5, fig.cap="Simulation results in the simple context.", include=knitr::is_html_output(), eval=knitr::is_html_output()----
IBS_ALL$met <- as.character(IBS_ALL$met)
IBS_ALL$met[IBS_ALL$met=="SL from RISCA"] <- "SL from survivalSL"
IBS_ALL$met <- factor(IBS_ALL$met,  c("Neural Network", "AFT Gamma", "PH Exponential", "RSF", "PH Breslow with stepwise AIC", "PH Breslow", "Elastic-Net PH model", "SL from survivalSL", "SL by Westling", "Perfect"))
IBS_ALL$schema[IBS_ALL$schema=="A1"] <- "N=200 for learning"
IBS_ALL$schema[IBS_ALL$schema=="A2"] <- "N=500 for learning"
p12 <- ggplot(subset(IBS_ALL,metric=="IBS"), aes(x=met, y=value, color=met)) +
  geom_boxplot()+
  facet_grid(schema~ Lear2)+
  scale_color_manual(values=c(col8,"wheat4","black"))+
  labs(x= "",
       y ="Integrated Brier Score (IBS)")+
  coord_flip()+
  theme(legend.position = "none",
        text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = 'white', color = 'grey'))
# plot(p12)
ggplotly(p12)


## ----ibs-scea-ggplot, echo = FALSE, fig.height = 4, fig.width = 5.5, fig.cap="Simulation results in the simple context.", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), fig.align = 'center'----
# IBS_ALL$met <- as.character(IBS_ALL$met)
# IBS_ALL$met[IBS_ALL$met=="SL from RISCA"] <- "SL from survivalSL"
# IBS_ALL$met <- factor(IBS_ALL$met,  c("Neural Network", "AFT Gamma", "PH Exponential", "RSF", "PH Breslow with stepwise AIC", "PH Breslow", "Elastic-Net PH model", "SL from survivalSL", "SL by Westling", "Perfect"))
# IBS_ALL$schema[IBS_ALL$schema=="A1"] <- "N=200 for learning"
# IBS_ALL$schema[IBS_ALL$schema=="A2"] <- "N=500 for learning"
# p12 <- ggplot(subset(IBS_ALL,metric=="IBS"), aes(x=met, y=value, color=met)) +
#   geom_boxplot()+
#   facet_grid(schema~ Lear2)+
#   scale_color_manual(values=c(col8,"wheat4","black"))+
#   labs(x= "",
#        y ="Integrated Brier Score (IBS)")+
#   coord_flip()+
#   theme(legend.position = "none",
#         text = element_text(size = 8),
#         axis.text.x = element_text(angle = 90),
#         panel.background = element_rect(fill = 'white', color = 'grey'))
# plot(p12)


## ----ibs-sceb-plotly, echo = FALSE, fig.height = 4, fig.width = 5.5, fig.cap="Simulation results in the complex context.", include=knitr::is_html_output(), eval=knitr::is_html_output()----
IBS_ALL.C$met <- as.character(IBS_ALL.C$met)
IBS_ALL.C$met[IBS_ALL.C$met=="SL from RISCA"] <- "SL from survivalSL"
IBS_ALL.C$met <- factor(IBS_ALL.C$met,  c("Neural Network", "AFT Gamma", "PH Exponential", "RSF", "PH Breslow with stepwise AIC", "PH Breslow", "Elastic-Net PH model", "SL from survivalSL", "SL by Westling", "Perfect"))
IBS_ALL.C$schema[IBS_ALL.C$schema=="B1"] <- "N=200 for learning"
IBS_ALL.C$schema[IBS_ALL.C$schema=="B2"] <- "N=500 for learning"
p12C <- ggplot(subset(IBS_ALL.C,metric=="IBS"), aes(x=met, y=value, color=met)) +
  geom_boxplot()+
  facet_grid(schema~ Lear2)+
  scale_color_manual(values=c(col8,"wheat4","black"))+
  labs(x= "",
       y ="Integrated Brier Score (IBS)")+
  coord_flip()+
  theme(legend.position = "none",
         text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = 'white', color = 'grey'))
ggplotly(p12C)


## ----ibs-sceb-ggplot, echo = FALSE, fig.height = 4, fig.width = 5.5, fig.cap="Simulation results in the complex context.", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), fig.align = 'center'----
# IBS_ALL.C$met <- as.character(IBS_ALL.C$met)
# IBS_ALL.C$met[IBS_ALL.C$met=="SL from RISCA"] <- "SL from survivalSL"
# IBS_ALL.C$met <- factor(IBS_ALL.C$met,  c("Neural Network", "AFT Gamma", "PH Exponential", "RSF", "PH Breslow with stepwise AIC", "PH Breslow", "Elastic-Net PH model", "SL from survivalSL", "SL by Westling", "Perfect"))
# IBS_ALL.C$schema[IBS_ALL.C$schema=="B1"] <- "N=200 for learning"
# IBS_ALL.C$schema[IBS_ALL.C$schema=="B2"] <- "N=500 for learning"
# p12C <-  ggplot(subset(IBS_ALL.C,metric=="IBS"), aes(x=met, y=value, color=met)) +
#   geom_boxplot()+
#   facet_grid(schema~ Lear2)+
#   scale_color_manual(values=c(col8,"wheat4","black"))+
#   labs(x= "",
#        y ="Integrated Brier Score (IBS)")+
#   coord_flip()+
#   theme(legend.position = "none",
#        text = element_text(size = 8),
#         axis.text.x = element_text(angle = 90),
#         panel.background = element_rect(fill = 'white', color = 'grey'))
# plot(p12C)


## ----echo=FALSE---------------------------------------------------------------
load("graph_runtimes.RData")


## ----run-plotly, echo = FALSE, fig.height = 4, fig.width = 5.5, fig.cap="Running times according to the sample size and the number of covariates. Results obtained for a MacBook Pro 2.6 GHz Intel Core i7 6 cores.", include=knitr::is_html_output(), eval=knitr::is_html_output()----
graphruntimes <- ggplot(res.tab, aes(x = ss, y = times, color = Covariates, group = Covariates)) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8),
        legend.text=element_text(size=8), legend.title=element_text(size=8),
        legend.position="bottom") +
  geom_line() + 
  xlab("Sample size") + 
  ylab("Running time (hours)") +
  geom_point()
ggplotly(graphruntimes)


## ----run-ggplot, echo = FALSE, fig.height = 4, fig.width = 5.5, fig.cap="Running times according to the sample size and the number of covariates. Results obtained for a MacBook Pro 2.6 GHz Intel Core i7 6 cores.", include=knitr::is_latex_output(), eval=knitr::is_latex_output(), fig.align = 'center'----
# graphruntimes <- ggplot(res.tab, aes(x = ss, y = times, color = Covariates, group = Covariates)) +
#   theme(axis.text=element_text(size=8), axis.title=element_text(size=8),
#         legend.text=element_text(size=8), legend.title=element_text(size=8),
#         legend.position="bottom") +
#   geom_line() +
#   xlab("Sample size") +
#   ylab("Running time (hours)") +
#   geom_point()
# plot(graphruntimes)


## ----echo=FALSE---------------------------------------------------------------
load("tables.RData")

table1$Name[table1$Name=="`cox.all`"] <- "`LIB_COXall`"
table1$Name[table1$Name=="`cox.aic`"] <- "`LIB_COXaic`"
table1$Name[table1$Name=="`cox.en`"] <- "`LIB_COXen`"
table1$Name[table1$Name=="`cox.lasso`"] <- "`LIB_COXlasso`"
table1$Name[table1$Name=="`cox.ridge`"] <- "`LIB_COXridge`"

table1$Name[table1$Name=="`aft.gamma`"] <- "`LIB_AFTgamma`"
table1$Name[table1$Name=="`aft.ggamma`"] <- "`LIB_AFTggamma`"
table1$Name[table1$Name=="`aft.llogis`"] <- "`LIB_AFTllogis`"
table1$Name[table1$Name=="`aft.weibull`"] <- "`LIB_AFTweibull`"

table1$Name[table1$Name=="`ph.exponential`"] <- "`LIB_PHexponential`"
table1$Name[table1$Name=="`ph.gompertz`"] <- "`LIB_PHgompertz`"
table1$Name[table1$Name=="`ph.spline`"] <- "`LIB_PHspline`"

table1$Name[table1$Name=="`rf.time`"] <- "`LIB_RSF`"
table1$Name[table1$Name=="`nnet.time`"] <- "`LIB_SNN`"

table1$Class[table1$Name=="`RSF`"] <- "rsf"
table1$Class[table1$Name=="`SNN`"] <- "snn"

table1$Westling <- "Yes"
table1$Westling[table1$Name %in% c("`LIB_SNN`", "`LIB_AFTgamma`",
                      "`LIB_AFTggamma`", "`LIB_AFTllogis`", "`LIB_COXaic`")] <- "No"
table1 <- rbind(table1[1:11,],
               c("`LIB_PHspline`", "PH model with natural cubic spline as baseline distribution (hyperparameters: `k`)", "flex.surv", "No"),
               table1[12:13,])

table1 <- table1[, c("Name", "Description", "Westling")]

table1latex$Name[table1latex$Name=="cox.all"] <- "LIB_COXall"
table1latex$Name[table1latex$Name=="cox.aic"] <- "LIB_COXaic"
table1latex$Name[table1latex$Name=="cox.en"] <- "LIB_COXen"
table1latex$Name[table1latex$Name=="cox.lasso"] <- "LIB_COXlasso"
table1latex$Name[table1latex$Name=="cox.ridge"] <- "LIB_COXridge"

table1latex$Name[table1latex$Name=="aft.gamma"] <- "LIB_AFTgamma"
table1latex$Name[table1latex$Name=="aft.ggamma"] <- "LIB_AFTggamma"
table1latex$Name[table1latex$Name=="aft.llogis"] <- "LIB_AFTllogis"
table1latex$Name[table1latex$Name=="aft.weibull"] <- "LIB_AFTweibull"

table1latex$Name[table1latex$Name=="ph.exponential"] <- "LIB_PHexponential"
table1latex$Name[table1latex$Name=="ph.gompertz"] <- "LIB_PHgompertz"
table1latex$Name[table1latex$Name=="ph.spline"] <- "LIB_PHspline"

table1latex$Name[table1latex$Name=="rf.time"] <- "LIB_RSF"
table1latex$Name[table1latex$Name=="nnet.time"] <- "LIB_SNN"

table1latex$Class[table1latex$Name=="RSF"] <- "rsf"
table1latex$Class[table1latex$Name=="SNN"] <- "snn"

table1latex$Westling <- "Yes"
table1latex$Westling[table1latex$Name %in% c("LIB_SNN", "LIB_AFTgamma",
                      "LIB_AFTggamma", "LIB_AFTllogis", "LIB_COXaic")] <- "No"
table1latex <- rbind(table1latex[1:11,],
               c("LIB_PHspline", "PH model with natural cubic spline as baseline distribution (hyperparameters: k)", "flex.surv", "No"),
               table1latex[12:13,])

table1latex <- table1latex[, c("Name", "Description", "Westling")]

table2$Function <- c("`plot.sltime`", "`predict.sltime`", "`summary.sltime`")

table2 <- rbind(table2, c("`print.sltime`", "To print the learners and their weights"))

table2$Description[table2$Function == "`summary.sltime`"] <- "To obtain metrics describing the prognostic capacities of the SL and the included learners."

table2latex$Function <- c("plot.sltime", "predict.sltime", "summary.sltime")

table2latex <- rbind(table2latex, c("print.sltime", "To print the learners and their weights"))

table2latex$Description[table2latex$Function == "summary.sltime"] <- "To obtain metrics describing the prognostic capacities of the SL and the included learners."


## ----listemeth-tab-interactive, eval = knitr::is_html_output(), layout = "c-body-outset"----
knitr::kable((table1latex), format = "html", caption = "List of possible learners (#: Breslow's estimation of the baseline hazard function). The last column lists the learners included in the survSuperLearner package proposed by Westling (2021). This package additionally considers generalized additive Cox regression and piecewise constant hazard regression.") %>%
kableExtra::column_spec(2, width = "13cm")


## ----listemeth-tab-static, eval = knitr::is_latex_output()--------------------
# knitr::kable((table1latex), format = "latex", caption = "List of possible learners (\\#: Breslow's estimation of the baseline hazard function). The last column lists the learners included in the survSuperLearner proposed by Westling (2021). This package additionally considers generalized additive Cox regression and piecewise constant hazard regression.", booktabs = TRUE) %>%
# kableExtra::column_spec(2, width = "8cm")%>%
#   kableExtra::kable_styling(font_size = 8, position = "center")


## ----s3-tab-interactive, eval = knitr::is_html_output(), layout = "c-body-outset"----
knitr::kable((table2), format = "html", caption = "List of the S3 functions applicable to an sltime object") %>%
kableExtra::column_spec(2, width = "15cm")


## ----s3-tab-static, eval = knitr::is_latex_output()---------------------------
# knitr::kable((table2latex), format = "latex", caption = "List of S3 functions applicable to an sltime object", booktabs = TRUE)%>%
#   kableExtra::column_spec(2, width = "11cm")%>%
#   kableExtra::kable_styling(font_size = 8,position = "center")
# 


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
library("survivalSL")
data(dataOFSEP); head(dataOFSEP)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
dataOFSEP$relapse.1 <- 1*(dataOFSEP$relapse=="1")
dataOFSEP$relapse.2 <- 1*(dataOFSEP$relapse=="2+")
dataOFSEP$edss.1 <- 1*(dataOFSEP$edss=="low")
dataOFSEP$edss.2 <- 1*(dataOFSEP$edss=="high")
dataOFSEP$t1.1 <- 1*(dataOFSEP$t1=="0")
dataOFSEP$t1.2 <- 1*(dataOFSEP$t1=="1+")


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
set.seed(117)
dataOFSEP$train <- 1*rbinom(n=dim(dataOFSEP)[1], size=1, prob=2/3)
dataTRAIN <- dataOFSEP[dataOFSEP$train==1,]
dataVALID <- dataOFSEP[dataOFSEP$train==0,]


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
sl1 <- survivalSL(metric="ci",  data=dataTRAIN, times="time", failures="event",
      pro.time=2, cov.quanti=c("age","duration"), cov.quali=c("period",  "gender",
            "relapse.1", "relapse.2", "edss.1", "edss.2", "t1.1", "t1.2"),
      methods=c("LIB_COXen", "LIB_PHspline", "LIB_AFTggamma", "LIB_RSF"),
      cv=30, optim.local.min=TRUE, progress=FALSE, seed=117)
print(sl1, digits=4)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
.tune <- vector("list",4)
.tune[[2]] <- list(k=1:6)
sl2 <- survivalSL(metric="ci",  data=dataTRAIN, times="time", failures="event",
      pro.time=2, cov.quanti=c("age","duration"), cov.quali=c("period",  "gender",
            "relapse.1", "relapse.2", "edss.1", "edss.2", "t1.1", "t1.2"),
      methods=c("LIB_COXen", "LIB_PHspline", "LIB_AFTggamma", "LIB_RSF"),
      cv=30, optim.local.min=TRUE, param.tune=.tune, progress=FALSE, seed=117)
print(sl2, digits=4)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
rbind(train = summary(sl2, method="sl", pro.time=2, digits=4),
      test =  summary(sl2,  newdata=dataVALID, method="sl",
                      pro.time=2, digits=4))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
rbind(train = summary(sl2, method="LIB_COXen", pro.time=2, digits=4),
      test =  summary(sl2,  newdata=dataVALID, method="LIB_COXen",
                      pro.time=2, digits=4))


## ----calibfig2, echo=TRUE, eval=TRUE, fig.cap="Calibration plot at 2 years for the validation sample.", fig.height = 5, fig.width = 5----
plot(sl2, newdata=dataVALID, times="time", failures="event", cex.lab=0.70,
     cex.axis=0.70, n.groups=5, pro.time=2, col=2)


## ----eval=FALSE, echo=TRUE----------------------------------------------------
# library(survivalSL)
# 
# # definition of the parameters related to the simulated data
# n.valid <- 500 # sample size for validation
# n.learn <- 500 # sample size for training
# 
# n <- n.valid + n.learn # overall sample size
# 
# max.time <- 50 # maximum follow-up time
# 
# mean.x <- 0; sd.x <- 1 # normal distribution of the quantitative predictors
# proba.x <- .5 # proportion of the binary predictors
# 
# a <- 2; b <- .05 # Weibull baseline distribution of the PH model
# beta <- c(log(1.8), log(1.8), log(1.3), 0, 0, 0) # regression coefficients
# 
# # simulation of  the training and validation samples
# x1 <- rnorm(n, mean.x, sd.x)
# x2 <- rbinom(n, 1, proba.x)
# x3 <- rbinom(n, 1, proba.x)
# x4 <- rnorm(n, mean.x, sd.x)
# x5 <- rbinom(n, 1, proba.x)
# x6 <- rbinom(n, 1, proba.x)
# x <- cbind(x1, x2, x3, x4, x5, x6) # matrix of the potential predictors
# 
# u <- runif(n, 0, 1)
# times <- 1/b*((-exp(-1*(x %*% beta))*(log(1-u)))**(1/a)) # time to event
# 
# censoring <- runif(n, min=0, max=max.time)
# 
# status <- ifelse(times <= censoring, 1, 0) # event status
# obs.times <- ifelse(times <= censoring, times, censoring) # follow-up times
# 
# data <- cbind(obs.times, status, as.data.frame(x))
# 
# data.simul <- list(data[1:n.valid,], data[(n.valid+1):n,])
# 
# # definition of the grid search related to the hyperparameters
# par1 <- vector("list", 6)
# 
# par1[[1]] <- list(alpha=seq(0,1,.1), lambda=NULL)
# 
# par1[[5]] <- list(n.nodes=c(2, 3, 4, 6, 10, 20, 32),  decay=c(0, 0.01, 0.1),
#                 batch.size=c(256L,128L),  epochs=c(10L,100L))
# 
# slres <- survivalSL(
#   methods=c("LIB_COXen", "LIB_COXall", "LIB_RSF", "LIB_SNN",
#             "LIB_AFTgamma", "LIB_PHexponential"),
#   metric="ibs",  data=data.simul[[1]],  times="obs.times",
#   failures="status", cov.quanti=c("x1","x4"),
#   cov.quali=c("x2","x3","x5","x6"), progress = FALSE,
#   param.tune=par1, cv=10, keep.predictions=TRUE)
# 
# # loss functions estimated from training sample (Figure 1)
# summary(slres)
# 
# # from validation sample (Figure 1)
# summary(slres, newdata=data.simul[[2]])


## ----eval=FALSE, echo=TRUE----------------------------------------------------
# library(survivalSL)
# 
# # definition of the parameters related to the simulated data
# n.valid <- 500 # sample size for validation
# n.learn <- 200 # sample size for training
# n <- n.valid + n.learn # overall sample size
# 
# max.time <- 50 # maximum follow-up time
# 
# b0.t <- (-0.4) # intercept related to the predictors' distribution
# b1.t <- log(2) # slope related to the predictors' distribution
# 
# a <- 2; b <- .05 # Weibull baseline distribution of the PH model
# b1.o <- 0.69 # regression coefficients of the PH model
# 
# # simulation of  the training and validation samples
# .x1 <- rnorm(n, 0, 1)
# .x2 <- rnorm(n, b0.t + b1.t * .x1, 1)
# .x3 <- rnorm(n, b0.t - b1.t * .x1 - b1.t * .x2, 1)
# .x4 <- rnorm(n, b0.t + b1.t * .x3, 1)
# .x5 <- rnorm(n, 0, 1)
# .x6 <- 1 * (rnorm(n, 0, 1) > 0.66)
# .x7 <- 1 * (rnorm(n, b0.t - b1.t * .x5, 1) > (-0.40))
# .x8 <- rnorm(n, b0.t - b1.t * .x6, 1)
# .x9 <- 1 * (rnorm(n, b0.t + b1.t * .x7, 1) > (-0.80))
# .x10 <- rnorm(n, b0.t + b1.t * .x8, 1)
# .x11 <- rnorm(n, 0, 1)
# .x12 <- 1 * (rnorm(n, b0.t + b1.t * .x9, 1) > (0.84))
# .x13 <- 1 * (rnorm(n, b0.t + b1.t * .x10, 1) > (-0.09))
# .x14 <- rnorm(n, b0.t - b1.t * .x12 - b1.t * .x11, 1)
# .x15 <- rnorm(n, b0.t - b1.t * .x12, 1)
# .x16 <- 1 * (rnorm(n, 0, 1) > (-0.66))
# .x17 <- 1 * (rnorm(n, b0.t - b1.t * .x16, 1) > (-0.92))
# .x18 <- rnorm(n, 0, 1)
# .x19 <- 1 * (rnorm(n, 0, 1) > (0.66))
# .x20 <- 1 * (rnorm(n, 0, 1) > (0.66))
# .x21 <- rnorm(n, 0, 1)
# .x22 <- 1 * (rnorm(n, 0, 1) > (0.66))
# 
# data.obs <- data.frame(x1=.x1, x2=.x2, x3=.x3, x4=.x4, x5=.x5, x6=.x6,
#     x7=.x7, x8=.x8, x9=.x9, x10=.x10, x11=.x11, x12=.x12, x13=.x13,
#     x14=.x14, x15=.x15, x16=.x16, x17=.x17, x18=.x18, x19=.x19,
#     x20=.x20, x21=.x21, x22=.x22)
# 
# bx <- b0.t + b1.t*data.obs$x1 - b1.t*data.obs$x3 + b1.t*data.obs$x5 -
#   b1.t*data.obs$x7 + b1.t*data.obs$x9 - b1.t*data.obs$x11 +
#   b1.t*data.obs$x13 - b1.t*data.obs$x15 - b1.t*data.obs$x17 +
#   b1.t*data.obs$x19 - b1.t*data.obs$x21
# 
# pr.t <- (exp(bx) / (1 + exp(bx)))
# data.obs$t.obs <- rbinom(n, 1, prob = pr.t)
# 
# bx <- b1.o*(data.obs$x2>-0.40) - b1.o*data.obs$x3 +
#   b1.o*0.5*(data.obs$x3^2) + b1.o*data.obs$x6 + b1.o*data.obs$x7 +
#   b1.o*data.obs$x10 + b1.o*0.5*(data.obs$x11^2) - b1.o*data.obs$x14 -
#   b1.o*(data.obs$x15>-0.57) + b1.o*data.obs$x18 + b1.o*data.obs$x19 +
#   b1.o*data.obs$t.obs + b1.o*0.5*data.obs$t.obs*data.obs$x18
# 
# u <- runif(n,0,1)
# times <- 1/b*((-exp(-bx)*(log(1-u)))**(1/a))
# 
# censoring <- runif(n, min=0, max=max.time)
# status <- ifelse(times <= censoring, 1, 0)
# obs.time <- ifelse(times <= censoring, times, censoring)
# 
# data <- cbind(obs.time, status, data.obs)
# 
# data.simul <- list(data[1:n.valid,], data[(n.valid+1):n,])
# 
# # definition of the grid search related to the hyperparameters
# par1 <- vector("list", 6)
# 
# par1[[1]] <- list(alpha=seq(0,1,.1),lambda=NULL)
# 
# par1[[4]] <- list(n.nodes=c(2, 3, 4, 6, 10, 20,32), decay=c(0, 0.01, 0.1),
#                 batch.size=c(256L,128L), epochs=c(10L,100L))
# 
# slres <- survivalSL(
#   methods=c("LIB_COXen", "LIB_COXall", "LIB_RSF", "LIB_SNN",
#             "LIB_AFTgamma", "LIB_PHexponential"),  metric="ibs",
#   data=data.simul[[1]], times="obs.time", failures="status",
#   cov.quanti=c("x1","x2","x3","x4","x5", "x8","x10","x11","x14",
#                "x15","x18","x21"),  cov.quali=c("x6","x7","x9","x12",
#                "x13","x16","x17","x19","x20","x22"),
#   param.tune=par1, cv=10, keep.predictions=TRUE, progress = FALSE)
# 
# # loss functions estimated from training sample (Figure 2)
# summary(slres)
# 
# # from validation sample (Figure 2)
# summary(slres, newdata=data.simul[[2]])


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
sl2.1 <- survivalSL(metric="ci",  data=dataTRAIN, times="time", failures="event",
      pro.time=2, cov.quanti=c("age","duration"), cov.quali=c("period",  "gender",
            "relapse.1", "relapse.2", "edss.1", "edss.2", "t1.1", "t1.2"),
      methods=c("LIB_COXen", "LIB_PHspline", "LIB_AFTggamma", "LIB_RSF"),
      cv=30, optim.local.min=TRUE, param.tune=.tune, progress=FALSE, seed=118)

sl2.2 <- survivalSL(metric="ci",  data=dataTRAIN, times="time", failures="event",
      pro.time=2, cov.quanti=c("age","duration"), cov.quali=c("period",  "gender",
            "relapse.1", "relapse.2", "edss.1", "edss.2", "t1.1", "t1.2"),
      methods=c("LIB_COXen", "LIB_PHspline", "LIB_AFTggamma", "LIB_RSF"),
      cv=30, optim.local.min=TRUE, param.tune=.tune, progress=FALSE, seed=119)

sl2.3 <- survivalSL(metric="ci",  data=dataTRAIN, times="time", failures="event",
      pro.time=2, cov.quanti=c("age","duration"), cov.quali=c("period",  "gender",
            "relapse.1", "relapse.2", "edss.1", "edss.2", "t1.1", "t1.2"),
      methods=c("LIB_COXen", "LIB_PHspline", "LIB_AFTggamma", "LIB_RSF"),
      cv=30, optim.local.min=TRUE, param.tune=.tune, progress=FALSE, seed=120)

rbind(
  seed.121  = summary(sl2,   newdata=dataVALID, times="time", failures="event",
                      method="sl", pro.time=2, digits=4),
  seed.122  = summary(sl2.1, newdata=dataVALID, times="time", failures="event",
                      method="sl", pro.time=2, digits=4),
  seed.123  = summary(sl2.2, newdata=dataVALID, times="time", failures="event",
                      method="sl", pro.time=2, digits=4),
  seed.124  = summary(sl2.3, newdata=dataVALID, times="time", failures="event",
                      method="sl", pro.time=2, digits=4) )

rbind(
  seed.121  =  sl2$weights$values,
  seed.122  = sl2.1$weights$values,
  seed.123  = sl2.2$weights$values,
  seed.124  = sl2.3$weights$values)


## ----scenarios, fig.cap = "Description of the two simulated scenarios. The first graph represents the simple scenario with 6 covariates, linear relationships, and no interaction. The last  graph  represents the complex scenario with 23 covariates, linear and non-linear relationships, and one interaction.", out.width = "100%"----
knitr::include_graphics("DAG.pdf")


## ----wscea-plotly, echo=FALSE, fig.cap="Weight distribution among 50 randomly estimated super learners.", fig.height = 5, include=knitr::is_html_output(), eval=knitr::is_html_output()----
load("graphique_A2C21.RData")
pw_A2_200 <- ggplot(subset(all_pw, schema=="A2"), aes(x=repli2, y=wei, fill=meth)) +
  # facet_wrap(schema~.)+
  geom_bar(stat="identity") +
  scale_fill_viridis_d("Methods",option="plasma", end=.9) +
  labs(x= "50 random simulated data sets",
       y ="Weights of the super learner") +
  theme(axis.text=element_text(size=9), axis.title=element_text(size=10),
        legend.text=element_text(size=8), legend.title=element_text(size=8),
        legend.position="bottom")
ggplotly(pw_A2_200)


## ----wscea-ggplot, echo=FALSE, fig.cap="Weight distribution among 50 randomly estimated super learners.", fig.height = 4, fig.width = 5.5, include=knitr::is_latex_output(), eval=knitr::is_latex_output()----
# load("graphique_A2C21.RData")
# pw_A2_200 <- ggplot(subset(all_pw, schema=="A2"), aes(x=repli2, y=wei, fill=meth)) +
#   # facet_wrap(schema~.)+
#   geom_bar(stat="identity")+
#   scale_fill_viridis_d("Methods",option="plasma", end=.9) +
#   labs(x= "50 random simulated data sets",
#        y ="Weights of the super learner") +
#   theme(axis.text=element_text(size=9), axis.title=element_text(size=10),
#         legend.text=element_text(size=8), legend.title=element_text(size=8),
#         legend.position="bottom")
# plot(pw_A2_200)


## ----calibfig3, eval=TRUE, echo=TRUE, fig.cap="Observed relapse-free survival of the OFSEP validation sample (Kaplan-Meier estimator) compared to the mean of the individual predictions.", out.width = "100%"----

plot(survfit(Surv(time, event) ~ 1, data = dataVALID), 
     ylab="Disease progression free survival",
     xlab="Time after the first anniversary of the treatment in years",
     cex.lab = 0.8, cex.axis=0.8)

.pred.matrix <- predict(sl2, newdata=dataVALID,
                        newtimes = seq(0, 6, by=0.05))$predictions$sl

.pred <- apply(.pred.matrix, MARGIN=2, FUN="mean")

lines(x=seq(0, 6, by=0.05), y=.pred, col=2)

legend("bottomleft", c("Observed survival", "95% confidence interval",
        "Mean of the predictions"), col=c(1, 1, 2), lty=c(1,2,1), cex=0.8)


## ----rocfig2, eval=TRUE, echo=TRUE,fig.cap="ROC curve of the SL (line in red) compared to the sensitivity and specificity of the modified Rio score (points in blue) for a prognostic up to 2 years in the OFSEP validation sample.", fig.height = 5, fig.width = 5----

.pred <- predict(sl2, newdata=dataVALID)

dataVALID$sl <- 1 - .pred$predictions$sl[,sum(.pred$times<2)]

roc.sl <- roc(times="time", failures="event", variable="sl",
                  confounders=~1, data=dataVALID, pro.time=2,
                  precision=seq(0.1, 0.9, by=0.01)) 
# Note: “confounders=~1” for usual time-dependent ROC curves,
# i.e., without considering confounder (doi:10.1177/0962280217702416)

roc.rio <- roc(times="time", failures="event", variable="rio",
                   confounders=~1, data=dataVALID, pro.time=2)

plot(roc.sl, col=2, type="l", xlab="1-Specificity", ylab="Sensitivity",
     cex.lab=0.8, cex.axis=0.8)

points(x=1-roc.rio$table$sp, y=roc.rio$table$se, col=4)

legend("bottomright", legend=
  paste("AUC =", round(roc.sl$auc, 3)), cex=0.8 )

