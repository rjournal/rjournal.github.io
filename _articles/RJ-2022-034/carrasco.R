## ----setup, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(ggplot2)
library(gridExtra)



## ---------------------------------------------------
library(iccCounts)
library(dplyr)
EPP %>% group_by(Year) %>% summarize(Mean=mean(Social),SD=sd(Social))


## ---------------------------------------------------
EPP_P<-icc_counts(EPP,y="Social",id="id",met="Year",type="con")
ICC(EPP_P)
VarComp(EPP_P)


## ---------------------------------------------------
set.seed(100)
EPP_P.gof <- GOF_check(EPP_P)


## ---- eval = FALSE----------------------------------
## plot(EPP_P.gof)


## ---------------------------------------------------
DispersionTest(EPP_P.gof)


## ---------------------------------------------------
ZeroTest(EPP_P.gof)


## ---------------------------------------------------
EPP_ZIP<-icc_counts(EPP,y="Social",id="id",met="Year",type="con",fam="zip")
ICC(EPP_ZIP)
VarComp(EPP_ZIP)


## ---------------------------------------------------
set.seed(100)
EPP_ZIP.gof <- GOF_check(EPP_ZIP)


## ---- eval = FALSE----------------------------------
## plot(EPP_ZIP.gof)


## ----figure1, echo=F, fig.width = 6, fig.height = 9, fig.cap="Goodness of fit for Sparrow fledglings paternity example. The Randomized Quantile Residuals (RQR) and counts of zeros of original data are compared to those from simulated data under the fitted model. The plots shown are RQR with envelopes, dispersion of RQR and count of zeros. Left column shows results for Poisson model while the plots for Zero Inflated Poisson (ZIP) model are on right column."----


p1<-plot(EPP_P.gof,type="envelope")+ 
  labs(caption="a) RQR envelopes plot. Poisson Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))
p1$layers[[1]]$aes_params$shape<-NULL
p1$layers[[1]]$aes_params$size<-0.75

p2<-plot(EPP_ZIP.gof,type="envelope")+ 
  labs(caption="b) RQR envelopes plot. ZIP Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))
p2$layers[[1]]$aes_params$shape<-NULL
p2$layers[[1]]$aes_params$size<-0.75

p3<-plot(EPP_P.gof,type="dispersion")+ 
  labs(caption="c) Dispersion plot. Poisson Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p4<-plot(EPP_ZIP.gof,type="dispersion")+ 
  labs(caption="d) Dispersion plot. ZIP Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p5<-plot(EPP_P.gof,type="zeros")+ 
  labs(caption="e) Proportion of zeros plot. Poisson Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p6<-plot(EPP_ZIP.gof,type="zeros")+ 
  labs(caption="f) Proportion of zeros plot. ZIP Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)


## ---------------------------------------------------
DispersionTest(EPP_ZIP.gof)
ZeroTest(EPP_ZIP.gof)


## ---------------------------------------------------
EPP_ZIP_0<-icc_counts(EPP,y="Social",id="id",fam="zip")


## ---------------------------------------------------
anova(EPP_ZIP$model,EPP_ZIP_0$model)


## ---- fig.show='hide'-------------------------------
EPP.BA<-plot_BA(EPP,y="Social",id="id",rm="Year") # Bland-Altman plot


## ---- eval = FALSE----------------------------------
## plot_BA(EPP,y="Social",id="id",type="bars") # Bar plot


## ---------------------------------------------------
summary(EPP.BA$data$Diff)
quantile(EPP.BA$data$Diff,probs=c(0.05,0.95))


## ---------------------------------------------------
AF_P <- icc_counts(AF, y = "y", id = "id", met = "met", type = "con")
ICC(AF_P)
VarComp(AF_P)


## ---------------------------------------------------
set.seed(100)
AF_P.gof <- GOF_check(AF_P)


## ---------------------------------------------------
DispersionTest(AF_P.gof)


## ---------------------------------------------------
AF_NB2 <- icc_counts(AF, y = "y", id = "id", met = "met", type = "con", fam = "nbinom2")
ICC(AF_NB2)
VarComp(AF_NB2)


## ---------------------------------------------------
set.seed(100)
AF_NB2.gof <- GOF_check(AF_NB2)


## ---- eval = FALSE----------------------------------
## plot(AF_NB2.gof)


## ----figure2, echo=F, fig.width = 6, fig.height =6, fig.cap="Goodness of fit for CD34 cell count example. The Randomized Quantile Residuals (RQR) of original data are compared to those from simulated data under the fitted model. The plots shown are RQR with envelopes, and dispersion of RQR. First row shows results for Poisson model while the plots for Negative Binomial model are on second row."----


p7<-plot(AF_P.gof,type="envelope")+ 
  labs(caption="a) RQR envelopes plot. Poisson Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))
p7$layers[[1]]$aes_params$shape<-NULL
p7$layers[[1]]$aes_params$size<-0.75

p8<-plot(AF_NB2.gof,type="envelope")+ 
  labs(caption="b) RQR envelopes plot. Negative Binomial model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))
p8$layers[[1]]$aes_params$shape<-NULL
p8$layers[[1]]$aes_params$size<-0.75

p9<-plot(AF_P.gof,type="dispersion")+ 
  labs(caption="c) Dispersion plot. Poisson Model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p10<-plot(AF_NB2.gof,type="dispersion")+ 
  labs(caption="d) Dispersion plot. Negative Binomial model")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))


grid.arrange(p7,p9,p8,p10,nrow=2)


## ---------------------------------------------------
DispersionTest(AF_NB2.gof)


## ---- fig.show='hide'-------------------------------
AF.BA <- plot_BA(AF,y="y",id="id", rm="met") # Bland-Altman plot


## ---- eval = FALSE----------------------------------
## plot_BA(AF,y="y",id="id", type="bars") # Bar plot


## ---------------------------------------------------
G_P <- icc_counts(Grimso, y = "Tot", id = "TransectID")
ICC(G_P)
VarComp(G_P)


## ---------------------------------------------------
set.seed(100)
G_P.gof <- GOF_check(G_P)


## ---- eval = F--------------------------------------
## plot(G_P.gof)


## ---------------------------------------------------
DispersionTest(G_P.gof)


## ----figure3, echo=F, fig.width = 6, fig.height =3, fig.cap="Goodness of fit for Tick counts example. The Randomized Quantile Residuals (RQR) of original data are compared to those from simulated data under the fitted model. The plots shown are RQR with envelopes, and dispersion of RQR for Poisson model."----


p11<-plot(G_P.gof,type="envelope")+ 
  labs(caption="a) RQR envelopes plot. Poisson Model")+
  theme(plot.caption=element_text(size=10, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))
p11$layers[[1]]$aes_params$shape<-NULL
p11$layers[[1]]$aes_params$size<-0.75

p12<-plot(G_P.gof,type="dispersion")+ 
  labs(caption="c) Dispersion plot. Poisson Model")+
  theme(plot.caption=element_text(size=10, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))



grid.arrange(p11,p12,nrow=1)


## ----fig.show='hide'--------------------------------
G.BA <- plot_BA(Grimso,y="Tot",id="TransectID",rm="Round") # Bland-Altman plot


## ---- eval = F--------------------------------------
## plot_BA(Grimso,y="Tot",id="TransectID", type="bars") # Bar plot


## ---------------------------------------------------
quantile(G.BA$data$Diff, probs=c(0.05,0.95))


## ---- fig.show='hide', echo=F-----------------------
p13<-plot_BA(EPP,y="Social",id="id",rm="Year")$plot +
  labs(caption="a) Bland-Altman plot. Sparrow fledglings \n paternity example") +
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p14<-plot_BA(EPP,y="Social",id="id",type="bars")$plot + 
  labs(caption="b) Bar plot of differences. Sparrow fledglings \n paternity example")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p15 <- plot_BA(AF,y="y",id="id", rm="met")$plot + 
  labs(caption="c) Bland-Altman plot. CD34+ count cell example")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p16 <- plot_BA(AF,y="y",id="id", type="bars")$plot + 
  labs(caption="d) Bar plot of differences. CD34+ count cell example")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p17 <- plot_BA(Grimso,y="Tot",id="TransectID",rm="Round")$plot + 
  labs(caption="e) Bland-Altman plot. Tick counts example")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))

p18 <- plot_BA(Grimso,y="Tot",id="TransectID", type="bars")$plot + 
  labs(caption="f) Bar plot of differences. Tick counts example")+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(15,0,0,0)),
        text = element_text(size = 8))


## ----figure4, echo=F, fig.width = 6, fig.height =9, fig.cap="Bland-Altman and Bar plots. The first column shows the Bland-Altman plots where difference between pairs of data from the same subject (Y-axis) is plotted against mean of data from the same subject (X-axis). The second column contains the Bar plots of the differences between pairs of data from the same subject. The plots for Sparrow fledglings paternity example are on the first row, the CD34+ count cell example plots are on second row, and plots for Tick counts example are on third row."----

grid.arrange(p13,p14,p15,p16,p17,p18,nrow=3)

