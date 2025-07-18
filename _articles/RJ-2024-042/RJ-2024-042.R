# Generated by `rjournal_pdf_article()` using `knitr::purl()`: do not edit by hand
# Please edit RJ-2024-042.Rmd to modify this file

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.align="center")
library(tidyverse)


## ----echo=FALSE---------------------------------------------------------------
library(UpAndDownPlots)
head(data.frame(AutoSalesX))


## ----CPI, echo = FALSE, fig.asp = 0.6,out.width='90%', fig.cap="UK Consumer Price Index changes August 2017 to August 2018.  The red dotted line shows the overall percentage change.  The rectangles with blue borders show the changes of the main sectors.  Heights are percentage changes, widths are index values in August 2017, so that the areas of the rectangles are each group contributions to the overall change.  The grey bars show the percentage and absolute changes for the individual components."----
library(UpAndDownPlots)
yp <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018",
  levs=c("Sector", "Component"), sortLev=c("orig", "orig"))
yd <- ud_plot(yp, drawFrom="SmallToBig")
yd$uad


## ----CPIlab, echo = FALSE, fig.asp = 1.2,out.width='90%', fig.cap="CPI changes ordered by percentage changes of sectors and by percentage changes of components within sectors.  Two sectors, Transport and Recreation and Culture, made the biggest contributions to change.  Prices increased for all components in most sectors."----
yq <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018", levs=c("Sector", "Component"), sortLev=c("perc", "perc"))
yf <- ud_plot(yq, labelvar="Sector", drawFrom="SmallToBig", vscale=c(-30, 30))
yf$uadl


## ----NIvars, echo = TRUE,out.width='90%', fig.cap="Percentage changes in population between 2011 and 2017 by age, gender, district, and area."----
library(gridExtra)
g4 <- dgroup(NIpop, byvars=c("age", "gender", "LGD2014_name", "area_name"),
             v1="y2011", v2="y2017")
grid.arrange(g4$uadgl)


## ----NIchanges, echo = FALSE--------------------------------------------------
library(knitr)
library(kableExtra)
Gbase <- NIpop %>% group_by(gender) %>% summarise(G1=sum(y2011), G2=sum(y2017), change = G2-G1, `% change`=100*(G2-G1)/G1) %>% select(-G1, -G2)
Abase <- NIpop %>% group_by(age) %>% summarise(A1=sum(y2011), A2=sum(y2017), change = A2-A1, `% change`=100*(A2-A1)/A1) %>% select(-A1, -A2)
GAbase <- NIpop %>% group_by(gender, age) %>% summarise(GA1=sum(y2011), GA2=sum(y2017), change = GA2-GA1, `% change`=100*(GA2-GA1)/GA1) %>% select(-GA1, -GA2)
AGbase <- NIpop %>% group_by(age, gender) %>% summarise(AG1=sum(y2011), AG2=sum(y2017), change = AG2-AG1, `% change`=100*(AG2-AG1)/AG1) %>% select(-AG1, -AG2)
K1 <- kable(Gbase, digits=2, booktabs=TRUE, linesep="")
kK1 <- column_spec(K1, 1:3, width="1.5cm")
K2 <- kable(GAbase, digits=2, booktabs=TRUE, linesep="")
kK2 <- column_spec(K2, 1:4, width="1.5cm")
K3 <- kable(Abase, digits=2, booktabs=TRUE, linesep="")
kK3 <- column_spec(K3, 1:3, width="1.5cm")
K4 <- kable(AGbase, digits=2, booktabs=TRUE, linesep="")
kK4 <- column_spec(K4, 1:4, width="1.5cm")
kables(list(kK1, kK2), caption="Absolute and percentage changes in N. Ireland population estimates by age and gender.")
kables(list(kK3, kK4))


## ----NIag, echo = TRUE,out.width='90%', fig.cap="Percentage changes in population between 2011 and 2017 by age and then gender (males above, females below)."----
ag <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("age", "gender"),
              sortLev=c("orig", "perc"))
kag <- ud_plot(ag, labelvar="age")
kag$uadl


## ----NIdag, echo = TRUE,out.width='90%', fig.cap="Percentage changes in population between 2011 and 2017 for three districts (changes outlined in blue), four age groups (outlined in brown), and gender (filled bars)."----
NIpopW <- NIpop %>% filter(LGD2014_name %in% c("Belfast", "Derry City and Strabane",
                "Newry, Mourne and Down"))
zdag <- ud_prep(NIpopW, v1="y2011", v2="y2017", levs=c("LGD2014_name", "age", "gender"),
                sortLev=c("perc", "orig", "perc"))
zdag2 <- ud_plot(zdag, labelvar="LGD2014_name", drawFrom="SmallToBig")
zdag2$uadl


## ----autoS, echo = FALSE, fig.width = 6, fig.asp = 1,out.width='90%', fig.cap="Compact car sales changes in Germany by manufacturer between 2017 and 2018.  The plot on the left is drawn with a baseline of zero change and ordered by percentage changes.  The plot on the right is drawn with a baseline of the overall market change and ordered by changes in market share."----
library(patchwork)
AutoSalesXcomp <- AutoSalesX %>% filter(Segment=="Compact")
yxp <- ud_prep(AutoSalesXcomp, v1="sales17", v2="sales18",
  levs=c("Manufacturer"), sortLev=c("perc"))
yM <- ud_plot(yxp, labelvar="Manufacturer")
AutoSalesXcomp <- AutoSalesXcomp %>% mutate(S17=sum(sales17),
  S18=sum(sales18), p17=100*sales17/S17, p18=100*sales18/S18, msch=p18-p17)
AutoSalesXcompS <- AutoSalesXcomp %>% arrange(msch)
yxs <- ud_prep(AutoSalesXcompS, v1="sales17", v2="sales18",
  levs=c("Manufacturer"), sortLev=c("orig"))
yS <- ud_plot(yxs, b=yM$TotPerc, labelvar="Manufacturer")
yM$uadl + yS$uadl

