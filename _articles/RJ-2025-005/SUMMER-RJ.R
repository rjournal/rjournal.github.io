##---- load-package
library(SUMMER)
library(ggplot2)
library(patchwork)

## ---- brfss-1
data(BRFSS)
data <- subset(BRFSS, !is.na(diab2) & !is.na(hracode))

## ---- brfss-2
data(KingCounty)
KingGraph <- getAmat(KingCounty, KingCounty$HRA2010v2_)

## ---- brfss-3
fit.BRFSS <- smoothSurvey(data = data, Amat = KingGraph, 
						  response.type = "binary", responseVar = "diab2", 
						  strataVar="strata", weightVar="rwt_llcp", 
						  regionVar="hracode", clusterVar = "~1")
head(fit.BRFSS$direct, n = 3)
head(fit.BRFSS$smooth, n = 3)

## ---- brfss-4 
g1 <- mapPlot(fit.BRFSS$direct, geo = KingCounty, 
			  by.data = "region", by.geo = "HRA2010v2_", 
			  variables = "direct.est", label = "Direct Estimates", 
			  legend.label = "Prevalence", ylim = c(0, 0.24))
g2 <- mapPlot(fit.BRFSS$smooth, geo = KingCounty, 
		      by.data = "region", by.geo = "HRA2010v2_", 
		      variables = "median", label = "Fay Herriot Estimates", 
		      legend.label = "Prevalence", ylim = c(0, 0.24))
g1 + g2



## ---- direct-estimate-1
data(DemoData)
head(DemoData[[1]])

## ---- direct-estimate-2
DemoDataNMR <- DemoData
for(i in 1:length(DemoData)){
	DemoDataNMR[[i]] <- subset(DemoData[[i]], age == "0")
}

## ---- direct-estimate-3
periods <- c("85-89", "90-94", "95-99", "00-04", "05-09", "10-14")
directNMR <- getDirectList(births = DemoDataNMR, years = periods, 
				   		   regionVar = "region", timeVar = "time", 
				   		   clusterVar = "~clustid + id", ageVar = "age", 
				   		   weightsVar = "weights")
directU5 <- getDirectList(births = DemoData, years = periods, 
						  regionVar = "region", timeVar = "time", 
						  clusterVar = "~clustid + id", ageVar = "age", 
						  weightsVar = "weights")

## ---- direct-estimate-4
directNMR.comb <- aggregateSurvey(directNMR)
directU5.comb <- aggregateSurvey(directU5)


## ---- smooth-direct-estimate-1
fhNMR <- smoothDirect(data = directNMR.comb, Amat = DemoMap$Amat, 
				      year.label = c(periods, "15-19"), year.range = c(1985, 2019), 
				      time.model = "rw2", type.st = 4, is.yearly = TRUE, m = 5)
fhU5 <- smoothDirect(data = directU5.comb, Amat = DemoMap$Amat, 
				      year.label = c(periods, "15-19"), year.range = c(1985, 2019), 
				      time.model = "rw2", type.st = 4, is.yearly = TRUE, m = 5)

## ---- smooth-direct-estimate-2
est.NMR <- getSmoothed(fhNMR, CI = 0.95)
est.U5 <- getSmoothed(fhU5, CI = 0.95)
g3 <- plot(est.NMR, per1000 = TRUE) + ggtitle("NMR")
g4 <- plot(est.NMR, per1000 = TRUE, plot.CI=TRUE) + facet_wrap(~region)
g5 <- plot(est.U5, per1000 = TRUE)  + ggtitle("U5MR")
g6 <- plot(est.U5, per1000 = TRUE, plot.CI=TRUE) + facet_wrap(~region)
(g3 + g4) / (g5 + g6)


 
## ---- load-map-1
data(MalawiMap)
MalawiGraph = getAmat(MalawiMap, names=MalawiMap$ADM2_EN)

## ---- bb-sub-1
load("Data/DHS_counts.rda")
agg.counts$strata <- NA
head(agg.counts)

## ---- bb-sub-2
fit.bb <- smoothCluster(data = agg.counts, Amat = MalawiGraph, 
						family = "betabinomial", year.label = 2000:2019, 
						time.model = "rw2", st.time.model = "ar1",
						age.group = c("0", "1-11", "12-23", "24-35", "36-47", "48-59"),
						age.n = c(1, 11, 12, 12, 12, 12), 
						age.time.group = c(1, 2, 3, 3, 3, 3),
						pc.st.slope.u = 2, pc.st.slope.alpha = 0.1,
						bias.adj = MalawiData$HIV.yearly, 
						bias.adj.by = c("years", "survey"),
						survey.effect = FALSE)


## ---- bb-sub-3
est.bb <-  getSmoothed(fit.bb, nsim = 1000, save.draws = TRUE) 

## ---- vis-1, fig.width = 7, fig.height = 5, out.width = "100%"
select <- c("Chitipa", "Karonga", "Rumphi", "Mzimba")
plot(subset(est.bb$overall, region %in% select), per1000 = TRUE, year.proj = 2016)


## ---- vis-1b, fig.width = 7, fig.height = 5, out.width = "100%"
load("Data/DHS_direct_hiv_adj.rda")
plot(subset(est.bb$overall, region %in% select), per1000 = TRUE, 
	 year.proj = 2016, plot.CI = TRUE,
	 data.add = direct.2015.hiv, label.add = "Direct Estimates", 
	 option.add = list(point = "mean", lower = "lower", upper = "upper")) + 
facet_wrap(~region, ncol = 4)


## ---- vis-2, fig.width = 7, fig.height = 3, out.width = "100%"
year.plot <- c("2007", "2010", "2013", "2016", "2019")
mapPlot(subset(est.bb$overall, years %in% year.plot), 
		geo = MalawiMap, by.data = "region", by.geo = "ADM2_EN", 
		is.long = TRUE, variables = "years", values = "median", 
		ncol = 5, direction = -1, per1000 = TRUE, legend.label = "U5MR")


## ---- vis-3, fig.width = 7, fig.height = 3, out.width = "100%"
hatchPlot(subset(est.bb$overall, years %in% year.plot), 
		  geo = MalawiMap, by.data = "region", by.geo = "ADM2_EN", 
		  is.long = TRUE, variables = "years", values = "median", 
		  lower = "lower", upper = "upper", hatch = "red",
		  ncol = 5, direction = -1, per1000 = TRUE, legend.label = "U5MR")


## ---- vis-4, fig.width = 7, fig.height = 3, out.width = "100%"
ridgePlot(draws = est.bb, Amat = MalawiGraph, year.plot = year.plot,
		  ncol = 5, per1000 = TRUE, order = -1, direction = -1) + xlim(c(0, 200))



## ---- check-1
r.time <- getDiag(fit.bb, field = "time")
r.space <- getDiag(fit.bb, field = "space")
r.interact <- getDiag(fit.bb, field = "spacetime", draws = est.bb$draws)



## ---- check-2
g.time <- ggplot(r.time, aes(x = years, y = median, ymin=lower, ymax=upper)) + 
				 geom_line() + 
				 geom_ribbon(color=NA, aes(fill = label), alpha = 0.3) +
				 facet_wrap(~group, ncol = 3) + 
				 theme_bw() + 
				 ggtitle("Age-specific Temporal effects")
g.space <- mapPlot(subset(r.space, label = "Total"), 
				   geo=MalawiMap, by.data="region", by.geo = "ADM2_EN", 
				   direction = -1, variables="median", 
				   removetab=TRUE, legend.label = "Effect") + 
		   ggtitle("Spatial effects") 
g.interact <- ggplot(r.interact, aes(x = years, y = median, group=region)) + 
					 geom_line() +  ggtitle("Interaction effects") 
g.time + g.space + g.interact + plot_layout(widths = c(3, 2, 3)) 


