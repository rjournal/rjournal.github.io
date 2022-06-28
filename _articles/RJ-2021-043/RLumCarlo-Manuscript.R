## ---- echo=FALSE, message=FALSE-----------------------------------------------
library(RLumCarlo)
library(ggplot2)
library(gridExtra)
library(scales)
library(grid)

##https://github.com/JohannesFriedrich/EnergyBandModels
theme <- theme(
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12, face = "bold")
)

electrons <- data.frame(
  x = seq(0,1, length.out = 10),
  y = rep(c(-2.2,-1.5, -1, -0.6, -0.3, 0), each = 10)
)


## ----Fig1, fig.path="figures/", cache = FALSE, echo=FALSE, fig.align="center", fig.width=10,fig.height=6, out.width="140mm", warning=FALSE, fig.cap="Energy-band model representation of the models implemented in \\CRANpkg{RLumCarlo}. Letters represent physical model parameters and arrows indicate allowed transitions. (A) Delocalised transition: the model consists of one single trap and one recombination centre. Transition processes involve the conduction band. (B) Localised transition: the model consists of two sub-conduction band energy levels. Charge transitions do not involve the conduction band, but take place locally. (C) Tunnelling transition: The model consists of one trap and one recombination centre. Transitions take place from the excited state into the recombination centre without involving the conduction band. Site note: For the creation of the plots we used \\CRANpkg{scales} and \\CRANpkg{ggplot2} (\\url{https://github.com/JohannesFriedrich/EnergyBandModels})."----
source("R_Helpers/EnergyBandModels.R")
gridExtra::grid.arrange(p_DELOC, p_LOC, p_TUN, ncol = 3)


## ---- eval=FALSE--------------------------------------------------------------
#> n <- 1000
#> t <- 1:100
#> P <- 0.2
#> remaining <- numeric(length(t))
#> 
#> for (t in t) {
#>   for (j in 1:max(n)) {
#>     if (runif(1) < P && n > 0)
#>       n <- n - 1
#> 
#>   }
#> 
#>   if (n > 0)
#>     remaining[t] <- n
#> 
#> }


## ----Fig4, fig.path="figures/", cache=TRUE, echo=FALSE, fig.align="center", fig.width=10,fig.height=4, out.width="140mm", warning=FALSE, fig.cap="Exemplary comparison of TL signals simulated for all three \\CRANpkg{RLumCarlo} models: (A) delocalised (DELOC), (B) localised (LOC), and (C) tunnelling (TUN). General physical parameters, such as $E$ (1.45$\\,$eV), $s$ ($3.5\\times10^{12}\\,$s$^{-1}$) and the stimulation temperature (100--450$\\,^{\\circ}$C) were kept constant for all models."----
par(mfrow = c(1,3))
results <- run_MC_TL_DELOC(
 s = 3.5e12,
 E = 1.45,
 n_filled = 200,
 R = 1,
 times = 100:450) %T>%
   plot_RLumCarlo(legend = T, main = "(A) Delocalised transition")

run_MC_TL_LOC(
 s = 3.5e12,
 E = 1.45,
 r = 1,
 times = 100:450) %>%
   plot_RLumCarlo(legend = T, main = "(B) Localised transition")

run_MC_TL_TUN(s = 3.5e12,
          E = 1.45,
          rho = 0.015,
          r_c = 0.85,
          times = 100:450) %>%
    plot_RLumCarlo(legend = T, main = "(C) Tunnelling transition")



## ---- eval=FALSE--------------------------------------------------------------
#> results <- run_MC_TL_DELOC(
#>  s = 3.5e12,
#>  E = 1.45,
#>  clusters = 10,
#>  n_filled = 200,
#>  R = 1,
#>  times = 100:450) %T>%
#>    plot_RLumCarlo(legend = T, main = "(A) Delocalised transition")


## ----Fig5, fig.path="figures/", cache=TRUE, echo=FALSE, fig.align="center", fig.width=10,fig.height=4, out.width="140mm", warning=FALSE, fig.cap="(A) Plot of the remaining electrons for the TL process using the delocalised transition model for which we show the luminescence signal in Fig.~\\ref{fig:Fig4}A. (B) Plot of remaining electrons for two models combined in one system. (C) Stochastic uncertainty structure from (B)."----
par(mfrow = c(1,3))

## first model
DELOC <- run_MC_TL_DELOC(
 s = 3.5e12,
 E = 1.45,
 n_filled = 200,
 R = 1,
 times = 100:450, 
 output = "remaining_e"
 ) %T>%
   plot_RLumCarlo(legend = T, main = "(A) TL DELOC - remaining electrons")

## 2nd model
LOC <- run_MC_TL_LOC(
 s = 3.5e12,
 E = 1.45,
 r = 1,
 n_filled = 200,
 times = 100:450, 
 output = "remaining_e",
 )

## combine models
plot_RLumCarlo(c(DELOC, LOC),  main = "(B) TL DELOC + LOC remaining electrons")

## get different uncertainty structure
df <- summary(c(DELOC, LOC), verbose = FALSE)
plot(
  x = df$time,
  y = (df$sd / df$mean) * 100, pch = 20,
  col = rgb(0,0,0,.2),
  xlab = "Temperature [\u00b0C]", ylab = "CV [%]",
  main = "(C) TL LOC + DELOC - uncertainty structure"
)
grid()


## ---- echo=FALSE--------------------------------------------------------------
str(DELOC)


## ---- eval=FALSE--------------------------------------------------------------
#> df <- summary(c(DELOC, LOC), verbose = FALSE)
#> plot(
#>   x = df$time,
#>   y = (df$sd / df$mean) * 100, pch = 20,
#>   col = rgb(0,0,0,.2),
#>   xlab = "Temperature [\u00b0C]", ylab = "CV [%]",
#>   main = "(C) TL LOC + DELOC - uncertainty structure"
#> )


## ---- eval=FALSE--------------------------------------------------------------
#> run_MC_TL_LOC(s = 3.5e12, E = 1.45, n_filled = 1000,
#>   clusters = create_ClusterSystem(100))


## ---- eval = FALSE------------------------------------------------------------
#> output <- RLumModel::model_LuminescenceSignals(
#>   sequence = list(IRR = c(20, 10, 1), TL = c(20, 400, 1)),
#>   model = "Bailey2001"
#> )


## ---- eval=FALSE--------------------------------------------------------------
#> TL110 <- RLumCarlo::run_MC_TL_DELOC(
#>   s = 5e12, E = 0.97, R = 5e-10, times = seq(20,400,2),
#>   N_e = output$`conc. level 1 (TL)`[1,2]/1e5)


## ---- eval=FALSE--------------------------------------------------------------
#> object <- c(TL110, TL230, TL325)


## ---- eval=FALSE--------------------------------------------------------------
#> RLumCarlo::plot_RLumCarlo(
#>   object = object,
#>   plot_value = "sum",
#>   add = TRUE,
#>   FUN = function(x) {x * 1/(1 + (1e+7 * exp(-0.61/(8.617e-5 * (object$time + 273)))))}
#> )


## ----Fig7, fig.path="figures/", cache=FALSE, echo=FALSE, fig.align="center", fig.width=8,fig.height=5, out.width="90mm", warning=FALSE, fig.cap="Simulation results of \\CRANpkg{RLumModel} and \\CRANpkg{RLumCarlo}. Qualitatively both approaches show a good match."----
output <- RLumModel::model_LuminescenceSignals(
  sequence = list(
    IRR = c(20, 10, 1),
    TL = c(20, 400, 1)),
  model = "Bailey2001", 
  verbose = FALSE,
  ylab = "Norm. TL",
  norm = TRUE,
  lwd = 2,
  mtext = "Bailey 2001",
  main = "RLumModel vs RLumCarlo"
)

## RLumCarlo
TL110 <- RLumCarlo::run_MC_TL_DELOC(
  s = 5e12, E = 0.97, R = 5e-10, times = seq(20,400,2), 
  N_e = output$`conc. level 1 (TL)`[1,2]/1e5)

TL230 <- RLumCarlo::run_MC_TL_DELOC(
  s = 5e14, E = 1.55, R = 5e-10, times = seq(20,400,2), 
  N_e = output$`conc. level 2 (TL)`[1,2]/1e5)

TL325 <- RLumCarlo::run_MC_TL_DELOC(
  s = 5e13, E = 1.7, R = 5e-10, times = seq(20,400,2), 
  N_e = output$`conc. level 3 (TL)`[1,2]/1e5)

## combine 
object <- c(TL110, TL230, TL325)

## RLumCarlo
RLumCarlo::plot_RLumCarlo(
  object = object,
  norm = TRUE,
  FUN = function(x) {x * 1/(1 + (1e+7 * exp(-0.61/(8.617e-5 * (object$time + 273)))))},
  plot_value = "sum",
  type = "p",
  grid = FALSE,
  col = rgb(0.5,0,0,0.5),
  add = TRUE)

## add legend
legend(
  "topright",
  bty = "n",
  lwd = 1,
  pch = c(NA, 1),
  col = c("black", rgb(0.5, 0, 0, 0.5)),
  legend = c("RLumModel", "RLumCarlo")
)
grid()

