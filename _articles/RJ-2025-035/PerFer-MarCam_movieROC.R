# R script to reproduce the results presented in the paper ----
# "movieROC: Visualizing the Decision Rules Underlying Binary Classification"

library(movieROC)
data(HCC)
str(HCC)
table(HCC$tumor)


## Figure 2 ----

par(mfrow = c(1,3))
for(gene in c("20202438", "18384097", "03515901")){
  roc <- gROC(X = HCC[,paste0("cg",gene)], D = HCC$tumor)
  plot_densities(roc, histogram = TRUE, lwd = 3, main = paste("Gene", gene),
                 legend = (gene == "03515901"), pos.legend = "topleft",
                 xlim = c(0.4*(gene == "20202438"),1))
  plot_densities(roc, lwd = 3, new = FALSE,
                 col = adjustcolor(c('#485C99','#8F3D52'), alpha.f = 0.8))}

par(mfrow = c(1,1))


## Figure 3 ----

for(gene in c("20202438", "18384097", "03515901")){
  roc <- gROC(X = HCC[,paste0("cg",gene)], D = HCC$tumor,
              side = ifelse(gene == "03515901", "left", "right"))
  plot(roc, col = "gray50", main = paste("Gene", gene), lwd = 2)
  groc <- gROC(X = HCC[,paste0("cg",gene)], D = HCC$tumor, side = "both")
  plot(groc, new = FALSE, lwd = 2)
  legend("bottomright", paste(c("AUC =", "gAUC ="),
                              format(c(roc$auc, groc$auc), digits = 3)),
         col = c("gray50", "black"), lwd = 2, bty = "n", inset = .01)
}


## Figure extra HTML ----

roc_selg1 <- gROC(X = HCC$cg20202438, D = HCC$tumor, side = "right")
roc_selg1
predict(roc_selg1, FPR = .1)

plot_densityROC(roc_selg1, C = .77, build.process = TRUE)
plot_buildROC(roc_selg1, C = .77, build.process = TRUE, reduce = FALSE)
movieROC(roc_selg1, reduce = FALSE, file = "StandardROC_gene20202438.gif")


## Figure 4 ----

X <- HCC[ ,c("cg20202438", "cg18384097")]; D <- HCC$tumor
biroc_12_PT <- multiROC(X, D, method = "fixedLinear", methodLinear = "PepeThompson")
biroc_12_Meis <- multiROC(X, D, method = "dynamicMeisner", verbose = TRUE)
biroc_12_lrm <- multiROC(X, D)
biroc_12_kernel <- multiROC(X, D, method = "kernelOptimal")
list_biroc <- list(PepeTh = biroc_12_PT, Meisner = biroc_12_Meis, 
                   LRM = biroc_12_lrm, KernelDens = biroc_12_kernel)
lapply(names(list_biroc), function(x) movieROC(list_biroc[[x]], display.method = "OV",
                                        xlab = "Gene 20202438", ylab = "Gene 18384097", 
                                        lwd.curve = 4, cex = 1.2, alpha.points = 1,
                                        file = paste0(x, ".gif")))


## Figure 5 ----

multiroc_PT <- multiROC(X = HCC[,c("cg20202438", "cg18384097", "cg03515901")],
                        D = HCC$tumor, method = "fixedLinear", methodLinear = "PepeThompson")
multiroc_PT
plot_buildROC(multiroc_PT, cex = 1.2, lwd.curve = 4, alpha.points = 1)
plot_buildROC(multiroc_PT, display.method = "OV", displayOV = c(1,3), cex = 1.2,
              xlab = "Gene 20202438", ylab = "Gene 03515901", lwd.curve = 4, alpha.points = 1)


## Figure 6 ----

groc_selg1 <- gROC(X = HCC$cg20202438, D = HCC$tumor, side = "both")
groc_selg1
predict(groc_selg1, FPR = .1)

groc_selg1_C <- gROC(X = HCC$cg20202438, D = HCC$tumor, side = "both", 
                     restric = TRUE, optim = TRUE)

plot_regions(roc_selg1, cex.legend = 1.5, plot.auc = TRUE, 
             main = "Standard right-sided assumption [Classification subsets]")
plot_regions(groc_selg1, plot.auc = TRUE, legend = F, main.plotroc = "gROC curve",
             main = "General approach [Classification subsets]")
plot_regions(groc_selg1_C, plot.auc = TRUE, legend = F, main.plotroc = "gROC curve",
             main = "General approach with restriction (C) [Classific. subsets]", 
             xlab = "Gene 20202438 expression intensity")


## Figure 7 ----

X <- HCC$cg18384097; D <- HCC$tumor
hroc_cubic_selg2 <- hROC(X, D)
hroc_cubic_selg2
hroc_rcs_selg2 <- hROC(X, D, formula.lrm = "D ~ rcs(X,8)")
hroc_lkr1_selg2 <- hROC(X, D, type = "kernel")
hroc_lkr3_selg2 <- hROC(X, D, type = "kernel", kernel.h = 3)
hroc_overfit_selg2 <- hROC(X, D, type = "overfitting")

groc_selg2_C <- gROC(X, D, side = "both", restric = TRUE, optim = TRUE)

list_hroc <- list(Cubic = hroc_cubic_selg2, Splines = hroc_rcs_selg2, 
                  Overfit = hroc_overfit_selg2, LikRatioEst_h3 = hroc_lkr3_selg2,
                  LikRatioEst_h1 = hroc_lkr1_selg2, gAUC_restC = groc_selg2_C)
AUCs <- sapply(list_hroc, function(x) x$auc)
round(AUCs, 3)

par(mfrow = c(2,3))
lapply(list_hroc, function(x) plot_funregions(x, FPR = .15, FPR2 = .5))

### To modify titles:
for(i in seq_along(list_hroc)){
  main <- NULL
  if(i == 4) main <- "Likelihood ratio estimation \n(bandwidth = 3)"
  if(i == 5) main <- "Likelihood ratio estimation \n(bandwidth = 1)"
  if(i == 6) main <- "General approach \n under restriction (C)"
  plot_funregions(list_hroc[[i]], FPR = .15, FPR2 = .5, main = main)
}


### Figure 8 ----

plot_regions(hroc_rcs_selg2, FPR = .5, cex.legend = 1.5, plot.auc = TRUE)
plot_regions(groc_selg2_C, FPR = .5, legend = FALSE, plot.auc = TRUE,
             main = "Classification subsets: General approach with restriction (C)",
             main.plotroc = "gROC curve",  xlab = "Gene 18384097 expression intensity")
