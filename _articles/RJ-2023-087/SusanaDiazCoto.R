# Unified ROC Curve Estimator for Diagnosis and Prognosis Studies:
# the sMSROC packages

# Code Examples and Figures

# Example 1 [Exploratory Data Analysis]. The Diabetes dataset
library(sMSROC)
data(diabet)
expl <- explore_table(marker=diabet$stab.glu, status=diabet$diab)
expl$table

# Figure 1 Left panel
library(ggplot2)
density <- explore_plot(marker=diabet$stab.glu, status=diabet$diab)
output  <- density$plot + xlab("Stabilized Glucose") +
                          scale_x_continuous(breaks=seq(0, 400, 50),
                                             labels=seq(0,400,50),
                                             limits=c(0, 400))
output

# Example 1 [ROC curve models]. The Diabetes dataset

# Stabilized glucose
roc_diabetes <- sMSROC(marker=diabet$stab.glu, status = diabet$diab, meth="S")
roc_diabetes
summary(roc_diabetes)

alt_mod <- glm(diabet$diab ~ diabet$stab.glu + diabet$age, family=binomial)
prob_model <- predict(alt_mod, type = 'response',
                      newdata = data.frame(diabet$diab, diabet$stab.glu, diabet$age))

roc_diabetes_prob <- sMSROC(marker=diabet$stab.glu, status=diabet$diab,
                            probs=prob_model)
roc_diabetes_prob

# Example 2 [ROC curve models]. The Kidney Transplant Failure Score (KTFS) dataset
data(ktfs)
roc_KTFS <-  sMSROC(marker=ktfs$score,
                    status=ktfs$failure,
                    observed.time=ktfs$time,
                    time=5,
                    meth="L",
                    conf.int="T",
                    ci.meth ="E")
roc_KTFS

# Figure 1 Right panel
density <- explore_plot(marker=ktfs$score,
                        status=ktfs$failure,
                        observed.time=ktfs$time,
                        time=5)
output <- density$plot + xlab("KTFS")
output

# Example 2 [ROC curve plots]. The Kidney Transplant Failure Score (KTFS) dataset
# ROC curve
plot_KTFS <- sMSROC_plot(sMS = roc_KTFS, m.value = 3.9)
plot_KTFS$rocplot

# Evolution of the AUCs
aucs <- evol_auc(marker= ktfs$score,
                 status = ktfs$failure,
                 observed.time = ktfs$time,
                 time = seq(2, 10),
                 meth = "L")
plot_aucs <- aucs$evol.auc +
             scale_x_continuous(limit = c(2, 10),
                                breaks = seq(2, 10, 1)) +
             scale_y_continuous(limit = c(0.4, 1),
                                breaks = seq(0.4, 1, 0.1))
df1 <- data.frame(x = c(2,10), y = c(0.5, 0.5))

plot_aucs <- plot_aucs +
geom_line(data = df1, aes(x, y), linewidth = 0.9, colour = "gray", linetype = "twodash")
plot_aucs

# Predictive model
probs <- probs_pred(roc_KTFS, var = "T")
plot_probs_pred <- probs$plot + xlab("KTFS")

plot_probs_pred

# Figure 2
library(cowplot)
par(mar = c(0,0,0,10))
ggdraw() +
  draw_plot(plot_KTFS$rocplot, x = 0, y = .40, width = .5, height = .6) +
  draw_plot(plot_probs_pred, x = .49, y = .4, width = .5, height = .6) +
  draw_plot(plot_aucs, x = 0, y = 0, width = 0.98, height = 0.4)

# Print Confidence Interval
conf_int_print(roc_KTFS)

# Example 3.The Fibrosis dataset.

# 5 Years
library(ggplot2)
data(fibrosis)
explore_table(marker=-fibrosis$Score,
              left=fibrosis$Start,
              right=fibrosis$Stop,
              time=5)$summary

roc_fibrosis_5 <- sMSROC(marker=-fibrosis$Score,
                         left=fibrosis$Start,
                         right=fibrosis$Stop,
                         meth="L",
                         time=5)
roc_fibrosis_5


# 10 years
roc_fibrosis_10 <- sMSROC(marker=-fibrosis$Score,
                         left=fibrosis$Start,
                         right=fibrosis$Stop,
                         meth="L",
                         time=10)
# 15 years
roc_fibrosis_15 <- sMSROC(marker=-fibrosis$Score,
                          left=fibrosis$Start,
                          right=fibrosis$Stop,
                          meth="L",
                          time=15)
# Figure 3
## Left panel
par(mfrow=c(1,2))
par(mar=c(5,5,1,1.5))
plot(1 - roc_fibrosis_5$SP, roc_fibrosis_5$SE, type="l", lwd=5,
         col="steelblue3", xlab="1 - Specificity", ylab="Sensitivity", cex.axis=1.5, cex.lab=1.5,
         main="           ", cex.main=2)
lines(c(0,1),c(0,1),lty=2)
lines(1 - roc_fibrosis_10$SP, roc_fibrosis_10$SE, lwd=5, col="steelblue3", lty=2)
lines(1 - roc_fibrosis_15$SP, roc_fibrosis_15$SE, lwd=5, col="steelblue3", lty=3)

lines(c(0.4,0.55), c(0.25,0.25), lwd=5, col="steelblue3")
text(0.56, 0.25, "5 Years", pos=4, cex=1.5)
lines(c(0.4,0.55), c(0.16,0.16), lwd=5, lty = 2, col="steelblue3")
text(0.56, 0.16, "10 Years", pos=4, cex=1.5)
lines(c(0.4,0.55), c(0.07,0.07), lwd=5, lty = 3, col="steelblue3")
text(0.56, 0.07, "15 Years", pos=4, cex=1.5)

# Figure 3
## Right panel
library(cenROC)
band_5 <- IntROC(L = fibrosis$Start,
                 R = fibrosis$Stop,
                 M = -fibrosis$Score,
                 t = 5,
                 plot = "FALSE")
band_10 <- IntROC(L = fibrosis$Start,
                 R = fibrosis$Stop,
                 M = -fibrosis$Score,
                 t = 10,
                 plot = "FALSE")
band_15 <- IntROC(L = fibrosis$Start,
                 R = fibrosis$Stop,
                 M = -fibrosis$Score,
                 t = 15,
                 plot = "FALSE")
par(mar=c(5,5,1,1.5))
plot(band_5$U, band_5$ROC, type="l", lwd=5,
     col="tomato3", xlab="1 - Specificity", ylab="Sensitivity", cex.axis=1.5, cex.lab=1.5,
     main="           ", cex.main=2)
lines(c(0,1),c(0,1),lty=2)
lines(band_10$U, band_10$ROC, lwd=5, col="tomato3", lty=2)
lines(band_15$U, band_15$ROC, lwd=5, col="tomato3", lty=3)

## Middle panel
### Younden
par(mfrow=c(1,1))
lambda <- seq(0, 1,length=101)
Yw <- lambda
Tw <- lambda

for (j in 1:101) {
  Yw[j]<- max(lambda[j]*roc_fibrosis_5$SE + (1-lambda[j])*roc_fibrosis_5$SP )
  Tw[j]<- roc_fibrosis_5$thres[which.max(lambda[j]*roc_fibrosis_5$SE + (1-lambda[j])*roc_fibrosis_5$SP )]
}

    plot(lambda, Yw, ylim = c(0.4,1), type="l", lwd=5,
     col="#003366", frame = FALSE,
     ylab = 'Weighted Youden Index',
     xlab = bquote(lambda),
     cex.axis=1.1, cex.lab=1.2)
lines(c(0,1), c(0.5, 0.5), col = 'grey', lty = 2)

points(lambda[c(11, 26, 51, 76, 91)], Yw[c(11, 26, 51, 76, 91)],
        pch=19, lwd =5)
text(lambda[c(11, 26, 51, 76, 91)], Yw[c(11, 26, 51, 76, 91)] - 0.05,
     Tw[c(11, 26, 51, 76, 91)])

## Bottom panel
# Evolution of the AUCs
# sMSROC estimator
aucs <- evol_auc(marker = -fibrosis$Score,
                 left = fibrosis$Start,
                 right = fibrosis$Stop,
                 time = seq(3, 18, 1))

df1 <- data.frame(x = c(3,18), y = c(0.5, 0.5))
plot_aucs <- aucs$evol.auc +
             scale_x_continuous(limit = c(3, 18),
                                breaks = seq(3, 18, 1)) +
             scale_y_continuous(limit = c(0.4, 1),
                                breaks = seq(0.4, 1, 0.1)) +
             geom_line(data = df1, aes(x, y), linewidth = 0.9,
                       colour = "gray", linetype = "twodash")

#cenROC estimator
time_cen <- seq(3, 18, 1)
l <- length(time_cen)
auc_cen  <- sapply(1:l, function(i){IntROC(fibrosis$Start,
                                          R = fibrosis$Stop,
                                          M = -fibrosis$Score,
                                          t = time_cen[i],
                                          plot = "FALSE")}$AUC)
d1    <- t(auc_cen[1,])
dcens <- data.frame(seq(3, 18, 1),
                    data.frame(t(as.data.frame(auc_cen[1,]))))
colnames(dcens) <- c("x", "y")

df <- data.frame(x = aucs$time, y = aucs$auc)

plot(df$x, df$y,
     xlim = c(3, 18),
     ylim = c(0.4, 1),
     type="l", lwd=5,
     col="steelblue3", frame = FALSE,
     ylab = 'AUC',
     xlab = 'Follow-up',
     cex.axis=1.1, cex.lab=1.2,
     xaxt = "n")
axis(1, at = seq(3, 18, 1), cex.axis=1.1)
lines(c(3,18), c(0.5, 0.5), col = 'grey', lty = 2)
lines(dcens$x, dcens$y, col = 'tomato3',
      type = "l", lwd=5)
