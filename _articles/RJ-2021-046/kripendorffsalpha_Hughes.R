
library(krippendorffsalpha)

nominal = matrix(c(1,2,3,3,2,1,4,1,2,NA,NA,NA,
                   1,2,3,3,2,2,4,1,2,5,NA,3,
                   NA,3,3,3,2,3,4,2,2,5,1,NA,
                   1,2,3,3,2,4,4,1,2,5,1,NA), 12, 4)
nominal
set.seed(42)
fit.full = krippendorffs.alpha(nominal, level = "nominal", control = list(parallel = FALSE), verbose = TRUE)
summary(fit.full)
(inf.6 = influence(fit.full, units = 6))
fit.full$alpha.hat - inf.6$dfbeta.units
fit.sub = krippendorffs.alpha(nominal[-6, ], level = "nominal", control = list(parallel = FALSE))
confint(fit.sub)
dev.new()
plot(fit.sub, xlim = c(0, 1), xlab = "Bootstrap Estimates", main = "Nominal Data", density = FALSE)

data(cartilage)
cartilage = as.matrix(cartilage)
set.seed(12)
fit.sed = krippendorffs.alpha(cartilage, level = "interval", control = list(bootit = 10000, parallel = TRUE, nodes = 3), verbose = TRUE)
summary(fit.sed)
dev.new()
plot(fit.sed, xlim = c(0.7, 0.9), xlab = "Bootstrap Estimates", main = "Cartilage Data (SED)")

L1.dist = function(x, y)
{
    d = abs(x - y)
    if (is.na(d))
        d = 0
    d
}

fit.L1 = krippendorffs.alpha(cartilage, level = L1.dist, control = list(bootit = 10000, parallel = TRUE, nodes = 3), verbose = TRUE)
summary(fit.L1)
dev.new()
plot(fit.L1, xlim = c(0.5, 0.7), xlab = "Bootstrap Estimates", main = "Cartilage Data (L1)")




