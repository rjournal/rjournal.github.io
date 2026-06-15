# Using QTLEMM for QTL mapping analysis

## Simulation examples

install.packages("QTLEMM")
library(QTLEMM)
set.seed(8000)
options(digits = 3)

# Data generation
eff <- c("a1" = -10, "a2" = 12, "a3" = 8, "a1:a3" = 1)
marker <- cbind(rep(1:3,each = 11), rep(seq(0, 100, 10), 3))
QTL <- cbind(c(1, 1, 2), c(23, 77, 55))

testdata <- progeny(QTL, marker, type = "RI", ng = 2, E.vector = eff, h2 = 0.5, size = 200)
names(testdata)
# [1] "phe" "E.vector" "marker.prog" "QTL.prog" "genetic.value" "VG" "VE"
y <- testdata$phe
geno <- testdata$marker.prog

# Interval mapping
IMtest <- IM.search (marker, geno, y, type = "RI", ng = 2, speed = 1, d.eff = FALSE, QTLdist  =  15, plot.all = TRUE, plot.chr = FALSE, console = FALSE)
names(IMtest)
# [1] "effect" "LRT.threshold" "detect.QTL" "model" "inputdata"
IMtest$LRT.threshold
#  95%
# 9.62

detQTL <- IMtest$detect.QTL
detQTL
#    chr cM    a1  LRT     R2
# 14   1 14 -7.00 24.1 0.1064
# 77   1 77  8.03 29.6 0.1324
# 153  2 53  6.14 17.3 0.0787

# Multiple interval mapping
dQTL <- detQTL[,1:2]
D.matrix <- D.make(nQTL = 3, type = "RI", a = TRUE, d = 0, aa = c(1, 3))

dim(D.matrix)
# [1] 27 4
head(D.matrix)
#     a1 a2 a3 a1:a3
# 222  1  1  1     1
# 221  1  1  0     0
# 220  1  1 -1    -1
# 212  1  0  1     1
# 211  1  0  0     0
# 210  1  0 -1    -1

cp.matrix <- Q.make(dQTL, marker, geno, type = "RI", ng = 2)$cp.matrix
dim(cp.matrix)
# [1] 200 27

MIMtest <- EM.MIM(D.matrix = D.matrix, cp.matrix = cp.matrix, y = y, console = FALSE)
names(MIMtest)
# [1] "QTL" "E.vector" "beta" "variance" "PI.matrix" "log.likelihood" "LRT" "R2"
# [9] "y.hat" "yu.hat" "iteration.number" "model"
MIMtest$E.vector
#    a1    a2   a3 a1:a3
# -9.61 10.29 6.35  1.66
c(MIMtest$log.likelihood, MIMtest$LRT, MIMtest$R2)
# [1] -772.192  145.114  0.411

MIMtest_ <- EM.MIM(D.matrix = D.matrix, IMresult = IMtest, console = FALSE)
MIMtest_$E.vector
#    a1    a2   a3 a1:a3
# -9.61 10.29 6.35 1.66

MIMv <- EM.MIMv(dQTL, marker, geno, D.matrix, cp.matrix = NULL, y, console = FALSE)
# MIMv <- EM.MIMv(D.matrix = D.matrix, IMresult = IMtest, console = FALSE)
names(MIMv)
# [1] "E.vector" "beta" "variance" "PI.matrix" "log.likelihood" "LRT" "R2" "y.hat"
# [9] "iteration.number" "avc.matrix" "EMvar"

round(MIMv$avc.matrix, 3)
#            QTL1   QTL2   QTL3     a1     a2     a3  a1:a3 residual.var     mu
# QTL1      0.015  0.017  0.013 -0.003  0.000  0.014  0.076       -0.073 -0.003
# QTL2      0.017  0.004 -0.006 -0.023  0.021 -0.003  0.041       -0.191  0.002
# QTL3      0.013 -0.006  0.065 -0.034  0.035  0.036  0.134       -0.688  0.009
# a1       -0.003 -0.023 -0.034  1.417 -0.354 -0.091  0.038        1.775  0.006
# a2        0.000  0.021  0.035 -0.354  1.585 -0.039  0.154       -2.462 -0.096
# a3        0.014 -0.003  0.036 -0.091 -0.039  1.463  0.257       -1.185 -0.034
# a1:a3     0.076  0.041  0.134  0.038  0.154  0.257  3.724       -2.787 -0.096
# variance -0.073 -0.191 -0.688  1.775 -2.462 -1.185 -2.787      179.411  0.030
# X1       -0.003  0.002  0.009  0.006 -0.096 -0.034 -0.096        0.030  0.650
round(MIMv$EMvar, 3)
#  QTL1  QTL2  QTL3    a1    a2    a3  a1:a3 residual.var     mu
# 0.015 0.004 0.065 1.417 1.585 1.463  3.724      179.411  0.650

dQTL2 <- cbind(c(1, 1), c(14, 77))
MIMs <- MIM.search(dQTL2, marker, geno, y, type = "RI", ng = 2, D.matrix = D.matrix, speed = 1, QTLdist = 15, console = FALSE)
names(MIMs)
# [1] "effect" "QTL.best" "effect.best" "model" "inputdata"
MIMs$QTL.best
#         chromosome position(cM)
# QTL 1            1           14
# QTL 2            1           77
# QTL new          2           54
MIMs$effect.best
#     a1     a2    a3  a1:a3     LRT log.likelihood    R2
# -9.619 10.302 6.385  1.806 145.876       -772.129 0.412

MIMp <- MIM.points(dQTL, marker, geno, y, type = "RI", ng = 2, D.matrix = D.matrix, speed = 2, scope = 10, console = FALSE)
# MIMp <- MIM.points(D.matrix = D.matrix, speed = 2, scope = 10, IMresult = IMtest, console = FALSE)
names(MIMp)
# [1] "effect" "QTL.best" "effect.best" "model" "inputdata"
MIMp$QTL.best
#      chromosome position(cM)
# [1,]          1           24
# [2,]          1           75
# [3,]          2           53
MIMp$effect.best
#      a1     a2    a3  a1:a3     LRT log.likelihood    R2
# -10.846 11.994 6.503  3.725 181.130       -765.371 0.464

## The yeast dataset example

load(system.file("extdata", "yeast.process.RDATA", package = "QTLEMM"))

geno <- yeast.process$geno
marker <- yeast.process$marker
pheno <- yeast.process$pheno

# Selective genotyping data
y0 <- pheno[, 3590]
y <- y0[y0>quantile(y0)[4] | y0<quantile(y0)[2]]
yu <- y0[y0 >= quantile(y0)[2] & y0 <= quantile(y0)[4]]
geno.s <- geno[y0 > quantile(y0)[4] | y0 < quantile(y0)[2],]

# Selective genotyping IM and MIM analysis
library(QTLEMM)
set.seed(8000)
IMtest2 <- IM.search(marker, geno.s, y, yu, sele.g = "f", type = "BC", ng = 1, plot.all = TRUE, plot.chr = FALSE, console = FALSE)
IMtest2$detect.QTL
#      chr  cM     a1  LRT     R2
# 626    3  53  0.893 22.0 0.1128
# 1579   5 112  0.753 16.0 0.0749
# 4523  13  22 -0.882 21.7 0.1048
IMtest2$LRT.threshold
# 95% 
#15.4

IMcon <- IM.search(marker, geno, y0, type = "BC", ng = 1, plot.all = TRUE, plot.chr = FALSE, console = FALSE)
IMcon$detect.QTL
#      chr  cM     a1  LRT    R2
# 624    3  53  0.904 29.0 0.210
# 1580   5 117  0.877 25.0 0.171
# 4511  13  22 -0.945 33.1 0.231

D.matrix <- D.make(3, type = "BC", aa = TRUE)
MIMtest2 <- EM.MIM(D.matrix = D.matrix, IMresult = IMtest2, console = FALSE)
MIMtest2$E.vector
#    a1    a2     a3  a1:a2  a1:a3  a2:a3
# 0.818 0.744 -0.954 -0.641  0.371 -0.423
c(MIMtest2$log.likelihood, MIMtest2$LRT, MIMtest2$R2)
# [1] -115.253  79.117  0.512

MIMp <- MIM.points(D.matrix = D.matrix, scope = 5, IMresult = IMtest2, console = FALSE)
MIMp$QTL.best
#      chromosome position(cM)
# [1,]          3           58
# [2,]          5          111
# [3,]         13           21
MIMp$effect.best
#    a1    a2     a3  a1:a2  a1:a3  a2:a3    LRT log.likelihood    R2
# 0.678 0.758 -0.964 -0.866  0.673 -0.499 82.214       -113.705 0.524


# Using QTLEMM for QTL hotspot detection

## The yeast genetic genomics dataset example

load(url("https://github.com/py-chung/QTLEMM/raw/main/inst/extdata/yeast.LOD.1.RDATA"))
load(url("https://github.com/py-chung/QTLEMM/raw/main/inst/extdata/yeast.LOD.2.RDATA"))
load(url("https://github.com/py-chung/QTLEMM/raw/main/inst/extdata/yeast.LOD.3.RDATA"))
load(url("https://github.com/py-chung/QTLEMM/raw/main/inst/extdata/yeast.LOD.4.RDATA"))
load(url("https://github.com/py-chung/QTLEMM/raw/main/inst/extdata/yeast.LOD.bin.RDATA"))
LOD <- rbind(yeast.LOD.1, yeast.LOD.2, yeast.LOD.3, yeast.LOD.4)
bin <- yeast.LOD.bin

library(QTLEMM)
set.seed(8000)
LOD.QTLdetect.result <- LOD.QTLdetect(LOD, bin, thre = 3, QTLdist = 20, console = FALSE)
names(LOD.QTLdetect.result)
# [1] "detect.QTL.number" "QTL.matrix" "EQF.matrix" "linkage.QTL.number"
# [5] "LOD.threshold" "bin"

EQF.permu.result <- EQF.permu(LOD.QTLdetect.result, npermu = 1000, alpha = 0.05, Q = TRUE, console = FALSE)
names(EQF.permu.result)
# [1] "EQF.matrix" "bin" "LOD.threshold" "cluster.number" "cluster.id" "cluster.matrix"
# [7] "permu.matrix.cluster" "permu.matrix.Q" "EQF.threshold"

EQF.plot(EQF.permu.result, plot.all = TRUE, plot.chr = FALSE)

## The GRAMENE rice database example

load(system.file("extdata", "gramene.chr.RDATA", package = "QTLEMM"))
load(system.file("extdata", "gramene.QTL.RDATA", package = "QTLEMM"))
head(gramene.chr)
#   CHR Center.cM. Length.cM.
# 1   1      74.2         184
# 2   2      55.5         161
# 3   3      84.3         166
# 4   4      19.7         133
# 5   5      51.8         121
# 6   6      66.8         127
head(gramene.QTL) # 9 trait categories in the second column
#   X       Trait chr     L     R
# 1 1 Biochemical   1  54.1  54.1
# 2 2       Vigor   1 147.4 147.4
# 3 3       Vigor   1 147.4 147.4
# 4 4       Vigor   1 147.4 147.4
# 5 5       Vigor   1 147.4 158.6
# 6 6       Vigor   1  54.1  54.1

library(QTLEMM)
Qhot.result <- Qhot(gramene.QTL, gramene.chr, save.pdf = TRUE)
names(Qhot.result)
# [1] "EQF" "P.threshold" "Q.threshold" "nHot"

load(system.file("extdata", "gramene.trait.RDATA", package = "QTLEMM"))
head(gramene.trait) # 236 traits in the second column
#   X                 Trait chr     L     R
# 1 1 leaf nitrogen content   1  54.1  54.1
# 2 2       root dry weight   1 147.4 147.4
# 3 3       root dry weight   1 147.4 147.4
# 4 4         tiller number   1 147.4 147.4
# 5 5         tiller number   1 147.4 158.6
# 6 6           root number   1  54.1  54.1

set.seed(8000)
Qhot.EQF.result <- Qhot.EQF(gramene.trait, gramene.chr[,3], bin.size = 0.5, permu = TRUE, ptime = 1000, alpha = 0.05, Q = TRUE, console = FALSE)
names(Qhot.EQF.result)
# [1] "EQF.matrix" "bin" "bin.size" "EQF.trait" "EQF.detect" "EQF.nondetect"
# [7] "cluster.matrix" "permu.matrix.cluster" "permu.matrix.Q" "EQF.threshold"
EQF.plot(Qhot.EQF.result, plot.all = TRUE, plot.chr = FALSE)

