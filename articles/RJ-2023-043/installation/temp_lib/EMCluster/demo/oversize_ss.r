library(EMCluster, quiet = TRUE)
set.seed(1234)

x <- da1$da
TC <- da1$class
lab <- da1$class
k <- 12

for(i in 1:10){
  id <- which(lab == i)
  lab[sample(id, length(id) / 2)] <- 0
}

ret.em <- init.EM(x, nclass = k, lab = lab, method = "em.EM")
ret.Rnd <- init.EM(x, nclass = k, lab = lab, method = "Rnd.EM",
                   EMC = .EMC.Rnd)
ret.Rndp <- init.EM(x, nclass = k, lab = lab, method = "Rnd.EM",
                    EMC = .EMC.Rndp)

par(mfrow = c(2, 2))
plotem(ret.em, x, main = "em")
plotem(ret.Rnd, x, main = "Rnd")
plotem(ret.Rndp, x, main = "Rnd+")


ret.all <-
cbind(
  c(ret.em$llhdval, ret.Rnd$llhdval, ret.Rndp$llhdval),
  c(RRand(ret.em$class, TC, lab = lab)$adjRand,
    RRand(ret.Rnd$class, TC, lab = lab)$adjRand,
    RRand(ret.Rndp$class, TC, lab = lab)$adjRand)
)
rownames(ret.all) <- c("em", "Rnd", "Rnd+")
colnames(ret.all) <- c("logL", "adjR")
ret.all
