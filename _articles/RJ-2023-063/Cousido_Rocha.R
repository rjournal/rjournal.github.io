### Package TwoSampleTest.HD in practice

### Hedenfalk data


library(Equalden.HD)
data("Hedenfalk")

X = log ( Hedenfalk[ , 1:7 ])
Y = log ( Hedenfalk[ , 8:15 ])

X = X[1:1000,]
Y = Y[1:1000,]
library ( TwoSampleTest.HD)
res1 <- TwoSampleTest.HD (X , Y , method = "spect" )
res2 <- TwoSampleTest.HD(X, Y, method = "boot")
res3 <- TwoSampleTest.HD(X, Y, method = "us")
res4 <- TwoSampleTest.HD(X, Y, method = "perm")


res1$statistic
res1$variance
res1$p
res1$n
res1$m
res1$method

library(ggplot2)

data=data.frame(Jk=res1$I.statistics,Genes=1:res1$p)
ggplot(data, aes(x=Genes, y=Jk)) +
  geom_point(shape=21, col=8) + geom_rug()+ggtitle("Individual test statistics")

pv=res4$I.permutation.p.values

order(pv)[1:10]
alpha=0.05
sum(p.adjust(pv,method = "BH")<=alpha)
which(p.adjust(pv,method = "BH")<=alpha)


p=res1$p;n=res1$n; m=res1$m
pv_t.test=1:p
pv_KS=1:p
pv_Wilcoxon=1:p
pv_Levene=1:p

library(car)
library(exactRankTests)
library(coin)

for (i in 1:p){
  pv_Wilcoxon[i]=wilcox.exact(X[i,],Y[i,])$p.value
  pv_t.test[i]=t.test(X[i,],Y[i,],var.equal = F)$p.value
  pv_KS[i]=ks.test(X[i,],Y[i,])$p.value
  pv_Levene[i]=leveneTest(c(X[i,],Y[i,]),as.factor(c(rep(1,n),rep(2,m))))$`Pr(>F)`[1]
}

sum(p.adjust(pv_Wilcoxon,method = "BH")<=alpha)
sum(p.adjust(pv_t.test,method = "BH")<=alpha)
sum(p.adjust(pv_KS,method = "BH")<=alpha)
sum(p.adjust(pv_Levene,method = "BH")<=alpha)


alpha_vec=seq(0.001, 0.10, by=0.001);len=length(alpha_vec)
R_Ji=1:len
R_t.test=1:len
R_KS=1:len
R_Wilcoxon=1:len
R_Levene=1:len

for (i in 1:len){
  R_Ji[i]=sum(p.adjust(pv,method = "BH")<=alpha_vec[i])
  R_Wilcoxon[i]=sum(p.adjust(pv_Wilcoxon,method = "BH")<=alpha_vec[i])
  R_t.test[i]=sum(p.adjust(pv_t.test,method = "BH")<=alpha_vec[i])
  R_KS[i]=sum(p.adjust(pv_KS,method = "BH")<=alpha_vec[i])
  R_Levene[i]=sum(p.adjust(pv_Levene,method = "BH")<=alpha_vec[i])
}

data=data.frame(Rejections=c(R_Ji,R_Wilcoxon,R_t.test,R_KS, R_Levene),
                Test=as.factor(c(rep("Jk", len),rep("Wilcoxon",len),rep("t.test",len),
                                 rep("KS", len), rep("Levene",len))), Alpha=rep(alpha_vec,5))


library(ggplot2)

ggplot(data, aes(x=Alpha, y=Rejections, colour=Test)) +
  geom_line() +ggtitle("")+xlab("FDR level")

### Simulated data


library(TwoSampleTest.HD)
n=m=4
p=1000

set.seed(123)

p <- 1000
n = m = 4
inds <- sample(1:4, p, replace = TRUE)
X <- matrix(rep(0, n * p), ncol = n)
for (j in 1:p){
  if (inds[j] == 1){
    X[j, ] <- rnorm(n)
  }
  if (inds[j] == 2){
    X[j, ] <- rnorm(n, sd = 2)
  }
  if (inds[j] == 3){
    X[j, ] <- rnorm(n, mean = 1)
  }
  if (inds[j] == 4){
    X[j, ] <- rnorm(n, mean = 1, sd = 2)
  }
}
rho <-  0.1
ind <- sample(1:p, rho * p)
li <- length(ind)
indsy <- inds
for (l in 1:li){
  if (indsy[ind[l]]==1){
    indsy[ind[l]]=3
  } else {
    if (indsy[ind[l]]==2){
      indsy[ind[l]]=4
    } else {
      if (indsy[ind[l]]==3){
        indsy[ind[l]]=1
      } else {
        indsy[ind[l]] = 2
      }
    }
  }
}
Y <- matrix(rep(0, m * p), ncol = m)
for (j in 1:p){
  if (indsy[j] == 1){
    Y[j,] <- rnorm(m)}
  if (indsy[j] == 2){
    Y[j, ] <- rnorm(m, sd = 2)
  }
  if (indsy[j]==3){
    Y[j, ] <- rnorm(m, mean = 1)
  }
  if (indsy[j] == 4){
    Y[j,] <- rnorm(m, mean = 1, sd = 2)
  }
}

TwoSampleTest.HD(X, Y, method = "spect")$p.value
TwoSampleTest.HD(X, Y, method = "spect_ind")$p.value
TwoSampleTest.HD(X, Y, method = "boot")$p.value
TwoSampleTest.HD(X, Y, method = "us")$p.value
TwoSampleTest.HD(X, Y, method = "us_ind")$p.value
res=TwoSampleTest.HD(X, Y, method = "perm")


pvalues=res$I.permutation.p.values

alpha=0.05

sum(p.adjust(pvalues,method = "BH")<=alpha)




pv_t.test=1:p
pv_KS=1:p
pv_Wilcoxon=1:p
pv_Levene=1:p

library(car)
library(exactRankTests)
library(coin)

for (i in 1:p){
  pv_Wilcoxon[i]=wilcox.exact(X[i,],Y[i,])$p.value
  pv_t.test[i]=t.test(X[i,],Y[i,],var.equal = F)$p.value
  pv_KS[i]=ks.test(X[i,],Y[i,])$p.value
  pv_Levene[i]=leveneTest(c(X[i,],Y[i,]),as.factor(c(rep(1,n),rep(2,m))))$`Pr(>F)`[1]
}

sum(p.adjust(pv_Wilcoxon,method = "BH")<=alpha)
sum(p.adjust(pv_t.test,method = "BH")<=alpha)
sum(p.adjust(pv_KS,method = "BH")<=alpha)
sum(p.adjust(pv_Levene,method = "BH")<=alpha)