###################################
##### install the package #########
###################################

install.packages('BMRMM')
library(BMRMM)

##################################
######### foxp2 data set #########
##################################

p.str <- "################"

print(paste(p.str, 'BMRMM run for foxp2', p.str))
res.fp2 <- BMRMM(data = foxp2, num.cov = 2, state.labels = c('d','m','s','u'), 
                 cov.labels = list(c("F", "W"), c("U", "L", "A")),
                 duration.distr = list('mixgamma', shape = rep(1, 4), rate = rep(1, 4)))

# Summary of foxp2 results
sm.fp2 <- summary(res.fp2)

# Figure 2
print(paste(p.str, 'Figure 2', p.str))
sm.fp2$trans.global
sm.fp2$dur.global
plot(sm.fp2, 'trans.global')
plot(sm.fp2, 'dur.global')

# Figure 3
print(paste(p.str, 'Figure 3', p.str))
plot(sm.fp2, 'trans.probs.mean')
plot(sm.fp2, 'trans.probs.sd')

# Figure 4
print(paste(p.str, 'Figure 4', p.str))
plot(sm.fp2, 'trans.local.mean.diff')
plot(sm.fp2, 'trans.local.null.test')

# Figure 5
print(paste(p.str, 'Figure 5', p.str))
main.hist.fp2 <- hist(res.fp2, xlim = c(0,1))
comp.hist.fp2 <- vector("list", 4)
for(comp in 1:4) {
  comp.hist.fp2[[comp]] <- hist(res.fp2, comp = comp)
}

# Printed results on Pages 10 & 11
print(paste(p.str, 'Printed results on Pages 10 & 11', p.str))
sm.fp2$dur.mix.params
sm.fp2$dur.mix.probs

# Figure 6
print(paste(p.str, 'Figure 6', p.str))
diag.BMRMM(res.fp2, cov.combs = list(c(1,1)), transitions = list(c(4,2)), components = c(2))

###################################
######### asthma data set #########
###################################

# please download the data set first

asthma <- read.csv('asthma.csv')

print(paste(p.str, 'BMRMM run for asthma', p.str))
res.asm <- BMRMM(data = asthma, num.cov = 3, state.labels = c(1, 2, 3), 
                 cov.labels = list(c('Mild-Moderate','Severe'), 
                                   c('BMI<25','BMI>=25'), 
                                   c('Women','Men')),
                 duration.distr = list('mixgamma', shape = rep(1, 4), rate = rep(1, 4)))

# Summary of asthma results
sm.asm <- summary(res.asm, digits = 3)

# Figure 7
print(paste(p.str, 'Figure 7', p.str))
sm.asm$trans.global
sm.asm$dur.global
plot(sm.asm, 'trans.global')
plot(sm.asm, 'dur.global')

# Figure 8
print(paste(p.str, 'Figure 8', p.str))
plot(sm.asm, 'trans.probs.mean')
plot(sm.asm, 'trans.probs.sd')

# Figure 9
print(paste(p.str, 'Figure 9', p.str))
plot(sm.asm, 'trans.local.mean.diff')
plot(sm.asm, 'trans.local.null.test')

# Figure 10
print(paste(p.str, 'Figure 10', p.str))
asm.main.hist <- hist(res.asm)
asm.comp.hist <- vector("list", 4)
for(comp in 1:4) {
  asm.comp.hist[[comp]] <- hist(res.asm, comp = comp)
}

# Printed results on Pages 15 & 16
print(paste(p.str, 'Printed results on Pages 15 & 16', p.str))
sm.asm$dur.mix.params
sm.asm$dur.mix.probs

# Figure 11
print(paste(p.str, 'Figure 11', p.str))
diag.BMRMM(res.asm, cov.combs = list(c(1, 2, 1)), 
           transitions = list(c(1, 1)), components = c(3))
