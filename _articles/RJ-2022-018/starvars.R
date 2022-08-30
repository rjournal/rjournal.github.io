####Installing the starvars package and setting the seed####

install.packages('starvars')
library(starvars)
library(ggplot2)
library(zoo)
library(reshape2)

set.seed(261088)

####Loading data####

data("techprices", package = "starvars")
techprices <- techprices

####Deriving realized covariances from stock prices####
RCOV <- rcov(techprices, freq = "monthly", make.ret = TRUE, cholesky = TRUE)

techchol <- RCOV$'Cholesky Factors' ##Cholesky factors of realized covariance matrices

####Plotting realized covariances in Figure 1####
rcovplot <- as.data.frame(RCOV$`Realized Covariances`)
colnames(rcovplot) <- c('GOOG', 'GOOG-MSFT', 'GOOG-AMZN', 'MSFT', 'MSFT-AMZN', 'AMZN')
rcovplot$date <- index(RCOV$`Realized Covariances`)
rcovplot <- melt(rcovplot, id = 'date')
colnames(rcovplot) <- c('Date', 'Covariance', 'RCOV')
fig1 <- ggplot(rcovplot) +
  geom_line(aes(x = Date, y = RCOV, colour = Covariance), size = 1) +
  facet_grid(Covariance~.)+theme_bw()

fig1 + theme(legend.position = "none")

####Testing for nonlinearity in the multivariate time series####
st <- lag(techchol,1)[-1,]
VLSTARjoint(techchol[-1,], st = st, st.choice = TRUE)

####Common breaks detection####
multiCUMSUM(techchol[-1], max.breaks = 3)

##Searching starting values for c and gamma
stvalues <- startingVLSTAR(techchol[-1,], p = 1, m = 2, st = st[,5], 
                           n.combi = 20, singlecgamma = FALSE, ncores = 4)

####Estimating a VLSTAR model####
fit.VLSTAR <- VLSTAR(techchol[-1,], p = 1, m = 2, st = st[,5], starting = stvalues,
                     method = 'NLS', n.iter = 30, ncores = 4)

summary(fit.VLSTAR)
plot(fit.VLSTAR, names = "y1")


####Make predictions two step ahead####
pred.bootstrap <- predict(fit.VLSTAR, n.ahead = 2, st.num = 3, method = 'bootstrap')
pred.bootstrap
plot(pred.bootstrap, type = 'single', names = 'y1')


sessionInfo()
