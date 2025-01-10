#WORKING CODE R-JOURNAL

install.packages(c("dplyr",
                   "ggplot2",
                   "Rmisc",
                   "reshape2",
                   "bootCT",
                   "tseries",
                   "urca",
                   "aTSA"))
library(dplyr)
library(ggplot2)
library(Rmisc)
library(aTSA)
library(reshape2)
library(bootCT)
library(tseries)
library(urca)

# Multiple plot function

multiplot  =  function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots  =  c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout  =  matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx  =  as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###########################################
# SIMULATE DATA FOR FIGURE 1 AND FIGURE 2 #
###########################################
corrm = matrix(c(   0,     0, 0,
                 0.25,     0, 0,
                  0.4, -0.25, 0), nrow = 3, ncol = 3, byrow = T)

Corrm = (corrm + t(corrm)) + diag(3)

sds = diag(c(1.3, 1.2, 1))

sigma.in = (sds %*% Corrm %*% t(sds))

gamma1 = matrix(c(0.6,   0,0.2,
                  0.1,-0.3,  0,
                    0,-0.3,0.2), nrow = 3, ncol = 3,byrow=T)
gamma2= gamma1*0.3

omegat = sigma.in[1,-1]%*%solve(sigma.in[-1,-1])
axx.in = matrix(c( 0.3, 0.5,
                  -0.4, 0.3), nrow = 2, ncol = 2, byrow = T)
ayx.uc.in = c(0.4,0.4)
ayy.in = 0.6

data.vecm.ardl_1 =
  sim_vecm_ardl(nobs=200,
                case = 1,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = ayy.in,
                mu.in = rep(0,3),
                eta.in = rep(0,3),
                azero.in = rep(0,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_2 =
  sim_vecm_ardl(nobs=200,
                case = 2,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = ayy.in,
                mu.in = rep(2,3),
                eta.in = rep(0,3),
                azero.in = rep(0,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_3 =
  sim_vecm_ardl(nobs=200,
                case = 3,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = ayy.in,
                mu.in = rep(0,3),
                eta.in = rep(0,3),
                azero.in = rep(2,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_4 =
  sim_vecm_ardl(nobs=200,
                case = 4,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = ayy.in,
                mu.in = rep(0,3),
                eta.in = rep(0.4,3),
                azero.in = rep(2,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_5 =
  sim_vecm_ardl(nobs=200,
                case = 5,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = ayy.in,
                mu.in = rep(0,3),
                eta.in = rep(0,3),
                azero.in = rep(0.8,3),
                aone.in = rep(0.4,3),
                burn.in = 100,
                seed.in = 999)

df1 = data.vecm.ardl_1$data
meltdf1  =  melt(df1,id="time")
p1 = ggplot(meltdf1,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE I") + theme_bw()+
  ylim(c(-10,12))

df2 = data.vecm.ardl_2$data
meltdf2  =  melt(df2,id="time")
p2 = ggplot(meltdf2,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE II") + theme_bw()+
  ylim(c(-10,12))

df3 = data.vecm.ardl_3$data
meltdf3  =  melt(df3,id="time")
p3 = ggplot(meltdf3,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE III") + theme_bw()+
  ylim(c(-10,12))

df4 = data.vecm.ardl_4$data
meltdf4  =  melt(df4,id="time")
p4 = ggplot(meltdf4,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE IV") + theme_bw()+
  ylim(c(-15,100))

df5 = data.vecm.ardl_5$data
meltdf5  =  melt(df5,id="time")
p5 = ggplot(meltdf5,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE V") + theme_bw()+
  ylim(c(-100,150))

multiplot(p1,p2,p3,p4,p5, cols=1)

# Degeneracy of second type
data.vecm.ardl_1 =
  sim_vecm_ardl(nobs=200,
                case = 1,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = 0,
                mu.in = rep(0,3),
                eta.in = rep(0,3),
                azero.in = rep(0,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_2 =
  sim_vecm_ardl(nobs=200,
                case = 2,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = 0,
                mu.in = rep(0.3,3),
                eta.in = rep(0,3),
                azero.in = rep(0,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_3 =
  sim_vecm_ardl(nobs=200,
                case = 3,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = 0,
                mu.in = rep(0,3),
                eta.in = rep(0,3),
                azero.in = rep(0.3,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_4 =
  sim_vecm_ardl(nobs=200,
                case = 4,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = 0,
                mu.in = rep(0,3),
                eta.in = rep(0.4,3),
                azero.in = rep(0.3,3),
                aone.in = rep(0,3),
                burn.in = 100,
                seed.in = 999)

data.vecm.ardl_5 =
  sim_vecm_ardl(nobs=200,
                case = 5,
                sigma.in = sigma.in,
                gamma.in = list(gamma1,gamma2),
                axx.in = axx.in,
                ayx.uc.in = ayx.uc.in,
                ayy.in = 0,
                mu.in = rep(0,3),
                eta.in = rep(0,3),
                azero.in = rep(0.3,3),
                aone.in = rep(0.3,3),
                burn.in = 100,
                seed.in = 999)

df1 = data.vecm.ardl_1$data
meltdf1  =  melt(df1,id="time")
p1 = ggplot(meltdf1,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE I") + theme_bw()

df2 = data.vecm.ardl_2$data
meltdf2  =  melt(df2,id="time")
p2 = ggplot(meltdf2,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE II") + theme_bw()

df3 = data.vecm.ardl_3$data
meltdf3  =  melt(df3,id="time")
p3 = ggplot(meltdf3,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE III") + theme_bw()

df4 = data.vecm.ardl_4$data
meltdf4  =  melt(df4,id="time")
p4 = ggplot(meltdf4,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE IV") + theme_bw()

df5 = data.vecm.ardl_5$data
meltdf5  =  melt(df5,id="time")
p5 = ggplot(meltdf5,aes(x=time,y=value,colour=variable,group=variable)) + geom_line() + ggtitle("CASE V") + theme_bw()

multiplot(p1,p2,p3,p4,p5, cols=1)

########################
# TIME ELAPSED TABLE 1 #
########################
# DO NOT RUN, IT TAKES A LOT OF TIME
# DATA GENERATION
corrm = matrix(c(0, 0, 0,
                 0.25, 0, 0,
                 0.4, -0.25, 0),
               nrow = 3, ncol = 3, byrow = T)

Corrm = (corrm + t(corrm)) + diag(3)

sds = diag(c(1.3, 1.2, 1))

sigma.in = (sds %*% Corrm %*% t(sds))

gamma1 = matrix(c(0.3,0.2,0.2,
                  0.1,-0.2,0.1,
                  0.3,-0.1,0),
                nrow = 3, ncol = 3,byrow = T)
gamma2= gamma1*0.3
gammax=list(gamma1,gamma2)

omegat = sigma.in[1,-1]%*%solve(sigma.in[-1,-1])
axx.in = matrix(c(0.3, 0.5, -0.4, 0.3), nrow = 2, ncol = 2, byrow = T)
ayxC.in = omegat%*%axx.in

data_sim=alist(n50=,n80=,n100=,n200=,n500=)
num=c(50,80,100,200,500)
set.seed(100)
for(j in 1:5){
  data_sim[[j]]=list()
  for(k in 1:100){
    data_sim[[j]][[k]] =
      sim_vecm_ardl(nobs = num[j],
                    case = 2,
                    sigma.in = sigma.in,
                    gamma.in = gammax,
                    axx.in = axx.in,
                    ayx.uc.in = c(0.6,0.5),
                    ayy.in = 0.7,
                    mu.in = rep(0.3,3),
                    eta.in = rep(0,3),
                    azero.in = rep(0,3),
                    aone.in = rep(0,3),
                    burn.in=num[j]/2)
    print(j)
  }
}

# APPLYING BOOTSTRAP AND RECORDING TIME
res_sim=list()
timerec=list()
bootr=c(200,500,1000,2000,5000)
set.seed(1)

for(m in 1:length(bootr)){
  res_sim[[m]]=list()
  timerec[[m]]=list()
  
  for(j in 1:length(num)){
    res_sim[[m]][[j]]=list()
    begin=Sys.time()
    
    for(k in 1:100){
      res_sim[[m]][[j]][[k]] = boot_ardl(data = data_sim[[j]][[k]]$data[,-4],
                                         nboot = bootr[m],
                                         case = 2,
                                         fix.ardl = rep(2,3),
                                         fix.vecm = 2,
                                         print = T)
      print(k)
    }
    
    end=Sys.time()
    print(j)
    print(m)
    timerec[[m]][[j]]=end-begin
    #save.image(file="timerec_res.RData",compress=T)
  }
  
}


#######################################
# APPLICATION ON GERMAN MACRO DATASET #
#######################################

# LOAD DATA
data("ger_macro")

# DATA PREPARATION
colnames(ger_macro)
LNDATA=apply(ger_macro[,-1],2,log)
col_ln=paste0("LN",colnames(ger_macro)[-1])
LNDATA=as.data.frame(LNDATA)
colnames(LNDATA)=col_ln
LNDATA=LNDATA%>%select(LNCONS,LNINCOME,LNINVEST)
LNDATA$DATE=ger_macro$DATE

# ADF TEST IN LEVELS Table 2
aTSA::adf.test(LNDATA$LNINVEST)
aTSA::adf.test(LNDATA$LNINCOME)
aTSA::adf.test(LNDATA$LNCONS)

# CREATE DIFFERENCE
lagdf1 = lag_mts(as.matrix(LNDATA[,-4]), k = c(1,1,1))
dlagdf0 = na.omit(LNDATA[,-4] - lagdf1)
colnames(dlagdf0)=paste0("D_",colnames(LNDATA)[-4])
dlagdf0$DATE=ger_macro$DATE[-1]

# ADF TEST IN DIFFERENCE Table 2
aTSA::adf.test(na.omit(dlagdf0$D_LNCONS))
aTSA::adf.test(na.omit(dlagdf0$D_LNINVEST))
aTSA::adf.test(na.omit(dlagdf0$D_LNINCOME))

# PLOT FOR FIGURE 5
dfmelt = melt(LNDATA, id = "DATE")
dfmelt = dfmelt%>%arrange(variable,DATE)

p1 = ggplot(dfmelt, 
            aes(x = DATE, y = value, colour = variable, group = variable)) +
  geom_line() + ggtitle("Level Variables (log-scale)") + theme_bw()

diff.dfmelt = melt(dlagdf0, id = "DATE")
diff.dfmelt = diff.dfmelt%>%arrange(variable,DATE)

p2 = ggplot(diff.dfmelt,
            aes(x = DATE, y = value, colour = variable, group = variable)) +
  geom_line() + ggtitle("Diff. Variables (log-scale)") + theme_bw()

plot(p1)
plot(p2)

# ARDL BOOT 
set.seed(999)

# Time elapsed
time0 = Sys.time()

# MODEL I
BCT_res_CONS = boot_ardl(data = LNDATA,
                         yvar = "LNCONS",
                         xvar = c("LNINCOME","LNINVEST"),
                         maxlag = 5,
                         a.ardl = 0.1,
                         a.vecm = 0.1,
                         nboot = 2000,
                         case = 3,
                         a.boot.H0 = c(0.05),
                         print = T)

# MODEL II
BCT_res_INC = boot_ardl(data = LNDATA,
                        yvar = "LNINCOME",
                        xvar = c("LNCONS","LNINVEST"),
                        maxlag = 5,
                        a.ardl = 0.1,
                        a.vecm = 0.1,
                        nboot = 2000,
                        case = 3,
                        a.boot.H0 = c(0.05),
                        print = T)

# MODEL III
BCT_res_INV = boot_ardl(data = LNDATA,
                        yvar = "LNINVEST",
                        xvar = c("LNCONS","LNINCOME"),
                        maxlag = 5,
                        a.ardl = 0.1,
                        a.vecm = 0.1,
                        nboot = 2000,
                        case = 3,
                        a.boot.H0 = c(0.05),
                        print = T)

time1=Sys.time()
runtime = time1-time0

# SUMMARY WITH OPTIONS
summary(BCT_res_CONS,out = "ARDL") # Table 3 ARDL, extract lags column for Table 4
summary(BCT_res_INC,out = "ARDL") # Table 3 ARDL, extract lags column for Table 4
summary(BCT_res_INV,out = "ARDL") # Table 3 ARDL, extract lags column for Table 4
summary(BCT_res_CONS,out = "VECM") # Table 3 VECM
summary(BCT_res_INC,out = "VECM") # Table 3 VECM
summary(BCT_res_INV,out = "VECM") # Table 3 VECM
summary(BCT_res_CONS,out = "cointVECM") # Table 3 VECM bottom row
summary(BCT_res_INC,out = "cointVECM") # Table 3 VECM bottom row
summary(BCT_res_INV,out = "cointVECM") # Table 3 VECM bottom row
summary(BCT_res_CONS,out = "cointARDL") # Table 4
summary(BCT_res_INC,out = "cointARDL") # Table 4
summary(BCT_res_INV,out = "cointARDL") # Table 4

########################################
# APPLICATION ON ITALIAN MACRO DATASET #
########################################

# LOAD DATA
data("ita_macro")

# ADF TEST IN LEVELS
aTSA::adf.test(ita_macro$LGDP)
aTSA::adf.test(ita_macro$LEXP)
aTSA::adf.test(ita_macro$LFI)

# CREATE DIFFERENCE
lagdf1 = lag_mts(as.matrix(ita_macro[,-1]), k = c(1,1,1))
dlagdf0 = na.omit(ita_macro[,-1] - lagdf1)
colnames(dlagdf0) = paste0("D_", colnames(ita_macro)[-1])
dlagdf0$YEAR = ita_macro$YEAR[-1]

# ADF TEST IN DIFFERENCE
aTSA::adf.test(na.omit(dlagdf0$D_LGDP))
aTSA::adf.test(na.omit(dlagdf0$D_LEXP))
aTSA::adf.test(na.omit(dlagdf0$D_LFI))

# PLOT FOR FIGURE 6
dfmelt = melt(ita_macro, id = "YEAR")
dfmelt = dfmelt%>%arrange(variable,YEAR)

p1 = ggplot(dfmelt,
            aes(x = YEAR, y = value, colour = variable, group = variable)) +
  geom_line() + ggtitle("Level Variables (log-scale)") + theme_bw()

diff.dfmelt = melt(dlagdf0, id = "YEAR")
diff.dfmelt = diff.dfmelt%>%arrange(variable,YEAR)

p2 = ggplot(diff.dfmelt,
            aes(x = YEAR, y = value, colour = variable, group = variable)) +
  geom_line() + ggtitle("Diff. Variables (log-scale)") + theme_bw()

plot(p1)
plot(p2)

# ARDL BOOT
set.seed(999)

# MODEL I
BCT_res_GDP = boot_ardl(data = ita_macro,
                        yvar = "LGDP",
                        xvar = c("LEXP","LFI"),
                        maxlag = 5,
                        a.ardl = 0.1,
                        a.vecm = 0.1,
                        nboot = 2000,
                        case = 3,
                        a.boot.H0 = c(0.05),
                        print = T)

# MODEL II
BCT_res_EXP = boot_ardl(data = ita_macro,
                        yvar = "LEXP",
                        xvar = c("LGDP","LFI"),
                        maxlag = 5,
                        a.ardl = 0.1,
                        a.vecm = 0.1,
                        nboot = 2000,
                        case = 3,
                        a.boot.H0 = c(0.05),
                        print = T)

# MODEL III
BCT_res_FDI = boot_ardl(data = ita_macro,
                        yvar = "LFI",
                        xvar = c("LGDP","LEXP"),
                        maxlag = 5,
                        a.ardl = 0.1,
                        a.vecm = 0.1,
                        nboot = 2000,
                        case = 3,
                        a.boot.H0 = c(0.05),
                        print = T)

# SUMMARY WITH OPTIONS TABLE 6
summary(BCT_res_GDP,out="ARDL") # extract lags
summary(BCT_res_EXP,out="ARDL") # extract lags
summary(BCT_res_FDI,out="ARDL") # extract lags
summary(BCT_res_GDP,out="cointARDL") # ARDL cointegration
summary(BCT_res_EXP,out="cointARDL") # ARDL cointegration
summary(BCT_res_FDI,out="cointARDL") # ARDL cointegration