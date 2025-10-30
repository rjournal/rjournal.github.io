##################################
## Replication file for 
## Qu and Yoon (2025)

## Package
library(QTE.RD)			# QTE.RD - v1.2.0

## Data and variables
data("ddk_2011")
set.seed(11445)

# Define variables
trk <- ddk_2011$tracking		# schools sampled for tracking
con <- ddk_2011$etpteacher		# students assigned to a contract teacher
hgh <- ddk_2011$highstream		# students assigned to high-ability class (in tracking schools)
yy  <- ddk_2011$ts_std			# endline total score
xx  <- ddk_2011$percentile		# percentile rank from the initial test

##################
## 1. RD design

yc <- yy[trk==1]
xc <- xx[trk==1]
dc <- hgh[trk==1]	# high-ability section (if tracking schools)
x0 <- 50
tlevel <- 1:9/10
hh <- 20

# 1.1. RDD without covariate
# unconditional effects

A <- rd.qte(y=yc,x=xc,d=dc,x0,z0=NULL,tau=tlevel,bdw=hh,bias=1)
A2 <- summary(A,alpha=0.1)
A2

# QTE plot
y.text <- "test scores"
m.text <- "Effects of assignment to lower vs. upper sections"
plot(A2,ytext=y.text,mtext=m.text)

# conditional quantiles plot
y.text <- "test scores"
m.text <- "Conditional quantile functions"
sub.text <- c("Upper section","Lower section")
plot(A2,ptype=2,ytext=y.text,mtext=m.text,subtext=sub.text)

# hypothesis tests
B <- rdq.test(y=yc,x=xc,d=dc,x0,z0=NULL,tau=tlevel,bdw=hh,bias=1,alpha=c(0.1,0.05),type=c(1,2,3,4))
B

# bandwidth selection
C <- rdq.bandwidth(y=yc,x=xc,d=dc,x0,z0=NULL,cv=1,val=(5:20))
C

# 1.2. RDD, boys vs. girls
# group-specific effects

zc <- ddk_2011$girl[trk==1]
z.eval <- c(0,1)
A <- rd.qte(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,tau=tlevel,bdw=hh,bias=1)
A2 <- summary(A,alpha=0.1)
A2

# QTE plot
y.text <- "test scores"
m.text <- c("Boys","Girls")
plot(A2,ytext=y.text,mtext=m.text)

# Conditional quantile plots
y.text <- "test scores"
m.text <- c("Boys","Girls")
sub.text <- c("Upper section","Lower section")
plot(A2,ptype=2,ytext=y.text,mtext=m.text,subtext=sub.text)

# tests
B <- rdq.test(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,tau=tlevel,bdw=hh,bias=1,alpha=c(0.1,0.05),type=c(1,2,3,4))
B

# bandwidth selection
C <- rdq.bandwidth(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,cv=1,val=(5:20))
C

# uniform bands with and without bias correction
D <- rdq.band(y=yc,x=cbind(xc,zc),d=dc,x0,z0=z.eval,tau=tlevel,bdw=hh,alpha=0.1)
D

##################
## 2. RCT design

# 2.1. Without covariate
x0 <- 50
dr <- trk

A <- rd.qte(y=yy,x=xx,d=dr,x0,z0=NULL,tau=tlevel,bdw=hh,bias=1)
A2 <- summary(A,alpha=0.1)
A2

# QTE plot
y.text = "test scores"
m.text = "Effects of tracking"
plot(A2,ytext=y.text,mtext=m.text)

# conditional quantiles plot
m.text <- "Conditional quantile functions"
sub.text <- c("Tracking schools","Non-tracking schools")
plot(A2,ptype=2,ytext=y.text,mtext=m.text,subtext=sub.text)

# try alternative evaluation points
A <- rd.qte(y=yy,x=xx,d=dr,x0=20,z0=NULL,tau=tlevel,bdw=hh,bias=1)
A

A <- rd.qte(y=yy,x=xx,d=dr,x0=80,z0=NULL,tau=tlevel,bdw=hh,bias=1)
A

# 2.2. Heterogeneous effects: student's gender & teacher's type

zw <- cbind(ddk_2011$girl,con)
z.eval <- cbind(rep(c(0,1),2),rep(c(0,1),each=2))
A <- rd.qte(y=yy,x=cbind(xx,zw),d=dr,x0=50,z0=z.eval,tau=tlevel,bdw=hh,bias=1)
A2 <- summary(A,alpha=0.1)

# QTE plot
m.text <- c("boys & regular teachers","girls & regular teachers","boys & contract teachers","girls & contract teachers")
plot(A2,ytext=y.text,mtext=m.text)

# 2.3. Heterogeneous effects: student's age

zw <- ddk_2011$agetest
z.eval <- c(7,9,10,11)
A <- rd.qte(y=yy,x=cbind(xx,zw),d=dr,x0=50,z0=z.eval,tau=tlevel,bdw=hh,bias=1)
A2 <- summary(A,alpha=0.1)

# QTE plot
y.text <- "test scores"
m.text <- c("age 7","age 9","age 10","age 12")
plot(A2,ytext=y.text,mtext=m.text)

# tests
B <- rdq.test(y=yy,x=cbind(xx,zw),d=dr,x0=50,z0=z.eval,tau=tlevel,bdw=hh,bias=1,alpha=c(0.1,0.05),type=c(1,2,3,4))
B

