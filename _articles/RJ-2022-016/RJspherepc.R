#
# This .R file includes all codes for generating all figures in the paper.  
#
# Generating Figure 1
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
#### example 1: half-great circle data
circle <- GenerateCircle(c(150, 60), radius = pi/2, T = 1000)
sigma <- 2                             # noise level
half.circle <- circle[circle[, 1] < 0, , drop = FALSE]
half.circle <- half.circle + sigma * rnorm(nrow(half.circle))
PGA(half.circle)
  
#### example 2: S-shaped data
# It consists of two parts: lon ~ Uniform[0, 20], lat = sqrt(20 * lon - lon^2) + N(0, sigma^2), 
# lon ~ Uniform[-20, 0], lat = -sqrt(-20 * lon - lon^2) + N(0, sigma^2).
n <- 500              
sigma <- 1                             # noise level
lon <- 60 * runif(n)  
lat <- (60 * lon - lon^2)^(1/2) + sigma * rnorm(n)
simul.S1 <- cbind(lon, lat)
lon2 <- -60 * runif(n)
lat2 <- -(-60 * lon2 - lon2^2)^(1/2) + sigma * rnorm(n)
simul.S2 <- cbind(lon2, lat2)
simul.S <- rbind(simul.S1, simul.S2)
PGA(simul.S)

#
# Generating Figure 2
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
#### example 1: half-great circle data
circle <- GenerateCircle(c(150, 60), radius = pi/2, T = 1000)
half.great.circle <- circle[circle[, 1] < 0, , drop = FALSE]
sigma <- 2                           # noise level
half.great.circle <- half.great.circle + sigma * rnorm(nrow(half.great.circle))
## find a principal circle
PC <- PrincipalCircle(half.great.circle)
result <- GenerateCircle(PC[1:2], PC[3], T = 1000)
## plot
sphereplot::rgl.sphgrid(col.lat = "black", col.long = "black")
sphereplot::rgl.sphpoints(half.great.circle, radius = 1, col = "blue", size = 9)
sphereplot::rgl.sphpoints(result, radius = 1, col = "red", size = 6)

#### example 2: circular data
n <- 700                             # the number of samples
sigma <- 5                           # noise level 
x <- seq(-180, 180, length.out = n)
y <- 45 + sigma * rnorm(n)
simul.circle <- cbind(x, y)
## find a principal circle
PC <- PrincipalCircle(simul.circle)
result <- GenerateCircle(PC[1:2], PC[3], T = 1000)
## plot the circular data
sphereplot::rgl.sphgrid(col.lat = "black", col.long = "black")
sphereplot::rgl.sphpoints(simul.circle, radius = 1, col = "blue", size = 9)
sphereplot::rgl.sphpoints(result, radius = 1, col = "red", size = 6)

#
# Generating Figure 3
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
#### example: waveform data
n <- 200
alpha <- 1/3; freq <- 4                           # amplitude and frequency of wave
sigma1 <- 2; sigma2 <- 10                         # noise levels  
lon <- seq(-180, 180, length.out = n)             # uniformly sampled longitude
lat <- alpha * 180/pi * sin(freq * lon * pi/180) + 10.        # latitude vector
## add Gaussian noises on latitude vector
lat1 <- lat + sigma1 * rnorm(length(lon)); lat2 <- lat + sigma2 * rnorm(length(lon))
wave1 <- cbind(lon, lat1); wave2 <- cbind(lon, lat2)
## implement principal curves by Hauberg to the waveform data
SPC.Hauberg(wave1, q = 0.05)
## implement SPC to the (noisy) waveform data
SPC(wave1, q = 0.05)
SPC(wave2, q = 0.05)

#
# Generating Figure 4
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
data(Earthquake)
# collect spatial locations (longitude and latitude denoted by degrees) of data
earthquake <- cbind(Earthquake$longitude, Earthquake$latitude)

#### example 1: plot the projection lines (option of plot.proj)
SPC(earthquake, q = 0.1, plot.proj = TRUE)

#### example 2: open principal curves (option of deletePoints)
SPC(earthquake, q = 0.04, deletePoints = TRUE)

#
# Generating Figure 5
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
## longitude and latitude are expressed in degrees
#### example 1: spiral data 
set.seed(40)
n <- 900                                          # the number of samples
sigma1 <- 1; sigma2 <- 2.5;                       # noise levels
radius <- 73; slope <- pi/16                      # radius and slope of spiral
## polar coordinate of (longitude, latitude)
r <- runif(n)^(2/3) * radius; theta <- -slope * r + 3 
## transform to (longitude, latitude)
correction <- (0.5 * r/radius + 0.3)              # correction of noise level
lon1 <- r * cos(theta) + correction * sigma1 * rnorm(n)
lat1 <- r * sin(theta) + correction * sigma1 * rnorm(n)
lon2 <- r * cos(theta) + correction * sigma2 * rnorm(n)
lat2 <- r * sin(theta) + correction * sigma2 * rnorm(n)
spiral1 <- cbind(lon1, lat1); spiral2 <- cbind(lon2, lat2)
## plot spiral data
rgl.sphgrid(col.lat = 'black', col.long = 'black')
rgl.sphpoints(spiral1, radius = 1, col = 'blue', size = 12)
## implement the LPG to (noisy) spiral data
LPG(spiral1, scale = 0.06, nu = 0.1, seed = 100)
LPG(spiral2, scale = 0.12, nu = 0.1, seed = 100)

#
# Generating Figure 6 
#
#### example 2: zigzag data
set.seed(10)
n <- 50                                # the number of samples is 6 * n = 300
sigma1 <- 2; sigma2 <- 5               # noise levels                   
x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- runif(n) * 20 - 20
y1 <- x1 + 20 + sigma1 * rnorm(n); y2 <- -x2 + 20 + sigma1 * rnorm(n)
y3 <- x3 + 60 + sigma1 * rnorm(n); y4 <- -x4 - 20 + sigma1 * rnorm(n)
y5 <- x5 - 20 + sigma1 * rnorm(n); y6 <- -x6 - 60 + sigma1 * rnorm(n)
x <- c(x1, x2, x3, x4, x5, x6); y <- c(y1, y2, y3, y4, y5, y6)
simul.zigzag1 <- cbind(x, y)
## plot the zigzag data
sphereplot::rgl.sphgrid(col.lat = 'black', col.long = 'black')
sphereplot::rgl.sphpoints(simul.zigzag1, radius = 1, col = 'blue', size = 12)
## implement the LPG to the zigzag data
LPG(simul.zigzag1, scale = 0.1, nu = 0.1, maxpt = 45, seed = 50)

## noisy zigzag data
set.seed(10)
z1 <- z2 <- z3 <- z4 <- z5 <- z6 <- runif(n) * 20 - 20
w1 <- z1 + 20 + sigma2 * rnorm(n); w2 <- -z2 + 20 + sigma2 * rnorm(n)
w3 <- z3 + 60 + sigma2 * rnorm(n); w4 <- -z4 - 20 + sigma2 * rnorm(n)
w5 <- z5 - 20 + sigma2 * rnorm(n); w6 <- -z6 - 60 + sigma2 * rnorm(n)
z <- c(z1, z2, z3, z4, z5, z6); w <- c(w1, w2, w3, w4, w5, w6)
simul.zigzag2 <- cbind(z, w)
## implement the LPG to the noisy zigzag data
LPG(simul.zigzag2, scale = 0.2, nu = 0.1, maxpt = 18, seed = 20)

#
# Generating Figure 7
#
#### example 3: tree data
## tree consists of stem, branches and subbranches
## generate stem
set.seed(10)
n1 <- 200; n2 <- 100; n3 <- 15 # the number of samples in stem, a branch, and a subbranch
sigma1 <- 0.1; sigma2 <- 0.05; sigma3 <- 0.01 # noise levels
noise1 <- sigma1 * rnorm(n1); noise2 <- sigma2 * rnorm(n2); noise3 <- sigma3 * rnorm(n3)
l1 <- 70; l2 <- 20; l3 <- 1            # length of stem, branches, and subbranches
rep1 <- l1 * runif(n1)                 # repeated part of stem
stem <- cbind(0 + noise1, rep1 - 10)
## generate branch
rep2 <- l2 * runif(n2)                 # repeated part of branch
branch1 <- cbind(-rep2, rep2 + 10 + noise2); branch2 <- cbind(rep2, rep2 + noise2)
branch3 <- cbind(rep2, rep2 + 20 + noise2); branch4 <- cbind(rep2, rep2 + 40 + noise2)
branch5 <- cbind(-rep2, rep2 + 30 + noise2)
branch <- rbind(branch1, branch2, branch3, branch4, branch5)
## generate subbranches
rep3 <- l3 * runif(n3)                 # repeated part in subbranches
branches1 <- cbind(rep3 - 10, rep3 + 20 + noise3)
branches2 <- cbind(-rep3 + 10, rep3 + 10 + noise3)
branches3 <- cbind(rep3 - 14, rep3 + 24 + noise3)
branches4 <- cbind(-rep3 + 14, rep3 + 14 + noise3)
branches5 <- cbind(-rep3 - 12, -rep3 + 22 + noise3)
branches6 <- cbind(rep3 + 12, -rep3 + 12 + noise3)
branches7 <- cbind(-rep3 - 16, -rep3 + 26 + noise3)
branches8 <- cbind(rep3 + 16, -rep3 + 16 + noise3)
branches9 <- cbind(rep3 + 10, -rep3 + 50 + noise3)
branches10 <- cbind(-rep3 - 10, -rep3 + 40 + noise3)
branches11 <- cbind(-rep3 + 12, rep3 + 52 + noise3)
branches12 <- cbind(rep3 - 12, rep3 + 42 + noise3)
branches13 <- cbind(rep3 + 14, -rep3 + 54 + noise3)
branches14 <- cbind(-rep3 - 14, -rep3 + 44 + noise3)
branches15 <- cbind(-rep3 + 16, rep3 + 56 + noise3)
branches16 <- cbind(rep3 - 16, rep3 + 46 + noise3)
branches17 <- cbind(-rep3 + 10, rep3 + 30 + noise3)
branches18 <- cbind(-rep3 + 14, rep3 + 34 + noise3)
branches19 <- cbind(rep3 + 16, -rep3 + 36 + noise3)
branches20 <- cbind(rep3 + 12, -rep3 + 32 + noise3)
sub.branches <- rbind(branches1, branches2, branches3, branches4, branches5, branches6,
                        +    branches7, branches8, branches9, branches10, branches11, branches12, branches13,
                        +    branches14, branches15, branches16, branches17, branches18, branches19, branches20)
## tree consists of stem, branch and subbranches
tree <- rbind(stem, branch, sub.branches)

## plot tree data
sphereplot::rgl.sphgrid(col.lat = 'black', col.long = 'black')
sphereplot::rgl.sphpoints(tree, radius = 1, col = 'blue', size = 12)

## implement the LPG function to tree data
LPG(tree, scale = 0.03, nu = 0.2, seed = 10)


#
# Generating Figure 8 (right)
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
data(Earthquake)
#### Collect spatial locations (longitude and latitude expressed in degrees) of data
earthquake <- cbind(Earthquake$longitude, Earthquake$latitude)  

## plot the circular data
sphereplot::rgl.sphgrid(col.lat = "black", col.long = "black")
sphereplot::rgl.sphpoints(earthquake, radius = 1, col = "blue", size = 9)


#
# Generating Figure 9
#
library(spherepc)
library(rgl)
library(sphereplot)
library(geosphere)
data(Earthquake)
#### collect spatial locations (longitude and latitude by degrees) of data
earthquake <- cbind(Earthquake$longitude, Earthquake$latitude)  

#### example 1: principal geodesic analysis (PGA)
PGA(earthquake)

#### example 2: principal circle
## get center and radius of principal circle
circle <- PrincipalCircle(earthquake)    
## generate a principal circle
PC <- GenerateCircle(circle[1:2], circle[3], T = 1000)   
## plot the principal circle
sphereplot::rgl.sphgrid(col.long = "black", col.lat = "black")                                  
sphereplot::rgl.sphpoints(earthquake, radius = 1, col = "blue", size = 12)
sphereplot::rgl.sphpoints(PC, radius = 1, col = "red", size = 9)

#
# Generating Figure 10
#
#### example 3: spherical principal curves and principal curves by Hauberg
SPC.Hauberg(earthquake, q = 0.1) # principal curves by Hauberg
SPC(earthquake, q = 0.1)         # spherical principal curves

#
# Generating Figure 11
#
#### example 4: spherical principal curves with q = 0.15, 0.1, 0.03, and 0.02.
SPC(earthquake, q = 0.15)         
SPC(earthquake, q = 0.1)
SPC(earthquake, q = 0.03)
SPC(earthquake, q = 0.02)

#
# Generating Figure 12
#
#### example 5: local principal geodesics (LPG)
LPG(earthquake, scale = 0.5, nu = 0.2, maxpt = 20, seed = 50)
LPG(earthquake, scale = 0.4, nu = 0.3, maxpt = 22, seed = 50)  









