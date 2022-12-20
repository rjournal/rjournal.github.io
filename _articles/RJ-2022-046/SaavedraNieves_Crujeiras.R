#--------------------------------------
# Installing and loading R packages
#--------------------------------------

install.packages("HDiR")
install.packages("NPCirc")
install.packages("DirStats")

library(HDiR)
library(NPCirc)
library(DirStats)

#---------------------------------
# Function dspheremix
#---------------------------------

data <- rbind(c(1, 0, 0), c(0, 0, 1))
dspheremix(x = data, model = 9)

#---------------------------------
# Function rspheremix
#---------------------------------

set.seed(1)
rspheremix(n = 100, model = 9)

#---------------------------------
# Function circ.hdr
#---------------------------------

f <- function(x)return(dcircmix(x, 13))
circ.hdr(f, level = 0.35)
circ.hdr(f, tau = 0.5)

#---------------------------------
# Function sphere.hdr
#---------------------------------

f <- function(x)return(dspheremix(x, model = 9))
sphere.hdr(f, level = 0.1, mesh = 10, deg = 3)
sphere.hdr(f, tau = 0.5, mesh = 10, deg = 3)

#---------------------------------
# Function circ.plugin.hdr
#---------------------------------

set.seed(1)
sample <- rcircmix(500, 13)
circ.plugin.hdr(sample, tau = 0.5, plot.hdrconf = TRUE, k = 2, col = "blue")
bw.CV(sample,upper=100); circ.boot.bw(sample, tau = 0.8, B=2)

#---------------------------------
# Function sphere.plugin.hdr
#---------------------------------

set.seed(1)
sample=rspheremix(500, model = 9)
sphere.plugin.hdr(sample, tau = 0.5, nborder = 2000)


set.seed(1)
sample <- rspheremix(500, model = 8)
bw.boot <- sphere.boot.bw(sample, bw = "rot", tau = 0.8, B = 2)
sphere.plugin.hdr(sample, bw = bw.boot, tau = 0.8)
sphere.plugin.hdr(sample, bw = "none", tau = 0.8)

#------------------------------------------------------------------------------------
# Function sphere.hdr | Plug-in estimation of density level set with uniform kernel
#------------------------------------------------------------------------------------

f <- function(x){
  set.seed(1)
  sample <- rspheremix(500, model = 3)
  return(kde_dir(x, data = sample, h = 0.4,
                 L = function(x) dunif(x)))
}
sphere.hdr(f, level = 0.3)


#-------------------------------------------
# Function circ.hdr | Regression level set
#-------------------------------------------

f <- function(t){
  set.seed(1)
  n <- 100
  x <- runif(n, 0, 2*pi)
  y <- sin(x)+0.5*rnorm(n)
  return(kern.reg.circ.lin(circular(x), y, t, bw = 10, method = "NW")$y)
}
circ.hdr(f, level = 0.5, plot.hdr = FALSE)


#---------------------------------
# Function circ.distances
#---------------------------------

set.seed(1)
sample <- rcircmix(100, 13)
f <- function(x)return(dcircmix(x, 13))
circ.distances(as.numeric(circ.hdr(f, tau = 0.5)$hdr), as.numeric(circ.plugin.hdr(sample, tau = 0.5)$hdr))


#---------------------------------
# Function sphere.distances
#---------------------------------

set.seed(1)
sample = rspheremix(1000, model = 9)
x <- sphere.plugin.hdr(sample, tau = 0.8, plot.hdr =TRUE)$hdr
y <- sphere.plugin.hdr(sample, tau = 0.5, plot.hdr = TRUE)$hdr
sphere.distances(x, y)


#---------------------------------
# Function circ.scatterplot
#---------------------------------

set.seed(1)
sample<- rcircmix(100, model = 13)
circ.scatterplot(sample, tau = c(0.2, 0.5, 0.8))

#---------------------------------
# Function sphere.scatterplot
#---------------------------------

set.seed(1)
sample = rspheremix(1000, model = 9)
sphere.scatterplot(sample, tau = c(0.2, 0.5, 0.8))

#-------------------------------------------------
# Function circ.plugin.hdr | sandhoppers dataset
#--------------------------------------------------

data(sandhoppers)
attach(sandhoppers)
britoF <- angle[(species == "brito")&(time == "morning")&(sex == "F")&(month == "October")]
circ.plugin.hdr(sample = britoF, tau = 0.8, plot.hdrconf = FALSE)
britoM <- angle[(species == "brito")&(time == "morning")&(sex == "M")&(month == "October")]
circ.plugin.hdr(sample = britoM, tau = 0.8, plot.hdrconf = FALSE)

#---------------------------------
# Function circ.boot.bw
#---------------------------------

bw.CV(circular(britoM,type="angles",units="radians"))
set.seed(1)
circ.boot.bw(britoM, tau = 0.8)
bw.CV(circular(britoF,type="angles",units="radians"))
set.seed(1)
circ.boot.bw(britoF, tau = 0.8)


#--------------------------------------
# Installing and loading R packages
#--------------------------------------

install.packages("ggplot2"); install.packages("Directional"); install.packages("maps"); install.packages("mapproj")
library(ggplot2); library(Directional); library(maps); library(mapproj)

#--------------------------------------------------
# Function sphere.plugin.hdr | earthquakes dataset
#---------------------------------------------------

data(earthquakes)
hdr <- as.data.frame(euclid.inv(sphere.plugin.hdr(euclid(earthquakes), tau = 0.8, plot.hdr = FALSE)$hdr))
world <- map_data("world")
g.earthquakes <- ggplot()+
  geom_map(data = world, map = world, mapping = aes(map_id = region), color = "grey90", fill = "grey80")+
  geom_point(data = earthquakes, mapping = aes(x = Longitude, y = Latitude), color = "red", alpha = 0.2, size = 0.75, stroke = 0)+
  geom_point(data = hdr, mapping = aes(x = Long, y = Lat), color = "darkblue", size = 1)+
  scale_y_continuous(breaks = NULL, limits = c(-90, 90))+
  scale_x_continuous(breaks = NULL, limits = c(-180, 180))+
  coord_map("mercator")
g.earthquakes

#---------------------------------
# Function sphere.boot.bw
#---------------------------------

set.seed(1)
sphere.boot.bw(euclid(earthquakes), tau = 0.8, B = 5)


#---------------------------------
# Function sphere.distances
#---------------------------------

hdr1 <- sphere.plugin.hdr(euclid(earthquakes), tau = 0.8, plot.hdr = FALSE)$hdr
hdr2 <- sphere.plugin.hdr(euclid(earthquakes), bw = 0.09, tau = 0.8, plot.hdr = FALSE)$hdr
sphere.distances(hdr1, hdr2)


#---------------------------------
# Function circ.scatterplot
#---------------------------------

circ.scatterplot(britoF, tau = c(0.2, 0.5, 0.8))
circ.scatterplot(britoM, tau = c(0.2, 0.5, 0.8))


#---------------------------------
# Function sphere.scatterplot
#---------------------------------

sphere.scatterplot(euclid(earthquakes), tau=c(0.2, 0.5, 0.8), bw = "rot", nborder = 1500)
