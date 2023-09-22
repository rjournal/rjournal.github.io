################################################################################
##
## AQuadtree: an R Package for Quadtree anonymization of point data
## R Code to reproduce all the examples 
##
## Copyright (C) 2020 Raymond Lagonigro
##   
## This code is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
##
## Package AQuadtree available at
## https://CRAN.R-project.org/package=AQuadtree
## The packages sp (https://CRAN.R-project.org/package=sp)
## and dplyr (https://CRAN.R-project.org/package=dplyr)
## are required
## The packages rgeos (https://CRAN.R-project.org/package=rgeos)
## and rgdal (https://CRAN.R-project.org/package=rgdal) are also useful
##
################################################################################
## Package and options

library(AQuadtree)
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

################################################################################
##
## 3.2	Provided data
##
################################################################################

data("BarcelonaPop", package = "AQuadtree")
summary(BarcelonaPop)

data("CharlestonPop", package = "AQuadtree")
summary(CharlestonPop)

################################################################################
##
## 3.3	The AQuadtree class
##
################################################################################

bcn.QT <- AQuadtree(BarcelonaPop)
class(bcn.QT)

plot(bcn.QT)

names(bcn.QT)
bcn.QT[310:315, ]

################################################################################
## 3.3.1	General usage
################################################################################

charleston.QT <- AQuadtree(CharlestonPop, dim = 10000, layers = 5)
summary(charleston.QT)

bcn.QT <- AQuadtree(BarcelonaPop,
		colnames = c('age','sex'),
		funs = c('mean', 'sum'))
summary(bcn.QT)

################################################################################
## 3.3.2	Anonymity threshold
################################################################################

bcn.QT <- AQuadtree(BarcelonaPop, colnames = c('age','sex'),
              	funs = c('mean', 'sum'), threshold=17,
              	thresholdField=c("sex.man", "sex.woman"))
summary(bcn.QT)

################################################################################
## 3.3.3	Balancing information loss and accuracy
################################################################################

# a subset of points corre- sponding to a cell of 1km, with bottom-left 
# corner (3660000, 2065000) and top-right corner (3661000, 2066000)
point.coord<-as.data.frame(coordinates(BarcelonaPop))
points.subset<-BarcelonaPop[ point.coord[,1]>=3660000 &
                  			     point.coord[,1]<=3661000 &
                  			     point.coord[,2]>=2065000 &
                  			     point.coord[,2]<=2066000,
                  			   ]
# plot the subset of points 
plot(points.subset, pch=19, cex=8)
length(points.subset)

# AQuadtree with a threshold value of 5 points per cell and the default 
# inequality value
bcn.QT <- AQuadtree(points.subset, threshold = 5)
plot(bcn.QT, residual=F, col="grey")
bcn.QT[bcn.QT$residual, ]$total / length(points.subset)
sd(bcn.QT[!bcn.QT$residual, ]$total)/mean(bcn.QT[!bcn.QT$residual, ]$total)

# inequality value 0.01
bcn.QT <- AQuadtree(points.subset, threshold = 5, ineq.threshold = 0.01)
plot(bcn.QT, residual=F, col="grey")
bcn.QT[bcn.QT$residual, ]$total / length(points.subset)
sd(bcn.QT[!bcn.QT$residual, ]$total)/mean(bcn.QT[!bcn.QT$residual, ]$total)

# inequality value 0.5
bcn.QT <- AQuadtree(points.subset, threshold = 5, ineq.threshold = 0.5)
plot(bcn.QT, residual=F, col="grey")
bcn.QT[bcn.QT$residual, ]$total / length(points.subset)
sd(bcn.QT[!bcn.QT$residual, ]$total)/mean(bcn.QT[!bcn.QT$residual, ]$total)


################################################################################
## 3.4.1	Join AQuadtrees
################################################################################

Barcelona.AQT_1 <- AQuadtree(BarcelonaPop, colnames="age", threshold=25, fun="mean")
Barcelona.AQT_2 <- AQuadtree(BarcelonaPop, colnames="sex", threshold=17,
                             thresholdField=c("sex.man", "sex.woman"))
# The joinAQuatrees function
Barcelona.AQT_1_2 <- joinAQuadtrees(Barcelona.AQT_1,
                                    Barcelona.AQT_2, mean.1 = "age", withResiduals = TRUE)
# Plot the three objects
plot(Barcelona.AQT_1[Barcelona.AQT_1$cellCode=="1kmN2065E3665",])
plot(Barcelona.AQT_2[Barcelona.AQT_2$cellCode=="1kmN2065E3665",])
plot(Barcelona.AQT_1_2[Barcelona.AQT_1_2$cellCode=="1kmN2065E3665",])

## Plot the objects using grayscales
plot(Barcelona.AQT_1[Barcelona.AQT_1$cellCode=="1kmN2065E3665",], lwd=20, residual=FALSE, col="transparent")
plot(Barcelona.AQT_1[Barcelona.AQT_1$cellCode=="1kmN2065E3665" & Barcelona.AQT_1$cellNum=='2',], col="gray20", lwd=20, residual=FALSE, add=TRUE)
plot(Barcelona.AQT_1[Barcelona.AQT_1$cellCode=="1kmN2065E3665" & Barcelona.AQT_1$cellNum %in% c('31451', '31452', '31459', '31460'),], col="gray45", lwd=20, residual=FALSE, add=TRUE)
plot(Barcelona.AQT_1[Barcelona.AQT_1$cellCode=="1kmN2065E3665" & Barcelona.AQT_1$cellNum %in% c('411', '412', '41553', '41554', '41561', '41562', '416'),], col="gray70", lwd=20, residual=FALSE, add=TRUE)
Barcelona.AQT_1[Barcelona.AQT_1$cellCode=="1kmN2065E3665",c("cellNum", "level", "total", "age")]

plot(Barcelona.AQT_2[Barcelona.AQT_2$cellCode=="1kmN2065E3665",], lwd=20, residual=FALSE, col="transparent")
plot(Barcelona.AQT_2[Barcelona.AQT_2$cellCode=="1kmN2065E3665" & Barcelona.AQT_2$cellNum %in% c('203', '207', '208'),], col="gray20", lwd=20, residual=FALSE, add=TRUE)
plot(Barcelona.AQT_2[Barcelona.AQT_2$cellCode=="1kmN2065E3665" & Barcelona.AQT_2$cellNum=='314',], col="gray45", lwd=20, residual=FALSE, add=TRUE)
plot(Barcelona.AQT_2[Barcelona.AQT_2$cellCode=="1kmN2065E3665" & Barcelona.AQT_2$cellNum=='4',], col="gray70", lwd=20, residual=FALSE, add=TRUE)
Barcelona.AQT_2[Barcelona.AQT_2$cellCode=="1kmN2065E3665",c("cellNum", "level", "total", "sex.man", "sex.woman")]

plot(Barcelona.AQT_1_2[Barcelona.AQT_1_2$cellCode=="1kmN2065E3665",], lwd=20, residual=FALSE, col="transparent")
plot(Barcelona.AQT_1_2[Barcelona.AQT_1_2$cellCode=="1kmN2065E3665" & Barcelona.AQT_1_2$cellNum=='2',], col="gray20", lwd=20, residual=FALSE, add=TRUE)
plot(Barcelona.AQT_1_2[Barcelona.AQT_1_2$cellCode=="1kmN2065E3665" & Barcelona.AQT_1_2$cellNum=='314',], col="gray45", lwd=20, residual=FALSE, add=TRUE)
plot(Barcelona.AQT_1_2[Barcelona.AQT_1_2$cellCode=="1kmN2065E3665" & Barcelona.AQT_1_2$cellNum=='4',], col="gray70", lwd=20, residual=FALSE, add=TRUE)
Barcelona.AQT_1_2[Barcelona.AQT_1_2$cellCode=="1kmN2065E3665",c("cellNum", "level", "total.1", "age.1", "total.2", "sex.man.2", "sex.woman.2")]


# use the joinAQuadtrees function to incorporate a set of points
# to an existing AQuadtree object
Charleston.AQT<-AQuadtree(CharlestonPop, threshold = 17)
head(Charleston.AQT)

CharlestonWomen65 <- CharlestonPop[CharlestonPop$origin=='white'
                                   & CharlestonPop$age=='over65', 'sex']
CharlestonWomen65.AQT<-AQuadtree(CharlestonWomen65, threshold = 1, colnames = 'sex')

Charleston.AQT.ext<-joinAQuadtrees(Charleston.AQT, CharlestonWomen65.AQT)
head(Charleston.AQT.ext)

length(Charleston.AQT)
length(Charleston.AQT.ext)

################################################################################
## 3.4.2	Aggregate points to an AQuadtree
################################################################################

Charleston.AQT.ext<-pointsToAQuadtree(Charleston.AQT, CharlestonWomen65)
tail(Charleston.AQT.ext)
length(Charleston.AQT)
length(Charleston.AQT.ext)

################################################################################
## 3.4.3	Create a fixed size grid
################################################################################

plot(BarcelonaCensusTracts)

plot(createGrid(BarcelonaCensusTracts, intersect = FALSE))
plot(rgeos::gUnaryUnion(BarcelonaCensusTracts), add=TRUE)

plot(createGrid(BarcelonaCensusTracts, intersect = TRUE, outline = FALSE))
plot(createGrid(BarcelonaCensusTracts, intersect = TRUE, outline = TRUE))

# print the ID's corresponding to the INSPIRE cell codes
Bcn.Grid<-createGrid(BarcelonaCensusTracts, intersect = TRUE, outline = FALSE)
row.names(Bcn.Grid)

# aggregate a set of points to the created grid
Bcn.Grid.ext<-aggregate(BarcelonaPop[,'age'], by=Bcn.Grid, FUN="mean")
head(as.data.frame(Bcn.Grid.ext))

# aggregate a set of points to the created grid using the 
# spatialPointsCellCodes function
Bcn.points<-spatialPointsCellCodes(BarcelonaPop)
Bcn.points.agr<-aggregate(age~cellCode, Bcn.points, "mean")
Bcn.Grid$cellCode<-row.names(Bcn.Grid)
Bcn.Grid.ext<-merge(Bcn.Grid, Bcn.points.agr)
head(as.data.frame(Bcn.Grid.ext))

# summarizing with the dplyr package
Bcn.points.agr<-summarise(group_by(as.data.frame(Bcn.points),cellCode),
                          	 total.points=n(), mean.age = mean(age), sd.age = sd(age))
Bcn.Grid.ext<-merge(Bcn.Grid, Bcn.points.agr)
head(as.data.frame(Bcn.Grid.ext))

