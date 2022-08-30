library(dplyr)
library(ggplot2)
library(reshape)
library(ggforce)

####Reading in Data####
#Reading in the time data
data.time <- rbind(read.csv("annulus.csv", header = TRUE),
                   read.csv("circle.csv", header = TRUE),
                   read.csv("torus.csv", header = TRUE),
                   read.csv("uniform.csv", header = TRUE))
#Adding Column Names
colnames(data.time) <- c("row", "measure.type", "point.cloud", "point.cloud.dim", 
                         "num.points", "feat.dim", "library", 
                         "time1", "time2", "time3", "time4", 
                         "time5", "time6", "time7",
                         "time8", "time9", "time10")

#Remove extraneaous row variable
data.time <- data.time %>% select(-"row")

#Creating average and standard deviation columns for time data
data.time$avg.time <- apply(data.time[,7:16],1,mean)
data.time$std <- apply(data.time[,7:16],1,sd)
data.time$min.time <- apply(data.time[,7:16],1,min)
data.time$max.time <- apply(data.time[,7:16],1,max)

#Create Factor labels for libraries
data.time$library <- factor(data.time$library,
                            levels = c("stats","Dionysus","GUDHI", "GUDHIalpha"),
                            labels = c("TDAstats (Ripser)", 
                                       "TDA (Dionysus)", "TDA (GUDHI Rips)", "TDA (GUDHI Alpha)"))

#Read in object size data
#data.mem <- read.csv("mem1.csv", header = FALSE)

#Name columns
#colnames(data.mem) <- c("measure.type", "point.cloud", "point.cloud.dim", 
#                        "num.points", "feat.dim", "library", "memory")

#Create Factor labels for libraries
#data.mem$library <- factor(data.mem$library,
#                           levels = c("stats","Dionysus","GUDHI", "GUDHIalpha"),
#                           labels = c("TDAstats (Ripser)", 
#                                      "TDA (Dionysus)", "TDA (GUDHI Rips)", "TDA (GUDHI Alpha)"))


####Figure_3####
#Goal: overview showing speed differences across engines on a canonical shape.

#Selected torus data scanning for two dimensional features
data.fig.1 <- subset(data.time, point.cloud == "torus" & feat.dim == 2 & 
                       library != "TDA (GUDHI Alpha)") 

#Point graph; X: Num.points; Y: Runtime; Color: TDA library
fig.1 <- ggplot(data.fig.1, aes(x=num.points, y=avg.time, color=library, shape=library)) + 
  geom_point()+
  geom_errorbar(data.fig.1, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "TDA Library",
       shape = "TDA Library",
       x = "Number of Points on Torus",
       y = "Average Run Time",
       title = "Rips Complex Run Times on 3D Torus Point Clouds",
       subtitle = "") 

#Editing the colors (Making everything blank)
fig.1 <- fig.1 + theme_classic() +
  theme(legend.position = c(0.85, 0.5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.title.align = 0) 

#Saving plot as png for later editing
ggsave("figure3.png", plot = fig.1,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)



####Figure_4####
#Goal Comparison of persistent homology calculation runtimes 
#across R packages for n-dimensional sphere (n = 2, 3, 4)

#Selected all boxes up to n-1 dimensional features
data.fig.2 <- subset(data.time, point.cloud == "circle" & library != "TDA (GUDHI Alpha)")
data.fig.2 <- data.fig.2[(data.fig.2$point.cloud.dim - 1 == data.fig.2$feat.dim), ]

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Sphere
fig.2 <- ggplot(data.fig.2, aes(x=num.points, y=avg.time, color=library, shape=library)) + 
  geom_point() +
  facet_wrap( ~ point.cloud.dim, ncol=3, scales = "free") + 
  geom_errorbar(data.fig.2, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "TDA Library",
       shape = "TDA Library",
       x = "Number of Points on N-Sphere",
       y = "Average Run Time",
       title = "Rips Complex Run Times For n-Dimensional Spheres",
       subtitle = "") +
  scale_x_continuous(limits=c(0,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-30,2100))


#Editing the colors (Making everything blank)
fig.2 <- fig.2 + theme_classic() +
  theme(legend.position = c(0.93, 0.50),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        legend.text.align = 0,
        legend.title.align = 0,
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(angle=-45, hjust=.025))

#Saving plot as png for later editing
ggsave("figure4.png", plot = fig.2,
       scale = 1, width = 12, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


####Figure 5: Runtime of persistent homology calculation as a function of feature dimension####
#Select all annulus data for all dimension point cloud and n-1 dim features
data.fig.3 <- subset(data.time, point.cloud == "uniform" & 
                       point.cloud.dim == 8 & library != "TDA (GUDHI Alpha)" & 
                       (num.points == 10 | num.points == 15| num.points == 20))
data.fig.3$num.points <- as.factor(data.fig.3$num.points)

#Point graph facet; X: Num.points; Y: Runtime; Color: TDA library; Facet: Dimensions of Annulus
fig.3 <- ggplot(data.fig.3, aes(x=feat.dim, y=avg.time, color=num.points, shape=num.points)) + 
  geom_point() + facet_wrap(~library, scales = "free") + 
  geom_errorbar(data.fig.3, mapping = aes(x=feat.dim, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) +
  labs(color = "Number of \nPoints",
       shape = "Number of \nPoints",
       x = "Feature Dimensions",
       y = "Average Run Time",
       title = "Run Times For Extracting Dimensional \nFeatures on an 8 Dimensional Box",
       subtitle = "") + scale_x_continuous(limits=c(0, 8), breaks=seq(0,8,1)) +
  scale_y_continuous(limits=c(0, 3))

#Editing the colors (Making everything blank)
fig.3 <- fig.3 + theme_classic() +
  theme(legend.position = c(0.93, 0.60),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        legend.text.align = 0,
        legend.title.align = 0,
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        axis.text.x = element_text(angle=0, hjust=.025))

#Saving plot as png for later editing
ggsave("figure5.png", plot = fig.3,
       scale = 1, width = 12, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


####Figure 6: Runtime of persistent homology calculation as a function of point cloud####
#Goal: show how increasing dimension slows down the calculation time.

#Select all 3D annuluses and change feature dimension to a factor variable
data.fig.4 <- subset(data.time, point.cloud == "annulus" & feat.dim == 1 & 
                       (library == "TDA (GUDHI Rips)" | library == "TDA (GUDHI Alpha)"))
data.fig.4$point.cloud.dim <- as.factor(data.fig.4$point.cloud.dim)
data.fig.4 <- as.data.frame(data.fig.4)

#Point graph facet; X: Num.points; Y: Runtime; Color: point.cloud dim; Facet: library
fig.4 <- ggplot(data.fig.4, aes(x=num.points, y=avg.time, color=point.cloud.dim, shape=point.cloud.dim)) + 
  geom_point() + facet_wrap(~library, scales = "free") + 
  geom_errorbar(data.fig.4, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std), width=28) +
  labs(color = "Annulus \nDimension",
       shape = "Annulus \nDimension",
       x = "Number of Points",
       y = "Average Run Time",
       title = "Rips vs Alpha Complex Run Times For Extracting \n1 Dimensional Features on N-dimensional Annuluses",
       subtitle = "") + 
  scale_x_continuous(limits=c(0,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-.02, 22))

#Editing the colors (Making everything blank)
fig.4 <- fig.4 + theme_classic() +
  theme(legend.position = c(0.95, 0.50),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        legend.text.align = 0,
        legend.title.align = 0,
        axis.text.x = element_text(angle=0, hjust=.025),
        strip.background = element_blank(),
        strip.text.x = element_text())

#Saving plot as png for later editing
ggsave("figure6.png", plot = fig.4,
       scale = 1, width = 8, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)

####Figure old-version: Object Size use of Rips complex vs alpha complex. Engine = GUDHI(alpha).#### 
#Goal: compare memory
#data.fig.5a <- subset(data.mem, point.cloud.dim == 3 & feat.dim == 2)

#Point graph facet; X: Num.points; Y: Memory; Color: Library; Facet: Pointcloud
#fig.5a <- ggplot(data.fig.5a, aes(x=num.points, y=memory, color=library, shape=library)) + 
#  geom_point() + facet_wrap(~point.cloud, scales = "free") + 
#  labs(color = "Complex",
#       shape = "Complex",
#       x = "Number of Points",
#       y = "Object Size",
#       title = "Object Size of Boundary Matrix",
#       subtitle = "")+ 
#  scale_x_continuous(limits=c(50,550), breaks = c(100, 200, 300, 400, 500)) + 
#  scale_y_continuous(limits=c(0, 1.8e10), breaks = c(1e5, 5e9, 1e10, 1.5e10)) 

#Editing the colors (Making everything blank)
#fig.5a <- fig.5a + theme_classic() +
#  theme(legend.position = c(.9, 0.8),
#        legend.spacing.x = unit(.005, 'cm'),
#        legend.title = element_text(size = 9),
#        legend.text = element_text(size = 7),
#        plot.title = element_text(hjust = 0.5),
#        legend.text.align = 0,
#        legend.title.align = 0.8,
#        axis.text.x = element_text(angle=0, hjust=.025),
#        strip.background = element_blank(),
#        #plot.margin = unit(c(1,3,1,1), "cm"),
#        strip.text.x = element_blank())
#fig.5a
#Saving plot as png for later editing
#ggsave("figure5a.png", plot = fig.5a,
#       scale = 1, width = 6, height = 4, units = "in",
#       dpi = 400, limitsize = TRUE)

#Show what Rips Object Size depends on
#data.fig.5b <- subset(data.mem, library == "TDA (GUDHI Rips)")
#data.fig.5b$feat.dim <- as.factor(data.fig.5b$feat.dim)

#Point graph facet; X: Num.points; Y: Memory; Color: Feat.Dim; 
#fig.5b <- ggplot(data.fig.5b, aes(x=num.points, y=memory, color=feat.dim, shape=feat.dim)) + 
#  geom_point() + 
#  labs(color = "Feature \nDimension",
#       shape = "Feature \nDimension",
#       x = "Number of Points",
#       y = "Object Size",
#       title = "Object Size of Rips Complex Boundary Matrix",
#       subtitle = "")+ 
#  scale_x_continuous(limits=c(50,550), breaks = c(100, 200, 300, 400, 500)) + 
#  scale_y_continuous(limits=c(0, 1.8e10), 
#                     labels = function(x) format(x, scientific = TRUE))

#Editing the colors (Making everything blank)
#fig.5b <- fig.5b + theme_classic() +
#  theme(legend.position = c(0.92, 0.550),
#        legend.title = element_text(size = 9),
#        legend.text = element_text(size = 7),
#        plot.title = element_text(hjust = 0.5),
#        legend.text.align = 0,
#        legend.title.align = 0,
#        axis.text.x = element_text(angle=0, hjust=.025),
#        strip.background = element_blank(),
#        #plot.margin = unit(c(1,3,1,1), "cm"),
#        strip.text.x = element_blank())

#Saving plot as png for later editing
#ggsave("figure5b.png", plot = fig.5b,
#       scale = 1, width = 8, height = 6, units = "in",
#       dpi = 400, limitsize = TRUE)

#Show what Alpha Complex Object Size depends on
#data.fig.5c <- subset(data.mem, library == "TDA (GUDHI Alpha)")
#This is necessary since Alpha Complex Object Size Function Did Not
#Calculate Feat Dimensions lower than Point Cloud Dim - 1 in all cases
#This subset is necessary to remove the extraneous conditions that
#were not passed on to the alpha complex object size
#data.fig.5c <- subset(data.fig.5c, point.cloud.dim - feat.dim == 1)

#data.fig.5c$feat.dim <- as.factor(data.fig.5c$feat.dim)


#Point graph facet; X: Num.points; Y: Memory; Color: Feat.Dim; 
#fig.5c <- ggplot(data.fig.5c, aes(x=num.points, y=memory, color=feat.dim, shape=feat.dim)) + 
#  geom_point() + facet_wrap(~point.cloud, scales = "free") +
#  labs(color = "Feature \nDimension",
#       shape = "Feature \nDimension",
#       x = "Number of Points",
#       y = "Object Size",
#       title = "Object Size of Alpha Complex Boundary Matrix",
#       subtitle = "")+ 
#  scale_x_continuous(limits=c(50,550), breaks = c(100, 200, 300, 400, 500)) + 
#  scale_y_continuous(limits=c(0, 1.24e6), 
#                     labels = function(x) format(x, scientific = TRUE))

#Editing the colors (Making everything blank)
#fig.5c <- fig.5c + theme_classic() + theme(
#  legend.position = c(.915, 0.945),
#  legend.title = element_text(size = 9),
#  legend.text = element_text(size = 7),
#  plot.title = element_text(hjust = 0.5),
#  legend.text.align = 0,
#  legend.title.align = 0,
  #plot.title = element_text(hjust = 0.5),
#  axis.text.x = element_text(angle=0, hjust=.025),
#  strip.background = element_blank(),
#  strip.text.x = element_blank(), 
  #plot.margin = unit(c(1,3,1,1), "cm"),
#  panel.spacing = unit(2, "lines"))

#Saving plot as png for later editing
#ggsave("figure5c.png", plot = fig.5c,
#       scale = 1, width = 8, height = 6, units = "in",
#       dpi = 400, limitsize = TRUE)


#####Figure 9 (for discussion) TDAstats vs GUDHIalpha for runtime.####
#Removing point cloud less than 20 since these points were specifically for multidimension box
data.fig.6 <- subset(data.time, point.cloud.dim == 3 & feat.dim == 2 & 
                       (library == "TDAstats (Ripser)" | library == "TDA (GUDHI Alpha)")) %>%
              subset(num.points >= 20)

#Point graph facet; X: Num.points; Y: time; Color: Library; Facet: Pointcloud
fig.6 <- ggplot(data.fig.6, aes(x=num.points, y=avg.time, color=library, shape=library)) + 
  facet_wrap(~point.cloud, scales = "free") + 
  geom_errorbar(data.fig.6, mapping = aes(x=num.points, 
                                          ymin=avg.time - std, 
                                          ymax=avg.time + std)) + geom_point() +
  labs(color = "Library",
       shape = "Library",
       x = "Number of Points",
       y = "Average Run Time (s)",
       title = "Vietoris-Rips (Ripser) vs Alpha Complex (GUDHI) \nRun Times on 3D Point Clouds",
       subtitle = "") + 
  scale_x_continuous(limits=c(5,550), breaks = c(100, 200, 300, 400, 500)) + 
  scale_y_continuous(limits=c(-.05, 84))

#Editing the colors (Making everything blank)
fig.6 <- fig.6 + theme_classic()
fig.6 <- fig.6 + theme(
  legend.position = c(.13, .18),
  legend.title = element_text(size = 9),
  legend.text = element_text(size = 7),
  plot.title = element_text(hjust = 0.5),
  legend.text.align = 0,
  legend.title.align = 0,
  legend.background=element_blank(),
  #plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle=0, hjust=.025),
  strip.background = element_blank(),
  strip.text.x = element_blank())
#plot.margin = unit(c(1,3,1,1), "cm"),
#panel.spacing = unit(2, "lines"))

#Saving plot as png for later editing
ggsave("figure9.png", plot = fig.6,
       scale = 1, width = 6, height = 4, units = "in",
       dpi = 400, limitsize = TRUE)


####Figs 1 & 2####
####Rips Complex
library("ggtda")

n <- 5;
set.seed(3)
d <- data.frame(
  x = runif(n, 0, .8),
  y = runif(n, 0, .8)
)
# compute the persistent homology
ph <- as.data.frame(TDAstats::calculate_homology(as.matrix(d), dim = 1))
print(head(ph, n = 12))
ph <- transform(ph, dim = as.factor(dimension))

prox <- .25
p_d1 <- ggplot(d, aes(x = x, y = y)) +
  theme_classic() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.line=element_blank())
p_sc1 <- ggplot(d, aes(x = x, y = y)) +
  theme_classic() +
  coord_fixed() +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris0() + theme(axis.title = element_blank(),
                           axis.text = element_blank(),
                           axis.ticks = element_blank(),
                           axis.line=element_blank())

prox <- .5
p_d2 <- ggplot(d, aes(x = x, y = y)) +
  theme_classic() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.line=element_blank())
p_sc2 <- ggplot(d, aes(x = x, y = y)) +
  theme_classic() +
  coord_fixed() +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris0() + theme(axis.title = element_blank(),
                           axis.text = element_blank(),
                           axis.ticks = element_blank(),
                           axis.line=element_blank())

prox <- .75
p_d3 <- ggplot(d, aes(x = x, y = y)) +
  theme_classic() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3") +
  geom_point() +theme(axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      axis.line=element_blank())
p_sc3 <- ggplot(d, aes(x = x, y = y)) +
  theme_classic() +
  coord_fixed() +
  stat_vietoris2(diameter = prox, fill = "darkgoldenrod") +
  stat_vietoris1(diameter = prox, alpha = .25) +
  stat_vietoris0() + theme(axis.title = element_blank(),
                           axis.text = element_blank(),
                           axis.ticks = element_blank(),
                           axis.line=element_blank())

# combine and save the plots
png(filename = "figure1.png",
    width = 6, height = 4, units = "in", res = 450)

gridExtra::grid.arrange(
  p_d1, 
  p_d2,
  p_d2,
  p_sc1, 
  p_sc2,
  p_sc3,
  nrow = 2)

dev.off()

####Alpha Complex
library(deldir)
library(ggplot2)

df <- d

#This creates the voronoi line segments
voronoi <- deldir(d$x, d$y)
voronoi$delsgs %>% mutate(
  distance = sqrt((x1-x2)^2+(y1-y2)^2),
)  -> voronoi$delsgs


#Now we can make a plot
alp.1 <- ggplot(data=df, aes(x=x,y=y)) +
  #Plot the union balls
  geom_circle(data= df, aes(x0=x,y0=y, r = .25/2), fill = "aquamarine", alpha = 1.0, inherit.aes = FALSE, color = NA) + 
  coord_fixed() + theme_classic() + theme(axis.title = element_blank(),
                                          axis.text = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.line=element_blank()) +
  #Plot the voronoi lines
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = .5,
    data = voronoi$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  #Plot the simplicies
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 1,
    data = subset(voronoi$delsgs, distance <= .25),
    linetype = 1,
    color= "black") +
  #Plot the points
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 2,
    color="#333333") 

alp.2 <- ggplot(data=df, aes(x=x,y=y)) +
  #Plot the union balls
  geom_circle(data= df, aes(x0=x,y0=y, r = .35/2), fill = "aquamarine", alpha = 1.0, inherit.aes = FALSE, color = NA) + 
  coord_fixed() + theme_classic() + theme(axis.title = element_blank(),
                                          axis.text = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.line=element_blank()) +
  #Plot the voronoi lines
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = .5,
    data = voronoi$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  #Plot the simplices
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 1,
    data = subset(voronoi$delsgs, distance <= .35),
    linetype = 1,
    color= "black") +
  #Plot the points
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 2,
    color="#333333") 



alp.3 <- ggplot(data=df, aes(x=x,y=y)) +
  #Plot union balls
  geom_circle(data= df, aes(x0=x,y0=y, r = .45/2), fill = "aquamarine", alpha = 1.0, inherit.aes = FALSE, color = NA) + 
  coord_fixed() + theme_classic() + theme(axis.title = element_blank(),
                                          axis.text = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.line=element_blank()) +
  #Plot the voronoi lines
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = .5,
    data = voronoi$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  #Plot the simplices
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 1,
    data = subset(voronoi$delsgs, distance <= .45),
    linetype = 1,
    color= "black")  + 
  #Plot the points
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 2,
    color="#333333")


# combine and save the plots
png(filename = "figure2.png",
    width = 6, height = 3, units = "in", res = 450)

gridExtra::grid.arrange(
  alp.1, 
  alp.2,
  alp.3,
  nrow = 1)

dev.off()




library(scatterplot3d)
library(recexcavAAR)
library(readr)
library(TDA)
library(TDAstats)

####Creating Point Clouds For Figure Visual Aid####
#At this point, all results are calculated
#Subsequent Code is just to add visual point cloud imagery to ggfigures

#Call point cloud functions for visualization
# add if statements to check for parameter validity

#####BOX DATA#####
# generates an n-dimension box (0, 1) of uniformly distributed points
unifbox <- function(num.points, data.dimensions) {
  # empty 
  to.calc.hom <- matrix(NA, nrow = num.points, ncol = data.dimensions)
  
  # adds a column of randomaly generated points 0 to 1 for how many ever dimensions specified
  for (i in 1:data.dimensions) {
    col <- runif(num.points, 0, 1)
    to.calc.hom[, i] <- col
  }
  
  return(to.calc.hom)
}

#####TORUS DATA#####
# very simple, uses native uniform torus function from TDA
torus <- function(num.points) {
  torusUnif(num.points, 1, 1)
}

#####UNIFORM CIRCLE DATA#####
# Uses the sphere picking tactic to make uniform distribution 
# Cite Marsaglia paper from Wolfram Alpha page
unifcircle <- function(num.points, data.dimensions) {
  # var that stores result (empty df setup)
  to.calc.hom <- matrix(NA, nrow = num.points, ncol = data.dimensions)
  #to.calc.hom <- as.data.frame(to.calc.hom)
  
  # returns 2-d circle data
  if (data.dimensions == 2) {
    angles <- runif(num.points, 0, 2*pi)
    to.calc.hom <- cbind(cos(angles), sin(angles))
  }
  
  # returns 3-d circle data
  if (data.dimensions == 3) {
    
    # each loop generates one row of data
    for (curr_row in 1:num.points) {
      
      # generate valid x1 and x2
      x1 <- runif(1, -1, 1)
      x2 <- runif(1, -1, 1)
      while (x1 ^ 2 + x2 ^ 2 >= 1) {
        x1 <- runif(1, -1, 1)
        x2 <- runif(1, -1, 1)
      }
      
      # generate coordinates of sphere
      x <- 2 * x1 * sqrt(1 - x1 ^ 2 - x2 ^ 2)
      y <- 2 * x2 * sqrt(1 - x1 ^ 2 - x2 ^ 2)
      z <- 1 - 2 * (x1 ^ 2 + x2 ^ 2)
      
      # store into data frame
      to.calc.hom[curr_row, ] <- c(x, y, z)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  if (data.dimensions == 4) { #follows same principle as previous but with more parameters
    # each loop generates one row of data
    for (curr_row in 1:num.points) {
      
      # generate valid w, x, y, z
      x <- runif(1, -1, 1)
      y <- runif(1, -1, 1)
      z <- runif(1, -1, 1)
      w <- runif(1, -1, 1)
      while (x ^ 2 + y ^ 2 >= 1 |
             w ^ 2 + z ^ 2 >= 1) {
        w <- runif(1, -1, 1)
        x <- runif(1, -1, 1)
        y <- runif(1, -1, 1)
        z <- runif(1, -1, 1)
      }
      
      # generate coordinates of sphere
      temp <- sqrt((1 - x ^ 2 - y ^ 2) / (w ^ 2 + z ^ 2))
      x1 <- x
      x2 <- y
      x3 <- z * temp
      x4 <- w * temp
      
      # store into data frame
      to.calc.hom[curr_row, ] <- c(x1, x2, x3, x4)
    }
    
    # cast df into matrix
    to.calc.hom <- as.matrix(to.calc.hom)
  }
  
  # return answer variable
  return(to.calc.hom)
}

#####NOISY CIRCLE DATA#####
# almost idential to unif circle, but all x and y coordinates are multiplied by a perturbance varying from .9 to 1.1
noisycircle <- function(num.points, data.dimensions,
                        noise.magnitude = 0.1) {
  # get non-noisy data
  to.calc.hom <- unifcircle(num.points, data.dimensions)
  
  # add noise
  for (curr.col in 1:data.dimensions) {
    noise <- runif(num.points, 1 - noise.magnitude,
                   1 + noise.magnitude)
    to.calc.hom[, curr.col] <- to.calc.hom[, curr.col] * noise
  }
  
  # return noisy data
  return(to.calc.hom)
}



#####bench time#####
# NB: maxscale = 5 is used in TDA package examples, so used here (no default)

# point data input is required. program that is calculated is based off text string
# dimensional features and iteration number for benchmark should also be specified
bench <- function(pointdata, TDA.library, featdim, num.iterations) {
  # TDAstats
  if (TDA.library == "stats") {
    time <- mark(calculate_homology(pointdata, dim = featdim, threshold = 4),
                 iterations = num.iterations)
    # TDA - Dionysus
  } else if (TDA.library == "Dionysus") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 4,
                          location = FALSE, library = "Dionysus"),
                 iterations = num.iterations)
    # TDA - GUDHI
  } else if (TDA.library == "GUDHI") {
    time <- mark(ripsDiag(pointdata, maxdimension = featdim, maxscale = 4,
                          location = FALSE, library = "GUDHI"),
                 iterations = num.iterations)
    # TDA - GUDHI (alpha complex)
  } else if (TDA.library == "GUDHIalpha") {
    time <- mark(alphaComplexDiag(pointdata, maxdimension = featdim,
                                  location = FALSE, library = "GUDHI"),
                 iterations = num.iterations)
    # none of the above
  } else {
    stop("Invalid TDA engine selected")
  }
  
  # row 1 column 12 is list of times
  return(time[1,12])
}

#####measure memory old version#####
#point data input is required. program that is calculated is based off 
#text string. Dimensional features and iteration number for benchmark 
#should also be specified. Uses rips filtration function
memory <- function(pointdata, TDA.library, feature.dimensions) { 
  if (TDA.library == "Dionysus") { 
    filtrate <- ripsFiltration(pointdata, maxdimension = feature.dimensions, maxscale = 5, 
                               library = "Dionysus")
    size <- object_size(filtrate[[1]])
    
  } else if (TDA.library == "GUDHI") {
    print("1")
    filtrate <- ripsFiltration(pointdata, maxdimension = feature.dimensions, maxscale = 5, 
                               library = "GUDHI")
    print("2")
    size <- object_size(filtrate[[1]])
    
  } else if (TDA.library == "GUDHIalpha") {
    print("1")
    filtrate <- alphaComplexFiltration(pointdata, 
                                       library = "GUDHI")
    print("2")
    size <- object_size(filtrate[[1]])
    
  } else {
    stop("Choose 'Dionysus', 'GUDHI', or 'GUDHIalpa'. Cannot use TDAstats")
  }
  
  return(size)
}

#####combined function#####
TDA_bench <- function(measure, data.type, data.dimensions, num.points,
                      feature.dimensions, TDA.library, num.iteration, file.name) {
  print(paste("Starting", measure, data.type, data.dimensions, num.points,
              feature.dimensions, TDA.library, Sys.time()))
  
  str.measure <- paste(measure)
  str.data.type <- paste(data.type)
  str.TDA.library <- paste(TDA.library)
  str.file.name <- paste(file.name)
  
  if (feature.dimensions > data.dimensions) {
    stop("Feature dimensions must be less than data dimensions")
  } else
    
    #step 1, generate the dataset
    if (data.type == "circle") {
      pointdata <- unifcircle(num.points, data.dimensions)
    } else if (data.type == "uniform") {
      pointdata <- unifbox(num.points, data.dimensions)
    } else if (data.type == "annulus") {
      pointdata <- noisycircle(num.points, data.dimensions)
    } else if (data.type == "torus") {
      pointdata <- torus(num.points)
    } else {
      stop("Invalid data type")
    }
  
  # step 2 benchmark, write all times to a csv
  if (measure == "time") {
    exec.time <- bench(pointdata, TDA.library,
                       feature.dimensions, num.iteration)
    exec.time.list <- unlist(exec.time[[1]])
    appnd <- tibble()
    row  <- c(str.measure, str.data.type, data.dimensions, num.points, 
              feature.dimensions, str.TDA.library, exec.time.list)
    row.apnd <- rbind(appnd, row)
    write_csv(row.apnd, path = str.file.name, na = "NA", append = TRUE)
  } else if (measure == "memory") {
    mem.data <- memory(pointdata, TDA.library,
                       feature.dimensions)
    row.apnd  <- as_tibble(cbind(str.measure, str.data.type, data.dimensions, num.points, 
                                 feature.dimensions, str.TDA.library, mem.data))
    write_csv(row.apnd, path = str.file.name, na = "NA", append = TRUE)
  } else stop("Select either 'memory' or 'time' as measurement")
}
####

#Create circle, zero the y so that the circle is looking normal
circle.2d <- unifcircle(200, 2)
circle.2d <- cbind(circle.2d, 0)
circle.2d <- circle.2d[,c(1,3,2)]

#Save Plot As Image
png(filename = "circle.png",
    width = 1.36*5, height = 1*5, units = "in", res = 300)

scatterplot3d(circle.2d, pch = 20, 
              grid=F, box=F,
              axis = F, angle = 0, highlight.3d = T)
dev.off()


#Create sphere
circle.3d <- unifcircle(900, 3)

#Save Plot As Image
png(filename = "sphere.png",
    width = 1.16*5, height = 1*5, units = "in", res = 300)

scatterplot3d(circle.3d, pch = 20, 
              grid=F, box=F,
              axis = F, angle = 5, highlight.3d = T)

dev.off()


#Create Annulus, zero the y so that the circle is looking normal
annulus.2d <- noisycircle(400, 2)
annulus.2d <- cbind(annulus.2d, 0)
annulus.2d <- annulus.2d[,c(1,3,2)]

#Save Plot As Image
png(filename = "2annulus.png",
    width = 1.26*5, height = 1*5, units = "in", res = 300)

scatterplot3d(annulus.2d, pch = 20, 
              highlight.3d = T,  
              grid=F, box=F,
              axis = F, angle = 0)
dev.off()


#Create 3d annulus
annulus.3d <- noisycircle(1300, 3)

#Save Plot As Image
png(filename = "3annulus.png",
    width = 1.16*5, height = 1*5, units = "in", res = 300)

scatterplot3d(annulus.3d, pch = 20, 
              grid=F, box=F,
              axis = F, angle = 5, highlight.3d = T)

dev.off()


#Create torus, rotate it so it actually looks like a torus
torus.3d <-   torusUnif(2400, 1.8, 5)
torus.3d <- rotate(torus.3d[,1], torus.3d[,2], torus.3d[,3], 
                   degrx = 45, degry = 0, degrz = 0, 
                   pivotx = NA_real_,
                   pivoty = NA_real_, pivotz = NA_real_)

#Save Plot As Image
png(filename = "torus.png",
    width = 1.36*5, height = 1*5, units = "in", res = 300)

scatterplot3d(torus.3d, pch = 20, 
              highlight.3d = T,
              grid=F, box=F,
              axis = F, angle = 0)

dev.off()

#Create 2d box, add 0 to y axis to make it look normal
box.2d <- unifbox(350, 2)
box.2d <- cbind(box.2d, 0)
box.2d <- box.2d[,c(1,3,2)]

#Save Plot As Image
png(filename = "square.png",
    width = 1.36*5, height = 1*5, units = "in", res = 300)
scatterplot3d(box.2d, pch = 20, 
              highlight.3d = T,  
              grid=F, box=F,
              axis = F, angle = 0)
dev.off()


#Create 3d box
box.3d <- unifbox(4000, 3)
box.3d <- rotate(box.3d[,1], box.3d[,2], box.3d[,3], 
                 degrx = 30, degry = 60, degrz = 30, 
                 pivotx = NA_real_,
                 pivoty = NA_real_, pivotz = NA_real_)

#Save Plot As Image
png(filename = "cube.png",
    width = 1.16*5, height = 1*5, units = "in", res = 300)
box.3d.plot <- scatterplot3d(box.3d, pch = 20, 
                             grid=F, box=F,
                             axis = F, angle = 5, highlight.3d = T)
dev.off()


library(magick)

####Raster the point cloud images on ggplot####
#Reading in Point Clouds and Cropping them
torus <- image_read("torus.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x410+225+150")

circle <- image_read("circle.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x450+225+125")

sphere <- image_read("sphere.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x525+225+150")

annulus.2 <- image_read("2annulus.png") %>% 
  image_scale("1200") %>% 
  image_crop("525x450+315+240")

annulus.3 <- image_read("3annulus.png") %>% 
  image_scale("1400") %>% 
  image_crop("525x525+410+320")

square <- image_read("square.png") %>% 
  image_scale("900") %>% 
  image_crop("525x450+175+75")

cube <- image_read("cube.png") %>% 
  image_scale("1000") %>% 
  image_crop("525x475+225+200")


#Read in Figure 3
fig1.unrast <- image_read("figure3.png")
#Combine Figure and Image
fig.1.rast <- image_composite(fig1.unrast, torus, offset = "+25+125", gravity = "northeast")
#Write Out File
image_write(fig.1.rast, path = 'figure3.png', format = 'png')


#Read in Figure 4
fig2.unrast <- image_read("figure4.png")
#Combine Figure and Image
fig2.rast1 <- circle %>% image_scale("200") %>%
  image_composite(fig2.unrast, ., offset = "+3225+205", 
                  gravity = "northeast")

fig2.rast2 <- sphere %>% image_scale("200") %>%
  image_composite(fig2.rast1, ., offset = "+1700+205", 
                  gravity = "northeast")

#Write Out File
image_write(fig2.rast2, path = 'figure5.png', format = 'png')

#Figure_5a
#fig5a.unrast <- image_read("figure5a.png")
#Combine Figure and Images
#fig5a.rast1 <- annulus.3 %>% image_scale("200") %>%
#  image_composite(fig5a.unrast, ., offset = "+1875+175", 
#                  gravity = "northeast")

#fig5a.rast2 <- sphere %>% image_scale("190") %>%
#  image_composite(fig5a.rast1, ., offset = "+720+175", 
#                  gravity = "northeast")

#fig5a.rast3 <- torus %>% image_scale("210") %>%
#  image_composite(fig5a.rast2, ., offset = "+1865+850", 
#                  gravity = "northeast")

#fig5a.rast4 <- cube %>% image_scale("210") %>%
#  image_composite(fig5a.rast3, ., offset = "+710+850", 
#                  gravity = "northeast")

#Write Out File
#image_write(fig5a.rast4, path = 'figure5a.png', format = 'png')


#Figure_5c
#fig5c.unrast <- image_read("figure5c.png")
#Combine Figure and Images
#fig5c.rast1 <- annulus.3 %>% image_scale("150") %>%
#  image_composite(fig5c.unrast, ., offset = "+1850+280", 
#                  gravity = "northeast")

#fig5c.rast2 <- sphere %>% image_scale("150") %>%
#  image_composite(fig5c.rast1, ., offset = "+410+550", 
#                  gravity = "northeast")

#fig5c.rast3 <- torus %>% image_scale("150") %>%
#  image_composite(fig5c.rast2, ., offset = "+1875+1300", 
#                  gravity = "northeast")

#fig5c.rast4 <- cube %>% image_scale("150") %>%
#  image_composite(fig5c.rast3, ., offset = "+325+1375", 
#                  gravity = "northeast")

#fig5c.rast5 <- annulus.2 %>% image_scale("150") %>%
#  image_composite(fig5c.rast4, ., offset = "+1850+775", 
#                  gravity = "northeast")

#fig5c.rast6 <- circle %>% image_scale("150") %>%
#  image_composite(fig5c.rast5, ., offset = "+400+830", 
#                  gravity = "northeast")

#fig5c.rast7 <- square %>% image_scale("150") %>%
#  image_composite(fig5c.rast6, ., offset = "+325+1910", 
#                  gravity = "northeast")

#image_write(fig5c.rast7, path = 'figure5c.png', format = 'png')

#Figure 6#
fig6.unrast <- image_read("figure6.png")
#Combine Figure and Images
fig6.rast1 <- annulus.3 %>% image_scale("185") %>%
  image_composite(fig6.unrast, ., offset = "+1975+225", 
                  gravity = "northeast")

fig6.rast2 <- sphere %>% image_scale("185") %>%
  image_composite(fig6.rast1, ., offset = "+820+225", 
                  gravity = "northeast")

fig6.rast3 <- torus %>% image_scale("185") %>%
  image_composite(fig6.rast2, ., offset = "+1965+900", 
                  gravity = "northeast")

fig6.rast4 <- cube %>% image_scale("185") %>%
  image_composite(fig6.rast3, ., offset = "+810+900", 
                  gravity = "northeast")
fig6.rast4

image_write(fig6.rast4, path = 'figure6.png', format = 'png')




#Other Figures
#These don't need rasterizing
#fig3 <- image_read("fig3.png") 
#image_write(fig3, path = 'fig3.png', format = 'png')

#fig4 <- image_read("fig4.png") 
#image_write(fig4, path = 'fig4.png', format = 'png')

#fig5b <- image_read("fig5b.png") 
#image_write(fig5b, path = 'fig5b.png', format = 'png')

#Combine Figure 5 b and c into one grid
#library(png)
#library(grid)
#library(gridExtra)

#plot2 <- readPNG('figure5b.png')
#plot3 <- readPNG('figure5c.png')

#png("figure5t.png", width = 6, height = 4, 
#    units = "in", res = 450)
#grid.arrange(
#  rasterGrob(plot2, interpolate = T), 
#  rasterGrob(plot3, interpolate = T), 
#  nrow=1)
#dev.off()

#Cropping White Space
#Crop white space for Intro Alpha
alpha <- image_read("figure2.png") 
alpha <- image_crop(alpha, "2700x1040", gravity = "North")
alpha <- image_crop(alpha, "2700x710", gravity = "South")
image_write(alpha, path = 'figure2.png', format = 'png')

#Crop white space for Figure 5t
fig5 <- image_read("figure5t.png") 
fig5 <- image_crop(fig5, "2700x1400", gravity = "North")
fig5 <- image_crop(fig5, "2700x1020", gravity = "South")
image_write(fig5, path = 'figure5t.png', format = 'png')

#####measure memory - new version; uncomment to re-measure memory#####
#library(dplyr)

# setup data to collect
#data_types <- c("circle", "annulus", "uniform", "torus")
#dims <- 2:4
#num_pts <- seq(from = 25, to = 250, by = 25)
#engines <- c("TDAstats", "GUDHI", "Dionysus")
#df <- expand.grid(data_types, dims, num_pts, engines,
#                  stringsAsFactors = FALSE) %>%
#  filter(!(Var1 == "torus" & Var2 != 3)) # torus has to have dimension 3

# output bash script to individualize sessions
#vapply(X = seq_len(nrow(df)),
#       FUN.VALUE = character(1),
#       FUN = function(curr_row) {
#         paste("Rscript",
#               "individual-measure.R",
#               paste(df[curr_row, ], collapse = " "))
#       }) %>%
  # rep(10) %>%
#  paste(collapse = "\n") %>%
#  cat("\n", file = "measure-mem.sh")

### NOW run measure-mem.sh (just created above, will create memory-use.csv)

# THEN run this
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(readr))

# input data
df_mem <- read.csv("memory-use.csv") %>%
  mutate(Memory = Memory / 1024)

# setup df
pl_exponents <- expand.grid(c("TDAstats", "GUDHI", "Dionysus"),
                            2:4,
                            c("circle", "annulus", "torus", "uniform"),
                            stringsAsFactors = FALSE) %>%
  filter(Var3!="torus" | Var2 == 3) %>%
  dplyr::rename(Engine = Var1,
                Dimension = Var2,
                Shape = Var3)

# return power law exponent from data
calc_pl_exp <- function(all_data, engine, dim, shape) {
  # only >= 100 points
  all_data <- all_data %>%
    filter(Engine == engine &
             Dimension == dim &
             DataType == shape) %>%
    mutate(LogPts = log10(NumPoints),
           LogMem = log10(Memory))
  
  lm_mod <- lm(LogMem ~ LogPts, data = all_data)
  tidy_mod <- tidy(lm_mod)
  
  return(tidy_mod$estimate[2])
}

calc_pl_exp_summ <- function(size, value) {
  log_pts <- log10(size)
  log_mem <- log10(value)
  
  lm_mod <- lm(log_mem ~ log_pts)
  tidy_mod <- tidy(lm_mod)
  
  return(tidy_mod$estimate[2])
}

# add calculated values to pl_exponents as an additional column
exp_vec <- vapply(X = seq_len(nrow(pl_exponents)),
                  FUN.VALUE = numeric(1),
                  FUN = function(row_num) {
                    calc_pl_exp(df_mem,
                                pl_exponents$Engine[row_num],
                                pl_exponents$Dimension[row_num],
                                pl_exponents$Shape[row_num])
                  }
)
pl_exponents <- pl_exponents %>%
  mutate(Exp = exp_vec)

#####fig 7#####
df_mem <- df_mem %>%
  mutate(Dimension = factor(Dimension, levels = 2:4, labels = c("2-dim", "3-dim", "4-dim")))

ggplot(df_mem, aes(x = NumPoints, y = Memory, color = Engine, shape = Engine)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  facet_grid(DataType ~ Dimension) +
  xlab("Number of points") +
  ylab("Memory (GB)") +
  theme_bw()

ggsave("figure7.png")

#####fig 8#####
get_runtime_data <- function(shape) {
  filepath <- paste0(shape,
                     ".csv")
  
  # read and transform data
  read_csv(filepath) %>%
    select(-c(X1, V1)) %>%
    dplyr::rename(Shape = V2,
                  Dimension = V3,
                  Size = V4,
                  Calc_Dim = V5,
                  Engine = V6) %>%
    filter(Calc_Dim == Dimension - 1) %>%
    filter(Engine != "GUDHIalpha") %>%
    pivot_longer(cols = paste0("V", 7:16)) %>%
    select(-c(name, Calc_Dim)) %>%
    group_by(Shape, Dimension, Size, Engine) %>%
    summarise(Value = median(value)) %>%
    mutate(Engine = gsub(x = Engine,
                         pattern = "stats",
                         replacement = "TDAstats")) %>%
    dplyr::rename(DataType = Shape)
}

# collect data
annulus <- get_runtime_data("annulus")
circle <- get_runtime_data("circle")
torus <- get_runtime_data("torus")
uniform <- get_runtime_data("uniform")

runtime_data <- rbind(annulus, circle, torus, uniform) %>%
  filter(Dimension %in% 2:4) %>%
  group_by(Engine, Dimension, DataType) %>%
  summarise(Exp = calc_pl_exp_summ(Size, Value)) %>%
  mutate(Type = "runtime")

memory_data <- pl_exponents %>%
  dplyr::rename(DataType = Shape) %>%
  mutate(Type = "memory")

combo_data <- rbind(runtime_data, memory_data) %>%
  mutate(Engine = gsub(Engine, pattern = "TDAstats", replacement = "Ripser"))

# plot big O exponents for memory
ggplot(combo_data, aes(x = Dimension, y = Exp, fill = Engine)) +
  facet_grid(Type ~ DataType) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Big-O exponent") +
  scale_y_continuous() +
  scale_fill_manual(values = c("red", "blue", "brown")) +
  theme_bw()

ggsave("figure8.png")
