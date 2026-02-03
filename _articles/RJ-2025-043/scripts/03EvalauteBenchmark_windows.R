#03EvalauteBenchmark.R
Comment <- "03EvalauteBenchmark_windows.R"

indir <- file.path(getwd(),"01Transformierte/win")
load(file.path(indir,"BaselinePerformance_100trials.rda"))


Base <- lapply(1:length(BaselinePerformance), function(i,x) {
  x <- x[[i]]
  return(cbind(i,mean(x[,1]),mean(x[,5])-mean(x[,4]),mean(x[,5])))
},BaselinePerformance)
BaselinePerformance_Mat <- do.call(rbind,Base)

load(file.path(indir,"SharedObjectPerformance_v2.rda"))

load(file.path(indir,"memsharePerformance.rda"))

# mem_at_end_SharedObject-mem_idle_SharedObject
# mem_at_end_memshare-mem_idle_memshare
# mem_at_end_base-mem_idle_base


CompareShare <- lapply(1:length(SharedObjectPerformance), function(i,x) {
  x <- x[[i]]
  return(cbind(i,x[,1],x[,5]-x[,4],x[,5]))
},SharedObjectPerformance)
CompareShareMat <- do.call(rbind,CompareShare)

CompareMem <- lapply(1:length(memsharePerformance), function(i,x) {
  x <- x[[i]]
  return(cbind(i,x[,1],x[,5]-x[,4],x[,5]))
},memsharePerformance)
CompareMemMat <- do.call(rbind,CompareMem)

CompareMemMat[,1] <- CompareMemMat[,1]+10

Compare <- rbind(CompareShareMat,CompareMemMat)
colnames(Compare) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
table(Compare[,1])

##Prepare accordingly to ggplot2
Size  <-  c(rep(c("10^1", "10^2", "10^3", "10^4","10^4.2","10^4.5","10^4.7"), 2))
Source  <-  c(rep("SharedObject", length(memsharePerformance)), rep("memshare", length(SharedObjectPerformance)))
names(Size)  <-  c(1:(length(memsharePerformance)),11:(11+length(SharedObjectPerformance)-1))
names(Source)  <-  c(1:(length(memsharePerformance)),11:(11+length(SharedObjectPerformance)-1))

df <- data.frame(
  x = Compare[,2],
  y = Compare[,3],
  label = Compare[,1]
)
df$label <- as.factor(df$label)
df$size <- Size[as.character(df$label)]
df$source <- Source[as.character(df$label)]

# size_colors <- c(
#   "10^1" = "lightblue",
#   "10^2" = "darkgreen",
#   "10^3" = "gold",
#   "10^4" = "red",
#   "10^5" = "black"
# )

# Define explicit shapes for each Source
source_shapes <- c(
  "SharedObject" = 15,  # square
  "memshare" = 17   # triangle
)

library(ggplot2)
# Plot


BaselinePerformance_Mat[,1] <- BaselinePerformance_Mat[,1]+100
BaselinePerformance_df <- as.data.frame(BaselinePerformance_Mat)
colnames(BaselinePerformance_df) <- c("class","xbase","ybase","z")
BaselinePerformance_df$xbase <- BaselinePerformance_df$xbase
BaselinePerformance_df$ybase <- BaselinePerformance_df$ybase

size_colors <- c(
  "10^1" = "lightblue",
  "10^2" = "darkgreen",
  "10^3" = "gold",
  "10^4" = "orange",
  "10^4.2" = "darkorange",
  "10^4.5" = "red",
  "10^4.7" = "darkred"#,
 # "10^5" = "blue"
)

source_shapes <- c(
  "SharedObject" = 21,   # circle with fill + border
  "memshare" = 24        # triangle with fill + border
)
FontSize <- 26 #for 2000x1000 png, for screen decrease

obj1 <- ggplot(df, aes(x = DataVisualizations::SignedLog(x), y = DataVisualizations::SignedLog(y))) +
  geom_point(
    aes(fill = size, shape = source),
    size = 3, stroke = 0.7, colour = "black", alpha = 0.5
  ) +
  # Magnitude legend (colors)
  scale_fill_manual(
    name = "Magnitude",
    values = size_colors,
    guide = guide_legend(override.aes = list(colour = size_colors))
  ) +
  # Type legend (shapes)
  scale_shape_manual(
    name = "Type",
    values = source_shapes
  ) +
  # Baseline legend (line)
  geom_line(
   data = BaselinePerformance_df,
   aes(x = DataVisualizations::SignedLog(xbase), y = DataVisualizations::SignedLog(ybase), colour = "Baseline"),
   inherit.aes = FALSE,
   linewidth = 1
  ) +
  scale_colour_manual(
    name = "",   # legend title blank
    values = c("Baseline" = "magenta"),
    guide = guide_legend(override.aes = list(linetype = 1))
  ) +
  labs(
   # title = "Benchmark of SharedObject vs. memshare",
    x = "Time in log(s)",
    y = "Difference in total RSS in log(MB)"
  ) +
  theme_minimal(base_size = FontSize) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
  )#+ theme(plot.title = element_text(hjust = 0.5))#+xlim(0,0.1)
obj1

obj2=ggplot(df, aes(x = DataVisualizations::SignedLog(x), y = DataVisualizations::SignedLog(y))) +
  geom_point(
    aes(fill = size, shape = source),
    size = 3, stroke = 0.7, colour = "black", alpha = 0.5
  ) +
  # Magnitude legend (colors)
  scale_fill_manual(
    name = "Magnitude",
    values = size_colors,
    guide = guide_legend(override.aes = list(colour = size_colors))
  ) +
  # Type legend (shapes)
  scale_shape_manual(
    name = "Type",
    values = source_shapes
  ) +
  # Baseline legend (line)
  geom_line(
    data = BaselinePerformance_df,
    aes(x = DataVisualizations::SignedLog(xbase), y = DataVisualizations::SignedLog(ybase), colour = "Baseline"),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  scale_colour_manual(
    name = "",   # legend title blank
    values = c("Baseline" = "magenta"),
    guide = guide_legend(override.aes = list(linetype = 1))
  ) +
  labs(
    #title = "Benchmark of SharedObject vs. memshare",
    x = "Time in log(s)",
    y = "Difference in total RSS in log(MB)"
  ) +
  theme_minimal(base_size = FontSize) +  xlim(0,0.1)+ylim(0,2)+ theme(legend.position = "none")
  # theme(
  #   legend.position = "right",
  #   legend.box = "vertical",
  #   legend.title = element_text(size = 10),
  #   legend.text  = element_text(size = 9)
  # )+ theme(plot.title = element_text(hjust = 0.5))+

DataVisualizations::Multiplot(obj2,obj1,ColNo = 2)

##prepare an object for ktable ----

dbt_mad <- function (x) 
{
  if (is.vector(x)) {
    centerMad  <-  mad(x)
    leftMad  <-  mad(x[x < median(x)])
    rightMad  <-  mad(x[x > median(x)])
  }
  else {
    centerMad <- matrix(1, ncol = ncol(x))
    leftMad <- matrix(1, ncol = ncol(x))
    rightMad <- matrix(1, ncol = ncol(x))
    for (i in 1:ncol(x)) {
      centerMad[, i]  <-  mad(x[, i], na.rm = TRUE)
      leftMad[, i]  <-  mad(x[x < median(x[, i])], na.rm = TRUE)
      rightMad[, i]  <-  mad(x[x > median(x[, i])], na.rm = TRUE)
    }
  }
  return(list(centerMad = centerMad, leftMad = leftMad, rightMad = rightMad))
}
amad <- function (x) 
{
  adjfactor <- 1.3
  ergMad <- dbt_mad(x)
  amad <- ergMad$centerMad * adjfactor
  leftAmad <- ergMad$leftMad * adjfactor
  rightAmad <- ergMad$rightMad * adjfactor
  return(list(amad = amad, leftAmad = leftAmad, rightAmad = rightAmad))
}

load(file.path(indir,"BaselinePerformanceParallel.rda"))
Base_par <- lapply(1:length(BaselinePerformanceParallel), function(i,x) {
  x <- x[[i]]
  return(data.frame(Exponent=names(BaselinePerformanceParallel)[i],x[,1],x[,5]-x[,4],x[,5]))
},BaselinePerformanceParallel)
BaselinePerformance_Par <- do.call(rbind,Base_par)
colnames(BaselinePerformance_Par) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
BaselinePerformance_Par$Type <- "Baseline Parallel"

load(file.path(indir,"BaselinePerformance_100trials.rda"))

Base <- lapply(1:length(BaselinePerformance), function(i,x) {
  x <- x[[i]]
  return(data.frame(Exponent=names(BaselinePerformance)[i],mean(x[,1]),mean(x[,5])-mean(x[,4]),mean(x[,5])))
},BaselinePerformance)
BaselinePerformance <- do.call(rbind,Base)
colnames(BaselinePerformance) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
BaselinePerformance$Type <- "Baseline"

load(file.path(indir,"memsharePerformance.rda"))
CompareMem <- lapply(1:length(memsharePerformance), function(i,x) {
  x <- x[[i]]
  return(data.frame(names(memsharePerformance)[i],x[,1],x[,5]-x[,4],x[,5]))
},memsharePerformance)
CompareMemDF <- do.call(rbind,CompareMem)
colnames(CompareMemDF) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
CompareMemDF_med <- aggregate(cbind(Diff_Sec,MemoryDiff_MB,mem_after_call) ~Exponent,data=CompareMemDF,FUN=median,na.rm=T)
CompareMemDF_med$Type <- "memshare"

CompareMemDF_amad <- aggregate(cbind(Diff_Sec,MemoryDiff_MB,mem_after_call) ~Exponent,data=CompareMemDF,FUN=amad)
CompareMemDF_amad$Type <- "memshare"

load(file.path(indir,"SharedObjectPerformance_v2.rda"))
CompareShare <- lapply(1:length(SharedObjectPerformance), function(i,x) {
  x <- x[[i]]
  return(data.frame(Exponent=names(SharedObjectPerformance)[i],x[,1],x[,5]-x[,4],x[,5]))
},SharedObjectPerformance)
CompareShareDF <- do.call(rbind,CompareShare)
colnames(CompareShareDF) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
CompareShareDF_med <- aggregate(cbind(Diff_Sec,MemoryDiff_MB,mem_after_call) ~Exponent,data=CompareShareDF,FUN=median,na.rm=T)
CompareShareDF_med$Type <- "SharedObject"

CompareShareDF_amad <- aggregate(cbind(Diff_Sec,MemoryDiff_MB,mem_after_call) ~Exponent,data=CompareShareDF,FUN=amad)
CompareShareDF_amad$Type <- "SharedObject"

DF_Results <- rbind(CompareShareDF_med,CompareMemDF_med,BaselinePerformance,BaselinePerformance_Par)

stat_amad <- rbind(CompareShareDF_amad,CompareMemDF_amad,BaselinePerformance,BaselinePerformance_Par)

save(file=file.path(file.path(getwd(),"05OurPublication/data"),"DF_Results_Windows.rda"),DF_Results,stat_amad,Comment)
