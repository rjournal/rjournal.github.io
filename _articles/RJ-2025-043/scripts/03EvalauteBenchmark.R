#03EvalauteBenchmark.R
Comment <- "03EvalauteBenchmark.R"

indir <- file.path(getwd(),"01Transformierte/mac")
load(file.path(indir,"BaselinePerformance.rda"))


Base <- lapply(1:length(BaselinePerformance), function(i,x) {
  x <- x[[i]]
  return(cbind(i,x[,1],x[,5]-x[,4],x[,5]))
},BaselinePerformance)
BaselinePerformance_Mat <- do.call(rbind,Base)

load(file.path(indir,"SharedObjectPerformance_v2.rda"))

load(file.path(indir,"memsharePerformance.rda"))

mem_at_end_SharedObject-mem_idle_SharedObject
mem_at_end_memshare-mem_idle_memshare
mem_at_end_base-mem_idle_base


CompareShare <- lapply(1:length(SharedObjectPerformance), function(i,x) {
  x <- x[[i]]
  return(cbind(i,x[,1],x[,5]-x[,4],x[,5]))
},SharedObjectPerformance)
CompareShareMat <- do.call(rbind,CompareShare)



# Scatter plot submitted to R jorunal and provided on Arxiv ----
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
Size  <-  c(rep(c("10^1", "10^2", "10^3", "10^4","10^4.2","10^4.5","10^4.7", "10^5"), 2))
Source  <-  c(rep("SharedObject", 8), rep("memshare", 8))
names(Size)  <-  c(1:8,11:18)
names(Source)  <-  c(1:8,11:18)

df <- data.frame(
  x = Compare[,2],
  y = Compare[,3],
  label = Compare[,1]
)
df$label <- as.factor(df$label)
df$size <- Size[as.character(df$label)]
df$source <- Source[as.character(df$label)]

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
  "10^4.7" = "darkred",
  "10^5" = "blue"
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

obj2 <- ggplot(df, aes(x = DataVisualizations::SignedLog(x), y = DataVisualizations::SignedLog(y))) +
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

# Error bar plot with line plot in the revised version of R Journal ----
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
amad_val <- function(x,na.rm){
  if(isTRUE(na.rm)){
    x <- x[is.finite(x)]
  }
  return (amad(x)$amad)
}
CompareMem <- lapply(1:length(memsharePerformance), function(i,x) {
  x <- x[[i]]
  return(cbind(i,x[,1],x[,5]-x[,4],x[,5]))
},memsharePerformance)
CompareMemMat <- do.call(rbind,CompareMem)

Compare <- rbind(CompareShareMat,CompareMemMat)
colnames(Compare) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
table(Compare[,1])
## --- mapping exponent -> Magnitude label (as in your Size vector) ----
exp_to_mag <- c(
  "1" = "10^1",
  "2" = "10^2",
  "3" = "10^3",
  "4" = "10^4",
  "5" = "10^4.2",
  "6" = "10^4.5",
  "7" = "10^4.7",
  "8" = "10^5"
)

## SharedObject: exponents 1..8
df_time_SO <- data.frame(
  method    = "SharedObject",
  exponent  = CompareShareMat[,1],
  magnitude = exp_to_mag[as.character(CompareShareMat[,1])],
  value     = CompareShareMat[,2],   # time (Diff_Sec)
  mem       = CompareShareMat[,3]    # memory diff
)

## memshare: exponents 1..8
df_time_MS <- data.frame(
  method    = "memshare",
  exponent  = CompareMemMat[,1],
  magnitude = exp_to_mag[as.character(CompareMemMat[,1])],
  value     = CompareMemMat[,2],
  mem       = CompareMemMat[,3]
)

## Baseline: exponents 1..8 
df_time_BL <- data.frame(
  method    = "Baseline",
  exponent  = BaselinePerformance_Mat[,1],
  magnitude = 10^as.numeric(names(BaselinePerformance)),#paste0("10^",names(BaselinePerformance)),
  value     = BaselinePerformance_Mat[,2],  # time
  mem       = BaselinePerformance_Mat[,3]   # memory diff
)

## Long tables for time and memory
df_time_long <- rbind(
  df_time_SO[,c("method","magnitude","value")],
  df_time_MS[,c("method","magnitude","value")]
)

df_mem_long <- rbind(
  df_time_SO[,c("method","magnitude","mem")],
  df_time_MS[,c("method","magnitude","mem")]
)
colnames(df_mem_long)[3] <- "value"   # same col name as df_time_long

MakeYmatrixForClassErrorbar <- function(df_long, MagnitudeLevels, MethodLevels) {
  df_long$magnitude <- factor(df_long$magnitude, levels = MagnitudeLevels)
  df_long$method    <- factor(df_long$method,    levels = MethodLevels)
  
  # maximum number of repetitions over all (method, magnitude)
  n_max <- max(table(df_long$magnitude, df_long$method))
  
  cols <- list()
  for (meth in MethodLevels) {
    for (mag in MagnitudeLevels) {
      v <- df_long$value[df_long$method == meth & df_long$magnitude == mag]
      if (length(v) == 0L) {
        v <- rep(NA_real_, n_max)
      } else if (length(v) < n_max) {
        v <- c(v, rep(NA_real_, n_max - length(v)))
      }
      cols[[paste(meth, mag, sep = "_")]] <- v
    }
  }
  
  Y <- do.call(cbind, cols)
  Xvalues <- MagnitudeLevels
  Cls     <- rep(seq_along(MethodLevels), each = length(MagnitudeLevels))
  
  list(Ymatrix = Y, Xvalues = Xvalues, Cls = Cls)
}

MagnitudeLevels <- c("10^1", "10^2", "10^3", "10^4",
                     "10^4.2", "10^4.5", "10^4.7", "10^5")
MethodLevels    <- c("SharedObject", "memshare")

## --- time ---
time_dat <- MakeYmatrixForClassErrorbar(df_time_long, MagnitudeLevels, MethodLevels)
Xvalues <- log(c(10^1,10^2,10^3,10^4,10^4.2,10^4.5,10^4.7,10^5),base = 10)
names(Xvalues) <- time_dat$Xvalues
CE_time <- DataVisualizations::ClassErrorbar(
  Xvalues      = Xvalues,
  Ymatrix      = time_dat$Ymatrix,
  Cls          = time_dat$Cls,
  ClassNames   = MethodLevels,
  SDfun = amad_val,
  ClassCols    = c("steelblue","orange"),
  ClassShape   = c(20, 18),
  main         = "",
  xlab         = "Magnitude",
  ylab         = "Time [s]",
  JitterPosition = 0,
  WhiskerWidth   = 0.15,
  Whisker_lwd    = 0.7,
  BW           = FALSE
)
p_time_full <- CE_time$ggobj +
  theme_minimal(base_size = FontSize) +
  ## x-axis with pretty 10^k labels (here only 10^1,10^2,10^3,10^4,10^5)
  scale_x_continuous(
    breaks = Xvalues[c(1:4, 8)],
    labels = as.expression(
      lapply(Xvalues[c(1:4, 8)], function(z) bquote(10^.(z)))
    )
  ) +
  ## Baseline as magenta line; also mapped to colour AND shape
  geom_line(
    data = df_time_BL,
    aes(
      x     = log(magnitude, base = 10),
      y     = value,
      colour = "Baseline",
      shape  = "Baseline"
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  ## Overwrite BOTH colour and shape scales so there is ONE legend
  scale_colour_manual(
    name   = "Type",
    values = c(
      "SharedObject" = "steelblue",
      "memshare"     = "orange",
      "Baseline"     = "magenta"
    )
  ) +
  scale_shape_manual(
    name   = "Type",
    values = c(
      "SharedObject" = 20,
      "memshare"     = 18,
      "Baseline"     = NA           # no point symbol for baseline
    ),
    guide  = guide_legend(
      override.aes = list(
        linetype = c("solid","blank", "blank"),  # Baseline as line
        size     = c( 1.2,3, 3)                   # point sizes / line size
      )
    )
  )

#p_time_full

## zoomed time (adjust ylim as needed)
p_time_zoom <- p_time_full +
  coord_cartesian(ylim = c(min(CE_time$Statistics$lower),
                           quantile(CE_time$Statistics$upper, 0.6)))


mem_dat <- MakeYmatrixForClassErrorbar(df_mem_long, MagnitudeLevels, MethodLevels)

Xvalues <- log(c(10^1,10^2,10^3,10^4,10^4.2,10^4.5,10^4.7,10^5),base = 10)
names(Xvalues) <- mem_dat$Xvalues
CE_mem <- DataVisualizations::ClassErrorbar(
  Xvalues      = Xvalues,
  Ymatrix      = mem_dat$Ymatrix,
  Cls          = mem_dat$Cls,
  ClassNames   = MethodLevels,
  SDfun = amad_val,
  ClassCols    = c("steelblue","orange"),
  ClassShape   = c(20, 18),
  main         = "",
  xlab         = "Magnitude",
  ylab         = "Memory overhead [MB]",
  JitterPosition = 0,
  WhiskerWidth   = 0.15,
  Whisker_lwd    = 0.7,
  BW           = FALSE
)

p_mem_full <- CE_mem$ggobj +  theme_minimal(base_size = FontSize) +
  ## x-axis with pretty 10^k labels (here only 10^1,10^2,10^3,10^4,10^5)
  scale_x_continuous(
    breaks = Xvalues[c(1:4, 8)],
    labels = as.expression(
      lapply(Xvalues[c(1:4, 8)], function(z) bquote(10^.(z)))
    )
  ) +
  ## Baseline as magenta line; also mapped to colour AND shape
  geom_line(
    data = df_time_BL,
    aes(
      x     = log(magnitude, base = 10),
      y     = mem,
      colour = "Baseline",
      shape  = "Baseline"
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  ## Overwrite BOTH colour and shape scales so there is ONE legend
  scale_colour_manual(
    name   = "Type",
    values = c(
      "SharedObject" = "steelblue",
      "memshare"     = "orange",
      "Baseline"     = "magenta"
    )
  ) +
  scale_shape_manual(
    name   = "Type",
    values = c(
      "SharedObject" = 20,
      "memshare"     = 18,
      "Baseline"     = NA           # no point symbol for baseline
    ),
    guide  = guide_legend(
      override.aes = list(
        linetype = c("solid","blank", "blank"),  # Baseline as line
        size     = c( 1.2,3, 3)                   # point sizes / line size
      )
    )
  )

p_mem_zoom <- p_mem_full +
  coord_cartesian(ylim = c(min(CE_mem$Statistics$lower),
                           quantile(CE_mem$Statistics$upper, 0.6)))

# helper to add panel label in top-left
add_panel_label <- function(p, label) {
  p + annotate(
    "text",
    x      = -Inf,
    y      = Inf,
    label  = label,
    hjust  = -0.1,      # nudge a bit right from -Inf
    vjust  =  1.1,      # nudge a bit down from Inf
    fontface = "bold",
  )
}

# add labels
p_time_full_lab <- add_panel_label(p_time_full, "A")
p_time_zoom_lab <- add_panel_label(p_time_zoom, "C (Zoom)")
p_mem_full_lab  <- add_panel_label(p_mem_full,  "B")
p_mem_zoom_lab  <- add_panel_label(p_mem_zoom,  "D (Zoom)")

DataVisualizations::Multiplot(
  p_time_full_lab,
  p_time_zoom_lab,
  p_mem_full_lab,
  p_mem_zoom_lab,
  ColNo = 2
)
##prepare an object for ktable ----




load(file.path(indir,"BaselinePerformanceParallel.rda"))
Base_par <- lapply(1:length(BaselinePerformanceParallel), function(i,x) {
  x <- x[[i]]
  return(data.frame(Exponent=names(BaselinePerformanceParallel)[i],x[,1],x[,5]-x[,4],x[,5]))
},BaselinePerformanceParallel)
BaselinePerformance_Par <- do.call(rbind,Base_par)
colnames(BaselinePerformance_Par) <- c("Exponent","Diff_Sec","MemoryDiff_MB","mem_after_call")
BaselinePerformance_Par$Type <- "Baseline Parallel"

load(file.path(indir,"BaselinePerformance.rda"))

Base <- lapply(1:length(BaselinePerformance), function(i,x) {
  x <- x[[i]]
  return(data.frame(Exponent <- names(BaselinePerformance)[i],x[,1],x[,5]-x[,4],x[,5]))
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

save(file=file.path(getwd(),"05OurPublication/data","DF_Results_mac.rda"),DF_Results,stat_amad,Comment)
