library(RaMS)
library(tidyverse)
library(data.table)
library(microbenchmark)
library(MSnbase)
library(Spectra)
BiocParallel::register(BiocParallel::SerialParam(stop.on.error = FALSE))

# Code for tab:MS1demo ----
msfile <- system.file("extdata", "LB12HL_AB.mzML.gz", package = "RaMS")
msdata <- grabMSdata(files = msfile, grab_what="MS1")
knitr::kable(head(msdata$MS1), format = "latex")

dim(msdata$MS1)

# Code for fig:baseRchrom ----
png(filename = "baseRchrom.png", width = 5.25, height=2, units = "in", res = 300)
par(mar=c(4.1, 4.1, 0.1, 0.1))

BPC <- tapply(msdata$MS1$int, msdata$MS1$rt, max)
plot(names(BPC), BPC, type="l", xlab = "Retention time")
par(mar=c(4.1, 4.1, 4.1, 4.1))

dev.off()

# Code for fig:ggplotchrom ----
BPC <- msdata$MS1 %>%
  group_by(rt) %>%
  summarize(BPC_int=max(int))
ggplot(BPC) + geom_line(aes(x=rt, y=BPC_int)) +
  labs(x="Retention time", y="BPC")

ggsave(filename = "ggplotchrom.png", plot = last_plot(), device = "png",
       width = 5.25, height = 2, units = "in", dpi = 300)

# Code for fig:speedsizecomp ----
# Warning: downloads 10 ~100MB files
set.seed(1)
n_files <- c(10, 5, 1)
n_repl <- 5

### Download files ----
base_dir <- tempdir()
base_url <- paste0("ftp://massive.ucsd.edu/MSV000080030/ccms_peak/",
                   "Forensic_study_80_volunteers/Forensic_Hands_mzXML/",
                   "Forensic_Hands_plate_1_mzXML/Sample/")
url_data <- RCurl::getURL(base_url) %>%
  data.frame(initcol=.) %>%
  separate_rows(initcol, sep = "\n") %>%
  mutate(filename=str_extract(initcol, "(?<=2017 ).*mzXML")) %>%
  slice_sample(n = max(n_files)) %>%
  mutate(final_url=paste0(base_url, filename))

filenames <- paste(base_dir, url_data$filename, sep = "\\")

Map(function(url_name, file_name) download.file(url_name, file_name, mode="wb",
                                                method = "libcurl"),
    url_data$final_url, filenames)

### Test load times ----
load_times <- lapply(n_files, function(n_files_i){
  to_load <- filenames[seq_len(n_files_i)]

  rams_method <- function()grabMSdata(files = to_load, grab_what = c("MS1", "metadata"))
  xinmem_method <- function()readMSData(files = to_load, msLevel. = 1, mode = "inMemory")
  xondisk_method <- function()readMSData(files = to_load, msLevel. = 1, mode = "onDisk")
  sps_mzr_method <- function()Spectra(to_load, source=MsBackendMzR())

  timing <- microbenchmark(rams_method(), xinmem_method(), xondisk_method(),
                           sps_mzr_method(), times = n_repl)
  cbind(timing, n_files=n_files_i)
}) %>% rbindlist()
saveRDS(load_times, file = "load_times.rds")

### Test load, subset, and plot times ----
subplot_times <- lapply(n_files, function(n_files_i){
  to_load <- filenames[seq_len(n_files_i)]

  rams_method <- function(){
    x_RaMS <- grabMSdata(files = to_load, grab_what = c("MS1", "metadata"))
    cmpd_data <- x_RaMS$MS1[mz%between%pmppm(432.2810, ppm = 20)]
    plot(cmpd_data$rt, cmpd_data$int, type="l")
  }
  xinmem_method <- function(){
    x_inmem <- readMSData(files = to_load, msLevel. = 1, mode = "inMemory")
    cmpd_data <- chromatogram(x_inmem, mz = pmppm(432.2810, ppm = 20))
    plot(cmpd_data)
  }
  xondisk_method <- function(){
    x_ondisk <- readMSData(files = to_load, msLevel. = 1, mode = "onDisk")
    cmpd_data <- chromatogram(x_ondisk, mz = pmppm(432.2810, ppm = 20))
    plot(cmpd_data)
  }
  getIntensities <- function(x, ...) {
    if (nrow(x)) {
      cbind(mz = NA_real_, intensity = x[, "intensity"])
    } else cbind(mz = NA_real_, intensity = NA_real_)
  }
  sps_mzr_method <- function(){
    sfs_filtered <- Spectra(to_load, source=MsBackendMzR()) |>
      filterMzRange(pmppm(432.2810, ppm = 20)) |>
      filterMsLevel(1)
    sfs_agg <- addProcessing(sfs_filtered, getIntensities)
    eic <- cbind(rt=rtime(sfs_agg), int=unlist(intensity(sfs_agg), use.names = FALSE))
    plot(eic[,"rt"], eic[,"int"], type="l")
  }

  timing <- microbenchmark(rams_method(), xinmem_method(), xondisk_method(),
                           sps_mzr_method(), times = n_repl)
  cbind(timing, n_files=n_files_i)
}) %>% rbindlist()
saveRDS(subplot_times, file = "subplot_times.rds")

### Test just plot times ----
justplot_times <- lapply(n_files, function(n_files_i){
  to_load <- filenames[seq_len(n_files_i)]

  x_RaMS <- grabMSdata(files = to_load, grab_what = c("MS1", "metadata"))
  x_inmem <- readMSData(files = to_load, msLevel. = 1, mode = "inMemory")
  x_ondisk <- readMSData(files = to_load, msLevel. = 1, mode = "onDisk")
  sps_mzr <- Spectra(to_load, source=MsBackendMzR())

  rams_method <- function(){
    cmpd_data <- x_RaMS$MS1[mz%between%pmppm(432.2810, ppm = 20)]
    plot(cmpd_data$rt, cmpd_data$int, type="l")
  }
  xinmem_method <- function(){
    cmpd_data <- chromatogram(x_inmem, mz = pmppm(432.2810, ppm = 20))
    plot(cmpd_data)
  }
  xondisk_method <- function(){
    cmpd_data <- chromatogram(x_ondisk, mz = pmppm(432.2810, ppm = 20))
    plot(cmpd_data)
  }
  getIntensities <- function(x, ...) {
    if (nrow(x)) {
      cbind(mz = NA_real_, intensity = x[, "intensity"])
    } else cbind(mz = NA_real_, intensity = NA_real_)
  }
  mzr_method <- function(){
    sfs_filtered <- sps_mzr |>
      filterMzRange(pmppm(432.2810, ppm = 20)) |>
      filterMsLevel(1)
    sfs_agg <- addProcessing(sfs_filtered, getIntensities)
    eic <- cbind(rt=rtime(sfs_agg), int=unlist(intensity(sfs_agg), use.names = FALSE))
    plot(eic[,"rt"], eic[,"int"], type="l")
  }

  timing <- microbenchmark(rams_method(), xinmem_method(), xondisk_method(),
                           mzr_method(), times = n_repl)
  cbind(timing, n_files=n_files_i)
}) %>% rbindlist()
saveRDS(justplot_times, file = "justplot_times.rds")

### Get object sizes ----
object_sizes <- lapply(n_files, function(n_files_i){
  to_load <- filenames[seq_len(n_files_i)]

  x_RaMS <- grabMSdata(files = to_load, grab_what = c("MS1", "metadata"))
  x_inmem <- readMSData(files = to_load, msLevel. = 1, mode = "inMemory")
  x_ondisk <- readMSData(files = to_load, msLevel. = 1, mode = "onDisk")
  sps_mzr <- Spectra(to_load, source=MsBackendMzR())

  og_size <- sum(file.size(to_load))

  sizes <-  c(RaMS = pryr::object_size(x_RaMS),
              in_mem = pryr::object_size(x_inmem),
              on_disk = pryr::object_size(x_ondisk),
              mzr = pryr::object_size(sps_mzr),
              mzXML_size = og_size)

  cbind(sizes, n_files=n_files_i) %>%
    as.data.frame() %>%
    rownames_to_column(var = "method")
}) %>% rbindlist()
saveRDS(object_sizes, file = "object_sizes.rds")

### Create plot ----
justplot_times <- readRDS("justplot_times.rds")
load_times <- readRDS("load_times.rds")
subplot_times <- readRDS("subplot_times.rds")
object_sizes <- readRDS("object_sizes.rds")

##### Time to read files in ----
file_read_gp <- load_times %>%
  mutate(n_files=factor(n_files)) %>%
  ggplot() +
  geom_boxplot(aes(x=n_files, y=time/1e+9, color=expr)) +
  scale_y_log10(breaks=c(1, 10, 60, 300),
                labels=c("One second", "Ten seconds",
                         "One minute", "Five minutes"),
                limits=c(1, 300)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 10)) +
  annotation_logticks(sides = "l", outside = TRUE,
                      long = unit(0.005, "npc"),
                      mid = unit(0.005, "npc"),
                      short = unit(0.005, "npc")) +
  ggtitle("Time required to load files") +
  labs(y=NULL, x="Number of files loaded") +
  coord_cartesian(clip = "off")


##### Time to read and plot a single chromatogram ----
readsubplot_gp <- subplot_times %>%
  mutate(n_files=factor(n_files)) %>%
  ggplot() +
  geom_boxplot(aes(x=n_files, y=time/1e+9, color=expr)) +
  scale_y_log10(breaks=c(1, 10, 60, 300),
                labels=c("One second", "Ten seconds",
                         "One minute", "Five minutes"),
                limits=c(1, 500)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 10)) +
  annotation_logticks(sides = "l", outside = TRUE,
                      long = unit(0.005, "npc"),
                      mid = unit(0.005, "npc"),
                      short = unit(0.005, "npc")) +
  ggtitle("Time to load, subset, and plot") +
  labs(y=NULL, x="Number of files loaded") +
  coord_cartesian(clip = "off")


##### Time to plot a single chromatogram ----
subplot_gp <- justplot_times %>%
  mutate(n_files=factor(n_files)) %>%
  ggplot() +
  geom_boxplot(aes(x=n_files, y=time/1e+9, color=expr)) +
  scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 60, 300),
                labels=c("1/100 second", "1/10 second", "One second",
                         "Ten seconds", "One minute", "Five minutes"),
                limits=c(0.01, 300)) +
  scale_color_discrete(labels=str_pad(c("RaMS", "MSnExp", "OnDiskMSnExp"),
                                      10, side = "right")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 10)) +
  annotation_logticks(sides = "l", outside = TRUE,
                      long = unit(0.005, "npc"),
                      mid = unit(0.005, "npc"),
                      short = unit(0.005, "npc")) +
  ggtitle("Time to subset and plot") +
  labs(y=NULL, x="Number of files loaded") +
  coord_cartesian(clip = "off")

##### Memory required ----
mem_levels <- c("RaMS", "in_mem", "on_disk", "mzr", "mzXML_size")
mem_gp <- object_sizes %>%
  mutate(method=factor(method, levels = mem_levels)) %>%
  ggplot(aes(x=factor(n_files), y=sizes,
             color=method, group=method)) +
  geom_line(lwd=1) +
  geom_point(size=2) +
  scale_color_manual(values = c(scales::hue_pal()(4), "black"),
                     labels=str_pad(c("RaMS", "MSnExp", "OnDiskMSnExp", "mzR backend",
                                      "True mzXML size"), 10, side = "right")) +
  scale_y_log10(labels=scales::label_bytes()) +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 10)) +
  ggtitle("Memory required") +
  labs(y=NULL, x="Number of files loaded") +
  annotation_logticks(sides = "l", outside = TRUE,
                      long = unit(0.005, "npc"),
                      mid = unit(0.005, "npc"),
                      short = unit(0.005, "npc")) +
  coord_cartesian(clip = "off")

##### Assemble with cowplot ----
library(cowplot)
base_plot <- plot_grid(
  file_read_gp,
  subplot_gp,
  readsubplot_gp,
  mem_gp + theme(legend.position = "none"),
  hjust = -1,
  nrow = 2,
  align = "v"
)
lab_plot <- ggdraw(add_sub(base_plot, label = "Number of files", size = 12))
legend <- get_legend(
  mem_gp + theme(
    legend.box.margin = margin(0, 0, 0, 12),
    legend.title = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = NA, color = NA),
    legend.spacing.x = unit(0.005, 'npc'))
)
plot_grid(legend, lab_plot, rel_heights = c(0.4, 3), ncol = 1)

ggsave(filename = "speedsizecomp.png", plot = last_plot(), device = "png",
       width = 5.5, height = 4.5, units = "in", dpi = 300)


### Clean up afterward ----
dev.off()
unlink(base_dir, recursive = TRUE)



# Recompile LaTeX doc with figures ----
tools::texi2pdf("RJwrapper.tex")



# Run checks with rjtools ----
library(rjtools)
initial_check_article(".")


