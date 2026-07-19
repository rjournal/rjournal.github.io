# Code to reproduce Figure 1 trace plots for paper submission
# "hdtg: An R package for high-dimensional truncated normal simulation" to the R Journal.
# The input data files are outputs from hzz_rjournal.R and are also available in this repository.

# ---- load packages, set file paths ----
library(this.path)
library(ggplot2)

this_dir <- dirname(this.path())
output_folder <- file.path(this_dir, "outputs") # outputs from hzz_rjournal.R
set.seed(111)

# ---- plotting functions ----
plot_time_facets_ordered <- function(t, n, samples_list, dim, main_title, 
                                     digits = 0, time_multiplier = 1) {
  # Order methods from fastest to slowest
  t <- t[order(t$elapsed), ]
  
  fastest_time <- min(t$elapsed)
  plot_time <- fastest_time * time_multiplier
  
  # Create data frame with elapsed time labels
  plot_data <- data.frame()
  
  for (i in 1:nrow(t)) {
    sampler <- t$sampler[i]
    elapsed <- t$elapsed[i]
    
    # Get n100 value for this sampler
    n100_val <- n$n100[n$sampler == sampler]
    
    # Create label with elapsed time
    label <- paste0(sampler, " (t100 = ", round(elapsed, digits), "s)")
    
    # Samples to plot in plot_time seconds
    samples_raw <- n100_val * (plot_time / elapsed)
    samples_to_plot <- round(samples_raw)
    print(paste(sampler, "samples to plot:", samples_to_plot))
    
    time_per_iter <- elapsed / n100_val
    time_seq <- seq(0, plot_time, length.out = samples_to_plot)
    n_samples <- nrow(samples_list[[sampler]])
    
    # Ensure we don't try to plot more samples than available
    if (samples_to_plot > n_samples) {
      warning(paste("For sampler", sampler, 
                    "requested", samples_to_plot, 
                    "samples but only", n_samples, "available."))
      samples_to_plot <- n_samples
    }
    
    plot_data <- rbind(plot_data, data.frame(
      time = time_seq,
      value = samples_list[[sampler]][((n_samples) - samples_to_plot + 1):n_samples, dim],
      sampler_label = label,
      original_sampler = sampler,
      elapsed_time = elapsed
    ))
  }
  
  # Ensure order is maintained (fastest to slowest)
  ordered_labels <- paste0(t$sampler, " (t100 = ", round(t$elapsed, digits), "s)")
  plot_data$sampler_label <- factor(plot_data$sampler_label, 
                                    levels = ordered_labels)
  
  subtitle_text <- paste("Ordered by efficiency | All traces show", 
                          round(plot_time, digits), "seconds")
  
  # Plot
  ggplot(plot_data, aes(x = time, y = value)) +
    geom_line(color = "black", size = 0.5) +
    facet_wrap(~ sampler_label, ncol = 1) +
    labs(
      title = main_title,
      subtitle = subtitle_text,
      x = "Time (seconds)",
      y = bquote(x[.(dim)])
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank()
    ) + 
    coord_cartesian(xlim = c(0, plot_time)) 
}

# ---- plot 1: CS d=1600 ----
t <- readRDS(file.path(output_folder, "timingCSh_d1600.rds"))
n <- readRDS(file.path(output_folder, "essCS_d1600.rds"))
colnames(t)[1] <- "sampler"

samples_list <- list(
  HHMC  = readRDS(file.path(output_folder, "samplesCS_d1600_HHMC.rds")),
  ZHMC  = readRDS(file.path(output_folder, "samplesCS_d1600_ZHMC.rds")),
  ZNUTS = readRDS(file.path(output_folder, "samplesCS_d1600_ZNUTS.rds")),
  MET   = readRDS(file.path(output_folder, "samplesCS_d1600_MET.rds"))
)

p <- plot_time_facets_ordered(t, n, samples_list, "CS, d=1600", dim = sample(1:1600, 1), digits = 1, time_multiplier = 2)
p
ggsave("time_normalized_CS_d1600.png", p, width = 7, height = 4, dpi = 300)

# ---- plot 2: HIV d=400 ----
t <- readRDS(file.path(output_folder, "timingHIVh_d400.rds"))
n <- readRDS(file.path(output_folder, "essHIV_d400.rds"))
colnames(t)[1] <- "sampler"

samples_list <- list(
  HHMC  = readRDS(file.path(output_folder, "samplesHIV_d400_HHMC.rds")),
  ZHMC  = readRDS(file.path(output_folder, "samplesHIV_d400_ZHMC.rds")),
  ZNUTS = readRDS(file.path(output_folder, "samplesHIV_d400_ZNUTS.rds")),
  MET   = readRDS(file.path(output_folder, "samplesHIV_d400_MET.rds"))
)

p <- plot_time_facets_ordered(t, n, samples_list, "HIV, d=400", dim = sample(1:400, 1), digits = 1)
p
ggsave("time_normalized_HIV_d400.png", p, width = 7, height = 4, dpi = 300)

# ---- plot 3: HIV d=1600 ----
t <- readRDS(file.path(output_folder, "timingHIVh_d1600.rds"))
n <- readRDS(file.path(output_folder, "essHIV_d1600.rds"))
colnames(t)[1] <- "sampler"

sample_data <- list(
  HHMC  = readRDS(file.path(output_folder, "samplesHIV_d1600_HHMC.rds")),
  ZHMC  = readRDS(file.path(output_folder, "samplesHIV_d1600_ZHMC.rds")),
  ZNUTS = readRDS(file.path(output_folder, "samplesHIV_d1600_ZNUTS.rds"))
)

p <- plot_time_facets_ordered(t, n, sample_data, "HIV, d=1600", dim = sample(1:1600, 1))
p
ggsave("time_normalized_HIV_d1600.png", p, width = 7, height = 4, dpi = 300)

# ---- plot 4: LKJ d=1600 ----
t <- readRDS(file.path(output_folder, "timingLKJh_d1600.rds"))
n <- readRDS(file.path(output_folder, "essLKJ_d1600.rds"))
colnames(t)[1] <- "sampler"

sample_data <- list(
  HHMC  = readRDS(file.path(output_folder, "samplesLKJ_d1600_HHMC.rds")),
  ZHMC  = readRDS(file.path(output_folder, "samplesLKJ_d1600_ZHMC.rds")),
  ZNUTS = readRDS(file.path(output_folder, "samplesLKJ_d1600_ZNUTS.rds"))
)

p <- plot_time_facets_ordered(t, n, sample_data, "LKJ, d=1600", dim = sample(1:1600, 1))
p
ggsave("time_normalized_LKJ_d1600.png", p, width = 7, height = 4, dpi = 300)
