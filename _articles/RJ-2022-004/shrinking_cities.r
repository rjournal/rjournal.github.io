### Auxilliary R script used to generate plots for the article
# "Measuring the Extent and Patterns of Urban Shrinkage for Small Towns Using R"
# by Cristiana Vîlcea, Liliana Popescu and Alin Clincea

options(width = 190) # allow longer lines to prevent wrapping of output in console

library("ggplot2") # already included via ggrepel
library("ggrepel")
library("gridExtra")
library("readxl")

data_file = "research_data.xlsx"

### Screeplots #################################
# Screeplot given the year, using the research data source
plot_data <- function(year) {
  indices <- read_excel(.GlobalEnv$data_file,
                        sheet = paste("data_", year, sep = ""))

  pca <-
    prcomp(indices[2:nrow(indices), 2:ncol(indices)], scale = TRUE)

  screeplot(pca,
            col = "#FFE6CC",
            border = "#D79B00",
            main = year)
  abline(
    h = 1,
    col = "black",
    lty = "dashed",
    lwd = 1
  )
}

# Prepare side by side display and then plot
layout(matrix(1:2, ncol = 2))
plot_data(1992)
plot_data(2018)
#################################################

### Biplots #####################################
# Prepares a graphic object to be plotted, given a proper data source
prepare_biplot_graphic <- function(sheet) {
  # Import data and prepare the data frame
  # Note: read_csv() returns a ready to use data frame while
  # read_excel() result needs to have the first column set as row names
  data <- read_excel(.GlobalEnv$data_file, sheet = sheet)
  data <- data.frame(names = data[, 1], data[, 2:ncol(data)])
  rownames(data) <- data[, 1]
  data[, 1] <- NULL

  # Assign k indices labels to columns
  colnames(data) = c("k1", "k2", "k3", "k4", "k5", "k6", "k7",
                     "k8", "k9", "k10", "k11", "k12", "k13")

  # Perform PCA using base function - prcomp()
  pca <- prcomp(data[, 1:ncol(data)], scale = TRUE)
  summary <- summary(pca)
  pca_loadings <- data.frame(t(cor(pca$x, data[, 1:ncol(data)])))
  pca_scores <- data.frame(pca$x)

  # Output function data (used in a table in the article; also in the .xlsx file)
  print(sheet)
  print(summary)
  print(pca_loadings)
  print(pca_scores)
  print("---------------------------------------")

  # Prepare plot
  graphic <- ggplot()
  graphic <- graphic + geom_point(data = pca_scores,
                                  aes(
                                    x = PC1,
                                    y = PC2,
                                    size = PC3,
                                    alpha = PC4
                                  ),
                                  colour = "#ED7D31")

  # ggrepel is used to avoid label overlapping
  # points for towns (scores)
  graphic <- graphic + geom_text_repel(
    data = pca_scores,
    aes(
      x = PC1,
      y = PC2,
      label = rownames(pca_scores)
    ),
    size = 3,
    colour = "#595959"
  )

  # segments for variation directions (loadings)
  graphic <- graphic + geom_text_repel(
    data = pca_loadings,
    aes(
      x = PC1 * 4,
      y = PC2 * 4,
      label = rownames(pca_loadings)
    ),
    size = 3.3,
    colour = "#2F5597"
  )

  graphic <- graphic + geom_segment(
    data = pca_loadings,
    aes(
      x = 0,
      y = 0,
      xend = (PC1 * 4),
      yend = (PC2 * 4)
    ),
    colour = "#2F5597",
    arrow = arrow(
      angle = 20,
      length = unit(0.15, "inches"),
      type = "open"
    ),
    alpha = 0.7,
    size = 1
  )

  # Labels and theme settings
  graphic <- graphic +
    xlab(paste("PC1 (", round(summary$importance[2] * 100, 1), "%)", sep = ""))
  graphic <- graphic +
    ylab(paste("PC2 (", round(summary$importance[5] * 100, 1), "%)", sep = ""))

  graphic <- graphic + theme_bw(base_size = 12)
  graphic <- graphic + theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) + ggtitle(sub(".*?(\\d{4}).*?", "\\1", sheet))

  return(graphic)
}

# Plot the graphics side by side
grid.arrange(prepare_biplot_graphic("data_1992"),
             prepare_biplot_graphic("data_2018"),
             ncol = 2)
#################################################

### Scatterplot #################################
# Import data
research_data <-
  read_excel(.GlobalEnv$data_file, sheet = "csi_values")

# Plot multiple series as dots
graphic <-
  ggplot(research_data, aes(x = Towns)) +
  geom_point(aes(y = `1992`, colour = "1992"), size = 3) +
  geom_point(aes(y = `2018`, colour = "2018"), size = 3) +
  geom_point(aes(y = `2030`, colour = "2030"), size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 0,
    vjust = 0.5
  )) +
  labs(x = "Towns", y = "Values", color = NULL)

print(graphic)
#################################################

### RDC plotted #################################
# Prepares a graphic object to be plotted, given a proper data source
prepare_rdc_graphic <- function(sheet) {
  data <- read_excel(.GlobalEnv$data_file, sheet = sheet)

  graphic <- ggplot(data, aes(x = ageing, y = shrinking)) +
    geom_point(aes(color = type), size = 3)

  n = "\n"
  quadrant_labels <- data.frame(
    x = c(2.5, -2.5, -2.5, 2.5),
    y = c(3, 3, -3, -3),
    text = c(
      paste("Type I:", "Faster ageing", "Shrinking/Lower growth", sep = n),
      paste("Type II:", "Slower ageing", "Shrinking/Lower growth", sep = n),
      paste("Type III:", "Slower ageing", "Higher growth", sep = n),
      paste("Type IV:", "Faster ageing", "Higher growth", sep = n)
    )
  )

  graphic <- graphic + geom_text(data = quadrant_labels,
                                 aes(x, y, label = text), size = 3)
  graphic <- graphic +
    scale_x_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
    scale_y_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0))

  graphic <- graphic + theme_bw(base_size = 10)
  graphic <- graphic + theme(legend.position = "none",
                             plot.title = element_text(hjust = 0.5)) +
    ggtitle(sub(".*_(.*?)", "\\1", sheet))

  return(graphic)
}

# Plot the graphics side by side
grid.arrange(prepare_rdc_graphic("rdc_future"),
             prepare_rdc_graphic("rdc_overall"),
             ncol = 2)
#################################################
