# ~~How this works~~
# First, when you knit the file 'penguins.Rmd'
# This script is read in in the chunk named `load-ext-chunks`
# From there, anytime you see a chunk with no code and perhaps only chunk options,
# it is evaluating the code between named "chunks" here.
# In general, chunks with the `*-web` name pattern are the interactive versions of the same
# graphic produced by the same-named (i.e., `*`) chunk.
# For example, `pca-web` is the interactive HTML version of `pca`.
# The actual name of the plot objects made in each chunk don't really matter-
# although we have tried to be consistent with `-int` as the suffix for the interactive version.
# Chunks with the `*-plotly` suffix are kept for posterity but not used in the manuscript.

## ---- penguin-pairs ---------------------------------------------------------
### Pairs plots for penguins and iris (with ggpairs)

# Function to specify opacity for ggpairs plots
# Thanks to: https://stackoverflow.com/questions/34975190/set-alpha-and-remove-black-outline-of-density-plots-in-ggpairs
ggpairs_alpha <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(..., alpha = 0.8, color = NA)
}

# Penguin pairs plot:

penguin_pairs <- penguins %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  select(species, where(is.numeric)) %>%
  ggpairs(aes(color = species, shape = species, fill = species, text = paste("Species: ", species)),
          diag = list(continuous = ggpairs_alpha),
          columns = c("flipper_length_mm", "body_mass_g",
                      "bill_length_mm", "bill_depth_mm"),
          columnLabels = c("Flipper length (mm)","Body mass (g)", "Bill length (mm)", "Bill depth (mm)"),
          upper = list(continuous = wrap("cor", size = 2.7)),
          lower = list(continuous = wrap(ggally_points, size = 1.3, alpha = 0.8))) +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  scale_fill_paletteer_d("colorblindr::OkabeIto") +
  scale_shape_manual(values = c(15,16,17)) +
  theme(
    text = element_text(size = 9),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA)
  )
penguin_pairs

## ---- iris-pairs ---------------------------------------------------------

# Iris pairs plot:

iris_pairs <- iris %>%
  ggpairs(aes(color = Species,
              fill = Species,
              shape = Species,
              text = paste("Species: ", Species)),
          diag = list(continuous = ggpairs_alpha),
          columns = c("Petal.Length", "Petal.Width",
                      "Sepal.Length", "Sepal.Width"),
          columnLabels = c("Petal length (cm)","Petal width (cm)", "Sepal length (cm)", "Sepal width (cm)"),
          upper = list(continuous = wrap("cor", size = 2.7, color = "black")),
          lower = list(continuous = wrap(ggally_points, size = 1.3, alpha = 0.8))) +
  scale_colour_manual(values = c("gray70","gray40","black")) +
  scale_fill_manual(values = c("gray70","gray40","black")) +
  theme(
    text = element_text(size = 9),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray80"),
    panel.border = element_rect(color = "gray80", fill = NA)
  )
iris_pairs

## ---- linear ---------------------------------------------------------

# Penguin linear relationships (flipper length versus body mass):

penguin_linear_base <-
  penguins %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  theme(legend.position = c(0.2,0.85)) +
  labs(x = "Flipper length (mm)",
       y = "Body mass (g)") +
  theme(plot.title.position = "plot",
        #panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70"),
        legend.title = element_blank())

penguin_linear <-
  penguin_linear_base +
  geom_point(aes(color = species, shape = species), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", aes(group = species, color = species), se = FALSE, show.legend = FALSE)


# To compare without penguin species as a variable (not in manuscript):
penguin_linear_all <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "gray50", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Flipper length (mm)",
       y = "Body mass (g)") +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70"))

# Iris linear relationships (petal dimensions):
iris_linear_base <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  scale_color_manual(values = c("gray70","gray40","black")) +
  theme(legend.position = c(0.2, 0.85)) +
  labs(x = "Petal length (cm)",
       y = "Petal width (cm)") +
  theme(plot.title.position = "plot",
        #panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70"),
        legend.title = element_blank())

iris_linear <- iris_linear_base +
  geom_point(aes(color = Species, shape = Species), size = 2, alpha = 0.8) +
  geom_smooth(aes(group = Species, color = Species), method = "lm", se = FALSE, show.legend = FALSE)

# To compare without iris species as a variable (not in manuscript):
iris_linear_all <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(color = "gray50", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Petal length (cm)",
       y = "Petal width (cm)") +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, color = "gray70"))

# Combine and save image:
linear_example <- (penguin_linear + iris_linear) + plot_annotation(tag_levels = 'A')
linear_example

# ggsave(here("fig","linear_example.png"), width = 6, height = 3)

## ---- linear-plotly -------------------------------------------------------------------------------

a <- list(
  text = "A",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

b <- list(
  text = "B",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

linear_figa <- ggplotly(penguin_linear,
                        height=500,
                        tooltip = c("x","y","colour")) %>%
  layout(annotations = a)

linear_figb <- ggplotly(iris_linear,
                        height=500,
                        tooltip = c("x","y","colour")) %>%
  layout(annotations = b)

subplot(linear_figa, linear_figb,
        nrows = 1,
        titleX = TRUE,
        titleY = TRUE,
        margin = 0.09,
        widths = c(0.5, 0.5)) %>%
  style(showlegend = FALSE)

## ---- linear-web -------------------------------------------------------------------------------

penguins_tooltip <- c(str_c("Flipper length (mm) = ", penguins$flipper_length_mm,
                           "\n Body mass (g) = ", penguins$body_mass_g,
                           "\n Species = ", penguins$species))

penguin_linear_int <-
  penguin_linear_base +
  geom_point_interactive(aes(color = species,
                             shape = species,
                             tooltip = penguins_tooltip,
                             data_id = species),
                         size = 2,
                         alpha = .8
                         ) +
  geom_smooth_interactive(aes(group = species,
                              color = species,
                              data_id= species),
                          method = lm,
                          se = FALSE
                          )

# girafe(ggobj = penguin_int)

iris_tooltip <- c(str_c("Petal length = ", iris$Petal.Length,
                        "\n Petal width = ", iris$Petal.Width,
                        "\n Species = ", iris$Species))

iris_linear_int <-
  iris_linear_base +
  geom_point_interactive(aes(color = Species,
                             shape = Species,
                             tooltip = iris_tooltip,
                             data_id = Species),
                         size = 2,
                         alpha = .8
                         ) +
  geom_smooth_interactive(aes(group = Species,
                              color = Species,
                              data_id= Species),
                          method = lm,
                          se = FALSE
  )

# girafe(ggobj = iris_int)

girafe(code = print(penguin_linear_int + iris_linear_int + plot_annotation(tag_levels = 'A')),
       width_svg = 8,
       height_svg = 4,
       options = list(
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "opacity:1; filter: brightness(90%);")
       ))

## ---- simpsons -------------------------------------------------------------------------------
# Simpson's Paradox example (bill dimensions, omitting species):
simpson_nospecies_base <- penguins %>%
  # doing this so ggiraph recognizes species across plots
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species)
  ) %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  theme(panel.border = element_rect(fill = NA, color = "gray70")) +
  labs(x = "Bill length (mm)", y = "Bill depth (mm)")

simpson_nospecies <- simpson_nospecies_base +
  geom_point(color = "gray40", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black")

# Bill dimensions, including species:
simpson_wspecies_base <-
  penguins %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species)
    ) %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, group = species)) +

  scale_color_paletteer_d("colorblindr::OkabeIto") +
  theme(panel.border = element_rect(fill = NA, color = "gray70")) +
  labs(x = "Bill length (mm)", y = "Bill depth (mm)") +
  guides(color = guide_legend("Species"),
           shape = guide_legend("Species"))

simpson_wspecies <-
  simpson_wspecies_base +
  geom_point(aes(color = species, shape = species), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species), show.legend = FALSE)

# Combining into a compound figure:
simpson_gg <- (simpson_nospecies | simpson_wspecies) + plot_annotation(tag_levels = 'A')
simpson_gg
#ggsave(here("fig","simpson_gg.png"), width = 6, height = 2.5, dpi = 500)

## ---- simpsons-plotly -------------------------------------------------------------------------------

simpson_figa <- ggplotly(simpson_nospecies,
                        height=500,
                        tooltip = c("x","y")) %>%
  layout(annotations = a)

simpson_figb <- ggplotly(simpson_wspecies,
                        height=500,
                        tooltip = c("x","y","colour")) %>%
  style(showlegend = FALSE, traces = 4:6) %>%
  layout(annotations = b)

subplot(simpson_figa, simpson_figb,
        nrows = 1,
        titleX = TRUE,
        titleY = TRUE,
        margin = 0.09,
        widths = c(0.5, 0.5))

## ---- simpsons-web -------------------------------------------------------------------------------

nospecies_tooltip <- c(str_c("Bill length (mm) = ", penguins$bill_length_mm,
                            "\n Bill depth (g) = ", penguins$bill_depth_mm,
                            "\n Species = ", penguins$species))

simpson_nospecies_int <-
  simpson_nospecies_base +
  geom_point_interactive(aes(tooltip = nospecies_tooltip,
                             data_id = species),
                         size = 2,
                         alpha = 0.8) +
  geom_smooth_interactive(method = lm,
                          se = FALSE,
                          color = "black"
  )

# girafe(ggobj = simpson_nospecies_int)

wspecies_tooltip <- c(str_c("Bill length (mm) = ", penguins$bill_length_mm,
                             "\n Bill depth (g) = ", penguins$bill_depth_mm,
                             "\n Species = ", penguins$species))

simpson_wspecies_int <-
  simpson_wspecies_base +
  geom_point_interactive(aes(color = species,
                             shape = species,
                             tooltip = wspecies_tooltip,
                             data_id = species),
                         size = 2,
                         alpha = 0.8) +
  geom_smooth_interactive(aes(color= species,
                              data_id= species),
                          method = lm,
                          se = FALSE
  )

# girafe(ggobj = simpson_wspecies_int)

girafe(code = print(simpson_nospecies_int + simpson_wspecies_int + plot_annotation(tag_levels = 'A')),
       width_svg = 8,
       height_svg = 4,
       options = list(
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "opacity:1; filter: brightness(90%);")
       ))


## ---- pca ---------------------------------------------------------
# From Alison Hill's post for palmerpenguins pkgdown site

### PENGUINS PCA:

# Omit year
penguins_noyr <- penguins %>%
  select(-year) %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  mutate(species = as.factor(species))

penguin_recipe <-
  recipe(~., data = penguins_noyr) %>%
  update_role(species, island, sex, new_role = "id") %>%
  step_naomit(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

penguin_pca <-
  penguin_recipe %>%
  tidy(id = "pca")

penguin_percvar <- penguin_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance")

# Make the penguins PCA biplot:

# Get pca loadings into wider format
pca_wider <- penguin_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

# define arrow style:
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

penguins_juiced <- juice(penguin_recipe)

# Make the penguins PCA biplot:
pca_plot <-
  penguins_juiced %>%
  ggplot(aes(PC1, PC2)) +
  coord_cartesian(
    xlim = c(-3, 4),
    ylim = c(-2.5, 2))  +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  guides(color = guide_legend("Species"),
        shape = guide_legend("Species")) +
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "gray70", fill = NA))
# For positioning (above):
# 1: bill_length
# 2: bill_depth
# 3: flipper length
# 4: body mass

penguins_biplot <- pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_point(aes(color = species, shape = species),
             alpha = 0.7,
             size = 2) +
  geom_shadowtext(data = pca_wider,
                  aes(x = PC1, y = PC2, label = terms),
                  nudge_x = c(0.7,0.7,1.7,1.2),
                  nudge_y = c(-0.1,-0.2,0.1,-0.1),
                  size = 4,
                  color = "black",
                  bg.color = "white")


penguin_screeplot_base <- penguin_percvar %>%
  ggplot(aes(x = component, y = value)) +
  scale_x_continuous(limits = c(0, 5), breaks = c(1,2,3,4), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  ylab("% of total variance") +
  theme(panel.border = element_rect(color = "gray70", fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

penguin_screeplot <- penguin_screeplot_base +
  geom_col(fill = "gray50") +
  geom_text(aes(label = round(value,2)), vjust=-0.25)

### IRIS PCA:

iris_recipe <-
  recipe(~., data = iris) %>%
  update_role(Species, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()
iris_pca <-
  iris_recipe %>%
  tidy(id = "pca")

### Iris PCA biplot:

# Get pca loadings into wider format
iris_wider <- iris_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

# define arrow style:
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

iris_juiced <- juice(iris_recipe)

# Make the iris PCA biplot:
iris_pca_plot <-
  iris_juiced %>%
  ggplot(aes(PC1, PC2)) +
  coord_cartesian(
    xlim = c(-3, 3),
    ylim = c(-3, 3)) +
  scale_colour_manual(values = c("gray70","gray40","black")) +
  theme(panel.background = element_rect(fill = NA, color = "gray70"),
        legend.position = "bottom")

iris_biplot <- iris_pca_plot  +
  geom_point(aes(color = Species, shape = Species),
             alpha = 0.8,
             size = 2) +
  geom_segment(data = iris_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_shadowtext(data = iris_wider,
                  aes(x = PC1, y = PC2, label = terms),
                  nudge_x = c(0.5,0.3,1,1.2),
                  nudge_y = c(-0.1,-0.2,0.1,-0.1),
                  size = 4,
                  color = "black",
                  bg.color = "white")


# 1:
# 2:
# 3: Petal.Length

iris_percvar <- iris_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance")

# Iris screeplot:

iris_screeplot_base <- iris_percvar %>%
  ggplot(aes(x = component, y = value)) +
  scale_x_continuous(limits = c(0, 5), breaks = c(1,2,3,4), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  ylab("% of total variance") +
  theme(panel.border = element_rect(color = "gray70", fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

iris_screeplot <- iris_screeplot_base +
  geom_col(fill = "gray50") +
  geom_text(aes(label = round(value,2)), vjust=-0.25)

# Combine biplots and screeplots for both iris & penguins:

(penguins_biplot | iris_biplot) / (penguin_screeplot | iris_screeplot) + plot_annotation(tag_levels = 'A')

#ggsave(here("fig","pca_plots.png"), width = 8, height = 8, dpi = 500)

## ---- pca-plotly ---------------------------------------------------------

pca_penguins_plotly <- plot_ly(juice(penguin_recipe),
               x = ~PC1,
               y = ~PC2,
               color = ~juice(penguin_recipe)$species,
               colors = as.character(paletteer_d("colorblindr::OkabeIto", n = 3)),
               type = 'scatter',
               mode = 'markers') %>%
  layout(
    legend=list(title=list(text='color')),
    plot_bgcolor = "#fff",
    xaxis = list(
      title = "0"),
    yaxis = list(
      title = "1"))

penguin_features <- pull(pca_wider[1])
for (i in seq(4)){
  pca_penguins_plotly <- pca_penguins_plotly %>%
    add_segments(x = 0, xend = pull(pca_wider[i, 2]), y = 0, yend = pull(pca_wider[i, 3]), line = list(color = 'black'),inherit = FALSE, showlegend = FALSE) %>%
    add_annotations(x=pull(pca_wider[i, 2]), y=pull(pca_wider[i, 3]), ax = 0, ay = 0,text = penguin_features[i], xanchor = 'center', yanchor= 'bottom')
}

pca_penguins_plotly

pca_iris_plotly <- plot_ly(juice(iris_recipe),
                               x = ~PC1,
                               y = ~PC2,
                               color = ~juice(iris_recipe)$Species,
                               colors = c("gray70","gray40","black"),
                               type = 'scatter',
                               mode = 'markers') %>%
  layout(
    legend=list(title=list(text='color')),
    plot_bgcolor = "#fff",
    xaxis = list(
      title = "0"),
    yaxis = list(
      title = "1"))

iris_features <- pull(iris_wider[1])
for (i in seq(4)){
  pca_iris_plotly <- pca_iris_plotly %>%
    add_segments(x = 0, xend = pull(iris_wider[i, 2]), y = 0, yend = pull(iris_wider[i, 3]), line = list(color = 'black'),inherit = FALSE, showlegend = FALSE) %>%
    add_annotations(x=pull(iris_wider[i, 2]), y=pull(iris_wider[i, 3]), ax = 0, ay = 0,text = iris_features[i], xanchor = 'center', yanchor= 'bottom')
}

pca_iris_plotly

## ---- pca-web ---------------------------------------------------------


penguins_biplot_tooltip <- str_c("Species = ", penguins_juiced$species,
                             "\n PC1 = ", round(penguins_juiced$PC1, 2),
                             "\n PC2 = ", round(penguins_juiced$PC2, 2))

penguins_biplot_int <- pca_plot +
  geom_segment_interactive(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_point_interactive(aes(color = species, shape = species,
                             tooltip = penguins_biplot_tooltip,
                             data_id = species),
             alpha = 0.7,
             size = 2) +
  geom_shadowtext(data = pca_wider,
                  aes(x = PC1, y = PC2, label = terms),
                  nudge_x = c(0.7,0.7,1.7,1.2),
                  nudge_y = c(-0.1,-0.2,0.1,-0.1),
                  size = 4,
                  color = "black",
                  bg.color = "white")


penguin_screeplot_int <- penguin_screeplot_base +
  geom_col_interactive(aes(tooltip = round(value,2)), fill = "gray50") +
  geom_text_interactive(aes(label = round(value,2)), vjust=-0.25, check_overlap = TRUE)

# girafe(ggobj = penguins_biplot_int)

iris_biplot_tooltip <- str_c("Species = ", iris_juiced$Species,
                               "\n PC1 = ", round(iris_juiced$PC1, 2),
                               "\n PC2 = ", round(iris_juiced$PC2, 2))

iris_biplot_int <- iris_pca_plot +
  geom_segment_interactive(data = iris_wider,
                           aes(xend = PC1, yend = PC2),
                           x = 0,
                           y = 0,
                           arrow = arrow_style) +
  geom_point_interactive(aes(color = Species, shape = Species,
                             tooltip = iris_biplot_tooltip,
                             data_id = Species),
                         alpha = 0.7,
                         size = 2) +
  geom_shadowtext(data = iris_wider,
                  aes(x = PC1, y = PC2, label = terms),
                  nudge_x = c(0.5,0,1,1),
                  nudge_y = c(-0.1,-0.2,0.2,-0.1),
                  size = 4,
                  color = "black",
                  bg.color = "white")

# girafe(ggobj = iris_biplot_int)

iris_screeplot_int <- iris_screeplot_base +
  geom_col_interactive(aes(tooltip = round(value,2)), fill = "gray50") +
  geom_text_interactive(aes(label = round(value,2)), vjust=-0.25, check_overlap = TRUE)



girafe(code = print((penguins_biplot_int | iris_biplot_int) /
                      (penguin_screeplot_int | iris_screeplot_int) +
                      plot_annotation(tag_levels = 'A')
                    ),
       width_svg = 8,
       height_svg = 8,
       options = list(
         opts_hover_inv(css = "opacity:0.2;"),
         opts_hover(css = "opacity:1; filter: brightness(90%);")
       ))


## ---- kmeans ---------------------------------------------------------

# TWO VARIABLE k-means comparison
# Penguins: Bill length vs. bill depth
pb_species <- penguins %>%
  select(species, starts_with("bill")) %>%
  drop_na() %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  mutate(species = as.factor(species))

# Prep penguins for k-means:
pb_nospecies <- pb_species %>%
  select(-species) %>%
  recipe() %>%
  step_normalize(all_numeric()) %>%
  prep() %>%
  juice()

# Perform k-means on penguin bill dimensions (k = 3, w/20 centroid starts)

# Save augmented data
set.seed(100)
pb_clust <-
  pb_nospecies %>%
  kmeans(centers = 3, nstart = 20) %>%
  broom::augment(pb_species)

# Get counts in each cluster by species
pb_clust_n <- pb_clust %>%
  count(species, .cluster) %>%
  pivot_wider(names_from = species, values_from = n, names_sort = TRUE) %>%
  arrange(.cluster) %>%
  replace_na(list(`Adelie` = 0))

### Iris k-means
# Iris: petal length vs. petal width

# Select only petal dimensions:
ip_species <- iris %>%
  select(Species, starts_with("Petal"))

# Remove species factor & scale petal dimensions:
ip_nospecies <- ip_species %>%
  select(-Species) %>%
  recipe() %>%
  step_normalize(all_numeric()) %>%
  prep() %>%
  juice()

# Perform k-means on iris petal dimensions (k = 3, w/20 centroid starts):
set.seed(100)
ip_clust <-
  ip_nospecies %>%
  kmeans(centers = 3, nstart = 20) %>%
  broom::augment(ip_species)

# Get iris counts in each cluster by species:
ip_clust_n <- ip_clust %>%
  count(Species, .cluster) %>%
  pivot_wider(names_from = Species, values_from = n, names_sort = TRUE) %>%
  arrange(.cluster) %>%
  replace_na(list(`setosa` = 0, `versicolor` = 0, `virginica` = 0))


# Plot penguin k-means clusters:
# make a base plot b/c https://github.com/plotly/plotly.R/issues/1942
pb_kmeans_base <-
  pb_clust %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  scale_fill_paletteer_d("colorblindr::OkabeIto") +
  scale_x_continuous(limits = c(30, 60),
                     breaks = c(30, 40, 50, 60)) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "gray70")) +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Species")
# ggpubr::stat_chull(aes(fill = .cluster, color = .cluster),
# alpha = 0.5, geom = "polygon", show.legend = FALSE)

pb_kmeans_gg <- pb_kmeans_base +
  geom_text(aes(label = .cluster,
                color = species),
            key_glyph = draw_key_rect,
            check_overlap = TRUE)

# Plot iris k-means clusters:
# make a base plot b/c https://github.com/plotly/plotly.R/issues/1942
ip_kmeans_base <-
  ip_clust %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, color = "gray70")) +
  scale_color_manual(values = c("gray70","gray50","black"))  +
  labs(x = "Petal length (cm)",
       y = "Petal width (cm)")
# ggpubr::stat_chull(aes(fill = .cluster, color = .cluster),
# alpha = 0.5, geom = "polygon", show.legend = FALSE)

ip_kmeans_gg <- ip_kmeans_base +
  geom_text(aes(label = .cluster),
            key_glyph = draw_key_rect,
            check_overlap = TRUE)

# Combine k-means plots for penguins & iris:
(pb_kmeans_gg | ip_kmeans_gg) + plot_annotation(tag_levels = "A")

# Save image:
# ggsave(here("fig","kmeans.png"), width = 8, height = 4.5, dpi = 500)

## ---- kmeans-plotly ---------------------------------------------------------

# Plot penguin k-means clusters:
pb_kmeans_plotly <- pb_kmeans_base +
  geom_text(aes(label = .cluster,
                color = species,
                text = paste("Species: ", species,
                             "\nCluster: ", .cluster,
                             "\nBill length (mm): ", bill_length_mm,
                             "\nBill depth (mm): ", bill_depth_mm)
                ),
            size = 3)

pb_kmeans_plotly <- ggplotly(pb_kmeans_plotly, height = 300, tooltip = "text") %>%
  layout(annotations = a)

# Plot iris k-means clusters:
ip_kmeans_plotly <- ip_kmeans_base +
  geom_text(aes(label = .cluster,
                color = Species,
                text = paste("Species: ", Species,
                             "\nCluster: ", .cluster,
                             "\nPetal Length (cm): ", Petal.Length,
                             "\nPetal width (cm): ", Petal.Width)
                ),
            size = 3)

ip_kmeans_plotly <- ggplotly(ip_kmeans_plotly, height = 300, tooltip = "text")  %>%
  layout(annotations = b)

# Combine k-means plots for penguins & iris:
subplot(pb_kmeans_plotly, ip_kmeans_plotly,
        nrows = 1,
        titleX = TRUE,
        titleY = TRUE,
        margin = 0.09) %>%
  layout() %>%
  style(showlegend = FALSE)

## ---- kmeans-web ---------------------------------------------------------

pb_tooltip <- str_c("Cluster: ", pb_clust$.cluster,
                      "\n Species: ", pb_clust$species,
                      "\n Bill length (mm): ", pb_clust$bill_length_mm,
                      "\n Bill depth (mm) : ", pb_clust$bill_depth_mm)

pb_kmeans_int <- pb_kmeans_base +
  geom_text_interactive(aes(
    label = .cluster,
    color = species,
    tooltip = pb_tooltip,
    data_id = species),
    size = 3,
    check_overlap = TRUE)

# pb_kmeans_int <- pb_kmeans_base +
#   geom_point_interactive(aes(
#     #label = .cluster,
#     color = .cluster,
#     shape = species,
#     tooltip = pb_tooltip,
#     data_id = .cluster),
#     alpha = .8,
#     size = 2)

# girafe(ggobj = pb_kmeans_int)

ip_tooltip <- str_c("Cluster: ", ip_clust$.cluster,
                      "\n Species: ", ip_clust$Species,
                      "\n Petal Length (cm): ", ip_clust$Petal.Length,
                      "\n Petal Width (cm): ", ip_clust$Petal.Width)

ip_kmeans_int <- ip_kmeans_base +
  geom_text_interactive(aes(
    label = .cluster,
    color = Species,
    tooltip = ip_tooltip,
    data_id = Species),
    size = 3,
    check_overlap = TRUE)

# girafe(ggobj = ip_kmeans_int)

girafe(code = print(pb_kmeans_int + ip_kmeans_int + plot_annotation(tag_levels = 'A')),
       width_svg = 8,
       height_svg = 4,
       options = list(
         opts_hover_inv(css = "opacity:0.1;"),
         opts_hover(css = "opacity:1; filter: brightness(90%);")
       ))
