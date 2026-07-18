#remotes::install_github("numbats/woylier")
library(cowplot)
library(tourr)
library(woylier)
library(tidyverse)
data("sine_curve")

# this is tailored for this specific matrix format!
draw_text_matrix <- function(m){
  # first build vector with the lines
  v <- character(nrow(m))
  for(i in 1:nrow(m)){
    if (m[i,1] > 0)
      v1 <- paste0(" ", sprintf("%0.3f", m[i,1]))
    else
      v1 <- paste0(sprintf("%0.3f", m[i,1]))
    # Prevent -0.000
    if (v1 == "-0.000") v1 <- " 0.000"
    if (m[i,2] > 0)
      v2 <- paste0(" ", sprintf("%0.3f", m[i,2]))
    else
      v2 <- paste0(sprintf("%0.3f", m[i,2]))
    # Prevent -0.000
    if (v2 == "-0.000") v2 <- " 0.000"
    v[i] <- paste0(v1, "  ", v2)
  }
  # now arrange them in draw_text
  y_vals <- c(0.75, 0.65, 0.55, 0.45)
  draw_text(v, x = 0.5, y = y_vals, size = 10)
}

set.seed(2022)
p <- 4
base1 <- tourr::basis_random(p, d=2)
base2 <- matrix(c(0, 0, 1, 0, 0, 0, 0, 1), ncol = 2)
d <- as.matrix(sine_curve[,3:6])

# we can get givens path directly
pg <- givens_full_path(base1, base2, nsteps = 4)

# givens_full_path begins with one step beyond the first base
#path_givens <- array(dim=c(4, 2, 5))
#path_givens[,,1] <- as.array(base1)
#path_givens[,,2:5] <- pg
# fixed in github version
path_givens <- pg

# no such function for geodesic path, so need to use workaround
pt_geo <- save_history(d, planned_tour(list(base1, base2)))[,,1:2]
# Last base is repeated, so only take 1 and 2
path_geo <- interpolate(pt_geo, angle = proj_dist(base1, base2)/4)
# This should work now - with updated tourr package

all_plots <- NULL

for(i in 1:5){
  a_givens <- path_givens[,,i]
  a_geo <- drop(path_geo[,,i])
  # need to get rid of tour attributes
  attributes(a_geo) <- attributes(a_givens)
  p_givens <- d %*% a_givens
  p_geo <- d %*% a_geo
  gg_givens <- ggplot(as_tibble(p_givens, .name_repair=function(x){paste0("V",1:length(x))}), aes(V1, V2)) +
    geom_point() +
    theme_void() +
    theme(aspect.ratio=1,
          panel.border = element_rect(fill = NA, colour = "black"))
  mat_givens <- ggdraw() + draw_text_matrix(a_givens) +
    theme(aspect.ratio=1)
  gg_geo <- ggplot(as_tibble(p_geo, .name_repair=function(x){paste0("V",1:length(x))}), aes(V1, V2)) +
    geom_point() +
    theme_void() +
    theme(aspect.ratio=1,
          panel.border = element_rect(fill = NA, colour = "black"))
  mat_geo <- ggdraw() + draw_text_matrix(a_geo) +
    theme(aspect.ratio=1)
  if(is.null(all_plots)){
    all_plots <- list(mat_givens, gg_givens, mat_geo, gg_geo)
    next
  }
  all_plots <- append(all_plots, list(mat_givens, gg_givens, mat_geo, gg_geo))
}


plot_grid(plotlist = all_plots, ncol = 4)
ggsave("compare-paths.png")
