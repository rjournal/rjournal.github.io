## ---- load-pkg
library(ferrn)
library(tidyverse)
library(patchwork)
library(tourr)
files <- paste0("data/", list.files(("data")))
purrr::walk(.x = files, ~load((.x), env = globalenv()))

## ----tour-path
knitr::include_graphics("figs/tour-path.png")

## ---- toy-search
# p1 <- holes_2d_better_max_tries %>%
#   mutate(max_tries = 500) %>%
#   explore_trace_search(label.size = 0.03,label.padding = 0.1, segment.size = 0,  extend_lower = 0.99) +
#   scale_color_continuous_botanical()
#
# p2 <- holes_2d_better_random %>%
#   mutate(max_tries = 25) %>%
#   explore_trace_search(label.size = 0.01, label.padding = 0.1, segment.size = 0, extend_lower = 0.99) +
#   scale_color_continuous_botanical()
#
# p <- (p1 | p2) + plot_layout(widths = c(1.5,2)) & theme_bw() & theme(legend.position = "none")
#
# ggsave(p, filename = "toy-search.svg",
#        path = ("figs"), device = "svg",
#        width = 10, height = 5)

knitr::include_graphics("figs/toy-search.svg")

## ---- toy-interp
# p1 <- holes_2d_better_max_tries %>%
#   mutate(group = "Algorithm 1") %>%
#   explore_trace_interp(accuracy_x = 4) +
#   scale_color_continuous_botanical()
#
# p2 <- holes_2d_better_random %>%
#   mutate(group = "Algorithm 3") %>%
#   explore_trace_interp( accuracy_x = 14) +
#   scale_color_continuous_botanical()
#
# p <- (p1 | p2) & ylim(0.8, 0.96)
#
# ggsave(p, filename = "toy-interp.svg",
#        path = ("figs"), device = "svg",
#        width = 10, height = 5)

knitr::include_graphics("figs/toy-interp.svg")

## ----toy-pca
# p <- bind_rows(holes_1d_geo, holes_1d_better) %>%
#   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#                    index = tourr::holes(), raw_data = boa5)  %>%
#   explore_space_pca(group = method, details = TRUE,
#                     interp_size = 1) +
#   scale_color_discrete_botanical() +
#   theme(legend.text = element_text(size = "10pt"))
#
# ggsave(p, filename = "toy-pca.svg",
#        path = ("figs"))
knitr::include_graphics("figs/toy-pca.svg")


## ----toy-pca-animated
# dt <- dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#                    index = tourr::holes(), raw_data = boa5)
# ani <- dt %>%
#   explore_space_pca(group = method, animate = TRUE, interp_size = 3,
#                     theo_size = 45, start_size = 10, end_size = 20) +
#   theme(legend.position = "none") +
#   scale_color_continuous_botanical(palette = "fern")
# animate(ani, nframes = 100, device = "png",
#         renderer = file_renderer("anim/pca/",
#                                  prefix = "pca", overwrite = TRUE))

frames <- c("0001", "0038", "0046", "0079", "0086", "0100")
ani <- paste0(("anim/"), "pca/", "pca", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)


## ----toy-pca-animated-interactive
# ani <- dt %>%
#   explore_space_pca(group = method, animate = TRUE, interp_size = 1,
#                     theo_size = 20, start_size = 3, end_size = 5) +
#   scale_color_discrete_botanical(palette = "fern")
# anim_save(ani,
#           filename = ("anim/toy-pca-animated.gif"),
#           renderer = gifski_renderer(width = 500, height = 500))
knitr::include_graphics("anim/toy-pca-animated.gif")


## ----toy-tour
# prep <- prep_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), flip = TRUE,
#                         group = method, palette = botanical_palettes$fern[c(1,6)], axes = "bottomleft",
#                         point_size = 3, end_size = 8)
#
# set.seed(123456)
# render(
#   prep$basis,
#   tour_path = grand_tour(),
#   dev = "png",
#   display = display_xy(col = prep$col, cex = prep$cex, pch = prep$pch,
#                        edges = prep$edges, edges.col = prep$edges_col,
#                        axes = "bottomleft"),
#   rescale = FALSE,
#   frames = 100,
#   ("anim","tour", "tour%03d.png")
# )
#

frames <- c("001", "021", "056", "072", "079", "088")
ani <- paste0(("anim/"), "tour/", "tour", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

## ----toy-tour-interactive
# prep <- prep_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), flip = TRUE,
#                         group = method, palette = botanical_palettes$fern[c(1,6)], axes = "bottomleft",
#                         rand_size = 0.5, point_size = 2, end_size = 5)
# set.seed(123456)
# render_gif(
#   prep$basis,
#   tour_path = grand_tour(),
#   display = display_xy(col = prep$col, cex = prep$cex, pch = prep$pch,
#                        edges = prep$edges, edges.col = prep$edges_col,
#                        axes = "bottomleft"),
#   rescale = FALSE,
#   frames = 100,
#   gif_file = ("anim","tour.gif")
# )
knitr::include_graphics("anim/tour.gif")

## ----toy-torus
# generate 2D paths
# set.seed(123456)
# holes_2d_geo_3var <-
#   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_geodesic),
#              rescale = FALSE)
#
# # last <- get_best(holes_2d_geo_3var)$basis %>% .[[1]]
# #
# # set.seed(123456)
# # holes_2d_geo_3var_polish <-
# #   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
# #                                                   search_f =  search_polish),
# #              rescale = FALSE, start = last)
#
# set.seed(123456)
# holes_2d_better_3var <-
#   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
#                                                   search_f =  search_better),
#              rescale = FALSE)
#
# # last <- get_best(holes_2d_better_3var)$basis %>% .[[1]]
# #
# # set.seed(123456)
# # holes_2d_better_3var_polish <-
# #   animate_xy(boa6[, 1:3], tour_path = guided_tour(holes(), d = 2,
# #                                                   search_f =  search_polish),
# #              rescale = FALSE, start = last)
# #
# # save(holes_2d_better_3var, file = ("data", "holes_2d_better_3var.rda"))
# # save(holes_2d_geo_3var, file = ("data", "holes_2d_geo_3var.rda"))
# #
# generating random sphere
# proj_d <-  2 # 2D because then you have 2 orthogonal circles
# d <- 6
# n_point <- 5000
# set.seed(123456)
# random <- map(1:n_point, ~basis_random(n = d/proj_d,  d=proj_d)) %>%
#   purrr::flatten_dbl() %>% matrix(ncol = d, byrow = TRUE) %>% as_tibble()
#
#
# path_geo <- bind_rows(holes_2d_geo_3var) %>% get_interp() %>% get_basis_matrix() %>% as_tibble()
# path_better <- bind_rows(holes_2d_better_3var) %>% get_interp() %>% get_basis_matrix() %>% as_tibble()
#
# basis <- bind_rows(path_geo, path_better, random) %>%
#   mutate(id = as.factor(ifelse(row_number() > nrow(path_geo) + nrow(path_better), "random",
#                      ifelse(row_number() <= nrow(path_geo), "geodesic", "better"))),
#          cex = ifelse(id == "random", 0.5, 3),
#          cex = ifelse(row_number() == 1, 5, cex)) %>%
#   group_by(id) %>%
#   mutate(cex = ifelse(row_number() == max(row_number()) & id != "random", 7, cex)) %>%
#   ungroup()
#
# pal <- RColorBrewer::brewer.pal(3, "Dark2")
# pal <- c(botanical_pal(reverse = TRUE)(2), "grey60")
# col <- pal[basis$id]
#
# set.seed(123)
# render(
#   basis[,1:d],
#   tour_path = grand_tour(),
#   dev = "png",
#   display = display_xy(cex = basis$cex, col = col, axes = "off"),
#   rescale = FALSE,
#   frames = 100,
#   ("anim","torus", "torus%03d.png")
# )
#
frames <- c("001", "017", "064", "068", "075", "100")
ani <- paste0(("anim/"), "torus/", "torus", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl)

## ----toy-torus-interactive
# proj_d <-  2 # 2D because then you have 2 orthogonal circles
# d <- 6
# n_point <- 5000
# set.seed(123456)
# random <- map(1:n_point, ~basis_random(n = d/proj_d,  d=proj_d)) %>%
#   purrr::flatten_dbl() %>% matrix(ncol = d, byrow = TRUE) %>% as_tibble()
# path_geo <- holes_2d_geo_3var %>% get_interp() %>% get_basis_matrix() %>% as_tibble()
# path_better <- holes_2d_better_3var %>% group_by(tries) %>% filter(loop != max(loop)) %>% get_interp() %>% get_basis_matrix() %>% as_tibble()
#
# basis <- bind_rows(path_geo, path_better, random) %>%
#   mutate(id = as.factor(ifelse(row_number() > nrow(path_geo) + nrow(path_better), "random",
#                                ifelse(row_number() <= nrow(path_geo), "geodesic", "better"))),
#          cex = ifelse(id == "random", 0.5, 2),
#          cex = ifelse(row_number() == 1, 2, cex)) %>%
#   group_by(id) %>%
#   mutate(cex = ifelse(row_number() == max(row_number()) & id != "random", 5, cex)) %>%
#   ungroup()
#
# pal <- RColorBrewer::brewer.pal(3, "Dark2")
# pal <- c(botanical_pal(reverse = TRUE)(2), "grey60")
# col <- pal[basis$id]
#
# # render gif
# set.seed(123)
# render_gif(
#   basis[,1:d],
#   tour_path = grand_tour(),
#   display = display_xy(cex = basis$cex ,col = col,  axes = "off"),
#   rescale = FALSE,
#   frames = 100,
#   gif_file = ("anim","torus.gif")
# )
knitr::include_graphics("anim/torus.gif")


## ----interruption
# clean up interrupt_no nand interrupt_yes
# origin <- interrupt_no
#
# clean_before <- origin %>%
#   select(basis, index_val, info, method, tries, loop, id) %>%
#   mutate( id = row_number(),
#           loop = ifelse(id == 1, 1, loop),
#           method = ifelse(id == 1, "search_better", method),
#           tries = ifelse(id == 1, tries, tries + 1)) %>%
#   group_by(tries, info) %>%
#   mutate(loop = ifelse(is.na(loop), row_number(), loop)) %>%
#   ungroup()
#
# last <- clean_before %>% filter(info == "new_basis") %>% mutate(info = "interpolation")
#
# before <- bind_rows(clean_before, last) %>%
#   arrange(tries) %>%
#   mutate(search = ifelse(info == "interpolation", "interp", "search")) %>%
#   group_by(tries, search) %>%
#   mutate(loop = row_number()) %>%
#   ungroup() %>%
#   mutate(id = row_number()) %>%
#   dplyr::select(-search)
#
# origin2 <- interrupt_yes
#
# after <- origin2 %>%
#   select(basis, index_val, info, method, tries, loop, id) %>%
#   mutate( id = row_number(),
#           loop = ifelse(id == 1, 1, loop),
#           method = ifelse(id == 1, "search_better", method),
#           tries = ifelse(id == 1, tries, tries + 1),
#           search = ifelse(info == "interpolation", "interp", "search")) %>%
#   group_by(tries, search) %>%
#   mutate(loop = row_number()) %>%
#   ungroup() %>%
#   dplyr::select(-search)
#
#
# save(before, file = ("data", "before.rda"))
# save(after, file = ("data", "after.rda"))

# interp <- before %>% get_interp()
#
# p1_anno <- bind_rows(
#   interp %>% filter(tries == 4, info == "interpolation") %>%
#     filter(index_val == max(index_val)) %>%
#     mutate(anno = "interpolation basis"),
#   interp %>% get_interp_last %>% filter(tries %in% c(3, 4)) %>%
#     mutate(anno = c("current basis", "target basis"))
# ) %>% arrange(id)
# #
# p1 <- before %>%
#   explore_trace_interp(accuracy_y = 0.001) +
#   scale_color_continuous_botanical() +
#   geom_hline(data = get_best(after) %>% mutate(id = 78), aes(yintercept = index_val), color = "grey90") +
#   ggrepel::geom_label_repel(data = p1_anno, aes(label = anno), box.padding = 0.5, alpha = 0.5) +
#   ggtitle("without interruption")
#
# p2 <- after %>%
#   explore_trace_interp(accuracy_y = 0.001) +
#
#   ggtitle("with interruption") +
#   scale_color_continuous_botanical()
#
# p <- (p1 | p2) & ylim(0.8, 0.9)
#
# ggsave(p, filename = "interrupt.svg",
#        path = ("figs"), width = 10, height = 5)

knitr::include_graphics("figs/interrupt.svg")

## ----polish
# set.seed(123456)
# holes_2d_better <-
#   animate_xy(boa6, tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_better, max.tries = 400),
#              rescale = FALSE)
# last_basis <- get_best(holes_2d_better)$basis %>% .[[1]]
# set.seed(123456)
# holes_2d_better_polish <-
#   animate_xy(boa6, tour_path = guided_tour(holes(), d = 2,
#                                            search_f =  search_polish),
#              rescale = FALSE, start = last_basis)
#
# save(holes_2d_better, file = ("data", "holes_2d_better.rda"))
# save(holes_2d_better_polish, file = ("data", "holes_2d_better_polish.rda"))
#
# set.seed(123456)
# render(
#   boa6,
#   tour_path = guided_tour(holes(), d = 2, search_f = search_better, max.tries = 400),
#   dev = "png",
#   display = display_xy(axes = "off", verbose = TRUE, col = botanical_palettes$fern[[1]]),
#   rescale = FALSE,
#   frames = 00,
#   file = ("anim","polish", "before%03d.png")
# )
# last_basis <- get_best(holes_2d_better)$basis %>% .[[1]]
#
# set.seed(123456)
# render(
#   boa6,
#   tour_path =  guided_tour(holes(), d = 2, search_f = search_polish),
#   dev = "png",
#   display = display_xy(axes = "off", verbose = TRUE, col = botanical_palettes$fern[[6]]),
#   start = last_basis,
#   rescale = FALSE,
#   frames = 100,
#   file = ("anim","polish", "after%03d.png")
# )

# p1 <- bind_rows(holes_2d_better, holes_2d_better_polish) %>%
#   clean_method() %>%
#   mutate(method = factor(method, levels = c("CRS", "polish"))) %>%
#   get_interp() %>%
#   explore_trace_interp(color = method, cutoff = 100, target_size = 2, interp_size = 2) +
#   scale_color_discrete_botanical(breaks = c("CRS", "polish"), label = c("CRS", "polish")) +
#   theme(legend.position = "bottom")
# wrap <- function(path) png::readPNG(path) %>% grid::rasterGrob() %>% wrap_plots()
#
# first <- ("anim","polish", "before001.png")
# before <- ("anim","polish", "before073.png")
# after <- ("anim","polish", "after006.png")
# file <- c(first, before, after)
# rl <- lapply(file, png::readPNG)
# gl <-  lapply(rl, grid::rasterGrob)
#
# lay <- rbind(c(1,2,3),
#              c(4,4,4))
#
# p <- gridExtra::grid.arrange(gl[[1]], gl[[2]], gl[[3]], p1, layout_matrix = lay)
#
# ggsave(p, filename = "polish.svg",
#        path = ("figs"), width = 10, height = 8)

knitr::include_graphics("figs/polish.svg")

## ---- noisy-better-geo
## code for generating kol_1d_geo, kol_1d_better and kol_1d_better_polish
# set.seed(123456)
# kol_1d_geo <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_geodesic, max.tries = 200),
#                rescale = FALSE)
#
# set.seed(123456)
# kol_1d_better <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_better, max.tries = 200),
#                rescale = FALSE)
#
# # last_basis <- get_best(kol_1d_better)$basis %>% .[[1]]
# #
# # set.seed(123456)
# # kol_1d_better_polish <-
# #   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
# #                                              search_f = search_polish, max.tries = 200),
# #                rescale = FALSE, start = last_basis)
#
# set.seed(123456)
# kol_1d_better_random <-
#   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
#                                              search_f =  search_better_random, max.tries = 200),
#                rescale = FALSE)
#
# # last_basis <- get_best(kol_1d_better_random)$basis %>% .[[1]]
# #
# # set.seed(123456)
# # kol_1d_better_random_polish <-
# #   animate_dist(boa5, tour_path = guided_tour(norm_kol(nrow(boa5)), d = 1,
# #                                              search_f =  search_polish, max.tries = 200),
# #                rescale = FALSE, start = last_basis)
#
# save(kol_1d_geo, file = ("data", "kol_1d_geo.rda"))
# save(kol_1d_better, file = ("data", "kol_1d_better.rda"))
# save(kol_1d_better_random, file = ("data", "kol_1d_better_random.rda"))


# index <- tourr::norm_kol(nrow(boa5))
# theo_best_index_val <- index(as.matrix(boa5) %*% matrix(c(0, 1, 0, 0, 0), nrow = 5, ncol = 1))
#
# dt <- dplyr::bind_rows(kol_1d_geo, kol_1d_better, kol_1d_better_random) %>%
#   bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0), ncol = 1), tourr::norm_kol(nrow(boa5)), raw_data = boa5)
#
# p1 <- dt %>%
#   explore_trace_interp(group = method, color = method, accuracy_y = 0.02) +
#   geom_hline(yintercept = theo_best_index_val, alpha = 0.5, linetype = 2) +
#   scale_color_discrete_botanical() +
#   facet_wrap(vars(fct_relevel(method, c("PD", "CRS", "SA"))), scales = "free_x") +
#   theme(strip.text = element_text(size = "10pt", margin = margin(0.15, 0, 0.15, 0, unit = "cm")))
#
# pca <- dt %>%
#   compute_pca(group = method) %>%
#   purrr::pluck("aug")
#
# p2 <- pca %>%
#   explore_space_pca(group = method, pca = FALSE, details = FALSE, start_size = 3, interp_size = 0.5, end_size = 5, theo_size = 15) +
#   add_search(dt = pca %>% group_by(method) %>% filter(str_detect(info, "search")) %>% filter(tries != max(tries)),
#              search_size = 0.5, search_col = method, search_alpha = 0.2) +
#   add_dir_search(dt = pca %>% filter(method == "PD") %>% get_dir_search(ratio = 10) %>% filter(tries != max(tries)),
#                  dir_col = method, dir_alpha = 0.2) +
#   scale_color_discrete_botanical() +
#   theme(legend.position = "none") +
#   facet_wrap(vars(fct_relevel(method, c("PD", "CRS", "SA")))) +
#   theme(strip.text = element_text(size = "10pt", margin = margin(0.15, 0, 0.15, 0, unit = "cm")))
#
# p <- (p1 / p2)
#
#
# ggsave(p, filename = "noisy-better-geo.svg",
#        path = ("figs"), width = 10, height = 8)
knitr::include_graphics("figs/noisy-better-geo.svg")

#
# compute_kol_sim <- function(optim_data, polish_data, search_f, alpha = 0.5, max.tries = 200){
#   #browser()
#   set.seed(123)
#   seed <- sample.int(10000, 20)
#   optim_data <- list()
#   polish_data <- list()
#
#   for (i in 1: length(seed)){
#     cat("i = ", i, "seed = ", seed[i], "\n")
#     set.seed(seed[i])
#     optim_data[[i]] <-
#       animate_dist(boa6,
#                    tour_path = guided_tour(norm_kol(nrow(boa6)), d = 1,
#                                            search_f = search_f, max.tries = max.tries, alpha),
#                    rescale = FALSE, verbose = TRUE) %>%
#       mutate(seed = seed[i])
#
#     last_basis <- get_best(optim_data[[i]])$basis %>% .[[1]]
#
#     set.seed(seed[i])
#     polish_data[[i]] <-
#       animate_dist(boa6,
#                    tour_path = guided_tour(norm_kol(nrow(boa6)), d= 1,
#                                            search_f = search_polish, max.tries = 500),
#                    rescale = FALSE, verbose = TRUE,
#                    start = last_basis) %>%
#       mutate(seed = seed[i])
#   }
#
#   list(optim_data = optim_data, polish_data = polish_data)
# }
#
# kol_better <- compute_kol_sim(kol_better_optim, kol_better_polish, search_f = search_better)
# kol_better_tuned <- compute_kol_sim(kol_better_optim_tuned, kol_better_polish_tuned, search_f = search_better, alpha = 0.7)
# kol_random <- compute_kol_sim(kol_random_optim, kol_random_polish, search_f = search_better_random)
# kol_random_tuned <- compute_kol_sim(kol_random_optim_tuned, kol_random_polish_tuned, search_f = search_better_random, alpha = 0.7)
#
# # clean up the four results
# sim_clean_up <- function(sim_data) {
#   sim_name <- rep(names(sim_data), each = 20)
#
#   optim_tidy <- purrr::map(sim_data,  ~.x$optim_data) %>%
#     unlist(recursive = FALSE) %>%
#     map2(sim_name, ~.x %>% mutate(sim = .y)) %>%
#     bind_rows()
#
#   polish_tidy <- purrr::map(sim_data,  ~.x$polish_data) %>%
#     unlist(recursive = FALSE) %>%
#     map2(sim_name, ~.x %>% mutate(sim = .y)) %>%
#     bind_rows()
#
#   all_tidy <- bind_rows(optim_tidy, polish_tidy) %>% mutate(seed = as.factor(seed))
#
#   all_tidy
# }
#
# find_best <- function(clean_up){
#   names <- clean_up$sim %>% unique()
#   best <- map_dfr(names, ~clean_up %>% filter(sim == .x) %>% get_best(seed)) %>%
#     rename(index_val_best = index_val) %>%
#     arrange(index_val_best) %>%
#     mutate(var_found = map_dbl(basis, ~abs(.x) %>% which.max()),
#            var_found = as.factor(ifelse(var_found == 2, "V2", "V7")))
#
#   best
# }
#
# all_kol <- sim_clean_up(list(kol_better = kol_better,
#                              kol_better_tuned = kol_better_tuned,
#                              kol_random = kol_random,
#                              kol_random_tuned= kol_random_tuned))
#
# clean <- all_kol %>%
#   left_join(find_best(all_kol) %>% select(seed, var_found, index_val_best, sim), by = c("seed", "sim") )
#
# save(clean, file = ("data", "clean.rda"))

## ----kol-result
# clean_pca <- clean %>%
#   filter(method != "search_polish") %>%
#   bind_theoretical(matrix = matrix(c(0, 1, 0, 0, 0, 0), nrow = 6),
#                    index = tourr::norm_kol(nrow(boa6)),
#                    raw_data = boa6) %>%
#   bind_theoretical(matrix = matrix(c(0, 0, 1, 0, 0, 0), nrow = 6),
#                    index = tourr::norm_kol(nrow(boa6)),
#                    raw_data = boa6) %>%
#   mutate(seed_sim = paste0(seed, sim),
#          alpha = ifelse(str_detect(sim, "tuned"), 0.7, 0.5),
#          optimiser = ifelse(str_detect(sim, "random"), "SA", "CRS"),
#          var_found = as.factor(ifelse(var_found == "V7", "global", "local")),
#          var_found = fct_relevel(var_found, "global", after = 1)) %>%
#   compute_pca(group = seed_sim) %>%
#   pluck("aug")
#
# global_max <- clean_pca %>%
#   filter(info == "theoretical") %>%
#   filter(index_val == max(index_val)) %>%
#   select(-alpha, -optimiser)
#
# local_max <- clean_pca %>%
#   filter(info == "theoretical") %>%
#   filter(index_val == min(index_val)) %>%
#   select(-alpha, -optimiser)
#
# p <- clean_pca %>% drop_na() %>% rename(`search size` = alpha) %>%
#   explore_space_pca(pca = FALSE, group = seed_sim, color = var_found,
#                     start_size = 1, start_alpha = 0.5, end_size = 2,
#                     interp_size = 0.5) +
#   add_theo(dt = global_max, theo_size = 20) +
#   add_theo(dt = local_max, theo_label = "x", theo_size = 10) +
#   scale_color_discrete_botanical() +
#   facet_grid(`search size` ~ optimiser, labeller = label_both) +
#   theme(strip.text = element_text(size = "10pt", margin = margin(.15, 0, .15, 0, "cm")),
#         legend.text = element_text(size = "10pt"))
#
# ggsave(p, filename = "kol-result.svg",
#        path = ("figs"), width = 10, height = 10)
knitr::include_graphics("figs/kol-result.svg")

## ---- flip-sign
# set.seed(2463)
# orientation_corrected <-
#   animate_dist(boa6,
#                tour_path = guided_tour(norm_kol(nrow(boa6)), d = 1,
#                                        search_f = search_better, max.tries = 200, 0.7),
#                rescale = FALSE)
#
# use ggobi/tourr version 0.6.0
# set.seed(2463)
# orientation_different <-
#   animate_dist(boa6,
#                tour_path = guided_tour(norm_kol(nrow(boa6)), d = 1,
#                                        search_f = search_better, max.tries = 200, 0.7),
#                rescale = FALSE)
#
# save(orientation_corrected, file = ("data", "orientation_corrected.rda"))
# save(orientation_different, file = ("data", "orientation_different.rda"))
#
# dt <- bind_rows(orientation_corrected %>% mutate(sign = "flipped"),
#                 orientation_different %>% mutate(sign = "original")) %>%
#   compute_pca() %>%
#   pluck("aug") %>%
#   mutate(sign = fct_relevel(as.factor(sign), c("original", "flipped")))
#
# p <- dt %>%
#   explore_space_pca(group = sign, pca = FALSE ,flip = FALSE, start_size = 3, interp_size = 1) +
#   add_anchor(dt = get_interp_last(dt), anchor_color = sign) +
#   scale_color_discrete_botanical() +
#   facet_wrap(vars(sign)) +
#   theme(strip.text.x = element_text(size = "10pt", margin = margin(.15, 0, .15, 0, "cm")),
#         legend.text = element_text(size = "10pt"))
#
# ggsave(p, filename = "flip-sign.svg",
#        path = ("figs"))
knitr::include_graphics("figs/flip-sign.svg")
