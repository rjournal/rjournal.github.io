# load packages
library(terra)
library(BoundaryStats)

# read in data
songs <- rast('data/2010_2022_song_boundaries.asc')
genetic <- rast('data/genetic_interpolation.asc') |>
  resample(songs)
songs <- crop(songs, genetic) |>
  mask(genetic)

# calculate spatial boundaries for variables
song_boundaries <- define_boundary(songs)
genetic_boundaries <- define_boundary(genetic, calculate_intensity = TRUE)

# plot boundary overlap (optional)
plot_boundary(genetic_boundaries, song_boundaries, trait_names = c('Genetic', 'Song'))

# create null distributions for statistics.
song_boundary_null <- boundary_null_distrib(songs, calculate_intensity = FALSE, cat = FALSE, n_iterations = 99, threshold = 0.2, model = 'gaussian')
genetic_boundary_null <- boundary_null_distrib(genetic, calculate_intensity = TRUE, cat = FALSE, n_iterations = 99, threshold = 0.2, model = 'gaussian')

boundary_overlap_null <- overlap_null_distrib(genetic, songs, rand_both = FALSE, n_iterations = 99, x_calculate_intensity = TRUE, threshold = 0.2, x_model = 'gaussian')

# run boundary statistics tests
n_boundaries(song_boundaries, song_boundary_null)
longest_boundary(song_boundaries, song_boundary_null)

n_boundaries(genetic_boundaries, genetic_boundary_null)
longest_boundary(genetic_boundaries, genetic_boundary_null)

# run boundary overlap statistics tests
n_overlap_boundaries(genetic_boundaries, song_boundaries, boundary_overlap_null)
average_min_x_to_y(genetic_boundaries, song_boundaries, boundary_overlap_null)
