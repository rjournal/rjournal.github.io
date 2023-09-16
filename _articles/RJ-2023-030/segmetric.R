
install.packages("segmetric")

library(segmetric)

# load example data sets
data("sample_ref_sf", package = "segmetric")
data("sample_seg_sf", package = "segmetric")

# create segmetric object
m <- sm_read(ref_sf = sample_ref_sf, seg_sf = sample_seg_sf)

sm_list_metrics()

# compute three metrics
sm_compute(m, c("AFI", "OS1", "US1"))

# compute the same three metrics as above
sm_compute(m, "AFI") %>%
    sm_compute("OS1") %>%
    sm_compute("US1")

# compute three metrics
sm_compute(m, c("AFI", "OS1", "US1")) %>%
    summary()

# register 'IoU' metric
sm_reg_metric(
    metric_id = "IoU",
    entry = sm_new_metric(
        fn = function(m, s, ...) {
            # m is the metric object, s is the subset
            #  for IoU, s is equivalent to sm_yprime(m)
            sm_area(s) / sm_area(sm_subset_union(s))
        },
        fn_subset = sm_yprime,
        name = "Intersection over Union",
        optimal = 1,
        description = "Values from 0 to 1 (optimal)",
        reference = "Jaccard (1912); Rezatofighi et al. (2019)"
    )
)

# describes the 'IoU' metric
sm_desc_metric("IoU")

# lists all supported metrics
sm_list_metrics()

library(segmetric)

# load data sets
data("ref_sf", package = "segmetric")
data("seg200_sf", package = "segmetric")
data("seg500_sf", package = "segmetric")
data("seg800_sf", package = "segmetric")
data("seg1000_sf", package = "segmetric")

# create segmetric object
m200 <- sm_read(ref_sf = ref_sf, seg_sf = seg200_sf)
m500 <- sm_read(ref_sf = ref_sf, seg_sf = seg500_sf)
m800 <- sm_read(ref_sf = ref_sf, seg_sf = seg800_sf)
m1000 <- sm_read(ref_sf = ref_sf, seg_sf = seg1000_sf)

plot(m200, 
     layers = "ref_sf", 
     plot_centroids = FALSE) 

plot(m200, 
     layers = "seg_sf", 
     plot_centroids = FALSE) 

plot(m500, 
     layers = "seg_sf", 
     plot_centroids = FALSE) 

plot(m800, 
     layers = "seg_sf", 
     plot_centroids = FALSE) 

plot(m1000, 
     layers = "seg_sf", 
     plot_centroids = FALSE)

# compute all metrics
metrics <- c("QR", "F_measure", "IoU", "M", "OS2", "US2")
m200 <- sm_compute(m200, metrics)
m500 <- sm_compute(m500, metrics)
m800 <- sm_compute(m800, metrics)
m1000 <- sm_compute(m1000, metrics)

# results
summary(m200)
summary(m500)
summary(m800)
summary(m1000)

plot(
    x = m500, 
    type = "subset", 
    subset_id = "Y_tilde", 
    plot_centroids = FALSE, 
    plot_legend = TRUE, 
    extent = segmetric::sm_seg(m500) 
)

plot(
    x = m500,
    type = "choropleth",
    metric_id = c("QR", "IoU", "M", "OS2", "US2"),
    break_style = "jenks",
    choropleth_palette = "RdYlBu",
    plot_centroids = FALSE,
    plot_axes = F,
    title = ""
)

