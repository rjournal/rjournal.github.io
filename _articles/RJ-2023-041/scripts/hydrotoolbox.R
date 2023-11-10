#* Methods section ----

library(hydrotoolbox)
library(readr)
library(readxl)

# path to data
my_path <- "./data"

#+++++++++++++++++++
# Rectangular data
#+++++++++++++++++++
#* Case B: multiple files (one per variable)
hm_create(class_name = "station") %>%
  hm_build_generic(path = my_path,
                   file_name = c("h_relativa_cuevas.csv",
                                 "p_atm_cuevas.csv",
                                 "precip_total_cuevas.csv",
                                 "temp_aire_cuevas.csv",
                                 "vel_viento_cuevas.csv"),
                   slot_name = c("rh", "patm", "precip",
                                 "tair", "wspd"),
                   by = c("hour", "45 min", "30 min", "1 hour", "15 min"),
                   FUN = read_csv  ) %>%
  hm_show()


#+++++++++++++++++++
# Excel files
#+++++++++++++++++++
#* Case B: single file - multiple sheets (one per variable)
hm_create(class_name = "station") %>%
  hm_build_generic(path = my_path,
                   file_name = "mnemos_guido.xlsx",
                   slot_name = c("qd", "evap",
                                 "tair","tmax",
                                 "tmin"),
                   by = c(q = "day", evap =  "day",
                          tair = "6 hour", tmax = "day",
                          tmin = NULL),
                   FUN = read_excel,
                   sheet = c(1L:5L),
                   skip = 3,
                   out_name = list( c("q_m3/s", "flag"),
                                    c("evap_mm", "flag"),
                                    c("tair", "flag"),
                                    c("tmax", "flag"),
                                    c("tmin", "flag")
                   )
  ) %>%
  hm_show()

#* Guido station case study----

library(hydrotoolbox)
library(readxl)

# package's data-base
path <- system.file("extdata", package = "hydrotoolbox")

# station building
guido <- 
  hm_create(class_name = "station") %>%
  hm_build_generic(path = path, 
                   file_name = "snih_qd_guido.xlsx", 
                   slot_name = "qd",
                   by = "1 day", 
                   out_name = list("q_m3/s"), 
                   sheet = 1L, 
                   FUN = read_excel)

# set the basin area
guido <- 
  guido %>% 
  hm_set(basin_area = 7110)

# get streamflow's report
guido %>% 
  hm_report(slot_name = "qd")

# ggplot2 daily flow
guido %>% 
  hm_plot(slot_name = "qd",
          col_name = list( "q_m3/s" ), 
          interactive = FALSE, 
          line_color = "dodgerblue",
          line_size = .7,
          y_lab = "Q(m3/s)", 
          from = "2010-07-01", 
          to = "2014-06-30") 

# plotly daily flow
guido %>% 
  hm_plot(slot_name = "qd",
          col_name = list( "q_m3/s" ), 
          interactive = TRUE, 
          line_color = "dodgerblue",
          line_size = .7,
          y_lab = "Q(m3/s)", 
          from = "2010-07-01", 
          to = "2014-06-30") 


# smooth with roll_fun
guido <- 
  guido %>%
  hm_mutate(slot_name = "qd",
            FUN = roll_fun,
            col_name = "last",
            pos = "c",
            k = 5,
            mean,
            out_name = "q_smooth") 

# remove doubtful records with set_value
guido <- 
  guido %>%
  hm_mutate(slot_name = "qd",
            FUN = set_value,
            col_name = "q_smooth",
            out_name = "q_set",
            value = rep(NA_real_, 2),
            from = c("1965-08-09", "1974-06-26"),
            to = c("1965-08-25", "1974-07-04") )

# aggregate daily mean streamflow 
# to mean monthly values
guido <- 
  guido %>%
  hm_agg(slot_name = "qd",
         col_name = "q_set", 
         fun = "mean", 
         period = "monthly", 
         out_name = "q_mean", 
         relocate = "qm", 
         allow_na = 2)

# extract the table 
tb_q_month <- 
  guido %>% 
  hm_get(slot_name = "qm")

# extract baisn area
basin_area <- 
  guido %>% 
  hm_get(slot_name = "basin_area")

# library
library(ggplot2)

# plot
gg_hm <-
  guido %>%
  hm_plot(slot_name = "qm", 
          col_name = list( c("q_mean") ), 
          line_color = "dodgerblue",
          line_size = .7,
          y_lab = "Q(m3/s)", x_lab = "", 
          legend_lab = "Mendoza River", 
          from = "1980-07-01", to = "1990-06-30") 

# customize the graph
gg_out <- 
  gg_hm + 
  geom_point(col = "red", size = .8) +
  theme_light() + 
  scale_x_date( date_breaks = "4 month", date_labels = "%Y-%m" ) +
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300) ) +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 10, face = "bold"), 
        legend.position = "none")


gg_out

#* HBV.IANIGLA output----
# dplyr contains mutate()
library(dplyr) 

# glacier mass balance simulation
cuevas_mb <- readRDS(file = "../data/cuevas_mb.rds" )

cuevas_mb <- 
  cuevas_mb %>%
  hm_mutate(slot_name = "compact",
            FUN = mutate, 
            `bm (m we)` = round( cuevas / 1000, 2 ),
            .keep = "all"
  ) 

cuevas_mb %>% hm_show()

# use hm_plot()
gg_out <- 
  cuevas_mb %>% 
  hm_plot(slot_name = "compact", 
          col_name = list("bm (m we)"), 
          line_color = "red3", 
          line_size = .7, 
          x_lab = "", y_lab = "MB (m we)"  )

# customize the figure
gg_out + 
  geom_point(col = "blue", size = .8) +
  geom_hline(yintercept = 0) +
  theme_light() + 
  scale_x_date( date_breaks = "2 year", date_labels = "%Y" ) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.25),
                     limits = c(-1.5, 1.5) ) +
  theme(axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 10, face = "bold"), 
        legend.position = "none") 