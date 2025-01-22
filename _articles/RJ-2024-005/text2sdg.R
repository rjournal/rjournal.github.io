library(tidyverse)
library(fastText)
library(text2sdg)
library(cowplot)


# Read and preprocess SNSF data -------------------------------------------
projects <- read_csv2("https://zenodo.org/records/11060662/files/GrantWithAbstracts.csv?download=1")
nrow_before <- nrow(projects)
# remove documents with missing abstracts
projects <- projects %>% 
  filter(!is.na(Abstract))

#number of removed projects because of missing abstract
nrow_before - nrow(projects)


# use fasttext to identify langauge and then remove nonenglish abstracts
# lid.176.bin file can be downloaded here: https://fasttext.cc/docs/en/language-identification.html
file_bin = file.path('lid.176.bin')

dtbl_res_in = fastText::language_identification(input_obj = projects$Abstract,
                                                pre_trained_language_model_path = file_bin,
                                                k = 1,
                                                th = 0.0,
                                                threads = 1,
                                                verbose = TRUE)
table(dtbl_res_in$iso_lang_1)

projects[["detected_language"]] <- dtbl_res_in$iso_lang_1


#how many dropped because of language?
projects %>% 
  filter(detected_language != "en") %>% 
  nrow(.)

#keep only english 
projects <- projects %>% 
  filter(detected_language == "en")

nrow_before - nrow(projects)

# create character vector
projects <- projects[["Abstract"]]



# detect SDGs with the ensemble model
sdgs_ensemble <- detect_sdg(text = projects,
                            synthetic = c("equal","triple"))
head(sdgs_ensemble)


# Run detect_sdg with the individual query models ---------------------------------------------

# detect SDGs
#shown
sdgs <- detect_sdg_systems(text = projects,
                           systems = c("Aurora", "Elsevier", "Auckland", "SIRIS", "SDSN", "SDGO"),
                           output = "features")

#shown
head(sdgs)

# plot
#shown
plot_sdg(sdgs)

#ggsave("3_plots/default_plot_revision.pdf", width = 12/1.5, height = 6/1.5)


#shown
plot_sdg(sdgs, 
         sdg_titles = TRUE) + 
  ggplot2::facet_wrap(~system, ncol= 1, scales = "free_y")

#ggsave("3_plots/default_plot_sdg_labels_revision.pdf", width = 8, height = 8)


# Correlation 
#shown
crosstab_sdg(sdgs)

#shown
crosstab_sdg(sdgs_ensemble)

#shown
crosstab_sdg(sdgs, compare = "sdgs", sdgs = 1:6)

# Query development -------------------------------------------------------
#shown
# definition of query set
my_example_system <- tibble::tibble(system = "my_example_system",
                                    query = c("marine AND fisheries", 
                                              "('marine fisheries') AND sea", 
                                              "?ish"),
                                    sdg = c(14,14,14))

detect_any(text = projects, 
           system = my_example_system)


# Applying text2sdg to non-English data -----------------------------------

df_backtrans <- read_rds("https://zenodo.org/records/11060662/files/backtrans_table.RDS?download=1")

orig_sdg <- detect_sdg_systems(df_backtrans$orig, systems = c("Aurora", "Elsevier", "Auckland", "SIRIS", "SDSN", "SDGO"))

backtrans_sdg <- detect_sdg_systems(df_backtrans$backtrans, systems = c("Aurora", "Elsevier", "Auckland", "SIRIS", "SDSN", "SDGO"))


#### prepare data for correlation test
systems = c("Aurora", "Elsevier", "Auckland", "SIRIS", "SDSN", "SDGO")

sdgs <- unique(orig_sdg$sdg)

phi_dat_orig <- tidyr::expand_grid(document = 1:length(levels(orig_sdg$document)), 
                                   system = systems, sdg = sdgs) %>% dplyr::mutate(document = as.factor(document)) %>% 
  dplyr::left_join(orig_sdg %>% dplyr::mutate(hit = 1) %>% 
                     dplyr::select(document, system, sdg, hit), by = c("document", 
                                                                       "system", "sdg")) %>% 
  dplyr::mutate(hit = dplyr::if_else(is.na(hit), 0, 1)) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(document, sdg) %>% 
  tidyr::pivot_wider(names_from = system, values_from = hit)



phi_dat_backtrans <- tidyr::expand_grid(document = 1:length(levels(backtrans_sdg$document)), 
                                        system = systems, sdg = sdgs) %>% dplyr::mutate(document = as.factor(document)) %>% 
  dplyr::left_join(backtrans_sdg %>% dplyr::mutate(hit = 1) %>% 
                     dplyr::select(document, system, sdg, hit), by = c("document", 
                                                                       "system", "sdg")) %>% 
  dplyr::mutate(hit = dplyr::if_else(is.na(hit), 0, 1)) %>%
  dplyr::distinct() %>% 
  dplyr::arrange(document, sdg) %>% 
  tidyr::pivot_wider(names_from = system, values_from = hit) 


library(psych)
tab_aurora <- table(phi_dat_orig$Aurora, phi_dat_backtrans$Aurora)
tab_elsevier <- table(phi_dat_orig$Elsevier, phi_dat_backtrans$Elsevier)
tab_auckland <- table(phi_dat_orig$Auckland, phi_dat_backtrans$Auckland)
tab_siris <- table(phi_dat_orig$SIRIS, phi_dat_backtrans$SIRIS)
tab_sdsn <- table(phi_dat_orig$SDSN, phi_dat_backtrans$SDSN)
tab_sdgo <- table(phi_dat_orig$SDGO, phi_dat_backtrans$SDGO)

phi(tab_aurora, digits = 2)
phi(tab_elsevier, digits = 2)
phi(tab_auckland, digits = 2)
phi(tab_siris, digits = 2)
phi(tab_sdsn, digits = 2)
phi(tab_sdgo, digits = 2)


# Performance -------------------------------------------------------------

benchmark_table <- read_rds("https://zenodo.org/records/11060662/files/benchmark_table_revision.rds?download=1")

#prep data
benchmark_table %>% 
  count(length, ndoc)

plot_table <- benchmark_table %>% 
  pivot_longer(-c(length, ndoc, index)) 


plot_table %>% 
  group_by(length, ndoc, name) %>% 
  summarise(median = median(value)) %>% 
  mutate(length = factor(length),
         ndoc = factor(ndoc),
         name = factor(name, levels = c("Aurora", "Elsevier","Auckland", "SIRIS", "OSDG", "SDSN"))) %>% 
  ggplot(aes(x = length, y = ndoc, fill = log(median))) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(median, 1)), color = "white", size = 3) +
  coord_fixed() +
  scale_fill_gradient(low ="#A5D7D2",
                      high = "#46505A") +
  facet_wrap(~name, nrow = 2) +
  guides(fill = guide_colourbar(barwidth = 14,
                                barheight = .5,
                                title = "Median Runtime (log seconds)",
                                title.theme = element_text(size = 9.5),
                                title.position = "top",
                                title.hjust =  .5)) +
  labs(x = "Document length", y = "Number of Documents") +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    strip.background =element_rect(fill="white"),
    legend.position = "top")

# ggsave("3_plots/benchmark_revision_final.pdf",
#       width = 10,
#       height = 12)





# Unique keywords per SDG for each system (Table 1) -----------------------
elsevier_queries %>% 
  select(sdg, query) %>% 
  mutate(query = str_replace_all(query, "OR | AND", "")) %>% 
  #get rid of all on-alphanumeric symbols 
  mutate(query_words = strsplit(gsub("[^[:alnum:] ]", "", query), " +")) %>% 
  group_by(sdg) %>% 
  unnest(query_words) %>% 
  mutate(query_words = str_to_lower(query_words)) %>% 
  distinct(sdg, query_words) %>% 
  #just to be sure
  filter(query_words != "AND | OR") %>% 
  count(sdg) %>%
  ungroup() %>% 
  summarise(mean = mean(n), sd = sd(n))


aurora_queries %>% 
  select(sdg, query) %>% 
  mutate(query = str_replace_all(query, "OR | AND", "")) %>% 
  #get rid of all on-alphanumeric symbols 
  mutate(query_words = strsplit(gsub("[^[:alnum:] ]", "", query), " +")) %>% 
  group_by(sdg) %>% 
  unnest(query_words) %>% 
  mutate(query_words = str_to_lower(query_words)) %>% 
  distinct(sdg, query_words) %>% 
  #just to be sure
  filter(query_words != "AND | OR") %>% 
  count(sdg) %>%
  ungroup() %>% 
  summarise(mean = mean(n), sd = sd(n))

auckland_queries  %>% 
  select(sdg, query) %>% 
  mutate(query = str_replace_all(query, "OR | AND", "")) %>% 
  #get rid of all on-alphanumeric symbols 
  mutate(query_words = strsplit(gsub("[^[:alnum:] ]", "", query), " +")) %>% 
  group_by(sdg) %>% 
  unnest(query_words) %>% 
  mutate(query_words = str_to_lower(query_words)) %>% 
  distinct(sdg, query_words) %>% 
  #just to be sure
  filter(query_words != "AND | OR") %>% 
  count(sdg) %>%
  ungroup() %>% 
  summarise(mean = mean(n), sd = sd(n))


siris_queries%>% 
  select(sdg, query) %>% 
  mutate(query = str_replace_all(query, "OR | AND", "")) %>% 
  #get rid of all on-alphanumeric symbols 
  mutate(query_words = strsplit(gsub("[^[:alnum:] ]", "", query), " +")) %>% 
  group_by(sdg) %>% 
  unnest(query_words) %>% 
  mutate(query_words = str_to_lower(query_words)) %>% 
  distinct(sdg, query_words) %>% 
  #just to be sure
  filter(query_words != "AND | OR") %>% 
  count(sdg) %>%
  ungroup() %>% 
  summarise(mean = mean(n), sd = sd(n))


sdgo_queries %>% 
  select(sdg, query) %>% 
  mutate(query = str_replace_all(query, "OR | AND", "")) %>% 
  #get rid of all on-alphanumeric symbols 
  mutate(query_words = strsplit(gsub("[^[:alnum:] ]", "", query), " +")) %>% 
  group_by(sdg) %>% 
  unnest(query_words) %>% 
  mutate(query_words = str_to_lower(query_words)) %>% 
  distinct(sdg, query_words) %>% 
  #just to be sure
  filter(query_words != "AND | OR") %>% 
  count(sdg) %>%
  ungroup() %>% 
  summarise(mean = mean(n), sd = sd(n))

sdsn_queries  %>% 
  select(sdg, query) %>% 
  mutate(query = str_replace_all(query, "OR | AND", "")) %>% 
  #get rid of all on-alphanumeric symbols 
  mutate(query_words = strsplit(gsub("[^[:alnum:] ]", "", query), " +")) %>% 
  group_by(sdg) %>% 
  unnest(query_words) %>% 
  mutate(query_words = str_to_lower(query_words)) %>% 
  distinct(sdg, query_words) %>% 
  #just to be sure
  filter(query_words != "AND | OR") %>% 
  count(sdg) %>%
  ungroup() %>% 
  summarise(mean = mean(n), sd = sd(n))






