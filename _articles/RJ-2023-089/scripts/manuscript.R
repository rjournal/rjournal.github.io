# title: "openalexR paper:An R-tool for collecting bibliometric data from OpenAlex"


#### NOTE:  #########################################################################################################
#### To replicate the analyses proposed in the manuscript, we commented on the lines of code that pertain to 
#### downloading data from OpenAlex. 
#### This is because bibliographic metadata change with very high frequency, and thus a download made at distinction 
#### of days could provide different results (e.g., number of citations, number of published articles, etc.).
#### The manuscript.rds file contains 4 objects:
#### - concept
#### - df
#### - sb_docs
#### - ngrams_data
####
#### manuscript.rds is larger then 10Mb then we uploaded it on Massimo Aria website.
#### It can be downloaded from https://massimoaria.com/data/manuscript.rds
#####################################################################################################################

options(timeout=1000)
download.file("https://massimoaria.com/data/manuscript.rds", destfile = "manuscript.rds", method = "auto")
load("manuscript.rds")

## load packages
library(openalexR)
library(dplyr)
library(knitr)
library(gghighlight)
library(ggplot2)
library(tidyr)
library(ggraph)
library(tidygraph)
library(ggtext)
library(wordcloud)
library(treemapify)
library(forcats)
library(ggrepel)


#To do this, we define a query on the entity "works" by filtering through the concept "bibliometrics" associated with the id https://openalex.org/C178315738.
#Let us first briefly describe the concept "bibliometrics".

# concept <- oa_fetch(
#   entity = "concepts",
#   identifier  = "https://openalex.org/C178315738",
#   count_only = FALSE,
#   verbose = FALSE
# )

concept %>% 
  select(.data$description) %>% 
  kable()

#Here the list of the ancestor concepts:
concept %>% 
  select(.data$ancestors) %>% 
  tidyr::unnest(ancestors) %>% 
  select(!wikidata) %>% 
  kable(digits = 3)

#Here the list of the equal-level related concepts
concept %>% 
  select(related_concepts) %>% 
  tidyr::unnest(related_concepts) %>% 
  select(!wikidata) %>% 
  filter(level==2) %>% 
  kable(digits = 3)

#Here the list of the descendant concepts:
concept %>% 
  select(.data$related_concepts) %>% 
  tidyr::unnest(.data$related_concepts) %>% 
  select(!wikidata) %>% 
  filter(level>2) %>% 
  kable(digits = 3)

#We can obtain information on the equal level concepts with the most works
related<- concept%>% 
  select(.data$related_concepts) %>% 
  tidyr::unnest(.data$related_concepts) %>%
  filter(level==2)

concept_df<- oa_fetch(
  entity = "concepts",
  identifier = c(concept$id,related$id)
)

#Trends in bibliometrics-related topics in the past 10 years.
concept_df %>%
  select(display_name, counts_by_year) %>%
  tidyr::unnest(counts_by_year) %>%
  filter(year < 2022) %>%
  ggplot() +
  aes(x = year, y =log( works_count), color = display_name) +
  facet_wrap(~display_name) +
  geom_line(size = 0.7) +
  labs(
    x = NULL, y = "log(Works count)",
  ) +
  guides(color = "none")+
  gghighlight(max(works_count) >1, label_params=list(max.overlaps=0))

#Now we check how many records the query returns by setting the count.only parameter equal to TRUE
# oa_fetch(
#   entity = "works",
#   title.search = "bibliometrics|science mapping",
#   abstract = TRUE,
#   count_only = TRUE,
#   verbose = TRUE
# )

#Then proceed to download metadata related to papers that contain the words "bibliometrics" or "science mapping" in the title 

# df <- oa_fetch(
#   entity = "works",
#   title.search = "bibliometrics|science mapping",
#   abstract = TRUE,
#   count_only = FALSE,
#   verbose = TRUE
# )

#We can obtain information on the Most relevant Journal
Venues <- df |>
  mutate(so = gsub("Journal of the|Journal of", "J.", so)) |>
  count(so) |>
  drop_na(so) |>
  slice_max(n, n = 6) |>
  pull(so)
Venues

MRV <- df |>
  mutate(so = gsub("Journal of the|Journal of", "J.", so)) |>
  
  filter(so %in% Venues, publication_year < 2022) |>
  count(so, publication_year, sort = TRUE) |>
  mutate(
    so = as_factor(so) |> fct_relevel(Venues),
    label = if_else(publication_year == max(publication_year),
                    as.character(so), NA_character_)) |>
  ggplot() +
  aes(x = publication_year, y = n, fill = so, color = so) +
  geom_area() +
  geom_text(aes(label = label, x = publication_year + 1),
            position = position_stack(vjust = 0.5),
            hjust = 0, na.rm = TRUE) +
  scale_y_continuous(expand = expansion(add = c(0, 0))) +
  scale_x_continuous(expand = expansion(add = c(0, 22.5))) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Total works", x = NULL) +
  guides(fill = "none", color = "none")
MRV


#We can obtain information on the Most relevant Authors
biblio_authors <- do.call(rbind.data.frame, df$author)

Authors <- biblio_authors  |>
  count(au_display_name) |>
  drop_na(au_display_name) |>
  slice_max(n, n = 10) |>
  pull(au_display_name)
Authors


mra <- biblio_authors  |>
  count(au_display_name) |>
  drop_na(au_display_name) |>
  slice_max(n, n = 10) |>
  mutate(au_display_name = forcats::fct_reorder(au_display_name, n)) |>
  ggplot() +
  aes(y = n, x = au_display_name) +
  geom_segment(
    aes(x=au_display_name, xend=au_display_name, y=0, yend=n), 
    color = "#a3ad62",
  ) +
  geom_point(color="#d46780", size=4) +
  coord_flip() +
  theme(legend.position="none") +
  xlab("") +
  ylab("Articles") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  )
mra

#We can obtain information on the Most relevant Institutions
Institutions <- biblio_authors  |>
  count(institution_display_name) |>
  drop_na(institution_display_name) |>
  slice_max(n, n = 10) |>
  pull(institution_display_name)
Institutions

mri <- biblio_authors  |>
  count(institution_display_name) |>
  drop_na(institution_display_name) |>
  slice_max(n, n = 10) |>
  mutate(institution_display_name= forcats::fct_reorder(institution_display_name, n)) |>
  ggplot() +
  aes(y = n, x = institution_display_name) +
  geom_segment(
    aes(x=institution_display_name, xend=institution_display_name, y=0, yend=n), 
    color = "#a3ad62",
  ) +
  geom_point(color="#d46780", size=4) +
  coord_flip() +
  theme(legend.position="none") +
  xlab("") +
  ylab("Articles") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = NA, color = "grey20")
  )
mri

#We can obtain information on the Most cited works

seminal_works <- df %>% show_works()

seminal_works <- slice_max(df, cited_by_count, n = 10)
info_seminal_works <- seminal_works%>%
  select(publication_year, display_name, so, cited_by_count)

info_seminal_works<- as.data.frame(info_seminal_works)
info_seminal_works

#snowball

# sb_docs <- oa_snowball(
#   identifier = seminal_works$id[1:2],
#   citing_filter = list(from_publication_date = "2022-01-01"),
#   verbose = TRUE
# )

sg_1 <- as_tbl_graph(sb_docs)

AU <- sb_docs$nodes %>% 
  select(author)
AU <- unlist(AU,recursive = FALSE)
AU <- unlist(lapply(AU, function(l){
  paste(l$au_display_name,collapse=";")
}))

g_citation <- ggraph(graph = sg_1, layout = "stress") +
  geom_edge_link(color = "grey60", alpha = 0.30, show.legend = FALSE) +
  geom_node_point(aes(fill = oa_input, size = cited_by_count), shape = 21) +
  scale_edge_width(range = c(0.1, 1.5), guide = "none") +
  scale_size(range = c(1, 3), guide = "none") +
  scale_fill_manual(values = c("#9DB9F1", "#4479E4"), na.value = "grey", name = "") +
  geom_node_point(
    data = ~ filter(.x, !oa_input),
    mapping = aes(size = cited_by_count),
    fill = "#a3ad62",
    shape = 21, color = "white"
  ) +
  geom_node_point(
    data = ~ filter(.x, oa_input),
    mapping = aes(size = cited_by_count),
    fill = "#d46780",
    shape = 21, color = "white"
  ) +
  theme_graph() +
  theme(legend.position = "bottom") +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = "transparent", color = NA)
  ) +
  guides(fill = "none") +
  guides(fill = "none", size = "none") +
  geom_node_label(aes(filter = oa_input, label = AU), nudge_y = 0.2, size = 3)
g_citation



#N-grams

ngrams_data <- oa_ngrams(sample(df$id, 1000), verbose = TRUE)
top_10 <- do.call(rbind.data.frame, ngrams_data$ngrams) |>
  dplyr::filter(ngram_tokens == 2, nchar(ngram) > 10) |>
  dplyr::arrange(desc(ngram_count)) |>
  dplyr::slice_max(ngram_count, n = 10, with_ties = FALSE)

treemap <- ggplot(top_10, aes(area = ngram_count, fill = ngram, label=ngram)) +
  geom_treemap(show.legend = F) +
  geom_treemap_text(colour = "black", place = "centre") 
treemap