# Generated by `rjournal_pdf_article()` using `knitr::purl()`: do not edit by hand
# Please edit RJ-2023-027.Rmd to modify this file

params <-
list(arxiv = FALSE)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(pvLRT)
library(ggplot2)
knitr_output <- ifelse(knitr::is_latex_output(), "latex", "html")
glue1 <- function(...) glue::glue(..., .open = "<<", .close = ">>")


## ----util-fns, include=FALSE--------------------------------------------------
add_and_at_final <- function(char_vec) {
  nn <- length(char_vec)
  out <- if (nn > 1) {
    c(char_vec[1:(nn-1)], paste("and", char_vec[nn]))
  } else {
    char_vec
  }
  out
}


## ----table-summary-existing-packages, echo=FALSE, results='asis'--------------
# if (knitr::is_html_output()) {

note2_txt <- glue::glue(
  "Aimed towards sequential \\
        testing-based vaccine safety \\
        assessments as used by the FDA."
)

pkg_summary_tab <- tribble(
  ~Package, ~note_tmp, ~method,
  glue::glue(
    "**PhViD**: Pharmacovigilance Signal Detection"
  ), 
  "note1",
  glue::glue("PRR, ROR, BCPNN, GPS"),
  
  
  glue::glue(
    "**openEBGM**: EBGM Disproportionality Scores \\
   for Adverse Event Data Mining"
  ), 
  "note1",
  glue::glue("MGPS"),
  
  glue::glue(
    "**sglr**: An R package for power \\
    and boundary calculations in pre-licensure \\
    vaccine trials using a sequential generalized \\
    likelihood ratio test"
  ), 
  "note2",
  glue::glue(
    "Sequential Generalized Likelihood Ratio decision boundaries"
  ),
  
  
  glue::glue(
    "**Sequential**: Exact sequential analysis \\
    for Binomial and Poisson data"
  ), 
  "note2",
  glue::glue("Max SPRT statistic"),
  
  # glue::glue(
  #   "**SPRT**: Wald’s sequential probability \\
  #  ratio test"
  # ), "note2",
  
  glue::glue(
    "**AEenrich**: Vaccine adverse event enrichment tests"
  ), 
  "note2",
  glue::glue(
    "a) Modified Fisher's exact test (for pre-selected significant AEs); \\
      b) Modified Kolmogorov Smirnov statistic"
  ),
  
  
  glue::glue(
    "**mds**: Medical Devices Surveillance"
  ), 
  "note3",
  glue::glue("Data preprocessing"),
  
  glue::glue(
    "**pvLRT**: A suite of likelihood ratio test based methods to use in pharmacovigilance"
  ), 
  "note4",
  glue::glue("(Pseudo) LRT approaches based on log-linear models")
) %>% 
  mutate(
    Notes = case_when(
      note_tmp == "note1" ~ glue::glue(
        "Aimed towards drug safety."
      ),
      note_tmp == "note2" ~ note2_txt,
      note_tmp == "note3" ~ glue::glue(
        "Provides functions for handling \\
        messy/unstructured medical devices data."
      ),
      note_tmp == "note4" ~ "Our package.",
      TRUE ~ NA_character_
    )
  ) %>%
  {
    if (knitr_output == "latex") {
      mutate(
        .,
        Package = Package %>% 
          strsplit("\\*\\*") %>% 
          map_chr(
            function(x) {
              glue1 <- function(...) glue::glue(..., .open = "<<", .close = ">>")
              glue1("\\CRANpkg{<<x[2]>>}<<x[3]>>")
            }
          )
      ) 
    } else .
  } %>%
  select(-note_tmp) %>% 
  mutate_all(~as.character(.)) %>% 
  rename(Method = method)

align_vec <- if (knitr::is_html_output()) c("l", "l") else c("|l", "l|")

table_caption <- glue::glue(
  "Existing R packages on CRAN with functionalities for pharmacovigilance."
)

out_tab <-
  pkg_summary_tab %>% 
  kableExtra::kbl(
    booktabs = TRUE, 
    format = knitr_output,
    escape = FALSE,
    vline = "|" ,
    align = c("l",  "l"),
    label = NULL,
    caption = table_caption
  ) %>%
  kableExtra::collapse_rows(columns = 3, valign = "top") %>% 
  # as.character() %>% 
  ifelse(
    knitr_output == "latex",
    as.character(.) %>% 
    stringi::stri_replace_all_fixed(
      "\\begin{tabular}[t]{l|l|l}",
      "\\begin{tabular}[t]{|p{12em}|p{12em}|p{12em}|}",
    ) %>% 
      stringi::stri_replace_all_fixed("\\centering", "") %>% 
      stringi::stri_replace_all_fixed(
        "\\begin{table}",
        glue1(
          "\\begin{table}
          \\centering
          \\label{tab:pkg_compare}
          "
        )
      ) %>%
      stringi::stri_replace_all_fixed(
        glue1("\\multirow[t]{-3}{*}{\\raggedright\\arraybackslash <<note2_txt>>}"),
        glue1(
          "\\multirowcell{-7}{
          Aimed towards sequential \\\\\ \\
          testing-based vaccine safety \\\\ \\
          as used by the FDA}"
        )
      ) %>% 
      stringi::stri_replace_all_fixed(
        "\\multirow[t]{-2}{*}",
        "\\multirow[t]{-1}{*}"
      ) %>% 
      as.character(),
    .
  )

cat(out_tab)
# out_tab %>% 
#   cat() #%>% 
# knitr::knit_print()
# } 
# else if (knitr::is_latex_output()) {
#   out_tab <- "
# \\begin{table}[htbp]
# \\centering
# \\label{tab:pkg_compare}
# \\caption{Existing CRAN packages with functionalities for pharmacovigilance.}
# \\begin{tabular}{|p{16em}|p{20em}|}
# \\toprule
# \\textbf{Package} & \\textbf{Notes} \\\\
# \\midrule
# \\CRANpkg{PhViD}: Pharmacovigilance Signal Detection & These two packages are aimed towards drug safety. \\\\
# \\cmidrule{1-1}    \\CRANpkg{openEBGM}: EBGM Disproportionality Scores for Adverse Event Data Mining & \\multicolumn{1}{r|}{} \\\\
# \\midrule
# \\CRANpkg{sglr}: An R package for power and boundary calculations in pre-licensure vaccine trials using a sequential generalized likelihood ratio test & These packages are aimed towards vaccine safety, particularly using sequential testing-based vaccine safety assessment procedures as used by the FDA. \\\\
# \\cmidrule{1-1}    \\CRANpkg{Sequential}: Exact sequential analysis for Binomial and Poisson data & \\multicolumn{1}{r|}{} \\\\
# \\cmidrule{1-1}    \\CRANpkg{SPRT}: Wald’s sequential probability ratio test & \\multicolumn{1}{r|}{} \\\\
# \\cmidrule{1-1}    \\CRANpkg{AEenrich}: Vaccine adverse event enrichment tests  & \\multicolumn{1}{r|}{} \\\\
# \\midrule
# \\CRANpkg{mds}: Medical Devices Surveillance & This  provides functions for handling messy/unstructured medical devices data. \\\\
# \\midrule
# \\CRANpkg{pvLRT}: A suite of likelihood ratio test based methods to use in pharmacovigilance. Contains various testing and post-processing functions. & Our package. \\\\
# \\bottomrule
# \\end{tabular}
# \\end{table}
# "
#   cat(out_tab)
# }



## ----load-pvlrt---------------------------------------------------------------
library(pvLRT)


## ----show_statin46, echo=TRUE-------------------------------------------------
data("statin46")
head(statin46)[, 1:3]


## ----run-poisson-statin46, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100) 
test_statin46_poisson <- pvlrt(   
  statin46,   
  zi_prob = 0,   
  test_drug_idx = 1:6   
)


## ----print-poisson-statin46---------------------------------------------------
test_statin46_poisson


## ----summary-poisson-statin46-------------------------------------------------
# summary(test_statin46_poisson) 


## ----create-bubbleplot-poisson-statin46---------------------------------------
pl_bubble_statin46_poisson <- plot(
  test_statin46_poisson,
  type = "bubbleplot",
  x_axis_measure = "lrstat",
  size_measure = "n",
  fill_measure = "p_value",
  AE = 15
)


## ----show-heatmap-poisson-statin46, fig.height=5, fig.width=13, out.width='100%', fig.align='center'----
library(ggplot2)
pl_bubble_statin46_poisson +
  theme(
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 9, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )


## ----barplot-poisson-statin46, fig.height=5, fig.width=13, out.width='100%', fig.align='center'----
pl_bar_statin46_poisson <- plot(
  test_statin46_poisson,
  type = "barplot",
  fill_measure = "p_value",
  x_axis_measure = "lrstat",
  AE = 15,
  show_n = TRUE,
  border_color = "black"
) 
pl_bar_statin46_poisson +
  theme(
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )


## ----create-heatmap-poisson-statin46, fig.height=7, fig.width=12, out.width='100%', fig.align='center'----
pl_heat_statin46_poisson <- plot(
  test_statin46_poisson,
  type = "heatmap",
  fill_measure = "p_value",
  AE = 15,
  show_n = TRUE,
  show_lrstat = TRUE,
  border_color = "black"
)
pl_heat_statin46_poisson +
  theme(
    axis.text = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )


## ----statin-46-heatmap-interactive-ploty, include=knitr::is_html_output(), eval=knitr::is_html_output(), warning=FALSE----
#> ## install plotly if not available
#> # if (!requireNamespace("plotly")) install.packages("plotly")
#> # interactive heatmap. turn all inscribed texts off
#> plotly::ggplotly(pl_heat_statin46_poisson)


## ----run-zip-statin46, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100) 
test_statin46_zip <- pvlrt(   
  statin46,   
  test_drug_idx = 1:6,   
  zi_prob = NULL,
  test_zi = TRUE
)


## ----print-zip-statin46-------------------------------------------------------
test_statin46_zip


## ----compare-signif-pairs-statin46--------------------------------------------
# extract_significant_pairs(test_statin46_poisson)
# extract_significant_pairs(test_statin46_zip)
all.equal(
  extract_significant_pairs(test_statin46_poisson),
  extract_significant_pairs(test_statin46_zip)
)


## ----compare-poisson-zip-statin46---------------------------------------------
cbind(BIC(test_statin46_poisson, test_statin46_zip), 
      AIC(test_statin46_poisson, test_statin46_zip))[, -3] # remove duplicated df col


## ----print-statin-------------------------------------------------------------
data("statin")
# head(statin)


## ----run-poisson-statin, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100)
test_statin_poisson <- pvlrt(
  statin,
  zi_prob = 0,
  test_drug_idx = 1:6
)
# test_statin_poisson # print the results


## ----run-zip-statin, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100)
test_statin_zip <- pvlrt(
  statin,
  test_drug_idx = 1:6,
  zi_prob = NULL,
  test_zi = TRUE
)
# test_statin_zip # print the results


## ----compare-poisson-zip-statin-----------------------------------------------
cbind(BIC(test_statin_poisson, test_statin_zip),
      AIC(test_statin_poisson, test_statin_zip))[, -3] #remove duplicated df col


## ----heatmap-zip-statin-latex, fig.height=6, fig.width=12, out.width='100%', fig.align='center', include=knitr::is_latex_output()----
pl_heat_statin_zip <- heatmap_pvlrt(
  test_statin_zip,
  AE = 15,
  show_n = TRUE,
  show_lrstat = TRUE
) 
pl_heat_statin_zip +
  theme(
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )


## ----heatmap-zip-statin-html, fig.height=6, fig.width=12, out.width='100%', fig.align='center', eval=knitr::is_html_output(), include=knitr::is_html_output()----
#> pl_heat_statin_zip <- heatmap_pvlrt(
#>   test_statin_zip,
#>   AE = 15,
#>   show_n = FALSE,
#>   show_lrstat = FALSE
#> )
#> plotly::ggplotly(pl_heat_statin_zip)


## ----print-gbca, echo=TRUE----------------------------------------------------
data("gbca")
# head(gbca) # show the first few rows


## ----obs-prop-zero-gbca-------------------------------------------------------
obs_prop_0 <- apply(gbca, 2, function(x) mean(x == 0))
round(obs_prop_0, 2)


## ----run-poisson-gbca, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100)
test_gbca_poisson <- pvlrt(
  gbca,
  zi_prob = 0,
  test_drug_idx = 1:9
)
# test_gbca_poisson ## print results


## ----run-zip-gbca, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100)
test_gbca_zip <- pvlrt(
  gbca,
  test_drug_idx = 1:9,
  zi_prob = NULL,
  test_zi = TRUE
)


## ----print-zip-gbca, include=knitr::is_html_output()--------------------------
# print results
test_gbca_zip


## ----compare-poisson-zip-gbca-------------------------------------------------
cbind(BIC(test_gbca_poisson, test_gbca_zip),
      AIC(test_gbca_poisson, test_gbca_zip))[, -3] # remove 


## ----heatmap-zip-gbca-latex, fig.height=10.5, fig.width=13.5, out.width='100%', fig.align='center', include=knitr::is_latex_output()----
pl_heat_gbca_zip <- heatmap_pvlrt(
  test_gbca_zip,
  AE = 15,
  show_n = TRUE,
  show_lrstat = TRUE
) 
pl_heat_gbca_zip +
  theme(
    axis.text = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )


## ----heatmap-zip-gbca-html, fig.align='center', include=knitr::is_html_output()----
pl_heat_gbca_zip <- heatmap_pvlrt(
  test_gbca_zip,
  AE = 15,
  show_n = FALSE,
  show_lrstat = TRUE
) 
plotly::ggplotly(pl_heat_gbca_zip)


## ----run-poisson-rv-list, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100)
test_rv_poisson_list <- lapply(
  list(old = rvold, young = rvyoung, combined = rv),
  function(this_data) {
    pvlrt(
      this_data,
      zi_prob = 0,
      test_drug_idx = 1
    )
  }
)


## ----run-zip-rv-list, cache=TRUE, echo=TRUE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
set.seed(100)
test_rv_zip_list <- lapply(
  list(old = rvold, young = rvyoung, combined = rv),
  function(this_data) {
    pvlrt(
      this_data,
      test_drug_idx = 1,
      zi_prob = NULL
    )
  }
)


## ----extract-zi-prob-rv-zip-list----------------------------------------------
rv_zi_prob <- test_rv_zip_list %>% 
  sapply(function(test) extract_zi_probability(test)["Rv"]) %>% 
  round(3)
rv_zi_prob


## ----compare-models-rv-all-data-----------------------------------------------
compare_models_rv <- mapply(
  function(test_poisson, test_zip, data_name) {
    AIC_both <- AIC(test_poisson, test_zip)$AIC
    BIC_both <- BIC(test_poisson, test_zip)$BIC
    data.frame(
      name = data_name, 
      AIC_poisson = AIC_both[1],
      BIC_poisson = BIC_both[1],
      AIC_zip = AIC_both[2],
      BIC_zip = BIC_both[2],
      optimal_AIC = c("poisson", "zip")[which.min(AIC_both)],
      optimal_BIC = c("poisson", "zip")[which.min(AIC_both)]
    )
  },
  test_rv_poisson_list,
  test_rv_zip_list,
  names(test_rv_poisson_list),
  SIMPLIFY = FALSE
) %>% 
  unname() %>% 
  do.call(rbind, .)

compare_models_rv


## ----optimal-model-rv-list----------------------------------------------------
test_rv_optimal_list <- list(
  old = test_rv_poisson_list$old,
  young = test_rv_poisson_list$young,
  combined = test_rv_zip_list$combined
)


## ----extract-signif-rv--------------------------------------------------------
signif_pairs_rv <- lapply(test_rv_zip_list, extract_significant_pairs) 


## ----extract-signif-rv-html-only, include=knitr::is_html_output()-------------
# print for summarized results
signif_pairs_rv


## ----heatmap-rv-all, message=FALSE, warning=FALSE, fig.height=9, fig.width=17, out.width='100%', fig.align='center'----
pl_heatmap_rv_list <- mapply(
  function(this_test, this_name) {
    heatmap_pvlrt(
      this_test,
      AE = 15,
      show_n = TRUE,
      show_lrstat = TRUE
    ) + 
      ggtitle(paste("age group:\n", this_name)) + 
      theme(plot.title = element_text(hjust = 0.5))
  },
  test_rv_optimal_list,
  names(test_rv_optimal_list),
  SIMPLIFY = FALSE
)
## install patchwork if not installed
# if (!requireNamespace("patchwork")) install.packages("pacthwork")
library(patchwork)
pl_heatmap_rv_comb <- pl_heatmap_rv_list$old + 
  pl_heatmap_rv_list$young + 
  pl_heatmap_rv_list$combined + 
  # combine the legends
  plot_layout(guides = "collect") & 
  # unified coloring scheme, 
  # see the 'fill_gradient_range' argument for heatmap_pvlrt
  scale_fill_gradientn(
    colors = c("darkred", "white"), 
    limits = c(0, 1)
  ) &
  theme(
    axis.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    title = element_text(size = 16, face = "bold")
  )

pl_heatmap_rv_comb


## ----summary-rv-res, include=FALSE--------------------------------------------
signif_set <- signif_pairs_rv %>% 
  lapply(
    . %>% 
      as_tibble() %>% 
      arrange(desc(lrstat)) %>% 
      pull(AE)
  )


## ----reference-section-heading, results='asis', echo=FALSE--------------------
ifelse(params$arxiv, "# References", " ") %>% cat()

