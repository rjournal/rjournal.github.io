## d-title h1, d-title p, d-title figure {

##   grid-column: page;

## }


## ----eval = FALSE-------------------------------------------------------------
#> devtools::install_github("kaneplusplus/forceps")


## ----waterfall, echo=FALSE, fig.cap='The data analysis waterfall.', out.width='5in', fig.align='center', fig.pos='htbp'----
knitr::include_graphics('waterfall.png')


## ----message = FALSE----------------------------------------------------------
library(forceps)
library(dplyr)

data(lc_adsl, lc_adverse_events, lc_biomarkers, lc_demography)

lc_trial <- consolidate(
  list(adsl = lc_adsl,
       adverse_events = lc_adverse_events %>% 
         cohort(on = "usubjid", name = "ae_long"),
       biomarkers = lc_biomarkers,
       demography = lc_demography %>% 
         select(-chemo_stop)
  ),
  on = "usubjid")

lc_trial


## ----echo = FALSE-------------------------------------------------------------
class_and_tag <- function(.x, new_class, ...) {
  if (!is.character(new_class)) {
    stop("New class should be of type character.")
  }
  dots <- list(...)
  for (n in names(dots)) {
    attributes(.x)[[n]] <- dots[[n]]
  }
  class(.x) <- c(new_class, class(.x))
  .x
}


## -----------------------------------------------------------------------------
library(listdown)

trial_summary <- list(
  `Outcome Information` = list(
    `Best Response` = lc_trial %>% 
      select(best_response, arm) %>% 
      class_and_tag("summary_table", by = "arm"),
    `Overall Survival` = lc_trial %>% 
      select(os_days, os_censor, arm) %>% 
      class_and_tag("survival_plot", 
                    time = "os_days", 
                    censor = "os_censor", 
                    x = "arm")),
  `Adverse Events` = lc_trial %>%
    select(best_response) %>%
    class_and_tag("summary_table"),
  `Biomarkers` = list(
    `EGFR` = lc_trial %>%
      select(egfr_mutation) %>%
      class_and_tag("summary_table")
  )
)

ld_cc_dendro(trial_summary)


## ----eval = FALSE-------------------------------------------------------------
#> library(gtsummary)
#> library(survival)
#> library(survminer)
#> library(dplyr)
#> 
#> make_summary <- function(x) {
#>   by <- attributes(x)$by
#>   tbl_summary(x, by = by)
#> }
#> 
#> make_survival_plot <- function(.x) {
#>   att <- attributes(.x)
#>   x <- ifelse(is.null(att$x), "1", att$x)
#>   form <- sprintf("Surv(%s, %s) ~ %s", att$time, att$censor, x) %>%
#>     as.formula()
#>   fit <- surv_fit(form, data = as.data.frame(.x))
#>   ggsurvplot(fit, data = as.data.frame(.x))
#> }


## -----------------------------------------------------------------------------
saveRDS(trial_summary, "cc.rds")

ld <- listdown(load_cc_expr = readRDS("cc.rds"),
               init_expr = source("decorators.r"),
               decorator = list(summary_table = make_summary,
                                survival_plot = make_survival_plot),
               echo = FALSE,
               message = FALSE)

ld_make_chunks(ld)[1:12]


## ----eval = FALSE-------------------------------------------------------------
#> library(knitr)
#> 
#> md_header <- ld_rmarkdown_header("Fake Data Trial Summary",
#>                                  output = "pdf_document")
#> 
#> ld_write_file(
#>   rmd_header = as.character(md_header),
#>   ld = ld_make_chunks(ld),
#>   file_name = "simple-data-trial-summary.rmd")
#> 
#> knit("simple-data-trial-summary.rmd")

```{.r .distill-force-highlighting-css}
```
