## d-title h1, d-title p, d-title figure {

##   grid-column: page;

## }


## ----message = FALSE, warning = FALSE-----------------------------------------
library(readr)

adakiep <- read_csv("adakiep.csv") 
adakiep


## ---- eval = FALSE------------------------------------------------------------
#> devtools::install_github("kaneplusplus/forceps@v0.0.5")


## ----message = FALSE, warning = FALSE-----------------------------------------
library(forceps)

data(lc_adverse_events)
data(lc_biomarkers)
data(lc_demography)
data(lc_adsl)

consolidated_describe_data(lc_adverse_events,
                           lc_biomarkers,
                           lc_demography,
                           lc_adsl)


## ---- message = FALSE---------------------------------------------------------
library(dplyr)

data(lc_adverse_events)

lc_adverse_events %>% head()

lc_adverse_events <- lc_adverse_events %>%
  cohort(on = "usubjid", name = "ae_long")

lc_adverse_events %>% head()


## -----------------------------------------------------------------------------
data(lc_adsl)
data(lc_biomarkers)
data(lc_demography)
data_list <- list(demography = lc_demography, 
                  biomarkers = lc_biomarkers, 
                  adverse_events = lc_adverse_events, 
                  adsl = lc_adsl)
duplicated_vars(data_list, on = "usubjid")

data_list$demography <- data_list$demography %>% 
  select(-chemo_stop)


## -----------------------------------------------------------------------------
consolidate(data_list, on = "usubjid")

```{.r .distill-force-highlighting-css}
```
