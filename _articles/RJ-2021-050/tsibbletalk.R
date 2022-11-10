## ----setup, echo = FALSE, cache = FALSE, include = FALSE----------------------
options("knitr.graphics.auto_pdf" = TRUE)
library(knitr)
opts_chunk$set(
  echo = FALSE, warning = FALSE, message = FALSE, comment = "#>",
  fig.path = 'figure/', fig.align = 'center', fig.show = 'hold',
  out.width = ifelse(is_html_output(), "100%", "\\textwidth")
)
opts_knit$set(root.dir = here::here())


## ----external-----------------------------------------------------------------
read_chunk("scripts/demo.R")
read_chunk("scripts/tourism.R")
read_chunk("scripts/pedestrian.R")


## ----load-pkgs----------------------------------------------------------------
library(feasts)
library(tsibble)
library(tsibbledata)
library(patchwork)


## ---- print-retail------------------------------------------------------------
print(aus_retail, n = 5)


## ----highlight-retail, fig.height = 3.6, fig.show = "hold", fig.cap = "Plots for the \\code{aus\\_retail} data, with the series of strongest seasonal strength highlighted. (a) An overlaid time series plot. (b) A scatter plot drawn from their time series features, where each dot represents a time series from (a)."----
library(tidyverse)
library(ggrepel)
library(gghighlight)

a <- aus_retail %>%
  as_tibble() %>% # gghighlight issue for group_by() + filter()
  mutate(group = paste(State, ":", Industry)) %>%
  ggplot(aes(x = Month, y = Turnover)) +
  geom_line(aes(group = group)) +
  gghighlight(
    State == "Queensland", Industry == "Department stores",
    label_params = list(vjust = 0, nudge_y = 1, label.size = 0.15)
  ) +
  scale_x_yearmonth(breaks = yearmonth(c("1990 Jan", "2000 Jan", "2010 Jan")))

b <- aus_retail %>%
  features(Turnover, feat_stl) %>%
  mutate(group = paste(State, ":", Industry)) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = group)) +
  labs(x = "Trend strength", y = "Seasonal strength") +
  gghighlight(
    State == "Queensland", Industry == "Department stores",
    use_direct_label = FALSE
  ) +
  geom_label_repel(aes(label = group), vjust = 0.4,
                   nudge_x = -0.1, label.size = 0.15)

a+b


## ----tourism-shared, echo = TRUE----------------------------------------------
library(tsibble)
library(tsibbletalk)
library(dplyr)
tourism_shared <- tourism_monthly %>%
  # Comment out the next line to run the full example
  filter(State %in% c("Tasmania", "Western Australia")) %>%
  mutate(Region = stringr::str_replace(Region, "Australia's ", "WA's ")) %>%
  as_shared_tsibble(spec = (State / Region) * Purpose)


## ----tourism-linking-fig, fig.cap = "Snapshot of exploring an ensemble of linked plots of the Australian tourism data, built on a \\code{tourism\\_shared} object. It also illustrates persistent linked brushing to compare two groups.", eval = knitr::is_latex_output()----
include_graphics("img/tourism-linking.png")


## ----plotly-key-tree, echo = TRUE---------------------------------------------
p_l <- plotly_key_tree(tourism_shared, height = 800, width = 800)


## ----tourism-series, echo = TRUE, eval = knitr::is_html_output()--------------
#> library(ggplot2)
#> p_tr <- tourism_shared %>%
#>   ggplot(aes(x = Month, y = Trips)) +
#>   geom_line(aes(group = Region), alpha = .5, size = .4) +
#>   facet_wrap(~ Purpose, scales = "free_y") +
#>   scale_x_yearmonth(date_breaks = "5 years", date_labels = "%Y")


## ----tourism-scatter, echo = TRUE, eval = knitr::is_html_output()-------------
#> library(feasts)
#> tourism_feat <- tourism_shared %>%
#>   features(Trips, feat_stl)
#> p_br <- tourism_feat %>%
#>   ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
#>   geom_point(aes(group = Region), alpha = .8, size = 2)


## ----tourism-multi, layout="l-body-outset", echo = TRUE, eval = knitr::is_html_output(), fig.cap = "Exploring an ensemble of linked plots of the Australian tourism data, built on a \\code{tourism\\_shared} object. Click one of the nodes in the hierarchical tree to enable persistent linked brushing to compare two groups. Points and lines can also be selected in other plots. (Only Western Australia and Tasmania are included for the interactive plot in the html version, so size reasons.)"----
#> library(plotly)
#> subplot(p_l,
#>   subplot(
#>     ggplotly(p_tr, tooltip = "Region", width = 700),
#>     ggplotly(p_br, tooltip = "Region", width = 700),
#>     nrows = 2),
#>   widths = c(.4, .6)) %>%
#>   highlight(dynamic = TRUE)


## ----wrap-ped, fig.show = "hold", fig.cap = "Snapshots wrapping after slicing the \\code{pedestrian20} data at different intervals, (a) none, (b) daily and (c) weekly. This type of interaction is made possible with Shiny elements.", fig.subcap = c("Initial overview state", "1-day state", "7-day state, anchoring to Monday"), fig.ncol = 1, eval = knitr::is_latex_output()----
include_graphics("img/wrap-0.png")
include_graphics("img/wrap-1.png")
include_graphics("img/wrap-7.png")


## ----wrap-ped-html, fig.show = "hold", fig.cap = "Animations showing wrapping after slicing the \\code{pedestrian20} data at different intervals, (a) none, (b) daily and (c) weekly. This type of interaction is made possible with Shiny elements.", fig.subcap = c("Initial overview state", "1-day state", "7-day state, anchoring to Monday"), fig.ncol = 1, eval = knitr::is_html_output()----
#> include_graphics("img/shiny-wrap.gif")


## ----load-ped, eval = FALSE---------------------------------------------------
#> library(rwalkr)
#> library(tsibble)
#> library(tsibbletalk)
#> library(plotly)
#> library(tidyverse)
#> library(lubridate)
#> 
#> sensors <- c("Birrarung Marr", "Bourke Street Mall (North)",
#>   "QV Market-Elizabeth St (West)", "Southern Cross Station")
#> 
#> pedestrian20 <- melb_walk_fast(year = 2020, sensor = sensors) %>%
#>   filter(Date < ymd("2020-06-01")) %>%
#>   mutate(Lockdown = ifelse(Date > ymd("2020-03-16"), "Yes", "No")) %>%
#>   as_tsibble(index = Date_Time, key = Sensor)


## ----ped-slice, echo = TRUE, eval = FALSE-------------------------------------
#> library(shiny)
#> p_line <- pedestrian20 %>%
#>   ggplot(aes(x = Date_Time, y = Count, colour = Lockdown)) +
#>   geom_line(size = .3) +
#>   facet_wrap(~ Sensor, scales = "free_y") +
#>   labs(x = "Date Time") +
#>   scale_colour_brewer(palette = "Dark2") +
#>   theme(legend.position = "none")
#> 
#> ui <- fluidPage(
#>   tsibbleWrapUI("dice")
#> )
#> server <- function(input, output, session) {
#>   tsibbleWrapServer("dice", ggplotly(p_line, height = 700), period = "1 day")
#> }
#> shinyApp(ui, server)


## ----pkg-bib, eval = FALSE----------------------------------------------------
#> pkgs <- c("tsibbledata", "feasts", "fable", "shiny", "htmlwidgets", "plotly",
#>   "rbokeh", "leaflet", "crosstalk", "loon")
#> write_bib(pkgs, "rpkgs.bib")

```{.r .distill-force-highlighting-css}
```
