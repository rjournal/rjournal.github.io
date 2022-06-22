## ---- load-pkgs
library(feasts)
library(tsibble)
library(tsibbledata)
library(patchwork)

## ---- print-retail
print(aus_retail, n = 5)

## ---- highlight-retail
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
