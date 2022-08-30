## ---- load-ped
library(rwalkr)
library(tsibble)
library(tsibbletalk)
library(plotly)
library(tidyverse)
library(lubridate)

sensors <- c("Birrarung Marr", "Bourke Street Mall (North)",
  "QV Market-Elizabeth St (West)", "Southern Cross Station")

pedestrian20 <- melb_walk_fast(year = 2020, sensor = sensors) %>% 
  filter(Date < ymd("2020-06-01")) %>% 
  mutate(Lockdown = ifelse(Date > ymd("2020-03-16"), "Yes", "No")) %>% 
  as_tsibble(index = Date_Time, key = Sensor)

## ---- ped-slice
library(shiny)
p_line <- pedestrian20 %>%
  ggplot(aes(x = Date_Time, y = Count, colour = Lockdown)) +
  geom_line(size = .3) +
  facet_wrap(~ Sensor, scales = "free_y") +
  labs(x = "Date Time") +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "none")

ui <- fluidPage(
  tsibbleWrapUI("dice")
)
server <- function(input, output, session) {
  tsibbleWrapServer("dice", ggplotly(p_line, height = 700), period = "1 day")
}
shinyApp(ui, server)
