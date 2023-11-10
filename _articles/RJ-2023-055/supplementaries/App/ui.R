# Define UI for random distribution app ----

library(shinydashboard)
library(plotly)
library(leaflet)
#library(leafpop)
library(plotly)
library(shinyalert)


if(system("hostname", intern = TRUE) == "atlasmortalidad.uclm.es") {
  library("leafpop",
    lib.loc = "/home/atlas/R/x86_64-redhat-linux-gnu-library/3.5")
} else {
  library("leafpop")
}


# Header con logo
dbHeader <- dashboardHeader(title = "EXCESS MORTALITY APP",
 tags$li(a(href = '', #'https://www.uclm.es',
   img(src = '',
   title = "Excess Mortality App", height = "30px"),
   style = "padding-top:10px; padding-bottom:10px;"),
   class = "dropdown"
 )
)

ui <- dashboardPage(title = "Excess Mortality App",
    skin = "red", 
  dbHeader,
  dashboardSidebar(
    #useShinyalert(),  # Set up shinyalert
    sidebarMenu(id = "tabs",
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      menuItem("Spatial Analysis", tabName = "spatial", icon = icon("map"),
        menuSubItem("Summary", tabName = "spsummary"),
        menuSubItem("Excess Mortality", tabName = "spexcess")
      ),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("calendar", lib = "glyphicon"),
        menuSubItem("Summary", tabName = "tmpsummary"),
        #menuSubItem("Mortality", tabName = "tmpmortality"),
        menuSubItem("Excess Mortality", tabName = "tmpexcess"),
        menuSubItem("Observed and Excess Mortality", tabName = "tmpexcess2")#,
        #menuSubItem("Post. Prob.", tabName = "tmppprob")
      ),
      menuItem("Spatio-Temporal Analysis", tabName = "st", icon = icon("globe"),
        menuSubItem("Summary", tabName = "stsummary"),
        menuSubItem("Excess mortality", tabName = "stexcess")
      ),
      #menuItem("Help", tabName = "help", icon = icon("question")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    ),
    hr(),
    selectInput(
      "variable",
      "Variable", 
      list("Relative excess mortality" = "REM",
        "Number of excess deaths" = "NED")),
    selectInput(
      "statistic",
      "Statistic", 
      list("Median" = "median", "Posterior probability" = "pprob")
    ),
    selectInput("country", "Country", 
      #c("England", "Greece", "Italy", "Spain", "Switzerland"),
      c("Italy"),
      selected = "Switzerland"),
    selectInput("sex", "Sex", list("Both" = "B", "Females" = "F", "Males" = "M"),
      selected = "Females"),
    #selectInput("agegroup", "Age Group", c("All", "40<", "40-59", "60-69", "70-79", "80+"),
    # 40< results now shown because of small observed cases
    # however, they are included in 'All'
    selectInput("agegroup", "Age Group", c("All", "40<", "40-59", "60-69", "70-79", "80+"),
      selected = "All"),
    selectInput("aggregation", "Aggregation", list("National"  = "country", "Region (NUTS2)" = "region", "Province (NUTS3)" = "province"), selected = "region")
  ),
  dashboardBody(
    tags$head(includeHTML(("google-analytics.html"))),
    tabItems(
      tabItem(tabName = "introduction", fluidPage(includeMarkdown("introduction.Rmd"))),
      tabItem(tabName = "spsummary",
        fluidPage(
        h4("Summary"),
          #dataTableOutput('tabspsummary')
          tableOutput('tabspsummary'),
          fluidPage(includeMarkdown("legend.Rmd"))
        )
      ),
      tabItem(tabName = "spexcess",
        fluidPage(
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
          h4("Excess mortality"),
tags$style(type = "text/css", "#leafletexcess {height: calc(100vh - 110px) !important;}"),
            shinycssloaders::withSpinner(leafletOutput("leafletexcess"),
              type = 6, color = "#990033"),
            includeMarkdown("legend_spatial.Rmd")
        )
      ),
      tabItem(tabName = "temporal", 
        fluidPage(
          h4("Excess Mortality"),
          tags$style(type = "text/css", "#leaflettseries {height: calc(100vh - 230px) !important;}"),
          shinycssloaders::withSpinner(leafletOutput("leaflettseries"),
            type = 6, color = "#990033"),
        )),
     tabItem(tabName = "tmpsummary",
       fluidPage(
       h4("Summary (national level)"),
         #dataTableOutput('tabspsummary')
         tableOutput('tabtmpsummary'),
         h5("This summary only provides statistics of national estimates."),
         fluidPage(includeMarkdown("legend_tabtemp.Rmd")) 
       )
    ),
    tabItem(tabName = "tmpmortality",
      fluidPage(
        h4("Mortality"),
        plotlyOutput("mortalityplot")
      )
    ),
     tabItem(tabName = "tmpexcess",
       fluidPage(
         h4("Excess Mortality"),
         plotlyOutput("excessplot"),
         fluidPage(includeMarkdown("legend_temp.Rmd"))
         )
     ),
     tabItem(tabName = "tmpexcess2",
       fluidPage(
         h4("Observed and Estimated Number of Deaths"),
         #plotOutput("excessplot2"),
         plotlyOutput("excessplot2"),
         htmlOutput("datasource"),
         HTML("Please, select an <I>age</I> and <I>sex</I> group. Actual number of deaths are represented by the black line while estimated number of deaths (under the counterfactual of no pandemic) are shown in the dark green line. Credible intervals at different levels are shown using the shaded regions. Check paper for details.")
         )
     ),
     tabItem(tabName = "tmppprob",
       fluidPage(
         h4("Excess Mortality"),
         plotlyOutput("pprobplot")
         )
     ),
     tabItem(tabName = "stsummary",
       fluidPage(
       h4("Summary"),
         tableOutput('tabstsummary'),
         fluidPage(includeMarkdown("legend.Rmd")) 
       )
    ),
     tabItem(tabName = "stexcess",
       fluidPage(
         h4("Spatio-Temporal Analysis"),
         tags$style(type = "text/css", "#leaflettseries {height: calc(100vh - 230px) !important;}"),
          shinycssloaders::withSpinner(leafletOutput("spacetimeplot"),
            type = 6, color = "#990033"),
         fluidPage(includeMarkdown("legend_spatiotemp.Rmd"))
         )
     ),
      tabItem(tabName = "contact", fluidPage(includeMarkdown("contact.Rmd")))

    )
  )
)

