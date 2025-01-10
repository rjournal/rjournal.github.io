ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = tags$div(
      tags$img(src = "shinymgr-hexsticker.png", height = "50px", width = "50px"),
      "shinymgr"
    )
  ),
  shinydashboard::dashboardSidebar(
    hr(),
    # https://fontawesome.com/icons?d=gallery&m=free
    shinydashboard::sidebarMenu(
      id = "tabs",
      menuItem(
        text = "New Analysis", 
        tabName = "NewAnalysis", 
        icon = icon("chart-pie")
        ),
      menuItem(
        text = "Generate Reports", 
        tabName = "GenerateReports", 
        icon = icon("file-alt")
        ),
      menuItem(
        text = "Developer Tools", 
        tabName = "DevTools", 
        icon = icon("wrench")
        )
    ) # end sidebarMenu
  ), # end dashboardSidebar

  shinydashboard::dashboardBody(
    tabItems(
      
      # New Analysis
      tabItem(
        tabName = "NewAnalysis",
        uiOutput("new_analysis")
      ), 
      
      # Generate Reports
      tabItem(
        tabName = "GenerateReports",
        uiOutput("new_report")
      ), 

      # developer Tools
      tabItem(
        tabName = "DevTools",
        tabsetPanel(
          id = "dev_tool_tabs",
          
          # builder goes in first tab
          tabPanel(
            title = "Build App",
            value = "build_tab",
            uiOutput("build_app")
          ),
          
          # database tab
          tabPanel(
           title = "Database",
           value = "shinymgr_db",
           uiOutput("my_db_output")
          ),
          
          # queries
          tabPanel(
            title = "Queries",
            value = "query_db",
            uiOutput("query_output")
          ),
          
          # tab for adding reports
          tabPanel(
            title = "Add Report",
            value = "add_report_tab",
            uiOutput("add_report_output")
          )
        ) # end tabsetPanel
      ) # end of builder page
    ) # end of tabItems
  ) # end of dashboardBody
) # end dashboard page
