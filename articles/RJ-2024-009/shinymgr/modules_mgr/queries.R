queries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dropdowns")),
    fluidRow(
      column(
        3,
        actionButton(
          inputId = ns("app_flow"),
          label = "Query App Flow"
        )
      ),
      column(
        3,
        actionButton(
          inputId = ns("app_stitching"),
          label = "Query App Stitching"
        )
      ),
      column(
        3,
        actionButton(
          inputId = ns("mod_info"),
          label = "Query Mod Info"
        )
      )
    ),
    h2("Query Result:"),
    reactableOutput(ns("query_result"))
  )
}


queries_server <- function(id, shinyMgrPath) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #app/mod dropdown
    output$dropdowns <- renderUI({
      fluidRow(
        column(
          6,
          selectInput(
            inputId = ns("app_name"),
            label = "Select an app to query:",
            choices = sort(qry_row(
              tableName = 'apps',
              colConditions = 'pkAppName',
              shinyMgrPath = shinyMgrPath
            )$pkAppName)
          )
        ),
        column(
          6,
          selectInput(
            inputId = ns("mod_name"),
            label = "Select a mod to query:",
            choices = sort(qry_row(
              tableName = 'modules',
              colConditions = 'pkModuleName',
              shinyMgrPath = shinyMgrPath
            )$pkModuleName)
          )
        )
      )
    })
    
    #query app flow
    observeEvent(input$app_flow, {
      output$query_result <- renderReactable(
        isolate(
          reactable(
            data = qry_app_flow(
              appName = input$app_name,
              shinyMgrPath = shinyMgrPath
            )
          )
        )
      )
    })
    
    #query app stitching
    observeEvent(input$app_stitching, {
      output$query_result <- renderReactable(
        isolate(
          reactable(
            data = qry_app_stitching(
              appName = input$app_name,
              shinyMgrPath = shinyMgrPath
            )
          )
        )
      )
    })
    
    observeEvent(input$mod_info, {
      output$query_result <- renderReactable(
        isolate(
          reactable(
            data = qry_mod_info(
              modName = input$mod_name,
              shinyMgrPath = shinyMgrPath
            )
          )
        )
      )
    })
    
  })
}
