
new_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("apps_available")),
    shinyjs::disabled(actionButton(
      ns('start_new_analysis'),
      'Select New Analysis'
    )),
    actionButton(
      ns('reset_new_analysis'),
      'Reset Analysis'
    ),
    wellPanel(
      tags$div(id = 'app_ui_goes_here')
    )
  )
}

new_analysis_server <- function(id, tabSelect, shinyMgrPath) {
  moduleServer(id, function(input, output, session) {
    ns_ct <- reactiveVal(1)
    ns <- session$ns
    
    # Making sure buttons are enabled properly, when required
    needs_reset <- reactiveValues(reset = reactive(TRUE))
    observe({
      if (needs_reset$reset()) {
        shinyjs::enable('select_analysis')
        shinyjs::enable('start_new_analysis')
        needs_reset$reset <- reactive(FALSE)
      }
    })
    
    # Get a list of available apps (from the database)
    analysis_list <- sort(qry_row(
      'apps',
      list(
        appActive = 1
      ),
      'pkAppName',
      shinyMgrPath
    )$pkAppName)
    
    # Get a list of available apps (from the directory contents)
    # NOTE: Un-comment the below code (and delete the code above) after development 
    # is complete to remove the dependency on the shinymgr sqlite database.
    # analysis_list <- sort(tools::file_path_sans_ext(dir('modules_app')))
    
    #  Render selectizeInput for choosing which analysis to launch
    output$apps_available <- renderUI({
      shinyjs::disabled(selectizeInput(
        ns("select_analysis"),
        "Select an analysis",
        choices = analysis_list,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = 'Select an analysis to run.', maxItems = 1)
      ))
    })

    observeEvent(input$start_new_analysis, {
      
      req(input$select_analysis)
      
      shinyjs::disable('start_new_analysis')
      shinyjs::disable('select_analysis')
      
      # Insert the UI for the App
      insertUI(
        selector = '#app_ui_goes_here',
        where = "beforeEnd",
        ui = tags$div(
          id = 'app_ui',
          tagList(
            eval(
              parse(
                text = paste0(
                  input$select_analysis,
                 '_ui(ns("mod_',
                  ns_ct(),
                  '"))'
                )
              )
            )
          )
        )
      )
  
      # Run the server for the App
      eval(
        parse(
          text = paste0(
            'rslt <- ',
            input$select_analysis,
            '_server("mod_',
            ns_ct(),
            '", shinyMgrPath)'
          )
        )
      )
    
      ns_ct(ns_ct() + 1) # Increment namespace counter
      
    })
    
    # Reset the analysis
    observeEvent(input$reset_new_analysis, {
      req(input$start_new_analysis)
      needs_reset$reset <- reactive(TRUE)
      
      removeUI(
        selector = '#app_ui',
        session = session
      )
    })

    # Needed to re-enable buttons on tab change before "start" button clicked once
    observeEvent(tabSelect(), {
      if (!is.null(input$start_new_analysis) && input$start_new_analysis == 0) {
        needs_reset$reset <- reactive(TRUE)
      }
    })
  })
}
