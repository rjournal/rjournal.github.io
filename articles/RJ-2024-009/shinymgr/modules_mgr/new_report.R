
new_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(
          4,
          tags$h2('1. Select Report'),
          selectizeInput(
            ns("select_app"),
            "Select an app",
            choices = dir('reports'),
            multiple = TRUE,
            selected = NULL,
            options = list(placeholder = 'Select an app to see available reports.', maxItems = 1)
          ),
          selectInput(
            ns('select_report'),
            'Select a report',
            choices = character(0)
          )
        ),
        column(
          4,
          tags$h2('2. Select Analysis Parameters'),
          uiOutput(ns('param_inputs'))
          # fileInput(
          #   ns("the_analysis"), 
          #   "Choose RDS File",
          #   multiple = FALSE,
          #   accept = ".RDS"
          # )
        ),
        column(
          4,
          tags$h2('3. Generate Report'),
          wellPanel(
            fluidRow(
              column(
                6,
                tags$h4('Download Report'),
                selectInput(
                  ns('output_type'),
                  label = NULL,
                  choices = c('pdf', 'html', 'word')
                ),
                downloadButton(
                  ns('generate_report'),
                  'Download Report'
                )
              ),
              column(
                6,
                tags$h4('View Report'),
                actionButton(
                  ns('view_report'),
                  'View in browser'
                )
              )
            )
          )
        )
      )
    ),
    uiOutput(ns('the_report'))
  )
}

new_report_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$select_app, {
      reports_avail <- list.files(
        paste('reports', input$select_app, sep = '/'), 
        pattern = ".Rmd$"
      )
      
      updateSelectInput(
        session,
        'select_report',
        'Select a report',
        choices = reports_avail
      )
      
      output$the_report <- renderUI({""})
    })
    
    observeEvent(input$select_report, {
      req(input$select_report)
      output$param_inputs <- renderUI({
        #get yaml header
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(
          paste('reports', input$select_app, input$select_report, sep = '/'), 
          tempReport, 
          overwrite = TRUE
        )
        rmdString <- readLines(tempReport)
        indices <- which(rmdString == "---")
        yamlheader <- rmdString[(min(indices)+1):(max(indices)-1)]
        yamllist <- yaml::yaml.load(yamlheader)
        #create taglist of ui stuff
        do.call(tagList, args =
          lapply(
            X = names(yamllist$params),
            FUN = function(X) {
              
              yamlName <- X
              
              #switch function to create all the ui stuff
              if (is.null(yamllist$params[[X]]$input)) {
                yamlInput <- yamllist$params[[X]]
                do.call(textInput, args = c(inputId = ns(yamlName), yamlInput))
              } else {
                #get rid of the input for shiny arguments
                yamlInput <- yamllist$params[[X]][-which(names(yamllist$params[[X]]) == "input")]
                switch(
                  yamllist$params[[X]]$input,
                  text = do.call(textInput, args = c(inputId = ns(yamlName), yamlInput)),
                  slider = do.call(sliderInput, args = c(inputId = ns(yamlName), yamlInput)),
                  checkbox = do.call(checkboxInput, args = c(inputId = ns(yamlName), yamlInput)),
                  numeric = do.call(numericInput, args = c(inputId = ns(yamlName), yamlInput)),
                  date = do.call(dateInput, args = c(inputId = ns(yamlName), yamlInput)),
                  select = do.call(selectInput, args = c(inputId = ns(yamlName), yamlInput)),
                  file = {
                    yamlInput <- yamlInput[-which(names(yamlInput) == "value")]
                    do.call(fileInput, args = c(inputId = ns(yamlName), yamlInput))
                  },
                  do.call(textInput, args = c(inputId = ns(yamlName), yamlInput))
                )
              }
            }
          )
        )
      })
    })
    
    output$generate_report <- downloadHandler(
      filename = function() {
        paste(
          'report', 
          ifelse(
            input$output_type == 'word',
            'doc',
            input$output_type
          ), 
          sep = '.'
        )
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(
          paste('reports', input$select_app, input$select_report, sep = '/'), 
          tempReport, 
          overwrite = TRUE
        )
        rmdString <- readLines(tempReport)
        indices <- which(rmdString == "---")
        yamlheader <- rmdString[(min(indices)+1):(max(indices)-1)]
        yamllist <- yaml::yaml.load(yamlheader)
        newParams <- list()
        for (X in names(yamllist$params)) {
          if (!is.null(yamllist$params[[X]]$input)) {
            if (yamllist$params[[X]]$input == "file") {
              newParams[[X]] <- input[[X]]$datapath
            } else {
              newParams[[X]] <- input[[X]]
            }
          } else {
            newParams[[X]] <- input[[X]]
          }
        }
        rmarkdown::render(
          tempReport, 
          output_file = file,
          output_format = paste(input$output_type, 'document', sep = '_'),
          params = newParams,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
    observeEvent(input$view_report, {
      req(input$select_report)
      output$the_report <- renderUI({
        
        #get yaml header (again)
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(
          paste('reports', input$select_app, input$select_report, sep = '/'), 
          tempReport, 
          overwrite = TRUE
        )
        rmdString <- readLines(tempReport)
        indices <- which(rmdString == "---")
        yamlheader <- rmdString[(min(indices)+1):(max(indices)-1)]
        yamllist <- yaml::yaml.load(yamlheader)
        #create list of params
        newParams <- list()
        for (X in names(yamllist$params)) {
          if (!is.null(yamllist$params[[X]]$input)) {
            if (yamllist$params[[X]]$input == "file") {
              newParams[[X]] <- input[[X]]$datapath
            } else {
              newParams[[X]] <- input[[X]]
            }
          } else {
            newParams[[X]] <- input[[X]]
          }
        }
        the_report <- rmarkdown::render(
          tempReport, 
          output_format = 'html_document',
          params = newParams
        )
        includeHTML(the_report)
      })
    })
  })
}
