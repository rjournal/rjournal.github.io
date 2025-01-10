add_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      id = "reportHeader",
      style = "background: lightblue",
      textOutput(outputId = ns("reportInstructions"))
    ),
    fileInput(
      inputId = ns("pkReportName"),
      label = "Select the file containing the report template:",
      multiple = FALSE
    ),
    textInput(
      inputId = ns("displayName"),
      label = "Report Display Name:"
    ),
    textAreaInput(
      inputId = ns("description"),
      label = "Report Description:",
      placeholder = "A description of the report"
    ),
    uiOutput(outputId = ns("appSelect")),
    textAreaInput(
      inputId = ns("notes"),
      label = "Notes:",
      placeholder = "Notes about connecting the report to this app"
    ),
    fluidRow(
      column(
        width = 2,
        actionButton(
          inputId = ns("submit"),
          label = "Add Report"
        )
      ),
      column(
        width = 2,
        actionButton(
          inputId = ns("cancel"),
          label = "Cancel"
        )
      )
    )
  )
  
} #end ui function

add_report_server <- function(id, shinyMgrPath) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #make selector for the apps
    output$appSelect <- renderUI({
      selectInput(
        inputId = ns("selectedApp"),
        label = "Which app is this report template for?",
        choices = c("", sort(qry_row(
          tableName = "apps", 
          colConditions = "pkAppName", 
          shinyMgrPath = shinyMgrPath
        )$pkAppName))
      )
    })
    
    #make text instructions
    output$reportInstructions <- renderText({
      "Register a .Rmd template in the database and connect it with an app.
      This tab records these templates in the reports and appReports table, but does 
      not store the files themselves. However, the files are copied to a subdirectory
      of the reports folder named after the app."
    })
    
    #cancel button erases everything in the inputs (and maybe the file too?)
    observeEvent(input$cancel, {
      updateSelectInput(
        session = session,
        inputId = "selectedApp",
        selected = ""
      )
      
      updateTextInput(
        session = session,
        inputId = "displayName",
        value = ""
      )
      
      updateTextAreaInput(
        session = session,
        inputId = "description",
        value = ""
      )
      
    }) #end cancel button press
    
    #submit button press
    observeEvent(input$submit, {
      #check that report file name is unique
      existingReports <- qry_row(
        tableName = "reports",
        colConditions = "pkReportName",
        shinyMgrPath = shinyMgrPath
      )[,]
      
      reportName <- gsub(pattern = "(\\w+)\\..*", replacement = "\\1", x = input$pkReportName$name,)
      print(reportName)
      
      if (reportName %in% existingReports) {
        showModal(
          modalDialog(
            title = "A report with this filename already exists in the database.",
            "Please delete the record or upload a different file.",
            easyClose = FALSE
          )
        )
      } else {
        #create dataframe to add to reports table
        newReport <- data.frame(
          pkReportName = reportName,
          displayName = input$displayName,
          reportDescription = input$description
        )
        
        #create dataframe to add to appReports table
        newAppReport <- data.frame(
          fkAppName = input$selectedApp,
          fkReportName = reportName,
          notes = input$notes
        )
        
        #trycatch to append each table and clear inputs if successful
        tryCatch(
          expr = {
            #create folder for the app if it doesn't already exist
            if (!dir.exists(paste0(shinyMgrPath, "/reports/", input$selectedApp))) {
              dir.create(paste0(shinyMgrPath, "/reports/", input$selectedApp))
            }
            
            #write the file in the folder
            reportPath <- paste0(shinyMgrPath, "/reports/", input$selectedApp, "/", reportName, ".Rmd")
            #create file if it doesn't already exist
            if (!file.exists(reportPath)) {
              file.create(paste0(shinyMgrPath, "/reports/", input$selectedApp, "/", reportName, ".Rmd"))
            }
            templateCon <- file(input$pkReportName$datapath)
            template <- readLines(templateCon)
            close(templateCon)
            
            fileCon <- file(paste0(shinyMgrPath, "/reports/", input$selectedApp, "/", reportName, ".Rmd"))
            writeLines(template, fileCon)
            close(fileCon)
            
            #append reports table
            reportResult <- qry_insert(
              tableName = "reports", 
              rowValues = newReport, 
              shinyMgrPath = shinyMgrPath
            )
            
            #append appReports table
            appReportResult <- qry_insert(
              tableName = "appReports",
              rowValues = newAppReport,
              shinyMgrPath = shinyMgrPath
            )
            
            #clear inputs
            updateSelectInput(
              session = session,
              inputId = "selectedApp",
              selected = ""
            )
            
            updateTextInput(
              session = session,
              inputId = "displayName",
              value = ""
            )
            
            updateTextAreaInput(
              session = session,
              inputId = "description",
              value = ""
            )
            
            #show modal when done
            showModal(
              modalDialog(
                title = "Done",
                paste0(reportResult, "row was added to the reports table."),
                br(),
                paste0(appReportResult, "row was added to the appReports table."),
                easyClose = TRUE
              )
            )
            
          },
          error = function(e) {
            showModal(
              modalDialog(
                title = "Report could not be added:",
                x,
                easyClose = FALSE
              )
            )
          }
        ) #end trycatch
      } #end else statement
    }) #end submit button click

  }) #end moduleServer
} #end server function
