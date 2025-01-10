source("global.R")

server <- function(input, output, session) {
  
  # call database module
  output$my_db_output <- renderUI({
    my_db_ui("my_db")
  })
  
  # make reactive connection object
  con <- reactiveVal({
    DBI::dbConnect(
      drv = RSQLite::SQLite(), 
      dbname = paste0(
        shinyMgrPath, 
        "/database/shinymgr.sqlite")
      )
  })
  
  # call server functions
  isolate({
    my_db_server("my_db", con)
    lapply(
      X = DBI::dbListTables(conn = con()), 
      FUN = function(X) {
        table_server(X, con)
      }
    ) 
  })

  # control the reactive db connection object 
  observeEvent(
    eventExpr = {
      input$dev_tool_tabs
      input$tabs
    }, 
    handlerExpr = {
      if (input$tabs == "DevTools" & input$dev_tool_tabs == "shinymgr_db") {
        print("connecting to database...")
        con(
          DBI::dbConnect(
            drv = RSQLite::SQLite(), 
            dbname = paste0(shinyMgrPath, "/database/shinymgr.sqlite"))
          )

      } else {
        if (DBI::dbIsValid(con())) {
          print("disconnecting...")
          DBI::dbDisconnect(con())
          print(con())
        } # end disconnecting if still connected
      } # end not being on database tab
    } # end handler expr
  ) # end observe event
  
  # disconnect from the database when the session is ended
  session$onSessionEnded(function(){
    isolate({
      print("session ended")
      if (DBI::dbIsValid(con())) {
        print("disconnecting...")
        DBI::dbDisconnect(con())
        print(con())
      } # end disconnecting if still connected
    })
  })
  
  # also disconnect if session stops
  onStop(function(){
    isolate({
      print("session stopped")
      if (DBI::dbIsValid(con())) {
        print("disconnecting...")
        DBI::dbDisconnect(con())
        print(con())
      } # end disconnecting if still connected
    })
  })
  
  # call the  new_analyses module ui -----------------------------
  output$new_analysis <- renderUI({
    new_analysis_ui("new_analysis")
  })
  
  new_analysis_server(
    id = "new_analysis", 
    tabSelect = reactive({input$tabs}), 
    shinyMgrPath = shinyMgrPath
    )
  
  # call the new_report module ui -----------------------------
  output$new_report <- renderUI({
    new_report_ui("new_report")
  })
  
  new_report_server(
    id = "new_report"
    )
  
  # call the buildApp module ui -----------------------------
  output$build_app <- renderUI({
    app_builder_ui("app_builder")
  })

  reset_builder <- app_builder_server(
    'app_builder',
    shinyMgrPath = shinyMgrPath
  )

  observeEvent(reset_builder$reset, {
    if (reset_builder$reset()) {
      # remove and re-insert builder tab
      output$build_app <- renderUI({
        app_builder_ui("app_builder")
      })
    }
  })
  
  # call the add_report module ui and server ----------
  output$add_report_output <- renderUI({
    add_report_ui("add_report")
  })
  
  add_report_server(
    id = "add_report", 
    shinyMgrPath = shinyMgrPath
  )
  
  #call the query module ui and server -----------
  output$query_output <- renderUI({
    queries_ui("queries")
  })
  queries_server(
    id = "queries",
    shinyMgrPath = shinyMgrPath
  )
  
} # end of server function
