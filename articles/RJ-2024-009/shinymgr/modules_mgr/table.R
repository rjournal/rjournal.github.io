table_ui <- function(id) {
  tagList(
    wellPanel(
      id = "tableName",
      style = "background: lightblue",
      textOutput(outputId = NS(id, id = "table_instructions"))
    ), #end wellPanel
    uiOutput(NS(id, "buttons")),
    br(),
    reactableOutput(outputId = NS(id, id = "show_table"))
  ) #end taglist
} #end ui function

table_server <- function(id, con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      #conditional delete button for apps and modules only
      if (id %in% c("apps", "modules", "reports")) {
        output$buttons <- renderUI({
          fluidRow(
            column(
              width = 3,
              actionButton(
                class = "btn-danger",
                inputId = ns("delete"),
                label = "Delete record",
                icon = icon("trash-alt")
              )
            ),
            column(
              width = 3,
              actionButton(
                class = "btn-secondary",
                inputId = ns("refresh"),
                label = "Refresh Table",
                icon = icon("sync")
              )
            )
          )
        }) #end render UI
      } else {
        output$buttons <- renderUI({
          fluidRow(
            column(
              width = 3,
              actionButton(
                class = "btn-secondary",
                inputId = ns("refresh"),
                label = "Refresh Table",
                icon = icon("sync")
              )
            )
          )
        }) 
      } #end ifelse for buttons
      
      #get list of column names for the table
      fieldlist <- DBI::dbListFields(conn = con(), name = id)
      #make dataframe of subset of dbDictionary
      fpath <- system.file("extdata", "dictionary.csv", package = "shinymgr")
      fullDictionary <- read.csv(fpath, na.strings = "")
      dictionary <- fullDictionary[fullDictionary$pkTableName == id,]
      
      
      #get primary keys
      primarykeys <- dictionary$pkFieldName[which(dictionary$pk == 1)]
      
      #get shinyTable entry
      spath <- system.file("extdata", "shinyTable.csv", package = "shinymgr")
      fullShinyTable <- read.csv(spath, na.strings = "")
      shinyTable <- fullShinyTable[fullShinyTable$fkTableName == id,]
      
      
      #instructions and set up table-------------
      output$table_instructions <- renderText({
        shinyTable[1, "description"]
      }) #end renderText
      
      # set a counter which will call the database when triggered
      update_trigger <- reactiveVal(1) 
      db_table <- eventReactive(
        eventExpr = {update_trigger()},
        valueExpr = {
          dbTab <- DBI::dbReadTable(conn = con(), name = id)
          columnOrder <- vector()
          endCols <- vector()
          for (i in 1:length(dictionary$pkFieldName)) {
            if (!is.na(dictionary$sortOrder[i])) {
              columnOrder <- c(columnOrder, dictionary$pkFieldName[i])
            } else {
              endCols <- c(endCols, dictionary$pkFieldName[i])
            }
          }
          columnOrder <- c(columnOrder, fieldlist[which(fieldlist %in% endCols)])
          dbTab <- dbTab[columnOrder]
        }
      )
      
      #setting up subTable ----------------------------
      if (nrow(shinyTable) != 0) {
        if (shinyTable$subTable != "" & !is.na(shinyTable$subTable)) {
          #get subDictionary
          subDictionary <- fullDictionary[fullDictionary$pkTableName == shinyTable$subTable,]
          
          #get subTable
          subTable <- eventReactive(
            eventExpr = {update_trigger()},
            valueExpr = {
              subTab <- DBI::dbGetQuery(
                conn = con(),
                statement = paste0(
                  "SELECT * FROM ", 
                  shinyTable$subTable,
                  ";"
                )
              )
              columnOrder <- subDictionary$pkFieldName
              subTab <- subTab[columnOrder]
            }
          )
          
        }
      }
      
      #create reactable--------------------
      output$show_table <- renderReactable({
        reactable(
          db_table(),
          selection = "single",
          filterable = TRUE,
          details = function(i) {
            if (nrow(shinyTable) != 0) {
              if (shinyTable$subTable != "" & !is.na(shinyTable$subTable)) {
                
                #render reactable
                output[[paste0("subTable_", i)]] <- renderReactable({
                  reactable(
                    subTable()[subTable()[[
                      subDictionary$pkFieldName[
                        which(subDictionary$foreignKeyField %in% primarykeys)
                      ]
                    ]] == db_table()[[primarykeys]][i], ],
                    outlined = TRUE
                  )
                })
                
                reactableOutput(NS(id, paste0("subTable_", i)))
              }
            } else {
              NULL
            }
          },
          onClick = "select"
        )
      })
      
      #delete row button-------------------
      observeEvent(
        eventExpr = input$delete,
        handlerExpr = {
          #get selected row
          rowIndex <- getReactableState(outputId = "show_table", name = "selected")
          
          if (is.null(rowIndex)) {
            showModal(
              modalDialog(
                title = "No row selected.",
                "Please select a row to delete.", 
                easyClose = TRUE
              )
            )
          } else {
            selectedRow <- db_table()[rowIndex[1],]
            
            if (id == "apps") {
              showModal(
                modalDialog(
                  title = "Confirm deletion of row:",
                  print(selectedRow[1,1]),
                  br(),
                  "Note that any associated tabs in the database will also be deleted.",
                  checkboxInput(
                    inputId = ns("fileDelete"),
                    label = "Delete any associated files",
                    value = FALSE
                  ),
                  footer = tagList(
                    actionButton(
                      inputId = NS(id, "confirm_delete"),
                      label = "Confirm",
                      class = "btn-danger"
                    ),
                    modalButton("Cancel")
                  )
                )
              ) #end modal
              
            } else {
              showModal(
                modalDialog(
                  title = "Confirm deletion of row:",
                  print(selectedRow[1,1]),
                  br(),
                  checkboxInput(
                    inputId = ns("fileDelete"),
                    label = "Delete any associated files",
                    value = FALSE
                  ),
                  footer = tagList(
                    actionButton(
                      inputId = NS(id, "confirm_delete"),
                      label = "Confirm",
                      class = "btn-danger"
                    ),
                    modalButton("Cancel")
                  )
                )
              ) #end modal 
            }
            
          }
        } #end handlerExpr
      ) #end delete row first button
      
      #delete confirmation----------------------
      observeEvent(
        eventExpr = input$confirm_delete,
        handlerExpr = {
          #get selected row
          rowIndex <- getReactableState(outputId = "show_table", name = "selected")

          selectedRow <- db_table()[rowIndex[1],]
          selectedDict <- dictionary
          tableName <- id
          tablePk <- selectedDict$pkFieldName[which(selectedDict$pk == 1)]
          
          #create the delete statement
          deleteStatement <- paste0(
            "DELETE FROM ", 
            tableName,
            " WHERE ")
          
          for (i in tablePk) {
            deleteStatement <- paste0(
              deleteStatement,
              i,
              " = '",
              selectedRow[[i]],
              "' AND "
            )
          } #end loop
            
          deleteStatement <- substr(deleteStatement, 1, nchar(deleteStatement)-5)
          deleteStatement <- paste0(deleteStatement, ";")
          
          #create another delete statement for tabs if deleting from the apps table
          if (id == "apps") {
            
            #get list of tabs from database
            tabNames <- DBI::dbGetQuery(
              con(),
              statement = paste0(
                "SELECT fkTabName FROM appTabs WHERE fkAppName = '", 
                selectedRow[1,"pkAppName"],
                "';"
              )
            )$fkTabName #end query
            
            deleteTabStatement <- paste0(
              "DELETE FROM tabs WHERE pkTabName IN ('",
              paste(tabNames, collapse = "','"),
              "');"
            )
          }

          #delete the row (trycatch)
          tryCatch(
            expr = {
              
              deleted <- 0
              # Turn on SQLite foreign key constraints
              rs <- DBI::dbSendQuery(con(), statement = "PRAGMA foreign_keys = ON;")
              dbClearResult(rs)
              if (id == "apps") {
                deleted <- deleted + dbExecute(conn = con(), statement = deleteTabStatement)
              }
              deleted <- deleted + dbExecute(conn = con(), statement = deleteStatement)
              
              #only runs if deletion works - update tables
              update_trigger(update_trigger() + 1)
              # db_table <- db_table()[-selectedRow,]
              
              #delete app file if it's still in the directory
              if (id == "apps" & input$fileDelete == TRUE) {
                if (file.exists(paste0(shinyMgrPath, "/modules_app/", selectedRow[1,"pkAppName"], ".R"))) {
                  file.remove(paste0(shinyMgrPath, "/modules_app/", selectedRow[1,"pkAppName"], ".R"))
                }
              }
              
              #if module, delete from module directory
              if (id == "modules" & input$fileDelete == TRUE) {
                if (file.exists(paste0(shinyMgrPath, "/modules/", selectedRow[1,"pkModuleName"], ".R"))) {
                  file.remove(paste0(shinyMgrPath, "/modules/", selectedRow[1,"pkModuleName"], ".R"))
                }
              }
              
              #if module, delete from module directory
              if (id == "reports" & input$fileDelete == TRUE) {
                file.remove(
                  list.files(
                    path = paste0(shinyMgrPath, "/reports"),
                    pattern = paste0(selectedRow[1, "pkReportName"], ".Rmd"),
                    recursive = TRUE,
                    full.names = TRUE
                  )
                )
              }
              
              showModal(
                modalDialog(
                  title = "Deletion successful",
                  paste0("Rows deleted: ", deleted),
                  easyClose = TRUE
                )
              )
            },
            error = function(x) {
              showModal(
                modalDialog(
                  title = "Row could not be deleted",
                  x,
                  easyClose = FALSE
                )
              )
            }
          ) #end trycatch
        }
      ) #end observe delete confirmation
      
      #refresh table (update trigger) ----------------------
      observeEvent(
        eventExpr = input$refresh,
        handlerExpr = {
          update_trigger <- update_trigger(update_trigger() + 1)
        }
      )
    }
  ) #end moduleServer
}
