my_db_ui <- function(id) {
  fpath <- system.file("extdata", "shinyTable.csv", package = "shinymgr")
  shinyTable <- read.csv(fpath, na.strings = "")
  tagList(
    wellPanel(
      id = "database",
      textOutput(outputId = NS(id, id = "welcome")) #end textOutput
    ), #end wellPanel
    do.call(
      tabsetPanel,
      c(
        id = NS(id, id = "main_tab"),
        type = "tabs",
        lapply(
          X = unique(shinyTable$primaryTab), 
          FUN = function(X) {
            tabPanel(
              title = X,
              do.call(
                tabsetPanel,
                c(
                  id = paste0(X, "_tabgroup"),
                  type = "tabs",
                  lapply(
                    X = shinyTable$fkTableName[shinyTable$primaryTab == X],
                    FUN = function(i) {
                      tabPanel(
                        title = i,
                        value = i,
                        uiOutput(NS(id, paste0(i, "_output")))
                      )
                    }
                  ) #end inner lapply
                )
              ) #end inner tabSetPanel
            ) #end primary tabPanel
          }
        ) #end outer lapply
      ) #end arguments
    )
  ) #end tagList
} #end ui function

my_db_server <- function(id, con) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #welcome message
      output$welcome <- renderText(
        expr = "This database page maintains a constant connection to the .sqlite database
        and is only disconnected when you navigate away from this tab."
      )
      
      #creating content for each tab
      lapply(
        X = DBI::dbListTables(
          conn = con()
        ), #end query
        FUN = function(X) {
          output[[paste0(X, "_output")]] <- renderUI({table_ui(X)})
        }
      )
      
    } #end moduleServer function
  ) #end moduleServer
} #end server function
