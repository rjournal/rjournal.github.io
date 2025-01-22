#!! ModName = save_analysis
#!! ModDisplayName = Save Analysis
#!! ModDescription = Save the inputs and results of an analysis as an RDS file in a location chosen by the user
#!! ModCitation = Baggins, Bilbo.  2021.  save_analysis.  Source code.
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = input !! all user inputs from each module !! list
#!! FunctionArg = returns !! all user returns from each module !! list

save_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("username"),
      label = "Enter your name here:"
    ),
    br(),
    textAreaInput(
      inputId = ns("notes"),
      label = "Notes",
      placeholder = "Notes about this analysis"
    ),
    br(),
    downloadButton(
      outputId = ns("save"),
      label = "Save Analysis as RDS"
    ),
    br()
  )
}

save_analysis_server <- function(id, appName, moduleInput, returns, metadata) {
  moduleServer(id, function(input, output, session) {
    output$save <- downloadHandler(
      filename = function() {
        paste0(appName, "_", input$username, "_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".RDS")
      },
      content = function(file) {

        # Initialize variable to hold analysis data
        analysis <- list(
          analysisName = paste0(
            appName, "_", 
            input$username, "_", 
            format(Sys.time(), "%Y_%m_%d_%H_%M")
          )
        )
        
        analysis[['app']] <- appName
        analysis[['username']] <- input$username

        inputNames <- names(moduleInput)
        for (i_return in 1:length(returns)) {
          for (i_input in grep(paste0('mod',i_return), inputNames)) {
            analysis[[inputNames[i_input]]] <- moduleInput[[inputNames[i_input]]]
          }
        }

        analysis[['returns']] <- returns # Add return data to analysis
        analysis[['notes']] <- input$notes
        analysis[['timestamp']] <- Sys.time()
        analysis[['metadata']] <- metadata #Add metadata
        
        #save whole app code
        app_code <- paste(
          readLines(
            paste0(
              getwd(), 
              '/modules_app/',
              appName,
              ".R"
            )
          ), 
          collapse = '\n'
        )
        
        analysis[['app_code']] <- app_code
        
        #save whole module codes for unique modules
        allMods <- vector()
        for (i_mod in grep('mod', names(metadata))) {
          allMods <- c(allMods, metadata[[i_mod]][["modName"]])
        }
        uniqueMods <- unique(allMods)
        for (modName in uniqueMods) {
          modCode <- paste(
            readLines(
              paste0(
                getwd(),
                '/modules/',
                modName,
                ".R"
              )
            ),
            collapse = '\n'
          )
          analysis[[paste0(modName, "_code")]] <- modCode
        }

        #save everything to an rds
        saveRDS(analysis, file = file)
      }
    )
  })
}
