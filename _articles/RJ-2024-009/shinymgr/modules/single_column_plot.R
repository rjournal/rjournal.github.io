#!! ModName = single_column_plot
#!! ModDisplayName = Plot Single Column
#!! ModDescription = Uses qplot to plot a column in a dataset
#!! ModCitation = Baggins, Bilbo.  (2022). single_column_plot. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = dataset !! dataframe to be explored !! data.frame
#!! FunctionReturn = selectedCol !! name of column selected !! string
#!! FunctionReturn = g !! plot of column distribution !! ggproto
#!! Package = ggplot2 !! 3.3.5 !!


# the ui function
single_column_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        uiOutput(outputId = ns("selectCol"))
      ),
      column(
        width = 8,
        plotOutput(outputId = ns("fig"))
      )
    )
  )
}


# the server function
single_column_plot_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    #create dropdown of
    ns <- session$ns
    output$selectCol <- renderUI({
      selectInput(
        inputId = ns("selectedCol"),
        label = "Select a column",
        choices = names(dataset())
      )
    })
    
    
    g <- reactive({
      req(input$selectedCol)
      columnValues <- dataset()[[input$selectedCol]]
      qplot(x = columnValues, main = paste0(input$selectedCol, " distribution")) +
        theme_classic()
    })
    
    output$fig <- renderPlot(g())
    #return column and plot
    return(
      reactiveValues(
        selectedCol = reactive(input$selectedCol),
        g = reactive(g())
      )
    )
  })
}
