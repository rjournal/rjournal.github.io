#!! ModName = subset_rows
#!! ModDisplayName = Subset Rows (random)
#!! ModDescription = Randomly selects rows to create a subset of a dataframe
#!! ModCitation = Baggins, Bilbo.  (2022). subset_rows. [Source code].
#!! ModNotes = 
#!! ModActive = 1
#!! FunctionArg = dataset !! dataframe to be subset !! data.frame
#!! FunctionReturn = subset_data !! subset of original data !! data.frame
#!! Package = reactable !! 0.3.0 !!

subset_rows_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      #make the sidebar with the numeric input, resample button, and whole dataframe
      sidebarPanel(
        numericInput(
          inputId = ns("sample_num"),
          label = "Number of rows to sample",
          value = 10,
          min = 1
        ),
        actionButton(
          inputId = ns("resample"),
          label = "Re-sample"
        ),
        br(), #extra line break to leave some space between the button and the table
        reactableOutput(ns("full_table"))
      ), #end of sidebar panel
      
      #have the main panel display the subset dataframe
      mainPanel(
        h2("These rows were randomly chosen:"),
        reactableOutput(ns("subset_table"))
      ) #end of main panel
    ) #end of sidebar layout
  )
} #end of ui function

subset_rows_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    #create the reactable object that will display the full table
    output$full_table <- renderReactable({
      reactable(data = dataset(), rownames = TRUE)
    })
    
    #create a vector of random indices based on the number selected that also listens for the button click
    index <- reactive({
      input$resample
      sample(1:nrow(dataset()), size = input$sample_num, replace = FALSE)
    })
    
    #create the reactable object that will display the subset table
    output$subset_table <- renderReactable({
      reactable(dataset()[index(),], rownames = TRUE)
    })
    
    return(
      reactiveValues(
        subset_data = reactive({
          dataset()[index(),]
        })
      )
    )
    
  }) #end moduleServer function
} #end server function
