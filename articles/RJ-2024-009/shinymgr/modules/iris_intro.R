#!! ModName = iris_intro
#!! ModDisplayName = Iris Explorer introduction Page
#!! ModDescription = This module is simply a page of text with instructions for the iris explorer module.
#!! ModCitation = Baggins, Bilbo.  (2022). iris_intro. [Source code].
#!! ModActive = 1


# the ui function
iris_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      textOutput(ns("instructions"))
    )
  )
}


# the server function
iris_intro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$instructions <- renderText({
      "These are instructions for the iris_explorer app. This app clusters data by user-specified columns,
      then takes a random subset of the data. The inputs and returns of each module can be downloaded as
      an .RDS file."
    })
  })
}
