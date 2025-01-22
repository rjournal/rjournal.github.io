add_app_metadata_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      ns('app_name'),
      'pkAppName'
    ),
    textInput(
      ns('app_display_name'),
      'Display Name'
    ),
    textInput(
      ns('app_descrip'),
      'Description'
    ),
    textInput(
      ns('app_videoURL'),
      'App Video URL'
    ),
    textInput(
      ns('app_CSS'),
      'Bootswatch theme name or CSS file name'
    ),
    textAreaInput(
      ns('app_notes'),
      'Notes', 
      rows = 6, 
      cols = 4
    ),
    checkboxInput(
      ns('app_active'),
      label = 'App Active', 
      value = TRUE
    )
  )
}

add_app_metadata_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactiveValues(
        app_name = reactive(input$app_name),
        app_display_name = reactive(input$app_display_name),
        app_descrip = reactive(input$app_descrip),
        app_videoURL = reactive(input$app_videoURL),
        app_CSS = reactive(input$app_CSS),
        app_notes = reactive(input$app_notes),
        app_active = reactive(input$app_active)
    )
  })
}
