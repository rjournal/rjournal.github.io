add_tab_metadata_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      ns('tab_name'),
      'pkTabName'
    ),
    textInput(
      ns('tab_display_name'),
      'Display Name'
    ),
    textAreaInput(
      ns('tab_inst'),
      'Instructions',
      rows = 6,
      cols = 4
    ),
    textAreaInput(
      ns('tab_notes'),
      'Tab Notes',
      rows = 6,
      cols = 4
    )
  )
}

add_tab_metadata_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactiveValues(
      tab_name = reactive(input$tab_name),
      tab_display_name = reactive(input$tab_display_name),
      tab_inst = reactive(input$tab_inst),
      tab_notes = reactive(input$tab_notes)
    )
  })
}
