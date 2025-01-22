add_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns('select_mod')),
    actionButton(ns('confirm_mod'), 'Confirm Module')
  )
}

add_mod_server <- function(id, available_mods, mod_table) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$select_mod <- renderUI({
      selectInput(ns('select_mod'), 'Select module', choices = available_mods()$mod_name)
    })
    
    observeEvent(input$confirm_mod, {
      shinyjs::disable('select_mod')
      shinyjs::disable('confirm_mod')
    })
    
    return_val <- eventReactive(input$confirm_mod, {
      reactiveValues(
        mod_name = reactive(input$select_mod)
      )
    })

    return_val
  })
}
