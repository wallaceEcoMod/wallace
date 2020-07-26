{{id}}_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module {{id}}")
  )
}

{{id}}_module_server <- function(input, output, session, common) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO SPP ####

    # METADATA ####
  })

  output$result <- renderText({
    # Result
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

{{id}}_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

{{id}}_module_map <- function(map, common) {
  # Map logic
}

{{id}}_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    {{id}}_knit = species$rmm$code$wallace$someFlag,
    var1 = species$rmm$code$wallace$someSetting1,
    var2 = species$rmm$code$wallace$someSetting2
  )
}

