poccs_occClassify_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module poccs_occClassify")
  )
}

poccs_occClassify_module_server <- function(input, output, session, common) {

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

poccs_occClassify_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

poccs_occClassify_module_map <- function(map, common) {
  # Map logic
}

poccs_occClassify_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    poccs_occClassify_knit = species$rmm$code$wallace$someFlag,
    var1 = species$rmm$code$wallace$someSetting1,
    var2 = species$rmm$code$wallace$someSetting2
  )
}

