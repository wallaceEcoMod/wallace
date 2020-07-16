envs_ecoclimate_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    tags$div(title = 'Select AOGCM',
             selectInput(ns("bcAOGCM"),
                         label = "Select the Atmospheric Oceanic General Circulation Model you want to use (**)",
                         choices = list("Select AOGCMs" = "",
                                        "CCSM" = "CCSM",
                                        "CNRM" = "CNRM",
                                        "MIROC" = "MIROC",
                                        "FGOALS" = "FGOALS",
                                        "GISS" = "GISS",
                                        "IPSL" = "IPSL",
                                        "MRI" = "MRI",
                                        "MPI" = "MPI")
                         )),
    tags$div(title = 'Select Scenario',
             selectInput(ns("bcScenario"),
                         label = "select the temporal scenario that you want to use (**)",
                         choices = list("Select Scenario" = "",
                                        "Present" = "Present",
                                        "Holocene (6,000 years ago)" = "Holo",
                                        "LGM (21,000 years ago)" = "LGM")
                        )),
    shinyWidgets::pickerInput(
      "bcSel",
      label = "Select bioclim variables (**)",
      choices = setNames(as.list(paste0('bio', sprintf("%02d", 1:19))),
                         paste0('bio', sprintf("%02d", 1:19))),
      multiple = TRUE,
      selected = paste0('bio', sprintf("%02d", 1:19)),
      options = list(`actions-box` = TRUE)),
    em("ecoClimate layers have a resolution of 0.5 degrees"), br(),
    actionButton(ns("goEcoClimData"), "Load Env Data")
  )
}

envs_ecoclimate_module_server <- function(input, output, session, common) {

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

envs_ecoclimate_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

envs_ecoclimate_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    envs_ecoclimate_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

