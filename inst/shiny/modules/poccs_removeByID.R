poccs_removeByID_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    numericInput(ns("removeID"), label = "Enter the record ID to be removed",
                 value = 0),
    actionButton(ns("goRemoveByID"), "Remove Occurrence")
  )
}

poccs_removeByID_module_server <- function(input, output, session, common) {

  logger <- common$logger
  occs <- common$occs
  spp <- common$spp
  curSp <- common$curSp

  observeEvent(input$goRemoveByID, {
    # FUNCTION CALL ####
    occs.rem <- poccs_removeByID(occs(),
                                 input$removeID,
                                 logger,
                                 spN = curSp())
    print(input$removeID)
    req(occs.rem)
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.rem

    # METADATA ####
    # if no removeIDs are recorded yet, make a list to record them
    # if at least one exists, add to the list
    if (is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs)) {
      spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <- input$removeID
    } else {
      spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <-
        c(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs,
          input$removeID)
    }

    common$update_component(tab = "Map")
  })

  # return(list(
  #   save = function() {
  #     # Save any values that should be saved when the current session is saved
  #   },
  #   load = function(state) {
  #     # Load
  #   }
  # ))

}

poccs_removeByID_module_map <- function(map, common) {
  occs <- common$occs
  # Map logic
  map %>% leaflet.extras::removeDrawToolbar() %>%
    clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs())
}

poccs_removeByID_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    removeByID_knit = !is.null(spp[[sp]]$rmm$code$wallaceSettings$removedIDs),
    removeByID_id = spp[[sp]]$rmm$code$wallaceSettings$removedIDs
  )
}

