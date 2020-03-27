poccs_thinOccs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$p(
      paste0('The minimum distance between occurrence locations (nearest ',
             'neighbor distance) in km for resulting thinned dataset. Ideally ',
             'based on species biology (e.g., home-range size).')
      ),
    numericInput(ns("thinDist"), label = "Thinning distance (km)",
                 value = 10), # Check default (value = 0)
    checkboxInput(ns("batch"), label = strong("Batch"),
                  value = TRUE), # Check default (value = FALSE)
    actionButton(ns("goThinOccs"), "Thin Occurrences")
  )
}

poccs_thinOccs_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  allSp <- common$allSp

  observeEvent(input$goThinOccs, {

    # loop over all species if batch is on
    if (input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for (sp in spLoop) {
      # FUNCTION CALL ####
      occs.thin <- poccs_thinOccs(spp[[sp]]$occs,
                                  input$thinDist,
                                  logger,
                                  spN = sp)
      req(occs.thin)

      # LOAD INTO SPP ####
      # record present occs before thinning (this may be different from occData$occOrig)
      spp[[sp]]$procOccs$occsPreThin <- spp[[sp]]$occs
      spp[[sp]]$occs <- occs.thin
      spp[[sp]]$procOccs$occsThin <- occs.thin

      # METADATA ####
      # perhaps there should be a thinDist metadata field?
      spp[[sp]]$rmm$code$wallaceSettings$thinDistKM <- input$thinDist
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

poccs_thinOccs_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs
  # Map logic
  # if you've thinned already, map thinned points blue
  # and kept points red
  if (!is.null(spp[[curSp()]]$procOccs$occsThin)) {

    occs.preThin <- spp[[curSp()]]$procOccs$occsPreThin
    map %>% clearAll() %>%
      addCircleMarkers(data = occs.preThin, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "blue",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      addLegend("bottomright", colors = c('red', 'blue'), title = "Occ Records",
                labels = c('retained', 'removed'), opacity = 1)
  } else {
    # if you haven't thinned, map all points red
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
  }
}

poccs_thinOccs_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    poccs_thinOccs_knit = FALSE
    # poccs_thinOccs_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

