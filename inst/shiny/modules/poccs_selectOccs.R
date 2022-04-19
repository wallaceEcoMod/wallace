poccs_selectOccs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    strong("Select occurrences intersecting drawn polygon"), br(),
    "(", HTML("<font color='blue'><b>NOTE</b></font>"),
    ': to begin drawing, click hexagon icon on map toolbar,
    and when complete, press "Finish" and then the "Select Occurrences" button)', br(), br(),
    actionButton(ns("goSelectOccs"), "Select Occurrences"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetOccs"), "Reset", class = 'butReset'),
    strong(" to original occurrence")
  )
}

poccs_selectOccs_module_server <- function(input, output, session, common) {

  logger <- common$logger
  occs <- common$occs
  spp <- common$spp
  curSp <- common$curSp

  observeEvent(input$goSelectOccs, {
    occs.sel <- poccs_selectOccs(occs(),
                                 spp[[curSp()]]$polySelXY,
                                 spp[[curSp()]]$polySelID,
                                 logger,
                                 spN = curSp())
    req(occs.sel)

    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.sel

    # REFERENCES ####
    knitcitations::citep(citation("leaflet.extras"))

    # METADATA ####
    polyX <- printVecAsis(round(spp[[curSp()]]$polySelXY[,1], digits = 4))
    polyY <- printVecAsis(round(spp[[curSp()]]$polySelXY[,2], digits = 4))
    spp[[curSp()]]$rmm$code$wallace$occsSelPolyCoords <- paste0('X: ', polyX, ', Y: ', polyY)

    common$update_component(tab = "Map")
  })

  # reset occurrences button functionality
  observeEvent(input$goResetOccs, {
    req(curSp())
    spp[[curSp()]]$occs <- spp[[curSp()]]$occData$occsCleaned
    spp[[curSp()]]$rmm$code$wallace$occsSelPolyCoords <- NULL
    spp[[curSp()]]$procOccs$occsThin <- NULL
    spp[[curSp()]]$rmm$code$wallace$removedIDs <- NULL
    logger %>% writeLog(
      hlSpp(curSp()), "Reset to original occurrences (n = ",
      nrow(spp[[curSp()]]$occs), ").")
  })
}

poccs_selectOccs_module_map <- function(map, common) {
  occs <- common$occs
  # Map logic
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs()) %>%
    leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                   rectangleOptions = FALSE, circleOptions = FALSE,
                                   markerOptions = FALSE, circleMarkerOptions = FALSE,
                                   editOptions = leaflet.extras::editToolbarOptions())
}

poccs_selectOccs_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    poccs_selectByID_knit = !is.null(species$rmm$code$wallace$occsSelPolyCoords),
    selectByID_xy_rmd = printVecAsis(species$polySelXY),
    selectByID_id_rmd = species$polySelID
  )
}

