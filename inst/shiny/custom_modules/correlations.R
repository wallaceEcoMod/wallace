correlations_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    ## Add a checkbox for batch processing (more than 1 species)
    tags$div(
      title = "Add Batch guidance text here (**)",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)),
    ## Give the action button a name and a label.
    actionButton(ns("runCorrs"), label = "Calculate correlations"),
    ## UI for reselecting variables after calculating correlations
    uiOutput(ns("VarSelect")),
    actionButton(ns("selectConfirm"), label = "Select Variables")
    )
}

correlations_module_server <- function(input, output, session, common) {
  ## Define common objects that will be used in this function
  logger <- common$logger
  spp    <- common$spp
  allSp  <- common$allSp
  curSp  <- common$curSp
  bgMask <- common$bgMask # raster stack after masking by background
  VarSelector <- common$VarSelector

  output$VarSelect <- renderUI({
    req(bgMask())
    shinyWidgets::pickerInput(
      "VarSelector",
      label = "Select variables (**)",
      choices = setNames(as.list(names(bgMask())), names(bgMask())),
      multiple = TRUE,
      selected = names(bgMask()),
      options = list(`actions-box` = TRUE))
  })

  ## When action button defined in the ui function above is clicked, do the following:
  observeEvent(input$runCorrs, {
    # Check that the background has already been selected
    req(bgMask())

    # Set up if you want batch to be allowed
    # allSp() is the list of species selected
    # curSp() refers to the currently selected species in the GUI
    if (input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    # If batch is true, loop through all species (allSp())
    for (sp in spLoop){
      # FUNCTION CALL ####
      smartProgress(logger, message = "Calculating pairwise correlations", {envCorrs <- raster::layerStats(x = spp[[sp]]$procEnvs$bgMask, na.rm = T, stat = "pearson")})
      # To update the log window
      logger %>% writeLog(hlSpp(sp), "Finished calculating correlations")
      #envCorrs <- raster::layerStats(x = bgMask(), na.rm = T, stat = "pearson")

      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$envCorrs <- envCorrs$`pearson correlation coefficient`
      # METADATA ####
    }
    # Switch to Results tab to display results
    common$update_component(tab = "Results")
  })

  # Define output as a table
  output$envCorrTable <- renderText({
    # Result
    knitr::kable(spp[[curSp()]]$procEnvs$envCorrs, format = 'html')
  })

  ## Observe when selection is confirmed
  observeEvent(input$selectConfirm, {
    req(spp[[curSp()]]$procEnvs$envCorrs)

    ## update bg object
    spp[[curSp()]]$procEnvs$bgMask <- spp[[curSp()]]$procEnvs$bgMask[[VarSelector()]]

    # Add a line to logger to identify which variables were selected
    # hlSpp() prints the species name in green, bold, and italics
    logger %>% writeLog(hlSpp(curSp()), "Selected: ", paste0(names(spp[[curSp()]]$procEnvs$bgMask), collapse = ", "))
  })
}

## Add code to update the map between species
correlations_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs

  if (is.null(spp[[curSp()]]$procEnvs$bgExt)) {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
  } else {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
    polys <- spp[[curSp()]]$procEnvs$bgExt@polygons[[1]]@Polygons
    if (length(polys) == 1) {
      xy <- list(polys[[1]]@coords)
    } else {
      xy <- lapply(polys, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "gray",
                    group = 'bgShp')
    }
    bb <- spp[[curSp()]]$procEnvs$bgExt@bbox
    map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
  }
}


correlations_module_result <- function(id) {
  ns <- NS(id)
  #spp <- common$spp
  #curSp <- common$curSp
  # Result UI as html
  htmlOutput(ns("envCorrTable"))
}

correlations_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    # correlations_knit = !is.null(spp[[sp]]$procEnvs$envCorrs),
    # correlations_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

