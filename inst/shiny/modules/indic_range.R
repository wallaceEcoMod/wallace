indic_range_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("indicRangeSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Range size" = "range",
                               "EOO" = "eoo",
                               "AOO" = "aoo")),
    uiOutput(ns("indicRangeSourceUI")),
    actionButton(ns("goRange"), "Calculate")
  )
}

indic_range_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapXfer <- common$mapXfer
  selAreaSource <- common$selAreaSource

  output$indicRangeSourceUI <- renderUI({
    req(curSp())
    if (input$indicRangeSel != "") {
      n <- c()
      l <- c()
      if (!is.null(spp[[curSp()]]$occs)) {
        if (input$indicRangeSel != "range") {
          n <- c("occs", n)
          l <- c("Occurrences", l)
        }
      }
      if (!is.null(spp[[curSp()]]$visualization$mapPred)) {
        n <- c("wallace", n)
        l <- c("Wallace SDM", l)
      }
      if (!is.null(spp[[curSp()]]$transfer$mapXfer)) {
        n <- c("xfer", n)
        l <- c("Transferred SDM", l)
      }
      if (!is.null(spp[[curSp()]]$mask$userSDM)) {
        n <- c("user", n)
        l <- c("User-specified SDM", l)
      }
      if (!is.null(spp[[curSp()]]$mask$prediction)) {
        n <- c("mask", n)
        l <- c("Masked SDM", l)
      }
      indicRangeSourceList <- setNames(as.list(n), l)
      shinyWidgets::pickerInput("selAreaSource",
                                label = "Select source for calculations",
                                choices = indicRangeSourceList,
                                multiple = FALSE)
    }
  })

  observeEvent(input$goRange, {
    # WARNING ####
    if (is.null(selAreaSource())) {
      logger %>%
        writeLog(type = 'warning',
                 hlSpp(curSp()),
                 "No source available for calculations (**).")
      return()
    }
    # ERRORS ####
    if (selAreaSource() == "occs") {
      if (is.null(spp[[curSp()]]$occs)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   "Get or upload occurrence data for this species before ",
                   "doing calculations (names must match).")
        return()
      }
    }
    if (selAreaSource() == "wallace") {
      if (is.null(spp[[curSp()]]$visualization$thresholds)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Generate a thresholded model before doing calculations.')
        return()
      }
    }
    if (selAreaSource() == "xfer") {
      if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Generate a thresholded prediction before doing calculations')
        return()
      }
    }
    if (selAreaSource() == "user") {
      if (!shiny::isTruthy(spp[[curSp()]]$mask$userThr)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Load a user thresholded prediction before doing calculations.')
        return()
      }
    }
    if (selAreaSource() == "mask") {
      if (!spp[[curSp()]]$mask$maskThr) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Mask a thresholded prediction before doing calculations.')
        return()
      }
    }

    #Processing
    if (input$indicRangeSel == "range") {
      smartProgress(logger, message = "Calculating a range size...",
        expr = {
          # FUNCTION CALL ####
          ## Select raster
          r <- switch (selAreaSource(),
                       wallace = spp[[curSp()]]$visualization$mapPred,
                       xfer = spp[[curSp()]]$transfer$mapXfer,
                       user = spp[[curSp()]]$mask$userSDM,
                       mask = spp[[curSp()]]$mask$prediction)
          areaRange <- indic_area(r, wkt = getWKT("wcea"), logger)
        }
      )
      req(areaRange)
      logger %>%
        writeLog(hlSpp(curSp()),
                 paste0("Species range size calculated based on ",
                        switch (selAreaSource(),
                                wallace = "wallace SDM",
                                xfer = "transferred SDM",
                                user = "user-specified SDM",
                                mask = "masked SDM"),
                        " (", round(areaRange, 2), " km^2)."))
      # LOAD INTO SPP ####
      spp[[curSp()]]$indic$rangeArea <- areaRange
      spp[[curSp()]]$indic$rangeSource <- selAreaSource()
      common$update_component(tab = "Results")
    ## if calculating EOO
    } else if (input$indicRangeSel == "eoo") {
      smartProgress(
        logger,
        message = "Calculating an EOO estimate...", {
          if (selAreaSource() == "occs") {
            p.pts <- spp[[curSp()]]$occs %>%
              dplyr::select(longitude, latitude)
            eoo <- indic_eoo(occs = p.pts, lon = "longitude", lat = "latitude",
                             wkt = getWKT("wcea"), logger = logger)
          } else {
            r <- switch (selAreaSource(),
                         wallace = spp[[curSp()]]$visualization$mapPred,
                         xfer = spp[[curSp()]]$transfer$mapXfer,
                         user = spp[[curSp()]]$mask$userSDM,
                         mask = spp[[curSp()]]$mask$prediction)
            eoo <- indic_eoo(r, wkt = getWKT("wcea"), logger = logger)
          }
        })
      req(eoo)
      logger %>%
        writeLog(hlSpp(curSp()),
                 paste0("Calculated an EOO estimate based on ",
                        switch (selAreaSource(),
                                occs = "occurrences",
                                wallace = "wallace SDM",
                                xfer = "transferred SDM",
                                user = "user-specified SDM",
                                mask = "masked SDM"),
                        " (", round(eoo$area, 2), " km^2). ",
                        "This is an approximation using the ",
                        "World Cylindrical Equal Area projection ",
                        "(IUCN recommendation)."))

      # LOAD INTO SPP ####
      spp[[curSp()]]$indic$EOOarea <- eoo$area
      spp[[curSp()]]$indic$EOOsource <- selAreaSource()
      spp[[curSp()]]$indic$EOOpoly <- eoo$eooPoly
      if (selAreaSource() != "occs") {
        r[r == 0] <- NA
        spp[[curSp()]]$indic$EOObaseRaster <- r
      }
      spp[[curSp()]]$flags$indicAreaMap <- "eoo"
      common$update_component(tab = "Map")
      ## if calculating AOO
    } else if (input$indicRangeSel == "aoo") {
      if (selAreaSource() == "occs") {
        p.pts <- spp[[curSp()]]$occs %>%
          dplyr::select(longitude, latitude)
        aoo <- indic_aoo(occs = p.pts, lon = "longitude", lat = "latitude",
                         wktFrom = getWKT("wgs84"), wktTo = getWKT("wcea"),
                         logger = logger)

      } else {
        r <- switch (selAreaSource(),
                     wallace = spp[[curSp()]]$visualization$mapPred,
                     xfer = spp[[curSp()]]$transfer$mapXfer,
                     user = spp[[curSp()]]$mask$userSDM,
                     mask = spp[[curSp()]]$mask$prediction)
        aoo <- indic_aoo(r, wktFrom = getWKT("wgs84"), wktTo = getWKT("wcea"),
                         logger = logger)
      }
      req(aoo)
      logger %>%
        writeLog(hlSpp(curSp()),
                 paste0("Calculated an AOO estimate based on ",
                        switch (selAreaSource(),
                                occs = "occurrences",
                                wallace = "wallace SDM",
                                xfer = "transferred SDM",
                                user = "user-specified SDM",
                                mask = "masked SDM"),
                        " (", round(aoo$area, 2), " km^2). ",
                        "This is an approximation using the ",
                        "World Cylindrical Equal Area projection ",
                        "(IUCN recommendation)."))
      # LOAD INTO SPP ####
      spp[[curSp()]]$indic$AOOarea <- aoo$area
      spp[[curSp()]]$indic$AOOsource <- selAreaSource()
      spp[[curSp()]]$indic$AOOraster <- aoo$AOOraster
      spp[[curSp()]]$flags$indicAreaMap <- "aoo"
      common$update_component(tab = "Map")
    }
  })

  output$areas <- renderText({
    resTxt <- c()
    # Result
    if (!is.null(spp[[curSp()]]$indic$rangeArea)) {
      resTxt <- paste0(
        "Range (", spp[[curSp()]]$indic$rangeSource, "): ",
        round(spp[[curSp()]]$indic$rangeArea, 2)," km^2", "\n")
    }
    if (!is.null(spp[[curSp()]]$indic$EOOarea)) {
      resTxt <- paste0(resTxt,
        "EOO (",  spp[[curSp()]]$indic$EOOsource, "): ",
        round(spp[[curSp()]]$indic$EOOarea, 2)," km^2","\n")
    }
    if (!is.null(spp[[curSp()]]$indic$AOOarea)) {
      resTxt <- paste0(resTxt,
        "AOO (", spp[[curSp()]]$indic$AOOsource, "): ",
        round(spp[[curSp()]]$indic$AOOarea, 2)," km^2"
      )
    }
    if (is.null(resTxt)) {
      "Run area calculation (**)"
    } else {
      resTxt
    }
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        indicRangeSel = input$indicRangeSel
        )
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'indicRangeSel', selected = state$indicRangeSel)
    }
  ))
}

indic_range_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("areas"))
}

indic_range_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs
  req(spp[[curSp()]]$flags$indicAreaMap)

  if (spp[[curSp()]]$flags$indicAreaMap == "eoo") {
    map %>% clearAll()
    polyEOO <- spp[[curSp()]]$indic$EOOpoly@polygons[[1]]@Polygons
    bb <- spp[[curSp()]]$indic$EOOpoly@bbox
    bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = "darkorange",
                labels = "EOO", opacity = 1)
    ##ADD polygon
    if (length(polyEOO) == 1) {
      xy <- list(polyEOO[[1]]@coords)
    } else {
      xy <- lapply(polyEOO, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4,
                    color = "darkorange", group = 'indic')
    }
    if (spp[[curSp()]]$indic$EOOsource == "occs") {
      map %>%
        addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                         radius = 5, color = 'red', fill = TRUE, fillColor = 'red',
                         fillOpacity = 0.2, weight = 2, popup = ~pop)
    } else {
      map %>%
        addLegend("bottomright", colors = 'darkgrey', labels = "Range map",
                  opacity = 1, layerId = 'prediction') %>%
        leafem::addGeoRaster(spp[[curSp()]]$indic$EOObaseRaster,
                             colorOptions = leafem::colorOptions(
                               palette = colorRampPalette(colors = 'darkgrey')),
                             opacity = 0.7, group = 'indic', layerId = 'indicRange')
    }
  } else if (spp[[curSp()]]$flags$indicAreaMap == "aoo") {
    map %>% clearAll()
    AOOras <- spp[[curSp()]]$indic$AOOraster
    # AOO raster transform to polygon for visualization
    AOOpol <- terra::rast(AOOras)
    AOOpol <- terra::as.polygons(AOOpol)
    AOOpol <- AOOpol %>% terra::project(getWKT("wgs84")) %>%
      sf::st_as_sf()

    # Zoom
    bb <- sf::st_bbox(AOOpol) %>% as.vector()
    bbZoom <- polyZoom(bb[1], bb[2], bb[3], bb[4], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])

    map %>%
      ##Add legend
      addLegend("bottomright", colors = "darkorange",
                labels = "AOO",
                opacity = 1, layerId = 'aooLegend') %>%
      ##ADD polygon
      leafem::addFeatures(AOOpol, fillColor = 'darkorange', fillOpacity = 0.9,
                          opacity = 0, group = 'indic', layerId = 'indicAOO')
    if (spp[[curSp()]]$indic$AOOsource == "occs") {
      map %>%
        addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                         radius = 5, color = 'red', fill = TRUE, fillColor = 'red',
                         fillOpacity = 0.2, weight = 2, popup = ~pop)
    }
  }
}

indic_range_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  # list(
  #   indic_range_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  # )
}

