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

          ## Unsuitable for NAs
          r[r == 0] <- NA
          ## Raster to polygon
          p <- terra::as.polygons(terra::rast(r))
          ## Project to SR-ORG 8287
          p <- terra::project(p, y = getWKT("wcea"))
          areaRange <- terra::expanse(p, unit = "km")
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
          } else {
            r <- switch (selAreaSource(),
                         wallace = spp[[curSp()]]$visualization$mapPred,
                         xfer = spp[[curSp()]]$transfer$mapXfer,
                         user = spp[[curSp()]]$mask$userSDM,
                         mask = spp[[curSp()]]$mask$prediction)
            r <- terra::rast(r)
            ## Unsuitable for NAs
            r[r == 0] <- NA
            p.pts <- terra::as.points(r) %>%
              terra::geom() %>% data.frame() %>%
              dplyr::select(x, y)
          }
          wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          # Create a minimum convex polygon around the occurrences
          eoo <- changeRangeR::mcp(p.pts, wgs84)
          # Project to WCEA (IUCN recommendation)
          eooPr <- terra::project(terra::vect(eoo), y = getWKT("wcea"))
          areaEOO <- terra::expanse(eooPr, unit = "km")
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
                        " (", round(areaEOO, 2), " km^2). ",
                        "This is an approximation using the ",
                        "World Cylindrical Equal Area projection ",
                        "(IUCN recommendation)."))

      # LOAD INTO SPP ####
      spp[[curSp()]]$rmm$data$indic$EOOarea <- areaEOO
      spp[[curSp()]]$rmm$data$indic$EOOsource <- selAreaSource()
      spp[[curSp()]]$rmm$data$indic$EOOpoly <- eoo
      common$update_component(tab = "Map")
      ## if calculating AOO
    } else if (input$indicRangeSel == "aoo") {
      r <- switch (selAreaSource(),
                   wallace = spp[[curSp()]]$visualization$mapPred,
                   xfer = spp[[curSp()]]$transfer$mapXfer,
                   user = spp[[curSp()]]$mask$userSDM,
                   mask = spp[[curSp()]]$mask$prediction)
      ## Unsuitable for NAs
      r[r == 0] <- NA
      ###
      if (selAreaSource() == "occs") {
        p.pts <- spp[[curSp()]]$occs %>%
          dplyr::select(longitude, latitude) %>%
          terra::vect(geom = c("longitude", "latitude"))
        terra::crs(p.pts) <- getWKT("wgs84")
        p.pts <- terra::project(p.pts, getWKT("wcea"))
        rast_temp <- terra::rast(terra::ext(p.pts), resolution = 2000,
                                 crs = getWKT("wcea"))
        AOOproj <- terra::rasterize(p.pts, rast_temp, field = 1)
        AOOarea <- terra::freq(AOOproj, value = 1)$count * 4
        AOOraster <- terra::project(AOOproj, getWKT("wgs84"))
      } else {
        AOO <- changeRangeR::AOOarea(r = r)
        AOOraster <- terra::rast(AOO$aooRaster) %>%
          terra::project(getWKT("wgs84"))
        AOOraster[AOOraster == 0] <- NA
        # Project to WCEA (IUCN recommendation)
        AOOv <- terra::as.polygons(AOOraster) %>%
          terra::project(y = getWKT("wcea"))
        AOOarea <- terra::expanse(AOOv, unit = "km")
      }
      req(AOOraster)
      logger %>%
        writeLog(hlSpp(curSp()),
                 paste0("Calculated an AOO estimate based on ",
                        switch (selAreaSource(),
                                occs = "occurrences",
                                wallace = "wallace SDM",
                                xfer = "transferred SDM",
                                user = "user-specified SDM",
                                mask = "masked SDM"),
                        " (", round(AOOarea, 2), " km^2). ",
                        "This is an approximation using the ",
                        "World Cylindrical Equal Area projection ",
                        "(IUCN recommendation)."))
      # LOAD INTO SPP ####
      spp[[curSp()]]$rmm$data$indic$AOOarea <- AOOarea
      spp[[curSp()]]$rmm$data$indic$AOOsource <- selAreaSource()
      spp[[curSp()]]$rmm$data$indic$AOOraster <- methods::as(AOOraster, "Raster")
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
    if (!is.null(spp[[curSp()]]$rmm$data$indic$EOOarea)) {
      resTxt <- paste0(resTxt,
        "EOO (",  spp[[curSp()]]$rmm$data$indic$EOOsource, "): ",
        round(spp[[curSp()]]$rmm$data$indic$EOOarea, 2)," km^2","\n")
    }
    if (!is.null(spp[[curSp()]]$rmm$data$indic$AOOarea)) {
      resTxt <- paste0(resTxt,
        "AOO (", spp[[curSp()]]$rmm$data$indic$AOOsource, "): ",
        round(spp[[curSp()]]$rmm$data$indic$AOOarea, 2)," km^2"
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
  map %>% clearAll()
  if(!is.null(spp[[curSp()]]$rmm$data$indic$EOOpoly)){

    polyEOO <- spp[[curSp()]]$rmm$data$indic$EOOpoly@polygons[[1]]@Polygons
    bb <- spp[[curSp()]]$rmm$data$indic$EOOpoly@bbox
    bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = "gray",
                title = "EOO", labels = "EOO",
                opacity = 1)
    ##ADD polygon
    if (length(polyEOO) == 1) {
      xy <- list(polyEOO[[1]]@coords)
    } else {
      xy <- lapply(polyEOO, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "gray",
                    group = 'indic')
    }
  }
  if (!is.null(spp[[curSp()]]$rmm$data$indic$AOOraster)) {
    AOOras <- spp[[curSp()]]$rmm$data$indic$AOOraster
    zoomExt <- raster::extent(AOOras)
    map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                      lat1 = zoomExt[3], lat2 = zoomExt[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = c('red', 'grey'),
                title = "AOO",
                labels = c("Presence", "Absence"),
                opacity = 1, layerId = 'expert') %>%
      ##ADD polygon
      addRasterImage(AOOras, colors = c('red', 'grey'),
                     opacity = 0.7, group = 'indic', layerId = 'AOO',
                     method = "ngb")
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

