indic_time_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    ##inputs must be: SDM so step 1 remains
    #Environmental variables (e g forest thorugh time as rasters (multiple))
    #Threshold numeric input
    #years used (numeric input?)
    span("Step 1:", class = "step"),
    span("Choose Input range", class = "stepText"), br(), br(),
    uiOutput(ns("indicTimeSourceUI")),
    actionButton(ns("goInputRangeTime"), "Select"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Choose environmental data", class = "stepText"), br(), br(),
    fileInput(ns("indicEnvs"), label = "Upload environmental rasters",
              accept = c(".tif", ".asc"), multiple = TRUE),
    textInput(ns("EnvThrVal"), "Set threshold value",
              placeholder = "for single threshold: 40, for range: 30,60 ", value=""),
    selectInput(ns("selBound") , label = "Select bounds to be used for calculations",
                choices = list("Lower" = "lower",
                               "Upper" = "upper",
                               "Not Applicable" = "neither",
                               "Both" = "both")),
    actionButton(ns("goInputEnvs"), "Load"),
    tags$hr(class = "hrDotted"),
    span("Step 3:", class = "step"),
    span("Choose years", class = "stepText"), br(), br(),
    textInput(ns("Years"), label = "Enter years to be used",
              placeholder = 'format: 2000, 2002',
              value = ""),
    actionButton(ns("goInputYears"), "Calculate")
  )
}

indic_time_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  selTimeSource <- common$selTimeSource

  # STEP 1 #####
  output$indicTimeSourceUI <- renderUI({
    req(curSp())
    n <- c()
    l <- c()
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
    if (!is.null(spp[[curSp()]]$indic$EOOpoly)) {
      n <- c("eoo", n)
      l <- c("Extent of Occurrence (EOO)", l)
    }
    if (!is.null(spp[[curSp()]]$indic$AOOraster)) {
      n <- c("aoo", n)
      l <- c("Area of Ocupancy (AOO)", l)
    }
    indicTimeSourceList <- setNames(as.list(n), l)
    shinyWidgets::pickerInput("selTimeSource",
                              label = "Select source for calculations",
                              choices = indicTimeSourceList,
                              multiple = FALSE)
  })

  observeEvent(input$goInputRangeTime, {
    # WARNING ####
    if (is.null(selTimeSource())) {
      logger %>%
        writeLog(type = 'warning',
                 hlSpp(curSp()),
                 "No source available for calculations (**).")
      return()
    }
    # ERRORS ####
    if (selTimeSource() == "wallace") {
      if (is.null(spp[[curSp()]]$visualization$thresholds)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Generate a thresholded model before doing calculations.')
        return()
      }
    }
    if (selTimeSource() == "xfer") {
      if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Generate a thresholded prediction before doing calculations')
        return()
      }
    }
    if (selTimeSource() == "user") {
      if (!shiny::isTruthy(spp[[curSp()]]$mask$userThr)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Load a user thresholded prediction before doing calculations.')
        return()
      }
    }
    if (selTimeSource() == "mask") {
      if (!spp[[curSp()]]$mask$maskThr) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Mask a thresholded prediction before doing calculations.')
        return()
      }
    }
    if (selTimeSource() == "eoo") {
      if (is.null(spp[[curSp()]]$indic$EOOpoly)) {
        logger %>%
          writeLog(
            type = 'error',
            'Do an EOO calculation in the area module before doing range calculations')
        return()
      }
    }
    if (selTimeSource() == "aoo") {
      if (is.null(spp[[curSp()]]$indic$AOOraster)) {
        logger %>%
          writeLog(
            type = 'error',
            'Do an AOO calculation in the area module before doing range calculations')
        return()
      }
    }
    rangeMap <- switch (selTimeSource(),
                        wallace = spp[[curSp()]]$visualization$mapPred,
                        xfer = spp[[curSp()]]$transfer$mapXfer,
                        user = spp[[curSp()]]$mask$userSDM,
                        mask = spp[[curSp()]]$mask$prediction,
                        eoo = sf::st_as_sf(spp[[curSp()]]$indic$EOOpoly),
                        aoo = spp[[curSp()]]$indic$AOOraster)
    # Transform to polygon
    if ("RasterLayer" %in% class(rangeMap)) {
      r <- terra::rast(rangeMap)
      r[r == 0] <- NA
      rangeMap <- terra::as.polygons(r)
      # Project AOO raster to WGS84
      if (selTimeSource() == "aoo") {
        rangeMap <- terra::project(rangeMap, getWKT("wgs84"))
      }
      rangeMap <- sf::st_as_sf(rangeMap)
    }
    req(rangeMap)
    logger %>%
      writeLog(hlSpp(curSp()),
               paste0("SDM area after masking for environmental variables ",
                      "through time will be calculated based on ",
                      switch (selTimeSource(),
                              wallace = "Wallace SDM",
                              xfer = "Transferred SDM",
                              user = "User-specified SDM",
                              mask = "Masked SDM",
                              eoo = "Extent of Occurrence (EOO)",
                              aoo = "Area of Ocupancy (AOO)"), "."))
    spp[[curSp()]]$indic$timeRange <- rangeMap
    common$update_component(tab = "Map")
  })

  observeEvent(input$goInputEnvs, {
    if (is.null(input$indicEnvs)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Raster files not uploaded")
      return()
    }
    if (is.null(input$EnvThrVal)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Please enter a threshold for environmental layers")
      return()
    }
    smartProgress(
      logger,
      message = "Loading environmental variables ", {
        envs <- raster::stack(input$indicEnvs$datapath)
        if (raster::nlayers(envs) == 1) {
          logger %>% writeLog(type = 'error', hlSpp(curSp()),
                              "Please upload more than one environmental variable")
          return()
        }
        if (is.na(raster::crs(envs))) {
          logger %>% writeLog(
            type = 'warning', hlSpp(curSp()),
            "Projection not found for layers. It is assume that layer datum ",
            "is WGS84 (**)"
          )
          raster::crs(envs) <- "EPSG:4326"
        }
        if (!raster::isLonLat(envs)) {
          envs <- terra::project(terra::rast(envs), getWKT("wgs84"))
          envs <- methods::as(envs, "Raster")
          logger %>% writeLog(
            type = 'warning', hlSpp(curSp()),
            "Original coordinate reference system (CRS) is not WGS84 (EPSG:4326). ",
            "Layers were reprojected to this CRS. (**)"
          )
        }
      })
    req(envs)
    threshold <- as.numeric(trimws(strsplit(input$EnvThrVal, ",")[[1]]))
    bound <- input$selBound
    if ((length(threshold) != 1 & bound == "lower") |
        (length(threshold) != 1 & bound == "upper")) {
      logger %>% writeLog(type = "error",
        hlSpp(curSp()),
        "Provided one threshold value.")
      return()
    }
    if (length(threshold) != 2 & bound == "both") {
      logger %>% writeLog(type = "error",
        hlSpp(curSp()),
        "Provided two threshold values.")
      return()
    }
    if (bound == "neither") {
      logger %>% writeLog(
        hlSpp(curSp()),
        "Provided threshold ommitted.")
      logger %>% writeLog(
        hlSpp(curSp()),
        paste0("Layers uploaded. Bound: ", input$selBound, "."))
    } else {
      logger %>% writeLog(
        hlSpp(curSp()),
        paste0("Layers uploaded. Bound: ", input$selBound, ". ",
               "Threshold(s): ", paste0(threshold, collapse = ", "), "."))
    }
    # LOAD SPP
    spp[[curSp()]]$indic$indicEnvs <- envs
    spp[[curSp()]]$indic$indicEnvsThr <- threshold
  })

  observeEvent(input$goInputYears, {
    req(spp[[curSp()]]$indic$indicEnvs)
    years <- trimws(strsplit(input$Years, ",")[[1]])

    if (raster::nlayers(spp[[curSp()]]$indic$indicEnvs) != length(years)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Please enter the years for all inputed variables.")
      return()
    }
    if (is.null(spp[[curSp()]]$indic$timeRange)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "No range map is selected for the calculations.")
      return()
    }
    rangeMap <- sf::as_Spatial(spp[[curSp()]]$indic$timeRange) %>%
      raster::rasterize(spp[[curSp()]]$indic$indicEnvs[[1]])
    rangeArea <- indic_time(range = rangeMap,
                            envs = spp[[curSp()]]$indic$indicEnvs,
                            thrh = spp[[curSp()]]$indic$indicEnvsThr,
                            bound = input$selBound,
                            logger, spN = curSp())
    req(rangeArea)
    # LOAD INTO SPP ####
    spp[[curSp()]]$indic$areaTime <- rangeArea
    spp[[curSp()]]$indic$years <- years
    common$update_component(tab = "Results")
  })

  output$timeAreas <- renderUI({
    # Result
    output$areaMasked <- renderPrint({
      paste0("Range area (in km^2) after masking for environmental variables ",
             "through time for: ",  spp[[curSp()]]$indic$years, " ",
             spp[[curSp()]]$indic$areaTime)})
    output$timePlot <- renderPlot({
      plot(y = spp[[curSp()]]$indic$areaTime, x = spp[[curSp()]]$indic$years,
           main = "Range area indic", ylab = "area (square km)", xlab = "Time")
      lines(y = spp[[curSp()]]$indic$areaTime, x = spp[[curSp()]]$indic$years)
    })
    tabsetPanel(
      tabPanel("Area through time plot",
               tagList(
                 plotOutput(session$ns('timePlot'))
               )),
      tabPanel("Area through time values",
               tagList(
                 verbatimTextOutput(session$ns('areaMasked'))
               ))
    )
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      # list(
        # selSource = input$selRasterSource,
        # EnvThrVal = input$EnvThrVal,
        # Years = input$Years)
    },
    load = function(state) {
      # Load
      # updateSelectInput(session, 'selRasterSource', selected = state$selRasterSource)
      # updateSelectInput(session, 'EnvThrVal', selected = state$EnvThrVal)
      # updateSelectInput(session, 'Years', selected = state$Years)
    }
  ))

}

indic_time_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  uiOutput(ns("timeAreas"))
}

indic_time_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp

  req(spp[[curSp()]]$indic$timeRange)
  map %>% clearAll()
  # Step 1 #
  timePoly <- spp[[curSp()]]$indic$timeRange
  # Zoom
  bb <- sf::st_bbox(timePoly) %>% as.vector()
  bbZoom <- polyZoom(bb[1], bb[2], bb[3], bb[4], fraction = 0.05)
  map %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])

  map %>%
    ##Add legend
    addLegend("bottomright", colors = "darkgrey",
              labels = "Range Map",
              opacity = 1, layerId = 'indicTimeLegend') %>%
    ##ADD polygon
    leafem::addFeatures(timePoly, fillColor = 'darkgrey', fillOpacity = 0.7,
                        opacity = 0, group = 'indic', layerId = 'indicTime')
}

indic_time_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  # list(
  #   indic_time_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  # )
}

