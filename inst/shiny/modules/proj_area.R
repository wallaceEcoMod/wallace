proj_area_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Study Region", class = "stepText"), br(), br(),
    selectInput(ns('projExt'), label = "Select method",
      choices = list("Draw polygon" = 'pjDraw',
                     "User-specified polygon" = 'pjUser')),
    conditionalPanel(sprintf("input['%s'] == 'pjUser'", ns("projExt")),
      fileInput(ns("userPjShp"),
                label = paste0('Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                               'CSV file with field order (longitude, latitude)'),
                accept = c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE),
      tags$div(title = paste0('Buffer area in degrees (1 degree = ~111 km). Exact',
                              ' length varies based on latitudinal position.'),
        numericInput(ns("userPjBuf"), label = "Study region buffer distance (degree)",
                     value = 0, min = 0, step = 0.5))),
    conditionalPanel(sprintf("input['%s'] == 'pjDraw'", ns("projExt")),
      p("Draw a polygon and select buffer distance"),
      tags$div(title = paste0('Buffer area in degrees (1 degree = ~111 km). Exact',
                              ' length varies based on latitudinal position.'),
        numericInput(ns("drawPjBuf"), label = "Study region buffer distance (degree)",
                     value = 0, min = 0, step = 0.5))),
    actionButton(ns("goProjExtArea"), "Create"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Transfer", class = "stepText"), br(),
    p("Transfer model to project extent (red) "),
    tags$div(
      title = paste0(
        'Create binary map of predicted presence/absence assuming ',
        'all values above threshold value represent presence. Also ',
        'can be interpreted as a "potential distribution" (see ',
        'guidance).'),
      selectInput(ns('threshold'), label = "Set threshold",
                  choices = list("No threshold" = 'none',
                                 "Minimum Training Presence" = 'mtp',
                                 "10 Percentile Training Presence" = 'p10',
                                 "Quantile of Training Presences" = 'qtp'))),
    conditionalPanel(sprintf("input['%s'] == 'qtp'", ns("threshold")),
      sliderInput(ns("trainPresQuantile"), "Set quantile",
                  min = 0, max = 1, value = .05)),
    conditionalPanel(paste0("input['", ns("threshold"), "'] == 'none'"),
                     uiOutput(ns("noThrs"))),
    actionButton(ns('goProjectArea'), "Transfer"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetProj"), "Reset", class = 'butReset'),
    strong(" projection extent ")
  )
}

proj_area_module_server <- function(input, output, session, common) {

  spp <- common$spp
  evalOut <- common$evalOut
  envs <- common$envs
  rmm <- common$rmm
  curSp <- common$curSp
  curModel <- common$curModel
  logger <- common$logger

  output$noThrs <- renderUI({
    ns <- session$ns
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms != "BIOCLIM") {
      h5("Prediction output is the same than Visualize component ")
    }
  })

  observeEvent(input$goProjExtArea, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>% writeLog(type = 'error',
          'Calculate a model prediction in model component before projecting.')
      return()
    }
    if (input$projExt == 'pjDraw') {
      if (is.null(spp[[curSp()]]$polyPjXY)) {
        logger %>% writeLog(type = 'error',
            paste0("The polygon has not been drawn and finished. Please use the ",
                   "draw toolbar on the left-hand of the map to complete the ",
                   "polygon."))
        return()
      }
    }
    if (input$projExt == 'pjUser') {
      if (is.null(input$userPjShp$datapath)) {
        logger %>% writeLog(type = 'error', "Specified filepath(s) ")
        return()
      }
    }

    # FUNCTION CALL ####
    if (input$projExt == 'pjDraw') {
      polyPj <- proj_draw(spp[[curSp()]]$polyPjXY, spp[[curSp()]]$polyPjID,
                          input$drawPjBuf, logger, spN = curSp())
      if (input$drawPjBuf == 0 ) {
        logger %>% writeLog(
          alfred.hlSpp(curSp()), 'Draw polygon without buffer.')
      } else {
        logger %>% writeLog(
          alfred.hlSpp(curSp()), 'Draw polygon with buffer of ', input$drawPjBuf,
          ' degrees.')
      }
      # METADATA ####
      polyX <- alfred.printVecAsis(round(spp[[curSp()]]$polyPjXY[, 1], digits = 4))
      polyY <- alfred.printVecAsis(round(spp[[curSp()]]$polyPjXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallace$drawExtPolyPjCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
      spp[[curSp()]]$rmm$code$wallace$PjBuff <- input$drawPjBuf
    }

    if (input$projExt == 'pjUser') {
      polyPj <- proj_userExtent(input$userPjShp$datapath, input$userPjShp$name,
                                input$userPjBuf, logger, spN = curSp())
      # ERRORS ####
      # Check that the extents of raster and projection extent instersects
      if (!rgeos::gIntersects(spp[[curSp()]]$project$pjExt,
                              methods::as(raster::extent(userProjEnvs),
                                          'SpatialPolygons'))) {
        logger %>%
          writeLog(type = 'error', 'Extents do not overlap')
        return()
      }
      # METADATA ####
      spp[[curSp()]]$rmm$code$wallace$PjBuff <- input$userPjBuf
      # get extensions of all input files
      exts <- sapply(strsplit(input$userPjShp$name, '\\.'),
                     FUN = function(x) x[2])
      if('csv' %in% exts) {
        spp[[curSp()]]$rmm$code$wallace$userPjExt <- 'csv'
        spp[[curSp()]]$rmm$code$wallace$userPjPath <- input$userPjShp$datapath
      }
      else if('shp' %in% exts) {
        spp[[curSp()]]$rmm$code$wallace$userPjExt <- 'shp'
        # get index of .shp
        i <- which(exts == 'shp')
        shpName <- strsplit(input$userPjShp$name[i], '\\.')[[1]][1]
        spp[[curSp()]]$rmm$code$wallace$userPjShpParams <-
          list(dsn = input$userPjShp$datapath[i], layer = shpName)
      }
    }

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjExt <- polyPj

    common$update_component(tab = "Map")
  })

  observeEvent(input$goProjectArea, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in model component before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$project$pjExt)) {
      logger %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    # Check that the extents of raster and projection extent intersects
    if (!rgeos::gIntersects(spp[[curSp()]]$project$pjExt,
                            methods::as(raster::extent(envs()), 'SpatialPolygons'))) {
      logger %>%
        writeLog(type = 'error', 'Extents do not overlap')
      return()
    }

    # FUNCTION CALL ####
    predType <- rmm()$prediction$notes
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      projArea.out <- proj_area(evalOut = evalOut(),
                                curModel = curModel(),
                                envs = envs(),
                                pjExt = spp[[curSp()]]$project$pjExt,
                                alg = spp[[curSp()]]$rmm$model$algorithms,
                                logger,
                                spN = curSp())
    } else {
      projArea.out <- proj_area(evalOut = evalOut(),
                                curModel = curModel(),
                                envs = envs(),
                                pjExt = spp[[curSp()]]$project$pjExt,
                                alg = spp[[curSp()]]$rmm$model$algorithms,
                                outputType = predType,
                                clamp = rmm()$model$algorithm$maxent$clamping,
                                logger,
                                spN = curSp())
    }

    projExt <- projArea.out$projExt
    projArea <- projArea.out$projArea

    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    occPredVals <- spp[[curSp()]]$visualization$occPredVals

    if(!(input$threshold == 'none')) {
      if (input$threshold == 'mtp') {
        thr <- stats::quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr <- stats::quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr <- stats::quantile(occPredVals, probs = input$trainPresQuantile)
      }
      projAreaThr <- projArea > thr
      logger %>% writeLog(alfred.hlSpp(curSp()), "Projection of model to new area with threshold ",
                          input$threshold, ' (', formatC(thr, format = "e", 2), ').')
    } else {
      projAreaThr <- projArea
      logger %>% writeLog(alfred.hlSpp(curSp()), "Projection of model to new area with ",
                          predType, ' output.')
    }
    raster::crs(projAreaThr) <- raster::crs(envs())
    # rename
    names(projAreaThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projAreaThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projAreaThr, predType)

    # METADATA ####
    spp[[curSp()]]$rmm$code$wallace$project_curModel <- curModel()
    spp[[curSp()]]$rmm$code$wallace$project_area <- TRUE
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      alfred.printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      alfred.printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    if (spp[[curSp()]]$rmm$data$environment$sources == 'WorldClim 1.4') {
      spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- 1960
      spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- 1990
    }
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      alfred.printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "transfer to user-selected new area"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <-
      spp[[curSp()]]$rmm$data$environment$sources
    spp[[curSp()]]$rmm$prediction$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$minVal <-
      alfred.printVecAsis(raster::cellStats(projAreaThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$maxVal <-
      alfred.printVecAsis(raster::cellStats(projAreaThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet <- thr
      if (input$threshold == 'qtp') {
        spp[[curSp()]]$rmm$code$wallace$transferQuantile <- input$trainPresQuantile
      } else {
        spp[[curSp()]]$rmm$code$wallace$transferQuantile <- 0
      }
    } else {
      spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdRule <- input$threshold

    if (!is.null(spp[[curSp()]]$rmm$model$algorithm$maxent$clamping)) {
      spp[[curSp()]]$rmm$prediction$transfer$environment1$extrapolation <-
        spp[[curSp()]]$rmm$model$algorithm$maxent$clamping
    }
    spp[[curSp()]]$rmm$prediction$transfer$notes <- NULL

    common$update_component(tab = "Map")
  })

  # Reset Projection Extent button functionality
  observeEvent(input$goResetProj, {
    spp[[curSp()]]$polyPjXY <- NULL
    spp[[curSp()]]$polyPjID <- NULL
    spp[[curSp()]]$project <- NULL
    logger %>% writeLog("Reset projection extent.")
  })

  return(list(
    save = function() {
      list(
        projExt = input$projExt,
        userPjBuf = input$userPjBuf,
        drawPjBuf = input$drawPjBuf,
        threshold = input$threshold,
        trainPresQuantile = input$trainPresQuantile
      )
    },
    load = function(state) {
      updateSelectInput(session, 'projExt', selected = state$projExt)
      updateNumericInput(session, 'userPjBuf', value = state$userPjBuf)
      updateNumericInput(session, 'drawPjBuf', value = state$drawPjBuf)
      updateSelectInput(session, 'threshold', selected = state$threshold)
      updateSliderInput(session, 'trainPresQuantile', value = state$trainPresQuantile)
    }
  ))
}

proj_area_module_map <- function(map, common) {

  spp <- common$spp
  evalOut <- common$evalOut
  curSp <- common$curSp
  rmm <- common$rmm
  mapProj <- common$mapProj

  # Map logic
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw', polylineOptions = FALSE, rectangleOptions = FALSE,
    circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )
  # Add just projection Polygon
  req(spp[[curSp()]]$project$pjExt)
  polyPjXY <- spp[[curSp()]]$project$pjExt@polygons[[1]]@Polygons
  if(length(polyPjXY) == 1) {
    shp <- list(polyPjXY[[1]]@coords)
  } else {
    shp <- lapply(polyPjXY, function(x) x@coords)
  }
  bb <- spp[[curSp()]]$project$pjExt@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>% clearAll() %>% removeImage('projRas') %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red",group = 'bgShp')
  }
  req(evalOut(), spp[[curSp()]]$project$pjEnvs)
  mapProjVals <- spp[[curSp()]]$project$mapProjVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if threshold specified
  if(rmm()$prediction$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'red')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "Thresholded Suitability<br>(Projected)",
                labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = 'proj')
  } else {
    # if no threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapProjVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, mapProjVals, na.color = 'transparent')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", pal = legendPal,
                title = "Predicted Suitability<br>(Projected)",
                values = mapProjVals, layerId = 'proj',
                labFormat = reverseLabels(2, reverse_order = TRUE))
  }
  # map model prediction raster and projection polygon
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    addRasterImage(mapProj(), colors = rasPal, opacity = 0.7,
                   layerId = 'projRas', group = 'proj', method = "ngb")
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", group = 'proj', fill = FALSE)
  }
}

proj_area_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    proj_area_knit = !is.null(species$rmm$code$wallace$project_area),
    curModel_rmd = species$rmm$code$wallace$project_curModel,
    outputType_rmd = species$rmm$prediction$notes,
    alg_rmd = species$rmm$model$algorithms,
    clamp_rmd = species$rmm$model$algorithm$maxent$clamping,
    ###arguments for creating extent
    polyPjXY_rmd = if(!is.null(species$rmm$code$wallace$drawExtPolyPjCoords)){
    alfred.printVecAsis(species$polyPjXY)} else {NULL},
    polyPjID_rmd =  if(!is.null(species$rmm$code$wallace$drawExtPolyPjCoords)){
     species$polyPjID} else {0},
    BgBuf_rmd = species$rmm$code$wallace$PjBuff,
    ##Determine the type of projection extent to use correct RMD function
    proj_area_extent_knit = !is.null(species$rmm$code$wallace$userPjShpParams),
    ##Use of threshold for projection
    proj_area_threshold_knit = !is.null(species$rmm$prediction$transfer$environment1$thresholdSet),
    proj_thresholdRule_rmd = species$rmm$prediction$transfer$environment1$thresholdRule,
    proj_threshold_rmd = if (!is.null(species$rmm$prediction$transfer$environment1$thresholdSet)){
    species$rmm$prediction$transfer$environment1$thresholdSet} else {0},
    proj_probQuantile_rmd = species$rmm$code$wallace$transferQuantile

  )
}
