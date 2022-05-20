xfer_user_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Study Region", class = "stepText"), br(), br(),
    selectInput(ns('xferExt'), label = "Select method",
                choices = list("Draw polygon" = 'xfDraw',
                               "Same extent" = 'xfCur',
                               "User-specified polygon" = 'xfUser')),
    conditionalPanel(sprintf("input['%s'] == 'xfUser'", ns("xferExt")),
                     fileInput(
                       ns("userXfShp"),
                       label = paste0(
                         'Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                         'CSV file with field order (longitude, latitude)'),
                       accept = c(".csv", ".dbf", ".shx", ".shp"),
                       multiple = TRUE),
                     tags$div(
                       title = paste0(
                         'Buffer area in degrees (1 degree = ~111 km). Exact',
                         ' length varies based on latitudinal position.'),
                       numericInput(ns("userXfBuf"),
                                    label = "Study region buffer distance (degree)",
                                    value = 0, min = 0, step = 0.5)
                     )),
    conditionalPanel(sprintf("input['%s'] == 'xfDraw'", ns("xferExt")),
                     p("Draw a polygon and select buffer distance"),
                     tags$div(
                       title = paste0(
                         'Buffer area in degrees (1 degree = ~111 km). Exact',
                         ' length varies based on latitudinal position.'
                       ),
                       numericInput(
                         ns("drawXfBuf"),
                         label = "Study region buffer distance (degree)",
                         value = 0, min = 0, step = 0.5)
                     )),
    conditionalPanel(sprintf("input['%s'] == 'xfCur'", ns("xferExt")),
                     p('You will use the same extent')),
    actionButton(ns("goXferExtUser"), "Create"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Transfer", class = "stepText"), br(),
    p("Transfer model to extent of transfer (red) "),
    uiOutput(ns("xferUserNames")),
    fileInput(ns("userXferEnvs"),
              label = paste0('Input rasters in single-file format (i.e. .tif, ',
                             '.asc). All rasters must have the same extent and ',
                             'resolution (cell size).'),
              accept = c(".asc", ".tif"), multiple = TRUE),
    tags$div(title = paste0('Create binary map of predicted presence/absence ',
                            'assuming all values above threshold value represent ',
                            'presence. Also can be interpreted as a "potential ',
                            'distribution" (see guidance).'),
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
    actionButton(ns('goTransferUser'), "Transfer"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetXfer"), "Reset", class = 'butReset'),
    strong(" extent of transfer")
  )
}

xfer_user_module_server <- function(input, output, session, common) {

  spp <- common$spp
  evalOut <- common$evalOut
  envs <- common$envs
  envs.global <- common$envs.global
  rmm <- common$rmm
  curSp <- common$curSp
  curModel <- common$curModel
  logger <- common$logger

  output$noThrs <- renderUI({
    ns <- session$ns
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms != "BIOCLIM") {
      h5("Prediction output is the same than Visualize component")
    }
  })


  # Render a text with filenames for user-specified rasters of transfer
  output$xferUserNames <- renderUI({
    req(curSp())
    sp <- curSp()[1]
    if(is.null(spp[[sp]]$envs)) return()
    envNames <- names(envs.global[[spp[[sp]]$envs]])
    tagList(
      tags$em("Your files must be named as: "),
      tags$p(paste(spp[[curSp()]]$rmm$data$environment$variableNames,
                   collapse = ", "))
    )
  })

  observeEvent(input$goXferExtUser, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(
          type = 'error',
          'Calculate a model prediction in model component before transferring.'
        )
      return()
    }
    if (input$xferExt == 'xfDraw') {
      if (is.null(spp[[curSp()]]$polyXfXY)) {
        logger %>%
          writeLog(
            type = 'error',
            paste0("The polygon has not been drawn and finished. Please use the ",
                   "draw toolbar on the left-hand of the map to complete the ",
                   "polygon.")
          )
        return()
      }
    }
    if (input$xferExt == 'xfUser') {
      if (is.null(input$userXfShp$datapath)) {
        logger %>% writeLog(type = 'error', paste0("Specified filepath(s)"))
        return()
      }
    }

    # FUNCTION CALL ####
    if (input$xferExt == 'xfDraw') {
      polyXf <- xfer_draw(spp[[curSp()]]$polyXfXY, spp[[curSp()]]$polyXfID,
                          input$drawXfBuf, logger, spN = curSp())
      if (input$drawXfBuf == 0 ) {
        logger %>% writeLog(
          hlSpp(curSp()), 'Draw polygon without buffer.')
      } else {
        logger %>% writeLog(
          hlSpp(curSp()), 'Draw polygon with buffer of ', input$drawXfBuf,
          ' degrees.')
      }
      # METADATA ####
      spp[[curSp()]]$rmm$code$wallace$XfBuff <- input$drawXfBuf
      polyX <- printVecAsis(round(spp[[curSp()]]$polyXfXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyXfXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallace$drawExtPolyXfCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
    }

    if (input$xferExt == 'xfUser') {
      polyXf <- xfer_userExtent(input$userXfShp$datapath, input$userXfShp$name,
                                input$userXfBuf, logger, spN = curSp())
      # METADATA ####
      spp[[curSp()]]$rmm$code$wallace$XfBuff <- input$userXfBuf
      # get extensions of all input files
      exts <- sapply(strsplit(input$userXfShp$name, '\\.'),
                     FUN = function(x) x[2])
      if('csv' %in% exts) {
        spp[[curSp()]]$rmm$code$wallace$userXfExt <- 'csv'
        spp[[curSp()]]$rmm$code$wallace$userXfPath <- input$userXfShp$datapath
      }
      else if('shp' %in% exts) {
        spp[[curSp()]]$rmm$code$wallace$userXfExt <- 'shp'
        # get index of .shp
        i <- which(exts == 'shp')
        shpName <- strsplit(input$userXfShp$name[i], '\\.')[[1]][1]
        spp[[curSp()]]$rmm$code$wallace$userXfShpParams <-
          list(dsn = input$userXfShp$datapath[i], layer = shpName)
      }
    }

    if (input$xferExt == 'xfCur') {
      polyXf <- spp[[curSp()]]$procEnvs$bgExt
      logger %>% writeLog(
        hlSpp(curSp()),
        'Extent of transfer equal to current extent region.')
    }
    # LOAD INTO SPP ####
    spp[[curSp()]]$transfer$xfExt <- polyXf

    common$update_component(tab = "Map")
  })

  observeEvent(input$goTransferUser, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in visualize component before transferring.')
      return()
    }
    if (is.null(spp[[curSp()]]$transfer$xfExt)) {
      logger %>% writeLog(type = 'error', 'Select extent of transfer first.')
      return()
    }
    if (is.null(input$userXferEnvs)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    # Check the number of selected files
    if (length(input$userXferEnvs$name) !=
        length(spp[[curSp()]]$rmm$data$environment$variableNames)) {
      logger %>%
        writeLog(type = 'error', "Number of files are not the same that the ",
                 "enviromental variables")
      return()
    }
    # Check if the filesnames are the same that envs()
    if (!identical(tools::file_path_sans_ext(sort(input$userXferEnvs$name)),
                   sort(spp[[curSp()]]$rmm$data$environment$variableNames))) {
      logger %>%
        writeLog(type = 'error',
                 paste0("Raster files don't have same names. You must name your",
                        " files as: "),
                 em(paste(spp[[curSp()]]$rmm$data$environment$variableNames,
                          collapse = ", ")), ".")
      return()
    }

    # Load raster ####
    userXferEnvs <- envs_userEnvs(rasPath = input$userXferEnvs$datapath,
                                  rasName = input$userXferEnvs$name)

    # ERRORS ####
    # Check that the extents of raster and extent of transfer intersects
    if (!rgeos::gIntersects(spp[[curSp()]]$transfer$xfExt,
                            methods::as(raster::extent(userXferEnvs),
                                        'SpatialPolygons'))) {
      logger %>%
        writeLog(type = 'error', 'Extents do not overlap')
      return()
    }

    # FUNCTION CALL ####
    predType <- rmm()$prediction$notes
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      xferUser.out <- xfer_userEnvs(evalOut = evalOut(),
                                    curModel = curModel(),
                                    envs = userXferEnvs,
                                    xfExt = spp[[curSp()]]$transfer$xfExt,
                                    alg = spp[[curSp()]]$rmm$model$algorithms,
                                    logger,
                                    spN = curSp())
    } else {
      xferUser.out <- xfer_userEnvs(evalOut = evalOut(),
                                    curModel = curModel(),
                                    envs = userXferEnvs,
                                    xfExt = spp[[curSp()]]$transfer$xfExt,
                                    alg = spp[[curSp()]]$rmm$model$algorithms,
                                    outputType = predType,
                                    clamp = rmm()$model$algorithm$maxent$clamping,
                                    logger,
                                    spN = curSp())
    }
    xferExt <- xferUser.out$xferExt
    xferUser <- xferUser.out$xferUser

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
      xferUserThr <- xferUser > thr
      logger %>% writeLog(
        hlSpp(curSp()), "Transferring of model to user-specified files",
                          'with threshold ', input$threshold, ' (',
                          formatC(thr, format = "e", 2), ').')
    } else {
      xferUserThr <- xferUser
      logger %>% writeLog(hlSpp(curSp()), "Transferring of model to user-specified files",
                          'with ', predType, ' output.')
    }
    raster::crs(xferUserThr) <- raster::crs(envs())
    # rename
    names(xferUserThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$transfer$xfEnvs <- xferExt
    spp[[curSp()]]$transfer$mapXfer <- xferUserThr
    spp[[curSp()]]$transfer$mapXferVals <- getRasterVals(xferUserThr, predType)

    # METADATA ####
    spp[[curSp()]]$rmm$code$wallace$transfer_curModel <- curModel()
    spp[[curSp()]]$rmd$transfer_user <-TRUE
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(xferExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(xferExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(xferExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(xferExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "transfer to user-specified files"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "user"

    spp[[curSp()]]$rmm$prediction$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(xferUserThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(xferUserThr, max), asChar = TRUE)
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
    spp[[curSp()]]$rmm$code$wallace$userXfName <- input$userXferEnvs$name
  })

  # Reset extent of transfer button functionality
  observeEvent(input$goResetXfer, {
    spp[[curSp()]]$polyXfXY <- NULL
    spp[[curSp()]]$polyXfID <- NULL
    spp[[curSp()]]$transfer <- NULL
    logger %>% writeLog("Reset extent of transfer.")
  })

  return(list(
    save = function() {
      list(
        xferExt = input$xferExt,
        userXfBuf = input$userXfBuf,
        drawXfBuf = input$drawXfBuf,
        threshold = input$threshold,
        trainPresQuantile = input$trainPresQuantile
      )
    },
    load = function(state) {
      updateSelectInput(session, 'xferExt', selected = state$xferExt)
      updateNumericInput(session, 'userXfBuf', value = state$userXfBuf)
      updateNumericInput(session, 'drawXfBuf', value = state$drawXfBuf)
      updateSelectInput(session, 'threshold', selected = state$threshold)
      updateSliderInput(session, 'trainPresQuantile', value = state$trainPresQuantile)
    }
  ))

}

xfer_user_module_map <- function(map, common) {

  spp <- common$spp
  evalOut <- common$evalOut
  curSp <- common$curSp
  rmm <- common$rmm
  mapXfer <- common$mapXfer

  # Map logic
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw', polylineOptions = FALSE, rectangleOptions = FALSE,
    circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )
  # Add just Polygon of transfer
  req(spp[[curSp()]]$transfer$xfExt)
  polyXfXY <- spp[[curSp()]]$transfer$xfExt@polygons[[1]]@Polygons
  if(length(polyXfXY) == 1) {
    shp <- list(polyXfXY[[1]]@coords)
  } else {
    shp <- lapply(polyXfXY, function(x) x@coords)
  }
  bb <- spp[[curSp()]]$transfer$xfExt@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>% clearAll() %>% removeImage('xferRas') %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red",group = 'bgShp')
  }
  req(evalOut(), spp[[curSp()]]$transfer$xfEnvs)
  mapXferVals <- spp[[curSp()]]$transfer$mapXferVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if no threshold specified
  if(rmm()$prediction$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'red')
    map %>% removeControl("xfer") %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "Thresholded Suitability<br>(Transferred)",
                labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = 'xfer')
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapXferVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, mapXferVals, na.color = 'transparent')
    map %>% removeControl("xfer") %>%
      addLegend("bottomright", pal = legendPal,
                title = "Predicted Suitability<br>(Transferred)",
                values = mapXferVals, layerId = 'xfer',
                labFormat = reverseLabel(2, reverse_order = TRUE))
  }
  # map model prediction raster and polygon of transfer
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('xferRas') %>%
    addRasterImage(mapXfer(), colors = rasPal, opacity = 0.7,
                   layerId = 'xferRas', group = 'xfer', method = "ngb")
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", group = 'xfer', fill = FALSE)
  }
}

xfer_user_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    xfer_user_knit = !is.null(species$rmd$transfer_user),
    curModel_rmd = species$rmm$code$wallace$transfer_curModel,
    outputType_rmd = species$rmm$prediction$notes,
    alg_rmd = species$rmm$model$algorithms,
    clamp_rmd = species$rmm$model$algorithm$maxent$clamping,
    userXfName_rmd = printVecAsis(species$rmm$code$wallace$userXfName),
    ##Use of threshold for transferring
    xfer_user_threshold_knit = !is.null(species$rmm$prediction$transfer$environment1$thresholdSet),
    xfer_thresholdRule_rmd = species$rmm$prediction$transfer$environment1$thresholdRule,
    xfer_threshold_rmd = if (!is.null(species$rmm$prediction$transfer$environment1$thresholdSet)){
      species$rmm$prediction$transfer$environment1$thresholdSet} else {0},
    xfer_probQuantile_rmd = species$rmm$code$wallace$transferQuantile,
    ##Determine the type of extent of transfer to use correct RMD function
    xfer_user_user_knit = !is.null(species$rmm$code$wallace$userXfShpParams),
    xfer_user_drawn_knit = !is.null(species$rmm$code$wallace$drawExtPolyXfCoords),
    ###arguments for creating extent
    polyXfXY_rmd = if(!is.null(species$rmm$code$wallace$drawExtPolyXfCoords)){
      printVecAsis(species$polyXfXY)} else {NULL},
    polyXfID_rmd =  if(!is.null(species$rmm$code$wallace$drawExtPolyXfCoords)){
      species$polyXfID} else {0},
    BgBuf_rmd = species$rmm$code$wallace$XfBuff,
    polyXf_rmd = if(is.null(species$rmm$code$wallace$drawExtPolyXfCoords) & is.null(species$rmm$code$wallace$userXfShpParams)){
      species$procEnvs$bgExt} else {NULL}
  )
}

