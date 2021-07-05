proj_user_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Study Region (**)", class = "stepText"), br(), br(),
    selectInput(ns('projExt'), label = "Select method (**)",
                choices = list("Draw polygon(**)" = 'pjDraw',
                               "Same extent (**)" = 'pjCur',
                               "User-specified polygon(**)" = 'pjUser')),
    conditionalPanel(sprintf("input['%s'] == 'pjUser'", ns("projExt")),
                     fileInput(
                       ns("userPjShp"),
                       label = paste0(
                         'Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                         'CSV file with field order (longitude, latitude)'),
                       accept = c(".csv", ".dbf", ".shx", ".shp"),
                       multiple = TRUE),
                     tags$div(
                       title = paste0(
                         'Buffer area in degrees (1 degree = ~111 km). Exact',
                         ' length varies based on latitudinal position.'),
                       numericInput(ns("userPjBuf"),
                                    label = "Study region buffer distance (degree)",
                                    value = 0, min = 0, step = 0.5)
                     )),
    conditionalPanel(sprintf("input['%s'] == 'pjDraw'", ns("projExt")),
                     p("Draw a polygon and select buffer distance(**)"),
                     tags$div(
                       title = paste0(
                         'Buffer area in degrees (1 degree = ~111 km). Exact',
                         ' length varies based on latitudinal position.'
                       ),
                       numericInput(
                         ns("drawPjBuf"),
                         label = "Study region buffer distance (degree)",
                         value = 0, min = 0, step = 0.5)
                     )),
    conditionalPanel(sprintf("input['%s'] == 'pjCur'", ns("projExt")),
                     p('You will use the same extent (**)')),
    actionButton(ns("goProjExtUser"), "Create(**)"), br(),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Project (**)", class = "stepText"), br(),
    p("Project model to projected extent (red) (**)"),
    uiOutput(ns("projUserNames")),
    fileInput(ns("userProjEnvs"),
              label = paste0('Input rasters in single-file format (i.e. .tif, ',
                             '.asc). All rasters must have the same extent and ',
                             'resolution (cell size). (**)'),
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
    actionButton(ns('goProjectUser'), "Project"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetProj"), "Reset", class = 'butReset'),
    strong(" projection extent (**)")
  )
}

proj_user_module_server <- function(input, output, session, common) {

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
      h5("Prediction output is the same than Visualize component (**)")
    }
  })


  # Render a text with filenames for user-specified projection rasters
  output$projUserNames <- renderUI({
    req(curSp())
    sp <- curSp()[1]
    if(is.null(spp[[sp]]$envs)) return()
    envNames <- names(envs.global[[spp[[sp]]$envs]])
    tagList(
      tags$em("Your files must be named as: (**)"),
      tags$p(paste(spp[[curSp()]]$rmm$data$environment$variableNames,
                   collapse = ", "))
    )
  })

  observeEvent(input$goProjExtUser, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(
          type = 'error',
          'Calculate a model prediction in model component before projecting.'
        )
      return()
    }
    if (input$projExt == 'pjDraw') {
      if (is.null(spp[[curSp()]]$polyPjXY)) {
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
    if (input$projExt == 'pjUser') {
      if (is.null(input$userPjShp$datapath)) {
        logger %>% writeLog(type = 'error', paste0("Specified filepath(s) (**)"))
        return()
      }
    }

    # FUNCTION CALL ####
    if (input$projExt == 'pjDraw') {
      polyPj <- proj_draw(spp[[curSp()]]$polyPjXY, spp[[curSp()]]$polyPjID,
                          input$drawPjBuf, logger, spN = curSp())
      if (input$drawPjBuf == 0 ) {
        logger %>% writeLog(
          hlSpp(curSp()), 'Draw polygon without buffer(**).')
      } else {
        logger %>% writeLog(
          hlSpp(curSp()), 'Draw polygon with buffer of ', input$drawPjBuf,
          ' degrees (**).')
      }
      # METADATA ####
      spp[[curSp()]]$rmm$code$wallace$PjBuff <- input$drawPjBuf
      polyX <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallace$drawExtPolyPjCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
    }

    if (input$projExt == 'pjUser') {
      polyPj <- proj_userExtent(input$userPjShp$datapath, input$userPjShp$name,
                                input$userPjBuf, logger, spN = curSp())
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

    if (input$projExt == 'pjCur') {
      polyPj <- spp[[curSp()]]$procEnvs$bgExt
      logger %>% writeLog(
        hlSpp(curSp()),
        'Projection extent equal to current extent region. (**)')
    }
    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjExt <- polyPj

    common$update_component(tab = "Map")
  })

  observeEvent(input$goProjectUser, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in visualize component before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$project$pjExt)) {
      logger %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    if (is.null(input$userProjEnvs)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    # Check the number of selected files
    if (length(input$userProjEnvs$name) !=
        length(spp[[curSp()]]$rmm$data$environment$variableNames)) {
      logger %>%
        writeLog(type = 'error', "Number of files are not the same that the ",
                 "enviromental variables (**)")
      return()
    }
    # Check if the filesnames are the same that envs()
    if (!identical(tools::file_path_sans_ext(sort(input$userProjEnvs$name)),
                   sort(spp[[curSp()]]$rmm$data$environment$variableNames))) {
      logger %>%
        writeLog(type = 'error',
                 paste0("Raster files don't have same names. You must name your",
                        " files as: (**) "),
                 em(paste(spp[[curSp()]]$rmm$data$environment$variableNames,
                          collapse = ", ")), ".")
      return()
    }

    # Load raster ####
    userProjEnvs <- envs_userEnvs(rasPath = input$userProjEnvs$datapath,
                                  rasName = input$userProjEnvs$name)

    # ERRORS ####
    # Check that the extents of raster and projection extent instersects
    if (!rgeos::gIntersects(spp[[curSp()]]$project$pjExt,
                            as(raster::extent(userProjEnvs), 'SpatialPolygons'))) {
      logger %>%
        writeLog(type = 'error', 'Extents do not overlap (**)')
      return()
    }

    # FUNCTION CALL ####
    predType <- rmm()$prediction$notes
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      projUser.out <- proj_userEnvs(evalOut = evalOut(),
                                    curModel = curModel(),
                                    envs = userProjEnvs,
                                    pjExt = spp[[curSp()]]$project$pjExt,
                                    alg = spp[[curSp()]]$rmm$model$algorithms,
                                    logger,
                                    spN = curSp())
    } else {
      projUser.out <- proj_userEnvs(evalOut = evalOut(),
                                    curModel = curModel(),
                                    envs = userProjEnvs,
                                    pjExt = spp[[curSp()]]$project$pjExt,
                                    alg = spp[[curSp()]]$rmm$model$algorithms,
                                    outputType = predType,
                                    clamp = rmm()$model$algorithm$maxent$clamping,
                                    logger,
                                    spN = curSp())
    }
    projExt <- projUser.out$projExt
    projUser <- projUser.out$projUser

    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    occPredVals <- spp[[curSp()]]$visualization$occPredVals

    if(!(input$threshold == 'none')) {
      if (input$threshold == 'mtp') {
        thr <- quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr <- quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr <- quantile(occPredVals, probs = input$trainPresQuantile)
      }
      projUserThr <- projUser > thr
      logger %>% writeLog(hlSpp(curSp()), "Projection of model to user-specified files (**)",
                          'with threshold ', input$threshold, ' (',
                          formatC(thr, format = "e", 2), ').')
    } else {
      projUserThr <- projUser
      logger %>% writeLog(hlSpp(curSp()), "Projection of model to user-specified files (**)",
                          'with ', predType, ' output.')
    }
    raster::crs(projUserThr) <- raster::crs(envs())
    # rename
    names(projUserThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projUserThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projUserThr, predType)

    # METADATA ####
    spp[[curSp()]]$rmm$code$wallace$project_curModel <- curModel()
    spp[[curSp()]]$rmd$project_user <-TRUE
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "project to user-specified files"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "user"

    spp[[curSp()]]$rmm$prediction$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projUserThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projUserThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdRule <- input$threshold
    if (!is.null(spp[[curSp()]]$rmm$model$algorithm$maxent$clamping)) {
      spp[[curSp()]]$rmm$prediction$transfer$environment1$extrapolation <-
        spp[[curSp()]]$rmm$model$algorithm$maxent$clamping
    }
    spp[[curSp()]]$rmm$prediction$transfer$notes <- NULL
    spp[[curSp()]]$rmm$code$wallace$userPjName <- input$userProjEnvs$name
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

proj_user_module_map <- function(map, common) {

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
  # if no threshold specified
  if(rmm()$prediction$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'red')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "Thresholded Suitability<br>(Projected)",
                labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = 'proj')
  } else {
    # if threshold specified
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

proj_user_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    proj_user_knit = !is.null(species$rmd$project_user),
    curModel_rmd = species$rmm$code$wallace$project_curModel,
    outputType_rmd = species$rmm$prediction$notes,
    alg_rmd = species$rmm$model$algorithms,
    clamp_rmd = species$rmm$model$algorithm$maxent$clamping,
    userPjName_rmd = printVecAsis(species$rmm$code$wallace$userPjName),
    ##Use of threshold for projection
    proj_user_threshold_knit = !is.null(species$rmm$prediction$transfer$environment1$thresholdSet),
    thresholdRule_rmd = species$rmm$prediction$transfer$environment1$thresholdRule,
    threshold_rmd = if (!is.null(species$rmm$prediction$transfer$environment1$thresholdSet)){
      species$rmm$prediction$transfer$environment1$thresholdSet} else {0},
    ##Determine the type of projection extent to use correct RMD function
    proj_user_user_knit = !is.null(species$rmm$code$wallace$userPjShpParams),
    proj_user_drawn_knit = !is.null(species$rmm$code$wallace$drawExtPolyPjCoords),
    ###arguments for creating extent
    polyPjXY_rmd = if(!is.null(species$rmm$code$wallace$drawExtPolyPjCoords)){
      printVecAsis(species$polyPjXY)} else {NULL},
    polyPjID_rmd =  if(!is.null(species$rmm$code$wallace$drawExtPolyPjCoords)){
      species$polyPjID} else {0},
    BgBuf_rmd = species$rmm$code$wallace$PjBuff,
    polyPj_rmd = if(is.null(species$rmm$code$wallace$drawExtPolyPjCoords) & is.null(species$rmm$code$wallace$userPjShpParams)){
      species$procEnvs$bgExt} else {NULL}
  )
}

