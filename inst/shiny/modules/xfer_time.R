xfer_time_module_ui <- function(id) {
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
    actionButton(ns("goXferExtTime"), "Create"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Transfer", class = "stepText"), br(),
    p("Transfer model to extent (red) "),
    radioButtons(ns('selTimeVar'), label = "Select source of variables",
                 choices = list("WorldClim" = "worldclim",
                                "ecoClimate" = "ecoclimate"),
                 inline = TRUE),
    conditionalPanel(sprintf("input['%s'] == 'worldclim'", ns("selTimeVar")),
                     selectInput(ns("selTime"), label = "Select time period",
                                 choices = list("Select period" = "",
                                                "2050" = 50,
                                                "2070" = 70)),
                     uiOutput(ns('selGCMui')),
                     selectInput(ns('selRCP'), label = "Select RCP",
                                 choices = list("Select RCP" = "",
                                                '2.6' = 26,
                                                '4.5' = 45,
                                                '6.0' = 60,
                                                '8.5' = 85))),
    conditionalPanel(sprintf("input['%s'] == 'ecoclimate'", ns("selTimeVar")),
                     tags$div(title = 'Select AOGCM',
                              selectInput(ns("xfAOGCM"),
                                          label = "Select the Atmospheric Oceanic General Circulation Model you want to use",
                                          choices = list("Select AOGCMs" = "",
                                                         "CCSM" = "CCSM",
                                                         "CNRM" = "CNRM",
                                                         "MIROC" = "MIROC",
                                                         "FGOALS" = "FGOALS",
                                                         "GISS" = "GISS",
                                                         "IPSL" = "IPSL",
                                                         "MRI" = "MRI",
                                                         "MPI" = "MPI")
                              )),
                     tags$div(title = 'Select Scenario',
                              selectInput(ns("xfScenario"),
                                          label = "select the temporal scenario that you want to use",
                                          choices = list("Select Scenario" = "",
                                                         "2080-2100 RCP 2.6" = "Future 2.6",
                                                         "2080-2100 RCP 4.5" = "Future 4.5",
                                                         "2080-2100 RCP 6" = "Future 6",
                                                         "2080-2100 RCP 8.5" = "Future 8.5",
                                                         "Holocene (6,000 years ago)" = "Holo",
                                                         "LGM (21,000 years ago)" = "LGM")
                              ))),
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
    actionButton(ns('goTransferTime'), "Transfer"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetXfer"), "Reset", class = 'butReset'),
    strong(" extent of transfer")
  )
}

xfer_time_module_server <- function(input, output, session, common) {

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
      h5("Prediction output is the same as Visualize component")
    }
  })

  GCMlookup <- c(AC = "ACCESS1-0", BC = "BCC-CSM1-1", CC = "CCSM4",
                 CE = "CESM1-CAM5-1-FV2", CN = "CNRM-CM5", GF = "GFDL-CM3",
                 GD = "GFDL-ESM2G", GS = "GISS-E2-R", HD = "HadGEM2-AO",
                 HG = "HadGEM2-CC", HE = "HadGEM2-ES", IN = "INMCM4",
                 IP = "IPSL-CM5A-LR", ME = "MPI-ESM-P", MI = "MIROC-ESM-CHEM",
                 MR = "MIROC-ESM", MC = "MIROC5", MP = "MPI-ESM-LR",
                 MG = "MRI-CGCM3", NO = "NorESM1-M")

  # dynamic ui for GCM selection: choices differ depending on choice of time period
  output$selGCMui <- renderUI({
    ns <- session$ns

    if (input$selTime == 'lgm') {
      gcms <- c('CC', 'MR', 'MC')
    } else if (input$selTime == 'mid') {
      gcms <- c("BC", "CC", "CE", "CN", "HG", "IP", "MR", "ME", "MG")
    } else {
      gcms <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD",
                "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
    }
    names(gcms) <- GCMlookup[gcms]
    gcms <- as.list(c("Select GCM" = "", gcms))
    selectInput(ns("selGCM"), label = "Select global circulation model",
                choices = gcms)
  })

  observeEvent(input$goXferExtTime, {
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
        logger %>% writeLog(type = 'error', paste0("Specified filepath(s) "))
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
        'Transferion extent equal to current extent region.')
    }
    # LOAD INTO SPP ####
    spp[[curSp()]]$transfer$xfExt <- polyXf

    common$update_component(tab = "Map")
  })

  observeEvent(input$goTransferTime, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(
          type = 'error',
          'Calculate a model prediction in visualization component before transferring.')
      return()
    }
    if (is.null(spp[[curSp()]]$transfer$xfExt)) {
      logger %>% writeLog(type = 'error', 'Select extent of transfer first.')
      return()
    }
    envsRes <- raster::res(envs())[1]
    if (envsRes < 0.01) {
      logger %>%
        writeLog(type = 'error',
                 paste0('Transfer to New Time currently only available with ',
                        'resolutions >30 arc seconds.'))
      return()
    }

    if(!all(names(envs()) %in% paste0('bio', sprintf("%02d", 1:19)))) {
      nonBios <- names(envs())[!names(envs()) %in% paste0('bio', sprintf("%02d", 1:19))]
      logger %>%
        writeLog(type = 'error', hlSpp(curSp()),
                 "Your model is using non-bioclimatic variables or non-conventional",
                 " names (i.e., ", paste0(nonBios, collapse = ", "),
                 "). You can not transfer to a New Time.")
      return()
    }

        # DATA ####
    if (input$selTimeVar == 'worldclim') {
      # code taken from dismo getData() function to catch if user is trying to
      # download a missing combo of gcm / rcp
      gcms <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE',
                'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
      rcps <- c(26, 45, 60, 85)
      m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,
                    0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol = 4)
      i <- m[which(input$selGCM == gcms), which(input$selRCP == rcps)]
      if (!i) {
        logger %>%
          writeLog(type = 'error',
                   paste0('This combination of GCM and RCP is not available. Please ',
                          'make a different selection.'))
        return()
      }
      smartProgress(
        logger,
        message = paste("Retrieving WorldClim data for", input$selTime,
                        input$selRCP, "..."),
        {
          xferTimeEnvs <-
            raster::getData('CMIP5', var = "bio", res = round(envsRes * 60, 1),
                            rcp = input$selRCP, model = input$selGCM,
                            year = input$selTime)
          names(xferTimeEnvs) <- paste0('bio', c(paste0('0',1:9), 10:19))
          # in case user subsetted bioclims
          xferTimeEnvs <- xferTimeEnvs[[names(envs())]]
        }
      )
    } else if (input$selTimeVar == 'ecoclimate') {
      smartProgress(
        logger,
        message = paste0("Retrieving ecoClimate data of GCM ", input$xfAOGCM,
                         " for ", input$xfScenario, "..."),
        {
          xferTimeEnvs <- envs_ecoClimate(input$xfAOGCM, input$xfScenario,
                                          as.numeric(gsub("bio", "", names(envs()))),
                                          logger)
        }
      )
    }

    # ERRORS ####
    # Check that the extents of raster and extent of transfer intersects
    if (!rgeos::gIntersects(spp[[curSp()]]$transfer$xfExt,
                            methods::as(raster::extent(xferTimeEnvs),
                                        'SpatialPolygons'))) {
      logger %>%
        writeLog(type = 'error', 'Extents do not overlap')
      return()
    }

    # FUNCTION CALL ####
    req(xferTimeEnvs)
    predType <- rmm()$prediction$notes
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      xferTime.out <- xfer_time(evalOut = evalOut(),
                                curModel = curModel(),
                                envs = xferTimeEnvs,
                                xfExt = spp[[curSp()]]$transfer$xfExt,
                                alg = spp[[curSp()]]$rmm$model$algorithms,
                                logger,
                                spN = curSp())
    } else {
      xferTime.out <- xfer_time(evalOut = evalOut(),
                                curModel = curModel(),
                                envs = xferTimeEnvs,
                                xfExt = spp[[curSp()]]$transfer$xfExt,
                                alg = spp[[curSp()]]$rmm$model$algorithms,
                                outputType = predType,
                                clamp = rmm()$model$algorithm$maxent$clamping,
                                logger,
                                spN = curSp())
    }
    xferExt <- xferTime.out$xferExt
    xferTime <- xferTime.out$xferTime

    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    occPredVals <- spp[[curSp()]]$visualization$occPredVals

    if(!(input$threshold == 'none')) {
      # use threshold from present-day model training area
      if (input$threshold == 'mtp') {
        thr <- stats::quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr <- stats::quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr <- stats::quantile(occPredVals, probs = input$trainPresQuantile)
      }
      xferTimeThr <- xferTime > thr
      if (input$selTimeVar == 'worldclim') {
        logger %>% writeLog(hlSpp(curSp()), "Transferion of model to ", paste0('20', input$selTime),
                            ' with threshold ', input$threshold, ' (',
                            formatC(thr, format = "e", 2), ") for GCM ",
                            GCMlookup[input$selGCM], " under RCP ",
                            as.numeric(input$selRCP)/10.0, ".")
      } else if (input$selTimeVar == 'ecoclimate') {
        logger %>% writeLog(hlSpp(curSp()), "Transferion of model to ", input$xfScenario,
                            ' with threshold ', input$threshold, ' (',
                            formatC(thr, format = "e", 2), ") for GCM ",
                            input$xfAOGCM, ".")
      }
    } else {
      xferTimeThr <- xferTime
      if (input$selTimeVar == 'worldclim') {
        logger %>% writeLog(hlSpp(curSp()), "Transferion of model to ", paste0('20', input$selTime),
                            ' with ', predType, " output for GCM ", GCMlookup[input$selGCM],
                            " under RCP ", as.numeric(input$selRCP)/10.0, ".")
      } else if (input$selTimeVar == 'ecoclimate') {
        logger %>% writeLog(hlSpp(curSp()), "Transferion of model to ", input$xfScenario,
                            ' with ', predType, " output for GCM ", input$xfAOGCM, ".")
      }
    }
    raster::crs(xferTimeThr) <- raster::crs(envs())
    # rename
    names(xferTimeThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$transfer$xfEnvs <- xferExt
    spp[[curSp()]]$transfer$xferTimeEnvs <- xferTimeEnvs
    spp[[curSp()]]$transfer$mapXfer <- xferTimeThr
    spp[[curSp()]]$transfer$mapXferVals <- getRasterVals(xferTimeThr, predType)
    if (input$selTimeVar == "worldclim") {
      spp[[curSp()]]$transfer$xfEnvsDl <- paste0('CMIP5_', envsRes * 60, "min_RCP",
                                                input$selRCP, "_", input$selGCM,
                                                "_", input$selTime)
    } else if (input$selTimeVar == "ecoclimate") {
      spp[[curSp()]]$transfer$xfEnvsDl <- paste0('ecoClimate_', input$xfScenario,
                                                '_', input$xfAOGCM)
    }

    # REFERENCES
    knitcitations::citep(citation("dismo"))

    # METADATA ####
    spp[[curSp()]]$rmm$code$wallace$transfer_curModel <- curModel()
    spp[[curSp()]]$rmm$code$wallace$transfer_time <- TRUE
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(xferExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(xferExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(xferExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(xferExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "transfer to user-selected new time"
    if (input$selTimeVar == "worldclim") {
      xferYr <- paste0('20', input$selTime)
      ###For RMD only
      spp[[curSp()]]$rmm$code$wallace$transfer_worldclim <- TRUE
      spp[[curSp()]]$rmm$code$wallace$transfer_GCM <- input$selGCM
      spp[[curSp()]]$rmm$code$wallace$transfer_RCP <- input$selRCP
      spp[[curSp()]]$rmm$code$wallace$transfer_Time <- input$selTime

      spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- xferYr
      spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- xferYr
      spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"
      spp[[curSp()]]$rmm$data$transfer$environment1$notes <-
        paste("transfer to year", xferYr, "for GCM",
              GCMlookup[input$selGCM], "under RCP",
              as.numeric(input$selRCP)/10.0)
    } else if (input$selTimeVar == "ecoclimate") {
      spp[[curSp()]]$rmm$code$wallace$transfer_ecoclimate <- TRUE
      spp[[curSp()]]$rmm$code$wallace$transfer_AOGCM <- input$xfAOGCM
      spp[[curSp()]]$rmd$transfer_Scenario <- input$xfScenario
      spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "ecoClimate"
      spp[[curSp()]]$rmm$data$transfer$environment1$notes <-
        paste("transfer to", input$xfScenario, "for GCM", input$xfAOGCM)
      if (input$xfScenario == "LGM") {
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- -21000
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- -21000
      } else if (input$xfScenario == "Holo") {
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- -6000
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- -6000
      } else {
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- 2080
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- 2100
      }
    }


    spp[[curSp()]]$rmm$prediction$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(xferTimeThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(xferTimeThr, max), asChar = TRUE)
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

  # Reset Transferion Extent button functionality
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
        selTimeVar = input$selTimeVar,
        selTime = input$selTime,
        selRCP = input$selRCP,
        xfAOGCM = input$xfAOGCM,
        xfScenario = input$xfScenario,
        threshold = input$threshold,
        trainPresQuantile = input$trainPresQuantile
      )
    },
    load = function(state) {
      updateSelectInput(session, 'xferExt', selected = state$xferExt)
      updateNumericInput(session, 'userXfBuf', value = state$userXfBuf)
      updateNumericInput(session, 'drawXfBuf', value = state$drawXfBuf)
      updateSelectInput(session, 'selTime', selected = state$selTime)
      updateSelectInput(session, 'selRCP', selected = state$selRCP)
      updateSelectInput(session, 'xfAOGCM', selected = state$xfAOGCM)
      updateSelectInput(session, 'xfScenario', selected = state$xfScenario)
      updateSelectInput(session, 'threshold', selected = state$threshold)
      updateSliderInput(session, 'trainPresQuantile', value = state$trainPresQuantile)
    }
  ))

}

xfer_time_module_map <- function(map, common) {

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
  # Add just transfer Polygon
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
  # if threshold specified
  if(rmm()$prediction$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'red')
    map %>% removeControl("xfer") %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "Thresholded Suitability<br>(Transferred)",
                labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = 'xfer')
  } else {
 # if no threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapXferVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, mapXferVals, na.color = 'transparent')
    map %>% removeControl("xfer") %>%
      addLegend("bottomright", pal = legendPal,
                title = "Predicted Suitability<br>(Transferred)",
                values = mapXferVals, layerId = 'xfer',
                labFormat = reverseLabel(2, reverse_order = TRUE))
  }
  # map model prediction raster and transfer polygon
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('xferRas') %>%
    addRasterImage(mapXfer(), colors = rasPal, opacity = 0.7,
                   layerId = 'xferRas', group = 'xfer', method = "ngb")
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", group = 'xfer', fill = FALSE)
  }
}

xfer_time_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    xfer_time_knit = !is.null(species$rmm$code$wallace$transfer_time),
    curModel_rmd = species$rmm$code$wallace$transfer_curModel,
    outputType_rmd = species$rmm$prediction$notes,
    alg_rmd = species$rmm$model$algorithms,
    clamp_rmd = species$rmm$model$algorithm$maxent$clamping,

    ##Determine the type of extent of transfer to use correct RMD function
    xfer_time_user_knit = !is.null(species$rmm$code$wallace$userXfShpParams),
    xfer_time_drawn_knit = !is.null(species$rmm$code$wallace$drawExtPolyXfCoords),
    ###arguments for creating extent
    polyXfXY_rmd = if(!is.null(species$rmm$code$wallace$drawExtPolyXfCoords)){
      printVecAsis(species$polyXfXY)} else {NULL},
    polyXfID_rmd =  if(!is.null(species$rmm$code$wallace$drawExtPolyXfCoords)){
      species$polyXfID} else {0},
    BgBuf_rmd = species$rmm$code$wallace$XfBuff,
    polyXf_rmd = if(is.null(species$rmm$code$wallace$drawExtPolyXfCoords) & is.null(species$rmm$code$wallace$userXfShpParams)){
      species$procEnvs$bgExt} else {NULL},
    ##Use of threshold for transferring
    xfer_time_threshold_knit = !is.null(species$rmm$prediction$transfer$environment1$thresholdSet),
    xfer_thresholdRule_rmd = species$rmm$prediction$transfer$environment1$thresholdRule,
    xfer_threshold_rmd = if (!is.null(species$rmm$prediction$transfer$environment1$thresholdSet)){
      species$rmm$prediction$transfer$environment1$thresholdSet} else {0},
    xfer_probQuantile_rmd = species$rmm$code$wallace$transferQuantile,
    ###for guidance text
      ##name of environmental variables used
    envs_name_rmd = species$rmm$data$transfer$environment1$sources,
    yearMin_rmd = species$rmm$data$transfer$environment1$yearMin,
    yearMax_rmd = species$rmm$data$transfer$environment1$yearMax,
    ###for getting the right environmental variables
    xfer_time_worldclim_knit = !is.null(species$rmm$code$wallace$transfer_worldclim),
    model_rmd = if (!is.null(species$rmm$code$wallace$transfer_worldclim)){
      species$rmm$code$wallace$transfer_GCM} else {NULL},
    rcp_rmd = if (!is.null(species$rmm$code$wallace$transfer_worldclim)){
      species$rmm$code$wallace$transfer_RCP} else {0},
    year_rmd = if (!is.null(species$rmm$code$wallace$transfer_worldclim)){
      species$rmm$code$wallace$transfer_Time} else {0},
    xfAOGCM_rmd = if(!is.null(species$rmm$code$wallace$transfer_ecoclimate)){
      species$rmm$code$wallace$transfer_AOGCM} else {NULL},
    xfScenario_rmd = if(!is.null(species$rmm$code$wallace$transfer_ecoclimate)){
      species$rmd$transfer_Scenario} else {NULL}
  )
}

