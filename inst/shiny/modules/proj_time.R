proj_time_module_ui <- function(id) {
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
    actionButton(ns("goProjExtTime"), "Create(**)"), br(),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Project (**)", class = "stepText"), br(),
    p("Project model to projected extent (red) (**)"),
    radioButtons(ns('selTimeVar'), label = "Select source of variables",
                 choices = list("Worldclim" = "worldclim",
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
                              selectInput(ns("pjAOGCM"),
                                          label = "Select the Atmospheric Oceanic General Circulation Model you want to use (**)",
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
                              selectInput(ns("pjScenario"),
                                          label = "select the temporal scenario that you want to use (**)",
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
    actionButton(ns('goProjectTime'), "Project")
  )
}

proj_time_module_server <- function(input, output, session, common) {

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
      h5("Prediction output is the same than Visualize component (**)")
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

  observeEvent(input$goProjExtTime, {
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

  observeEvent(input$goProjectTime, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      logger %>%
        writeLog(
          type = 'error',
          'Calculate a model prediction in visualization component before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$project$pjExt)) {
      logger %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    envsRes <- raster::res(envs())[1]
    if(envsRes < 0.01) {
      logger %>%
        writeLog(type = 'error',
                 paste0('Project to New Time currently only available with ',
                        'resolutions >30 arc seconds.'))
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
          projTimeEnvs <-
            raster::getData('CMIP5', var = "bio", res = envsRes * 60,
                            rcp = input$selRCP, model = input$selGCM,
                            year = input$selTime)
          names(projTimeEnvs) <- paste0('bio', c(paste0('0',1:9), 10:19))
          # in case user subsetted bioclims
          projTimeEnvs <- projTimeEnvs[[names(envs())]]
        }
      )
    } else if (input$selTimeVar == 'ecoclimate') {
      smartProgress(
        logger,
        message = paste0("Retrieving ecoClimate data of GCM ", input$pjAOGCM,
                         " for ", input$pjScenario, "..."),
        {
          projTimeEnvs <- envs_ecoClimate(input$pjAOGCM, input$pjScenario,
                                          as.numeric(gsub("bio", "", names(envs()))),
                                          logger)
        }
      )
    }

    # FUNCTION CALL ####
    predType <- rmm()$prediction$notes
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      projTime.out <- proj_time(evalOut = evalOut(),
                                curModel = curModel(),
                                envs = projTimeEnvs,
                                pjExt = spp[[curSp()]]$project$pjExt,
                                alg = spp[[curSp()]]$rmm$model$algorithms,
                                logger,
                                spN = curSp())
    } else {
      projTime.out <- proj_time(evalOut = evalOut(),
                                curModel = curModel(),
                                envs = projTimeEnvs,
                                pjExt = spp[[curSp()]]$project$pjExt,
                                alg = spp[[curSp()]]$rmm$model$algorithms,
                                outputType = predType,
                                clamp = rmm()$model$algorithm$maxent$clamping,
                                logger,
                                spN = curSp())
    }
    projExt <- projTime.out$projExt
    projTime <- projTime.out$projTime

    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    occPredVals <- spp[[curSp()]]$visualization$occPredVals

    if(!(input$threshold == 'none')) {
      # use threshold from present-day model training area
      if (input$threshold == 'mtp') {
        thr <- quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr <- quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr <- quantile(occPredVals, probs = input$trainPresQuantile)
      }
      projTimeThr <- projTime > thr
      if (input$selTimeVar == 'worldclim') {
        logger %>% writeLog(hlSpp(curSp()), "Projection of model to ", paste0('20', input$selTime),
                            ' with threshold ', input$threshold, ' (',
                            formatC(thr, format = "e", 2), ") for GCM ",
                            GCMlookup[input$selGCM], " under RCP ",
                            as.numeric(input$selRCP)/10.0, ".")
      } else if (input$selTimeVar == 'ecoclimate') {
        logger %>% writeLog(hlSpp(curSp()), "Projection of model to ", input$pjScenario,
                            ' with threshold ', input$threshold, ' (',
                            formatC(thr, format = "e", 2), ") for GCM ",
                            input$pjAOGCM, ".")
      }
    } else {
      projTimeThr <- projTime
      if (input$selTimeVar == 'worldclim') {
        logger %>% writeLog(hlSpp(curSp()), "Projection of model to ", paste0('20', input$selTime),
                            ' with ', predType, " output for GCM ", GCMlookup[input$selGCM],
                            " under RCP ", as.numeric(input$selRCP)/10.0, ".")
      } else if (input$selTimeVar == 'ecoclimate') {
        logger %>% writeLog(hlSpp(curSp()), "Projection of model to ", input$pjScenario,
                            ' with ', predType, " output for GCM ", input$pjAOGCM, ".")
      }
    }
    raster::crs(projTimeThr) <- raster::crs(envs())
    # rename
    names(projTimeThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$projTimeEnvs <- projTimeEnvs
    spp[[curSp()]]$project$mapProj <- projTimeThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projTimeThr, predType)
    if (input$selTimeVar == "worldclim") {
      spp[[curSp()]]$project$pjEnvsDl <- paste0('CMIP5_', envsRes * 60, "min_RCP",
                                                input$selRCP, "_", input$selGCM,
                                                "_", input$selTime)
    } else if (input$selTimeVar == "ecoclimate") {
      spp[[curSp()]]$project$pjEnvsDl <- paste0('ecoClimate_', input$pjScenario,
                                                '_', input$pjAOGCM)
    }

    # METADATA ####
    spp[[curSp()]]$rmd$project$curModel <- curModel()
    spp[[curSp()]]$rmd$project$time <- TRUE
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "project to user-selected new time"
    if (input$selTimeVar == "worldclim") {
      projYr <- paste0('20', input$selTime)
      ###For RMD only
      spp[[curSp()]]$rmd$data$transfer$environment1$worldclim <- TRUE
      spp[[curSp()]]$rmd$data$transfer$environment1$GCM <- input$selGCM
      spp[[curSp()]]$rmd$data$transfer$environment1$RCP <- input$selRCP
      spp[[curSp()]]$rmd$data$transfer$environment1$Time <- input$selTime

      spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- projYr
      spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- projYr
      spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"
      spp[[curSp()]]$rmm$data$transfer$environment1$notes <-
        paste("projection to year", projYr, "for GCM",
              GCMlookup[input$selGCM], "under RCP",
              as.numeric(input$selRCP)/10.0)
    } else if (input$selTimeVar == "ecoclimate") {
      spp[[curSp()]]$rmd$data$transfer$environment1$ecoclimate <- TRUE
      spp[[curSp()]]$rmd$data$transfer$environment1$AOGCM <- input$pjAOGCM
      spp[[curSp()]]$rmd$data$transfer$environment1$Scenario <- input$pjScenario
      spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "ecoClimate"
      spp[[curSp()]]$rmm$data$transfer$environment1$notes <-
        paste("projection to", input$pjScenario, "for GCM", input$pjAOGCM)
      if (input$pjScenario == "LGM") {
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- -21000
        spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- -21000
      } else if (input$pjScenario == "Holo") {
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
      printVecAsis(raster::cellStats(projTimeThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$prediction$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projTimeThr, max), asChar = TRUE)
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

    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        projExt = input$projExt,
        userPjBuf = input$userPjBuf,
        drawPjBuf = input$drawPjBuf,
        selTimeVar = input$selTimeVar,
        selTime = input$selTime,
        selRCP = input$selRCP,
        pjAOGCM = input$pjAOGCM,
        pjScenario = input$pjScenario,
        threshold = input$threshold,
        trainPresQuantile = input$trainPresQuantile
      )
    },
    load = function(state) {
      updateSelectInput(session, 'projExt', selected = state$projExt)
      updateNumericInput(session, 'userPjBuf', value = state$userPjBuf)
      updateNumericInput(session, 'drawPjBuf', value = state$drawPjBuf)
      updateSelectInput(session, 'selTime', selected = state$selTime)
      updateSelectInput(session, 'selRCP', selected = state$selRCP)
      updateSelectInput(session, 'pjAOGCM', selected = state$pjAOGCM)
      updateSelectInput(session, 'pjScenario', selected = state$pjScenario)
      updateSelectInput(session, 'threshold', selected = state$threshold)
      updateSliderInput(session, 'trainPresQuantile', value = state$trainPresQuantile)
    }
  ))

}

proj_time_module_map <- function(map, common) {

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
    shp <- polyPjXY[[1]]@coords
  } else {
    shp <- lapply(polyPjXY, function(x) x@coords)
  }
  bb <- spp[[curSp()]]$project$pjExt@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>% clearAll() %>% removeImage('projRas') %>%
    addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "red",
                group = 'bgShp') %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
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
  colnames(shp) <- c("longitude", "latitude")
  map %>%
    clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    addRasterImage(mapProj(), colors = rasPal, opacity = 0.7,
                   layerId = 'projRas', group = 'proj', method = "ngb") %>%
    addPolygons(lng = shp[,1], lat = shp[,2], layerId = "projExt",
                fill = FALSE, weight = 4, color = "red", group = 'proj')
}

proj_time_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    proj_time_knit = !is.null(species$rmd$project$time),
    curModel_rmd = species$rmd$project$curModel,
    outputType_rmd = species$rmm$prediction$notes,
    alg_rmd = species$rmm$model$algorithms,
    clamp_rmd = species$rmm$model$algorithm$maxent$clamping,

    ##Determine the type of projection extent to use correct RMD function
    proj_time_user_knit = !is.null(species$rmm$code$wallace$userPjShpParams),
    proj_time_drawn_knit = !is.null(species$rmm$code$wallace$drawExtPolyPjCoords),
    ###arguments for creating extent
    polyPjXY_rmd = if(!is.null(species$rmm$code$wallace$drawExtPolyPjCoords)){
      printVecAsis(species$polyPjXY)} else {NULL},
    polyPjID_rmd =  if(!is.null(species$rmm$code$wallace$drawExtPolyPjCoords)){
      species$polyPjID} else {0},
    BgBuf_rmd = species$rmm$code$wallace$PjBuff,
    polyPj_rmd = if(is.null(species$rmm$code$wallace$drawExtPolyPjCoords) & is.null(species$rmm$code$wallace$userPjShpParams)){
      species$procEnvs$bgExt} else {NULL},
    ##Use of threshold for projection
    proj_time_threshold_knit = !is.null(species$rmm$prediction$transfer$environment1$thresholdSet),
    thresholdRule_rmd = species$rmm$prediction$transfer$environment1$thresholdRule,
    threshold_rmd = if (!is.null(species$rmm$prediction$transfer$environment1$thresholdSet)){
      species$rmm$prediction$transfer$environment1$thresholdSet} else {0},
    ###for guidance text
      ##name of environmental variables used
    envs_name_rmd = species$rmm$data$transfer$environment1$sources,
    yearMin_rmd = species$rmm$data$transfer$environment1$yearMin,
    yearMax_rmd = species$rmm$data$transfer$environment1$yearMax,
    ###for getting the right environmental variables
    proj_time_worldclim_knit = !is.null(species$rmd$data$transfer$environment1$worldclim),
    model_rmd = if (!is.null(species$rmd$data$transfer$environment1$worldclim)){
      species$rmd$data$transfer$environment1$GCM} else {NULL},
    rcp_rmd = if (!is.null(species$rmd$data$transfer$environment1$worldclim)){
      species$rmd$data$transfer$environment1$RCP} else {0},
    year_rmd = if (!is.null(species$rmd$data$transfer$environment1$worldclim)){
      species$rmd$data$transfer$environment1$Time} else {0},
    pjAOGCM_rmd = if(!is.null(sppecies$rmd$data$transfer$environment1$ecoclimate)){
      species$rmd$data$transfer$environment1$AOGCM} else {NULL},
    pjScenario_rmd = if(!is.null(sppecies$rmd$data$transfer$environment1$ecoclimate)){
      species$rmd$data$transfer$environment1$Scenario} else {NULL}
  )
}

