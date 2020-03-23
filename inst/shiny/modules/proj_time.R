proj_time_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Study Region (**)", class = "stepText"), br(), br(),
    selectInput(ns('projExt'), label = "Select method (**)",
                choices = list("Same extent (**)" = 'pjCur',
                               "Draw polygon(**)" = 'pjDraw',
                               "User-specified(**)" = 'pjUser')),
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
                               '8.5' = 85)),
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
    if (spp[[curSp()]]$rmm$model$algorithm != "BIOCLIM") {
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
                          input$drawPjBuf, logger)
      if (input$drawPjBuf == 0 ) {
        logger %>% writeLog(
          em(spName(curSp())), ' : Draw polygon without buffer(**).')
      } else {
        logger %>% writeLog(
          em(spName(curSp())), ' : Draw polygon with buffer of ', input$drawPjBuf,
          ' degrees (**).')
      }
      # METADATA ####
      polyX <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallaceSettings$drawExtPolyPjCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
    }

    if (input$projExt == 'pjUser') {
      polyPj <- penvs_userBgExtent(input$userPjShp$datapath,
                                   input$userPjShp$name,
                                   input$userPjBuf, logger)
      # METADATA ####
      # get extensions of all input files
      exts <- sapply(strsplit(input$userPjShp$name, '\\.'),
                     FUN = function(x) x[2])
      if('csv' %in% exts) {
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjExt <- 'csv'
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjPath <- input$userPjShp$datapath
      }
      else if('shp' %in% exts) {
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjExt <- 'shp'
        # get index of .shp
        i <- which(exts == 'shp')
        shpName <- strsplit(input$userPjShp$name[i], '\\.')[[1]][1]
        spp[[curSp()]]$rmm$code$wallaceSettings$userPjShpParams <-
          list(dsn = input$userPjShp$datapath[i], layer = shpName)
      }
    }

    if (input$projExt == 'pjCur') {
      polyPj <- spp[[curSp()]]$procEnvs$bgExt
      logger %>% writeLog(
        em(spName(curSp())),
        ' : Projection extent equal to current extent region. (**)')
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

    # DATA ####
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

    # FUNCTION CALL ####
    predType <- rmm()$output$prediction$notes
    projTime.out <- proj_time(evalOut(), curModel(), projTimeEnvs, predType,
                              alg = rmm()$model$algorithm,
                              clamp = rmm()$model$maxent$clamping,
                              spp[[curSp()]]$project$pjExt, logger)
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
      logger %>% writeLog("Projection of model to ", paste0('20', input$selTime),
                             " for ", em(spName(curSp())), ' with threshold ',
                             input$threshold, ' (', formatC(thr, format = "e", 2),
                             ") for GCM ", GCMlookup[input$selGCM],
                             " under RCP ", as.numeric(input$selRCP)/10.0, ".")
    } else {
      projTimeThr <- projTime
      logger %>% writeLog("Projection of model to ", paste0('20', input$selTime),
                             " for ", em(spName(curSp())), ' with ', predType,
                             " output for GCM ", GCMlookup[input$selGCM],
                             " under RCP ", as.numeric(input$selRCP)/10.0, ".")
    }
    raster::crs(projTimeThr) <- raster::crs(envs())
    # rename
    names(projTimeThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projTimeThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projTimeThr, predType)

    # METADATA ####
    projYr <- paste0('20', input$selTime)
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- projYr
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- projYr
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "project to user-selected new time"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"
    spp[[curSp()]]$rmm$data$transfer$environment1$notes <-
      paste("projection to year", projYr, "for GCM",
            GCMlookup[input$selGCM], "under RCP",
            as.numeric(input$selRCP)/10.0)

    spp[[curSp()]]$rmm$output$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$output$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projTimeThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$output$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projTimeThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$transfer$environment1$thresholdRule <- input$threshold
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL

    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
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
  if(rmm()$output$transfer$environment1$thresholdRule != 'none') {
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

# proj_time_module_rmd <- function(species) {
#   # Variables used in the module's Rmd code
#   list(
#     module_knit = species$rmm$code$wallaceSettings$someFlag,
#     var1 = species$rmm$code$wallaceSettings$someSetting1,
#     var2 = species$rmm$code$wallaceSettings$someSetting2
#   )
# }

