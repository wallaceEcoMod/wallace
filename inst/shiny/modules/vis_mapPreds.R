vis_mapPreds_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(
      title = paste0('Create binary map of predicted presence/absence assuming',
                     ' all values above threshold value represent presence.',
                     ' Also can be interpreted as a "potential distribution"',
                     '(see guidance).'),
      selectInput(ns('threshold'), label = "Set threshold",
                  choices = list("No threshold" = 'none',
                                 "Minimum Training Presence" = 'mtp',
                                 "10 Percentile Training Presence" = 'p10',
                                 "Quantile of Training Presences" = 'qtp'))),
    conditionalPanel(
      sprintf("input['%s'] == 'qtp'", ns("threshold")),
      sliderInput(ns("trainPresQuantile"), "Set quantile",
                  min = 0, max = 1, value = .05)
    ),
    conditionalPanel(paste0("input['", ns("threshold"), "'] == 'none'"),
                     uiOutput(ns("maxentPredType"))),
    actionButton(ns("goMapPreds"), "Plot")
  )
}

vis_mapPreds_module_server <- function(input, output, session, common) {

  spp <- common$spp
  evalOut <- common$evalOut
  curSp <- common$curSp
  allSp <- common$allSp
  curModel <- common$curModel
  bgMask <- common$bgMask
  occs <- common$occs
  logger <- common$logger
  bgShpXY <- common$bgShpXY

  output$maxentPredType <- renderUI({
    ns <- session$ns
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms != "BIOCLIM") {
        tags$div(
          title = 'Please see guidance for an explanation of different Maxent output types.',
          radioButtons(ns('maxentPredType'), label = "Prediction output",
                       choices = list("cloglog", "logistic", "raw"),
                       inline = TRUE))
    }
  })

  observeEvent(input$goMapPreds, {
    # ERRORS ####
    if(is.null(evalOut())) {
      logger %>% writeLog(
        type = 'error',
        "Models must be run before visualizing model predictions.")
      return()
    }

    if(is.na(input$threshold)) {
      logger %>% writeLog(
        type = 'error', "Please select a thresholding rule.")
      return()
    }

    # pick the prediction that matches the model selected
    predSel <- evalOut()@predictions[[curModel()]]
    raster::crs(predSel) <- raster::crs(bgMask())
    if(is.na(raster::crs(predSel))) {
      logger %>% writeLog(
        type = "error",
        paste0("Model prediction raster has undefined coordinate reference ",
               "system (CRS), and thus cannot be mapped. This is likely due to",
               " undefined CRS for input rasters. Please see guidance text for",
               " module 'User-specified Environmental Data' in component",
               " 'Obtain Environmental Data' for more details."))
      return()
    }
    # PROCESSING ####
    # define predType based on model type
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      predType <- "BIOCLIM"
      m <- evalOut()@models[[curModel()]]
      predSel <- dismo::predict(m, bgMask(), useC = FALSE)
      # define crs
      raster::crs(predSel) <- raster::crs(bgMask())
      # define predSel name
      names(predSel) <- curModel()
    } else if (spp[[curSp()]]$rmm$model$algorithms %in% c("maxent.jar", "maxnet")) {
      if (is.null(input$maxentPredType)) {
        predType <- "cloglog"
      } else {
        predType <- input$maxentPredType
      }
      # if selected prediction type is not raw, transform
      # transform and redefine predSel
      smartProgress(
        logger,
        message = paste0("Generating ", input$maxentPredType,
                         " prediction for model ", curModel(), "..."), {
                           m <- evalOut()@models[[curModel()]]
                           clamping <- spp[[curSp()]]$rmm$model$algorithm$maxent$clamping
                           if (spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
                             if (predType == "raw") predType <- "exponential"
                             predSel <- predictMaxnet(m, bgMask(),
                                                             type = predType,
                                                             clamp = FALSE)
                           } else if (spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
                             outputFormat <- paste0("outputformat=", predType)
                             if (clamping == TRUE) {
                               doClamp <- "doclamp=true"
                             } else {
                               doClamp <- "doclamp=false"
                             }
                             predSel <- dismo::predict(m, bgMask(),
                                                       args = c(outputFormat, doClamp),
                                                       na.rm = TRUE)
                           }
                         })
      # define crs
      raster::crs(predSel) <- raster::crs(bgMask())
      # define predSel name
      names(predSel) <- curModel()

    }

    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    # find predicted values for occurrences for selected model
    # extract the suitability values for all occurrences
    occs.xy <- occs()[c('longitude', 'latitude')]
    # determine the threshold based on the current, not projected, prediction
    occPredVals <- raster::extract(predSel, occs.xy)
    # get all thresholds
    # get the chosen threshold value
    if (input$threshold != 'none') {
      if (input$threshold == 'mtp') {
        thr.sel <- stats::quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr.sel <- stats::quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr.sel <- stats::quantile(occPredVals, probs = input$trainPresQuantile)
      }
      predSel.thr <- predSel > thr.sel
      # rename prediction raster if thresholded
      names(predSel.thr) <- paste0(curModel(), '_', predType)
      nameAlg <- ifelse(spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM",
                        "",
                        paste0(" ", spp[[curSp()]]$rmm$model$algorithms, " "))
      logger %>% writeLog(hlSpp(curSp()),
                          input$threshold, ' threshold selected for ', nameAlg, predType,
                          ' (', formatC(thr.sel, format = "e", 2), ').')
    } else {
      predSel.thr <- predSel
    }

    # write to log box
    if (predType == 'BIOCLIM') {
      logger %>% writeLog(
        hlSpp(curSp()), "BIOCLIM model prediction plotted.")
    } else if (input$threshold != 'none'){
      logger %>% writeLog(
        hlSpp(curSp()), spp[[curSp()]]$rmm$model$algorithms,
        " model prediction plotted.")
    } else if (input$threshold == 'none'){
      logger %>% writeLog(
        hlSpp(curSp()), spp[[curSp()]]$rmm$model$algorithms, " ",
        predType, " model prediction plotted.")
    }

    # LOAD INTO SPP ####
    spp[[curSp()]]$visualization$occPredVals <- occPredVals
    if (input$threshold != 'none') {
      spp[[curSp()]]$visualization$thresholds <- thr.sel # were you recording multiple before?
    }
    spp[[curSp()]]$visualization$mapPred <- predSel.thr
    spp[[curSp()]]$visualization$mapPredVals <- getRasterVals(predSel.thr, predType)
    # METADATA ####
    spp[[curSp()]]$rmd$vis_curModel <- curModel()
    spp[[curSp()]]$rmm$prediction$Type <- predType
    spp[[curSp()]]$rmm$prediction$binary$thresholdRule <- input$threshold
    if (input$threshold != 'none') {
      spp[[curSp()]]$rmm$prediction$binary$thresholdSet <- thr.sel
      if (input$threshold == 'qtp') {
        spp[[curSp()]]$rmm$code$wallace$trainPresQuantile <- input$trainPresQuantile
      } else {
        spp[[curSp()]]$rmm$code$wallace$trainPresQuantile <- 0
      }
    } else {
      spp[[curSp()]]$rmm$prediction$binary$thresholdSet <- NULL
      spp[[curSp()]]$rmm$prediction$continuous$minVal <- min(occPredVals)
      spp[[curSp()]]$rmm$prediction$continuous$maxVal <- max(occPredVals)
    }
    spp[[curSp()]]$rmm$prediction$notes <- predType

    # REFERENCES
    knitcitations::citep(citation("dismo"))


    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        threshold = input$threshold,
        trainPresQuantile = input$trainPresQuantile
      )
    },
    load = function(state) {
      updateSelectInput(session, "threshold", selected = state$threshold)
      updateSliderInput(session, 'trainPresQuantile', value = state$trainPresQuantile)
    }
  ))

}

vis_mapPreds_module_map <- function(map, common) {

  spp <- common$spp
  curSp <- common$curSp
  mapPred <- common$mapPred
  rmm <- common$rmm
  occs <- common$occs
  bgShpXY <- common$bgShpXY

  # Map logic
  req(mapPred())
  mapPredVals <- spp[[curSp()]]$visualization$mapPredVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if threshold specified
  if (rmm()$prediction$binary$thresholdRule != 'none') {
    rasPal <- c('gray', 'blue')
    map %>% clearAll() %>%
      addLegend("bottomright", colors = c('gray', 'blue'),
                title = "Thresholded Suitability<br>(Training)",
                labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = "train")
  } else {
    # if no threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapPredVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, mapPredVals, na.color = 'transparent')
    map %>% clearAll() %>%
      addLegend("bottomright", pal = legendPal,
                title = "Predicted Suitability<br>(Training)",
                values = mapPredVals, layerId = "train",
                labFormat = reverseLabel(2, reverse_order = TRUE))
  }

  # function to map all background polygons
  mapBgPolys <- function(map, bgShpXY) {
    for (shp in bgShpXY) {
      map %>%
        addPolygons(lng = shp[,1], lat = shp[,2], fill = FALSE,
                    weight = 4, color = "blue", group = 'proj')
    }
  }
  # map model prediction raster
  map %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = 'red',
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    addRasterImage(mapPred(), colors = rasPal, opacity = 0.7,
                   group = 'vis', layerId = 'mapPred', method = "ngb") %>%
    # add background polygon(s)
    mapBgPolys(bgShpXY())
}

vis_mapPreds_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_mapPreds_knit = !is.null(species$visualization$mapPred),
    vis_map_threshold_knit = !is.null(species$rmm$prediction$binary$thresholdSet),
    vis_map_maxnet_knit = if(!is.null(species$rmm$model$algorithms)){
      species$rmm$model$algorithms == "maxnet"} else {FALSE},
    vis_map_maxent_knit = if(!is.null(species$rmm$model$algorithms)){
      species$rmm$model$algorithms == "maxent.jar"} else {FALSE},
    vis_map_bioclim_knit = if(!is.null(species$rmm$model$algorithms)){
      species$rmm$model$algorithms == "BIOCLIM"} else {FALSE},
    alg_rmd = if(!is.null(species$rmm$model$algorithms)){species$rmm$model$algorithms} else {NULL},
    curModel_rmd = if(!is.null(species$rmd$vis_curModel)){species$rmd$vis_curModel} else {NULL},
    clamp_rmd =  species$rmm$model$algorithm$maxent$clamping,
    predType_rmd = species$rmm$prediction$Type,
    threshold_rmd = if (!is.null(species$rmm$prediction$binary$thresholdSet)) {
      species$rmm$prediction$binary$thresholdSet} else {0},
    thresholdRule_rmd = species$rmm$prediction$binary$thresholdRule,
    probQuantile_rmd = species$rmm$code$wallace$trainPresQuantile
  )
}

