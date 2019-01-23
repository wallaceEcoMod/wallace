
mapPreds_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Please see guidance for an explanation of different Maxent output types.',
             radioButtons(ns('maxentPredType'), label = "Prediction output",
                          choices = list("raw", "logistic", "cloglog"), selected = "raw", inline = TRUE)),
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'none',
                                        "Minimum Training Presence" = 'mtp', 
                                        "10 Percentile Training Presence" = 'p10')))
  )
}

mapPreds_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if(is.null(results())) {
      shinyLogs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    if(is.na(input$threshold)) {
      shinyLogs %>% writeLog(type = 'error', "Please select a thresholding rule.")
      return()
    }
    
    # pick the prediction that matches the model selected
    print(results()$predictions[[curModel()]])
    predSel <- results()$predictions[[curModel()]]
    raster::crs(predSel) <- raster::crs(bgMask())
    if(is.na(raster::crs(predSel))) {
      shinyLogs %>% writeLog(type = "error", "Model prediction raster has undefined 
                             coordinate reference system (CRS), and thus cannot be 
                             mapped. This is likely due to undefined CRS for input 
                             rasters. Please see guidance text for module 'User-specified 
                             Environmental Data' in component 'Obtain Environmental Data' 
                             for more details.")
      return()
    }
    
    # PROCESSING ####
    # define predType based on model type
    if(rmm()$model$algorithm == "BIOCLIM") {
      predType <- "BIOCLIM"
    }
    if(rmm()$model$algorithm %in% c("maxent.jar", "maxnet")) {
      predType <- input$maxentPredType
      # if selected prediction type is not raw, transform
      if(predType != "raw") {
        # transform and redefine predSel 
        
        smartProgress(shinyLogs, message = paste0("Generating ", input$maxentPredType, " prediction for model ", curModel(), "..."), {
          m <- results()$models[[curModel()]]
          clamping <- rmm()$model$maxent$clamping
          if(rmm()$model$algorithm == "maxnet") {
            predSel <- ENMeval::maxnet.predictRaster(m, bgMask(), type = input$maxentPredType, clamp = clamping)
          }
          if(rmm()$model$algorithm == "maxent.jar") {
            predSel <- dismo::predict(m, bgMask(), args = paste0("outputformat=", input$maxentPredType))
          }
        })
        # define crs 
        raster::crs(predSel) <- raster::crs(bgMask())
        # define predSel name
        names(predSel) <- curModel()
      } 
    }
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    
    # find predicted values for occurrences for selected model
    # extract the suitability values for all occurrences
    occs.xy <- occs()[c('longitude', 'latitude')]
    # determine the threshold based on the current, not projected, prediction
    occPredVals <- raster::extract(predSel, occs.xy)
    # get all thresholds
    thr <- getAllThresh(occPredVals)
    
    # get the chosen threshold value
    if(input$threshold != 'none') {
      thr.sel <- thr[[input$threshold]]
      predSel.thr <- predSel > thr.sel
      # rename prediction raster if thresholded
      names(predSel.thr) <- paste0(curModel(), '_', predType)
      shinyLogs %>% writeLog(curSp(), ": ", input$threshold, ' threshold selected
                             for ', predType, ': ', thr, '.')
    } else {
      predSel.thr <- predSel
    }
    
    # write to log box
    shinyLogs %>% writeLog(curSp(), ": ", rmm()$model$algorithm, " ", predType,
                           " model prediction plotted.")
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$results[[predType]] <- predSel
    spp[[curSp()]]$visualization$thresholds <- thr
    spp[[curSp()]]$visualization$mapPred <- predSel.thr
    spp[[curSp()]]$visualization$mapPredVals <- getRasterVals(predSel.thr, predType)
    
    # METADATA ####
    spp[[curSp()]]$rmm$output$prediction$thresholdRule <- input$threshold
    if(input$threshold != 'none') {
      spp[[curSp()]]$rmm$output$prediction$thresholdSet <- thr.sel
    } else {
      spp[[curSp()]]$rmm$output$prediction$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$prediction$notes <- predType
    
  })
}

mapPreds_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  req(mapPred())
  mapPredVals <- spp[[curSp()]]$visualization$mapPredVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if no threshold specified
  if (rmm()$output$prediction$thresholdRule != 'none') {
    rasPal <- c('gray', 'blue')
    map %>% clearAll() %>%
      addLegend("bottomright", colors = c('gray', 'blue'),
                title = "Thresholded Suitability<br>(Training)", labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = "train")
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapPredVals, na.color='transparent')
    rasPal <- colorNumeric(rasCols, mapPredVals, na.color='transparent')
    map %>% clearAll() %>%
      addLegend("bottomright", pal = legendPal, title = "Predicted Suitability<br>(Training)",
                values = mapPredVals, layerId = "train",
                labFormat = reverseLabels(2, reverse_order=TRUE))
  }
  # map model prediction raster
  map %>%
    map_occs(occs()) %>%
    addRasterImage(mapPred(), colors = rasPal, opacity = 0.7,
                   group = 'vis', layerId = 'mapPred', method = "ngb") %>%
    # add background polygon(s)
    mapBgPolys(bgShpXY())
}

mapPreds_INFO <- infoGenerator(modName = "Map Prediction", 
                               modAuts = "Jamie M. Kass, Robert Muscarella, Bruno
                               Vilela, Gonzalo E. Pinilla-Buitrago, Robert P.
                               Anderson", 
                               pkgName = "dismo")
