
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
    if(is.null(results())) {
      shinyLogs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    # if BIOCLIM, set predType to BIOCLIM instead of Maxent type
    if(rmm()$model$algorithm == "BIOCLIM") {
      predType <- "BIOCLIM"
    } else if(rmm()$model$algorithm == "Maxent") {
      predType <- input$maxentPredType
    }
    
    # pick the prediction that matches the model selected
    predSel <- results()$predictions[[curModel()]]
    
    if(is.na(raster::crs(predSel))) {
      shinyLog %>% writeLog(type = "error", "Model prediction raster has undefined 
                            coordinate reference system (CRS), and thus cannot be 
                            mapped. This is likely due to undefined CRS for input 
                            rasters. Please see guidance text for module 'User-specified 
                            Environmental Data' in component 'Obtain Environmental Data' 
                            for more details.")
      return()
    }
    
    if(rmm()$model$algorithm == "Maxent") {
      predSel <- maxentPredTransform(results(),
                                     curModel(),
                                     bgMask(), 
                                     predType, 
                                     shinyLogs)
      names(predSel) <- curModel()
      # put transformed predictions into results list
      spp[[curSp()]]$results[[predType]] <- predSel
    }
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    
    # find predicted values for occurrences for selected model
    # extract the suitability values for all occurrences
    occs.xy <- occs()[c('longitude', 'latitude')]
    # determine the threshold based on the current, not projected, prediction
    occPredVals <- raster::extract(predSel, occs.xy)
    # get all thresholds
    thresholds <- getAllThresh(occPredVals)
    
    # get the chosen threshold value
    if(!(input$threshold == 'none')) {
      thr <- thresholds[[input$threshold]]
      predThr <- predSel > thr
      shinyLogs %>% writeLog(curSp(), ":", input$threshold, 'threshold selected for', predType, ':', thr, '.')
    } else {
      predThr <- predSel
    }
    # threshold prediction and rename
    names(predThr) <- paste0(curModel(), '_thresh_', predType)
    
    # save to spp
    spp[[curSp()]]$visualization$thresholds <- thresholds
    spp[[curSp()]]$visualization$mapPred <- predThr
    spp[[curSp()]]$visualization$mapPredVals <- getRasterVals(predThr, predType)
    
    # write to log box
    shinyLogs %>% writeLog(curSp(), ":", rmm()$model$algorithm, predType, "model prediction plotted.")
    
    # METADATA
    spp[[curSp()]]$rmm$output$prediction$thresholdRule <- input$threshold
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$output$prediction$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$output$prediction$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$prediction$notes <- predType
    
  })
}

mapPreds_INFO <- infoGenerator(modName = "Map Prediction", 
                               modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                               pkgName = "dismo")
