
mapPreds_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Please see guidance for an explanation of different Maxent output types.',
             radioButtons(ns('maxentPredType'), label = "Prediction output",
                          choices = list("raw", "logistic", "cloglog"), selected = "raw", inline = TRUE)),
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshhold'), label = "Set threshold",
                         choices = list("No threshold" = 'noThresh',
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
    rasName <- paste0(curModel(), '_thresh_', predType)
    threshPrediction  <- threshPred(occs(), 
                                    predSel, 
                                    input$threshhold, 
                                    rasName)
    threshVal <- round(raster::cellStats(threshPrediction, min), digits = 3)
    shinyLogs %>% writeLog(input$threshhold, 'threshold selected for', predType, ': ', threshVal, '.')
    
    
    # save to spp
    spp[[curSp()]]$visualization$mapPred <- threshPrediction
    spp[[curSp()]]$visualization$mapPredVals <- getRasterVals(threshPrediction, predType)
    
    # write to log box
    shinyLogs %>% writeLog(rmm()$model$algorithm, predType, "model prediction plotted.")
    
    # METADATA
    spp[[curSp()]]$rmm$output$prediction$thresholdRule <- input$threshhold
    if(rmm()$model$algorithm == "Maxent") {
      spp[[curSp()]]$rmm$output$prediction$notes <- predType
    }
    
  })
}

mapPreds_INFO <- infoGenerator(modName = "Map Prediction", 
                               modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                               pkgName = "dismo")
