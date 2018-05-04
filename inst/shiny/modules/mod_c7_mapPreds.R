
mapPreds_UI <- function(id) {
  ns <- NS(id)
  tagList(
    threshPred_UI(ns('threshPred')),
    tags$div(title='Please see guidance for an explanation of different Maxent output types.',
             radioButtons(ns('maxentPredType'), label = "Prediction output",
                          choices = list("raw", "logistic", "cloglog"), selected = "raw", inline = TRUE)),
    threshPred_UI(ns('threshPred'))
  )
}

mapPreds_MOD <- function(input, output, session) {
  reactive({
    if(is.null(results())) {
      shinyLogs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
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
      predSel <- maxentPredTransform(results(), bgMask(), input$maxentPredType, shinyLogs)
      # put transformed predictions into results list
      spp[[curSp()]]$results[[input$maxentPredType]] <- predSel
    }
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    getThreshPred <- callModule(threshPred_MOD, "threshPred", predSel)
    threshPrediction <- getThreshPred()
    print(threshPrediction)
    
    # save to spp
    spp[[curSp()]]$visualization$mapPred <- threshPrediction
    spp[[curSp()]]$visualization$mapPredVals <- getVals(threshPrediction, input$maxentPredType)
    
    # write to log box
    shinyLogs %>% writeLog(rmm()$model$algorithm, input$predType, "model prediction plotted.")
    
    # METADATA
    if(rmm()$model$algorithm == "Maxent") {
      spp[[curSp()]]$rmm$output$prediction$notes <- input$maxentPredType
    }
    
  })
}

mapPreds_INFO <- infoGenerator(modName = "Map Prediction", 
                               modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                               pkgName = "dismo")
