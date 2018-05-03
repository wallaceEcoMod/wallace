
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
    if(is.null(spp[[curSp()]]$results)) {
      shinyLogs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    # pick the prediction that matches the model selected
    predSel <- spp[[curSp()]]$results$predictions[[curModel()]]
    
    if(spp[[curSp()]]$rmm$model$algorithm == "Maxent") {
      spp[[curSp()]]$rmm$output$prediction$notes
    } 
    
    
    
    if (is.na(raster::crs(predSel))) {
      rvs %>% writeLog(type = "error", "Model prediction raster has undefined 
                       coordinate reference system (CRS), and thus cannot be 
                       mapped. This is likely due to undefined CRS for input 
                       rasters. Please see guidance text for module 'User-specified 
                       Environmental Data' in component 'Obtain Environmental Data' 
                       for more details.")
      return()
    }
    
    # generate binary prediction based on selected thresholding rule 
    predSel.thr.call <- callModule(threshPred_MOD, "threshPred", predSel)
    predSel.thr <- predSel.thr.call()
    pjPred <- predSel.thr$pred
    rvs$comp7.thr <- predSel.thr$thresh
    
    # write to log box
    logs %>% writeLog("BIOCLIM model prediction plotted.")

    return(pjPred)
  })
}

mapPreds_INFO <- infoGenerator(modName = "Map Prediction", 
                               modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson", 
                               pkgName = "dismo")
