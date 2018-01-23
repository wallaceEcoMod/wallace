
mapPreds_UI <- function(id) {
  ns <- NS(id)
  tagList(
    threshPred_UI(ns('threshPred'))
  )
}

mapPreds_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      rvs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    # record for RMD
    rvs$comp7 <- c(rvs$comp7, 'map')
    
    # pick the prediction that matches the model selected
    predSel <- rvs$modPreds[[rvs$modSel]]
    
    if (is.na(raster::crs(predSel))) {
      rvs %>% writeLog(type = "error", "Model prediction raster has undefined 
                       coordinate reference system (CRS), and thus cannot be 
                       mapped. This is likely due to undefined CRS's for input 
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
    rvs %>% writeLog("BIOCLIM model prediction plotted.")

    return(pjPred)
  })
}
