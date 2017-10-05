
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
