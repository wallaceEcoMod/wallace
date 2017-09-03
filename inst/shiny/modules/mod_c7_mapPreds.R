
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
    # (same for all Maxent prediction types because they scale the same)
    predSel.thr <- callModule(threshPred_MOD, "threshPred", predSel)
    
    return(predSel.thr)
  })
}
