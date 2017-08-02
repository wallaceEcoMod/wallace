
mapPreds_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('predThresh'), label = "Set threshold",
                choices = list("No threshold" = 'noThresh',
                               "Minimum Training Presence" = 'mtp', 
                               "10 Percentile Training Presence" = 'p10')),
    shinyBS::bsPopover(ns('predThresh'), title = 'Tip',
                       'Create binary map of predicted presence/absence assuming 
                       all values above threshold value represent presence. Also 
                       can be interpreted as a "potential distribution" (see guidance).',
                       placement = 'right', options = list(container = "body"))
  )
}

mapPreds_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      rvs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    # record for RMD
    rvs$comp7.thr <- input$predThresh
    rvs$comp7 <- c(rvs$comp7, 'map')
    
    # pick the prediction that matches the model selected
    predSel <- rvs$modPreds[[rvs$modSel]]
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    if (input$predThresh != 'noThresh') {
      occValsSel <- rvs$modOccVals[,rvs$modSel]
      x <- thresh(occValsSel, input$predThresh)
      # revert to raw prediction
      predSel <- predSel > x
      names(predSel) <- paste0(rvs$modSel, '_thresh_', input$predThresh)
    }
    
    return(predSel)
  })
}
