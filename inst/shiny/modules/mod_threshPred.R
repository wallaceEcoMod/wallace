
threshPred_UI <- function(id) {
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

threshPred_MOD <- function(input, output, session, pred) {
  reactive({
    # record for RMD
    rvs$comp7.thr <- input$predThresh
    
    # pick the prediction that matches the model selected
    # predSel <- rvs$modPreds[[rvs$modSel]]
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    if (input$predThresh != 'noThresh') {
      # find predicted values for occurrences for selected model
      # extract the suitability values for all occurrences
      occs.xy <- rvs$occs[c('longitude', 'latitude')]
      occPredVals <- raster::extract(pred, occs.xy)
      # get the chosen threshold value
      x <- thresh(occPredVals, input$predThresh)
      # threshold model prediction
      pred <- pred > x
      # rename
      names(pred) <- paste0(rvs$modSel, '_thresh_', input$predThresh)
      rvs %>% writeLog(input$predThresh, 'threshold selected: value =', 
                       round(x, digits = 3), '.')
    }
    
    return(pred)
  })
}
