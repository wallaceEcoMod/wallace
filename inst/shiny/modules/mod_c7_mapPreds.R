
mapPreds_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('predType'), label = "Prediction output",
                choices = list("raw" = 'raw', "logistic" = 'log'),
                selected = 'raw'),
    shinyBS::bsPopover(ns('predType'), title = 'Tip',
                       'Briefly, raw is "relative occurrence rate" where sum of 
                       pixel values is 1, and logistic is interpreted as "probability 
                       of presence" where raw values are converted to a value between 
                       0 and 1, with the main assumption that species prevalence is 0.5 
                       (see guidance).',
                       placement = 'right', options = list(container = "body")),
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
    rvs$comp7.type <- input$predType
    rvs$comp7.thr <- input$predThresh
    rvs$comp7 <- c(rvs$comp7, 'map')
    
    # initially pick raw prediction (if Maxent)
    predSel <- rvs$modPreds[[rvs$modSel]]
    names(predSel) <- paste0(rvs$modSel, '_raw')
    
    # generate binary prediction based on selected thresholding rule 
    # (same for raw and logistic Maxent predictions because they scale the same)
    if (input$predThresh != 'noThresh') {
      occValsSel <- rvs$modOccVals[,rvs$modSel]
      x <- thresh(occValsSel, input$predThresh)
      # revent to raw prediction
      predSel <- predSel > x
      names(predSel) <- paste0(rvs$modSel, '_thresh_', input$predThresh)
    }
    
    # if no threshold selected and type is log, plot Maxent logisitic prediction
    if (input$predType == 'log' & input$predThresh == 'noThresh') {
      if (is.null(rvs$modPredsLog)) {
        rvs %>% writeLog(type = 'error', 'No logistic predictions found. 
                         These are only applicable for Maxent models.')
        return()
      }
      predSel <- rvs$modPredsLog[[rvs$modSel]]
      names(predSel) <- paste0(rvs$modSel, '_log')
    }
    
    rvs$predThresh <- input$predThresh
    rvs$predType <- input$predType
    
    return(predSel)
  })
}
