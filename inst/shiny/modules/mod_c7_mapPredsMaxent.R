
mapPredsMaxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('predType'), label = "Prediction output",
                choices = list("raw", "logistic", "cloglog"),
                selected = "raw"),
    shinyBS::bsPopover(ns('predType'), title = 'Tip',
                       'Briefly, raw is "relative occurrence rate" where sum of 
                       pixel values is 1, and logistic is interpreted as "probability 
                       of presence" where raw values are converted to a value between 
                       0 and 1, with the main assumption that species prevalence is 0.5 
                       (see guidance).',
                       placement = 'right', options = list(container = "body")),
    threshPred_UI(ns('threshPred'))
  )
}

mapPredsMaxent_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$mods)) {
      rvs %>% writeLog(type = 'error', "Models must first be run in component 6.")
      return()
    }
    
    # record for RMD
    rvs$comp7.type <- input$predType
    rvs$comp7 <- c(rvs$comp7, 'map')
    
    # initially pick raw prediction
    predSel <- rvs$modPreds[[rvs$modSel]]
    names(predSel) <- paste0(rvs$modSel, '_raw')
    
    # argument for predict function
    pargs <- paste0("outputformat=", rvs$comp7.type)
    
    if (input$predType == 'logistic') {
      # Generate logistic predictions for each model
      if (is.null(rvs$modPredsLog)) {
        withProgress(message = "Generating logistic predictions...", {
          logPredsList <- sapply(rvs$mods, function(x) dismo::predict(x, rvs$bgMsk, args=pargs))
          rvs$modPredsLog <- raster::stack(logPredsList)
          names(rvs$modPredsLog) <- names(rvs$modPreds)
        })  
      }
      predSel <- rvs$modPredsLog[[rvs$modSel]]
      names(predSel) <- paste0(rvs$modSel, '_log')
    } else if (input$predType == 'cloglog') {
      # Generate cloglog predictions for each model
      if (is.null(rvs$modPredsCLL)) {
        withProgress(message = "Generating cloglog predictions...", {
          cllPredsList <- sapply(rvs$mods, function(x) dismo::predict(x, rvs$bgMsk, args=pargs))
          rvs$modPredsCLL <- raster::stack(cllPredsList)
          names(rvs$modPredsCLL) <- names(rvs$modPreds)
        })  
      }
      predSel <- rvs$modPredsCLL[[rvs$modSel]]
      names(predSel) <- paste0(rvs$modSel, '_cll')
    }
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    predSel.thr.call <- callModule(threshPred_MOD, "threshPred", predSel)
    predSel.thr <- predSel.thr.call()
    
    return(predSel.thr)
  })
}
