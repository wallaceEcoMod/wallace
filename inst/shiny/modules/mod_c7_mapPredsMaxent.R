
mapPredsMaxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Please see guidance for an explanation of different Maxent output types.',
             radioButtons(ns('predType'), label = "Prediction output",
                choices = list("raw", "logistic", "cloglog"), selected = "raw", inline = TRUE)),
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
    pjPred <- predSel.thr$pred
    rvs$comp7.thr <- predSel.thr$thresh
    
    # write to log box
    rvs %>% writeLog("Maxent", input$predType, "model prediction plotted.")
    
    return(pjPred)
  })
}
