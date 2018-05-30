projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'noThresh',
                                        "Minimum Training Presence" = 'mtp', 
                                        "10 Percentile Training Presence" = 'p10')))
  )
}

projectArea_MOD <- function(input, output, session) {
  reactive({
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                             before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. 
                             Please use the draw toolbar on the left-hand of the map to complete
                             the polygon.")
      return()
    }
    
    predType <- rmm()$output$prediction$notes
    
    projArea <- c8_projectArea(results(),
                               curModel(), 
                               envs(),
                               predType,
                               spp[[curSp()]]$polyPjXY,
                               spp[[curSp()]]$polyPjID,
                               shinyLogs)
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    
    if(!(input$threshold == 'noThresh')) {
      # use threshold from present-day model training area
      thr <- spp[[curSp()]]$visualization$thresholds[[input$threshold]]
      projAreaThr <- projArea > thr
      shinyLogs %>% writeLog("Projection of model to new area for", curSp(), 'with threshold', input$threshold, ': ', thr, '.')
    } else {
      projAreaThr <- projArea
      shinyLogs %>% writeLog("Projection of model to new area for", curSp(), 'with', predType, 'output.')
    }
    
    # rename
    names(projAreaThr) <- paste0(curModel(), '_thresh_', predType)
    
    # save to spp
    spp[[curSp()]]$project$mapProj <- projAreaThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projAreaThr, predType)
    
    # METADATA
    spp[[curSp()]]$rmm$output$transfer <- NULL
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
    spp[[curSp()]]$rmm$output$project$thresholdRule <- input$threshold
  })
}

projectArea_INFO <- infoGenerator(modName = "Project to New Extent",
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                  pkgName = "dismo")
