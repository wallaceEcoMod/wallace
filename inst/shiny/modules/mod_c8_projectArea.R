projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    threshPred_UI(ns('threshPred'))
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
    
    outputType <- spp[[curSp()]]$rmm$output$prediction$notes
    
    projArea <- c8_projectArea(results(),
                               curModel(), 
                               envs(),
                               outputType,
                               spp[[curSp()]]$polyPjXY,
                               spp[[curSp()]]$polyPjID,
                               shinyLogs)
    
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    projArea.thr.call <- callModule(threshPred_MOD, "threshPred", projArea)
    projArea.thr <- projArea.thr.call()
    
    # save to spp
    spp[[curSp()]]$project$mapProj <- projArea.thr
    spp[[curSp()]]$project$mapProjVals <- getVals(projArea.thr, outputType)
    
    # METADATA
    spp[[curSp()]]$rmm$output$transfer <- NULL
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
    spp[[curSp()]]$rmm$output$project$thresholdRule <- input$threshPred
  })
}

projectArea_INFO <- infoGenerator(modName = "Project to New Extent",
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                  pkgName = "dismo")
