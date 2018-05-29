projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshhold'), label = "Set threshold",
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
    rasName <- paste0(curModel(), '_thresh_', predType)
    projArea.thr  <- threshPred(occs(), 
                                projArea, 
                                rmm()$output$prediction$thresholdRule, 
                                rasName)
    
    # save to spp
    spp[[curSp()]]$project$mapProj <- projArea.thr
    spp[[curSp()]]$project$mapProjVals <- getVals(projArea.thr, predType)
    
    # METADATA
    spp[[curSp()]]$rmm$output$transfer <- NULL
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
    spp[[curSp()]]$rmm$output$project$thresholdRule <- input$threshhold
  })
}

projectArea_INFO <- infoGenerator(modName = "Project to New Extent",
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                  pkgName = "dismo")
