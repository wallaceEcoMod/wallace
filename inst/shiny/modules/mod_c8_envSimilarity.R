envSimilarity_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

envSimilarity_MOD <- function(input, output, session) {
  
  reactive({
    # ERRORS ####
    if (is.null(mapProj())) {
      shinyLogs %>% writeLog(type = 'error', 'Project to new area or time first.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. Please 
                                    use the draw toolbar on the left-hand of the map to complete the polygon.")
      return()
    }
    
    # FUNCTION CALL ####
    projYr <- spp[[curSp()]]$rmm$data$transfer$environment1$yearMax
    time <- ifelse(projYr == "1990", "present-day", projYr)
    mss <- c8_mess(occs(), 
                   bg(), 
                   bgMask(), 
                   spp[[curSp()]]$project$pjEnvs, 
                   time, 
                   shinyLogs)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$project$mess <- mss
    spp[[curSp()]]$project$messVals <- getRasterVals(mss)
  })
}

envSimilarity_INFO <- infoGenerator(modName = "Calculate Environmental Similarity",
                                    modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                    pkgName = "dismo")
