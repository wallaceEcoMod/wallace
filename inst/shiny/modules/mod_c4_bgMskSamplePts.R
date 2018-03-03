
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points", value = 10000, min = 1, step = 1)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session, rvs) {
  reactive({
    # FUNCTION CALL ####
    bgMask <- c4_bgMask(spp[[curSp()]]$occs, 
                        spp[[curSp()]]$envs, 
                        spp[[curSp()]]$procEnvs$bgExt, 
                        logs, shiny = TRUE)
    req(bgMask)
    bgPts <- c4_bgSample(spp[[curSp()]]$occs, 
                         bgMask, 
                         input$bgPtsNum, 
                         logs, shiny = TRUE)
    req(bgPts)
    withProgress(message = "Extracting values...", {
      bgEnvsVals <- raster::extract(spp[[curSp()]]$envs, bgPts)
    })
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$procEnvs$bgMask <- bgMask
    spp[[curSp()]]$procEnvs$bgPts <- bgPts
    spp[[curSp()]]$procEnvs$bg.envsVals <- bgEnvsVals
    
    # RMD VALUES ####
    spp[[curSp()]]$rmm$model$maxent$backgroundSizeSet <- input$bgPtsNum
    
    # RETURN ####
    # output the species name
    # return(bgExt)
  })
}
