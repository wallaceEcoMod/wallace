
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points", value = 10000, min = 1, step = 1),
    checkboxInput(ns("bgMskAllSp"), label = "Batch for all species?", value = TRUE)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session) {
  reactive({
    
    req(spp[[curSp()]]$procEnvs$bgExt)
    
    if(input$bgMskAllSp == TRUE) {
      spVec <- allSp()
    }else{
      spVec <- curSp()
    }
    
    for(i in spVec) {
      # FUNCTION CALL ####
      bgMask <- c4_bgMask(spp[[i]]$occs, 
                          spp[[i]]$envs, 
                          spp[[i]]$procEnvs$bgExt, 
                          logs, shiny = TRUE)
      req(bgMask)
      bgPts <- c4_bgSample(spp[[i]]$occs, 
                           bgMask, 
                           input$bgPtsNum, 
                           logs, shiny = TRUE)
      req(bgPts)
      withProgress(message = paste0("Extracting background values for ", i, "..."), {
        bgEnvsVals <- as.data.frame(raster::extract(spp[[i]]$envs, bgPts))
        names(bgEnvsVals) <- paste0('env_', names(bgEnvsVals))
      })
      
      # LOAD INTO SPP ####
      spp[[i]]$procEnvs$bgMask <- bgMask
      # add columns for env variables beginning with "envs_" to bg tbl
      spp[[i]]$bg <- cbind(taxon_name = "background", occID = NA, bgPts, record_type = NA, bgEnvsVals)
      print(spp[[i]]$bg)
      
      # METADATA ####
      spp[[i]]$rmm$model$maxent$backgroundSizeSet <- input$bgPtsNum
    }
  })
}
