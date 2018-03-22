bioclim_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("bioclimAllSp"), label = "Batch for all species?", value = TRUE)
  )
}

bioclim_MOD <- function(input, output, session) {
  reactive({
    
    # ERRORS ####
    if (is.null(spp[[i]]$occs$grp)) {
      logs %>% writeLog(type = 'error', "Before building a model, please partition 
                        occurrences for cross-validation.")
      return()
    }
    
    if(input$bioclimAllSp == TRUE) {
      spVec <- allSp()
    }else{
      spVec <- curSp()
    }
    
    for(i in spVec) {  
      # FUNCTION CALL ####
      m.bioclim <- c6_bioclim(spp[[i]]$occs, 
                              spp[[i]]$bg, 
                              spp[[i]]$occs$grp, 
                              spp[[i]]$bg$grp, 
                              spp[[i]]$procEnvs$bgMask, 
                              logs, shiny = TRUE)
      
      req(m.bioclim)
      
      # LOAD INTO SPP ####
      spp[[i]]$mod <- m.bioclim
      
      # METADATA ####
      spp[[i]]$rmm$model$algorithm <- "BIOCLIM"
      spp[[i]]$rmm$model$bioclim$notes <- "dismo package implementation"
    }
  })
}
# occVals <- raster::extract(e$predictions, values$modParams$occ.pts)
# 
# values$mtps <- min(occVals)  # apply minimum training presence threshold
# 
# # Define 10% training presence threshold
# if (length(occVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
#   n90 <- floor(length(occVals) * 0.9)
# } else {  # if greater than or equal to 10 occ values, round up
#   n90 <- ceiling(length(occVals) * 0.9)
# }
# 
# values$p10s <- rev(sort(occVals))[n90]  # apply 10% training presence threshold