
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points", value = 10000, min = 1, step = 1), # Check default (value = 10000)
    checkboxInput(ns("batch"), label = strong("Batch"), value = T) # Check default (value = FALSE)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session) {
  reactive({
    # WARNING ####
    if (input$bgPtsNum < 1) {
      shinyLogs %>% writeLog(type = 'warning', "Enter a non-zero number of background points.")
      return()
    }
    
    req(bgExt())
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      # FUNCTION CALL ####
      bgMask <- c4_bgMask(spp[[sp]]$occs, 
                          envs.global[[spp[[sp]]$envs]],
                          spp[[sp]]$procEnvs$bgExt, 
                          shinyLogs)
      req(bgMask)
      bgPts <- c4_bgSample(spp[[sp]]$occs, 
                           bgMask, 
                           input$bgPtsNum, 
                           shinyLogs)
      req(bgPts)
      withProgress(message = paste0("Extracting background values for ", spName(sp), "..."), {
        bgEnvsVals <- as.data.frame(raster::extract(bgMask, bgPts))
      })
      
      if(sum(rowSums(is.na(raster::extract(bgMask, spp[[sp]]$occs[,c("longitude", "latitude")])))) > 0) {
        shinyLogs %>% writeLog(type = "error", "One or more occurrence points have NULL raster values for ", 
                               spName(sp), ". This can sometimes happen for points on the margin of the study extent.",
                               " Please increase the buffer slightly to include them.")
        return()
      }
      
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgMask <- bgMask
      # add columns for env variables beginning with "envs_" to bg tbl
      spp[[sp]]$bg <- cbind(scientific_name = paste0("bg_", spName(spp[[sp]])), bgPts, 
                           occID = NA, year = NA, institution_code = NA, country = NA, 
                           state_province = NA, locality = NA, elevation = NA, 
                           record_type = NA, bgEnvsVals)
      # sample background points
      spp[[sp]]$bgPts <- bgPts
      
      # METADATA ####
      spp[[sp]]$rmm$model$maxent$backgroundSizeSet <- input$bgPtsNum
    }
  })
}

bgMskSamplePts_INFO <- infoGenerator(modName = "Sample Background Points",
                                     modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                                     pkgName = NULL)
