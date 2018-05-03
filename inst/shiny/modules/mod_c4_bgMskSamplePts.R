
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points", value = 10000, min = 1, step = 1)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session) {
  reactive({
    
    req(bgExt())
    
    for(sp in spIn()) {
      # FUNCTION CALL ####
      bgMask <- c4_bgMask(spp[[sp]]$occs, 
                          spp[[sp]]$envs, 
                          spp[[sp]]$procEnvs$bgExt, 
                          shinyLogs)
      req(bgMask)
      bgPts <- c4_bgSample(spp[[sp]]$occs, 
                           bgMask, 
                           input$bgPtsNum, 
                           shinyLogs)
      req(bgPts)
      withProgress(message = paste0("Extracting background values for ", spName(spp[[sp]]), "..."), {
        bgEnvsVals <- as.data.frame(raster::extract(spp[[sp]]$envs, bgPts))
      })
      
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgMask <- bgMask
      # add columns for env variables beginning with "envs_" to bg tbl
      spp[[sp]]$bg <- cbind(taxon_name = paste0("bg_", spName(spp[[sp]])), bgPts, 
                           occID = NA, year = NA, institution_code = NA, country = NA, 
                           state_province = NA, locality = NA, elevation = NA, 
                           record_type = NA, bgEnvsVals)
      
      # METADATA ####
      spp[[sp]]$rmm$model$maxent$backgroundSizeSet <- input$bgPtsNum
    }
  })
}

bgMskSamplePts_INFO <- infoGenerator(modName = "Sample Background Points",
                                     modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                                     pkgName = NULL)
