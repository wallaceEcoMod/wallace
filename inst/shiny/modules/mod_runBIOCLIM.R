runBIOCLIM_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

runBIOCLIM_MOD <- function(input, output, session) {
  reactive({
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    # PROCESSING ####
    for(sp in spLoop) {
      # ERRORS ####
      if(is.null(spp[[sp]]$occs$partition)) {
        shinyLogs %>% writeLog(type = 'error', "Before building a model, please
                               partition occurrences for cross-validation for ",
                               spName(spp[[sp]]), ".")
        return()
      }
      
      # FUNCTION CALL ####
      m.bioclim <- runBIOCLIM(occs = spp[[sp]]$occs, 
                              bg = spp[[sp]]$bg, 
                              occsGrp = spp[[sp]]$occs$partition, 
                              bgGrp = spp[[sp]]$bg$partition, 
                              bgMsk = spp[[sp]]$procEnvs$bgMask, 
                              shinyLogs)
      
      req(m.bioclim)
      
      # LOAD INTO SPP ####
      spp[[sp]]$evalOut <- m.bioclim
      
      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "BIOCLIM"
      spp[[sp]]$rmm$model$bioclim$notes <- "ENMeval/dismo package implementation"
    }
  })
}

runBIOCLIM_INFO <- infoGenerator(modName = "BIOCLIM",
                              modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson",
                              pkgName = c("ENMeval", "dismo"))
