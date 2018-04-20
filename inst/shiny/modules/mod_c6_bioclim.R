bioclim_UI <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

bioclim_MOD <- function(input, output, session) {
  reactive({
    
    for(sp in spIn()) {
      # ERRORS ####
      if (is.null(spp[[sp]]$occs$partition)) {
        logs %>% writeLog(type = 'error', "Before building a model, please partition 
                          occurrences for cross-validation for", spName(spp[[sp]]), ".")
        return()
      }
      
      # FUNCTION CALL ####
      m.bioclim <- c6_bioclim(spp[[sp]]$occs, 
                              spp[[sp]]$bg, 
                              spp[[sp]]$procEnvs$bgMask, 
                              logs, shiny = TRUE)
      
      req(m.bioclim)
      
      # LOAD INTO SPP ####
      spp[[sp]]$modelList <- m.bioclim
      
      # METADATA ####
      spp[[sp]]$rmm$model$algorithm <- "BIOCLIM"
      spp[[sp]]$rmm$model$bioclim$notes <- "dismo package implementation"
    }
  })
}

bioclim_INFO <- infoGenerator(modName = "BIOCLIM",
                              modAuts = "Jamie M. Kass, Robert Muscarella, Bruno Vilela, Robert P. Anderson",
                              pkgName = c("ENMeval", "dismo"))
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