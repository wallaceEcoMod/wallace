selectOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

selectOccs_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.sel <- c2_selectOccs(spp[[curSp()]]$occs, 
                              spp[[curSp()]]$polySelXY,
                              spp[[curSp()]]$polySelID, 
                              logs, shiny = TRUE)
    req(occs.sel)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.sel
    
    # METADATA ####
    polyX <- printVecAsis(round(spp[[curSp()]]$polySelXY[,1], digits=4))
    polyY <- printVecAsis(round(spp[[curSp()]]$polySelXY[,2], digits=4))
    spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords <- paste0('X: ', polyX, ', Y: ', polyY)
    
    return(occs.sel)
  })
}

selectOccs_INFO <- infoGenerator(modName = "Select Occurrences On Map",
                              modAuts = "Jamie M. Kass, Robert P. Anderson",
                              pkgName = "leaflet.extras")
