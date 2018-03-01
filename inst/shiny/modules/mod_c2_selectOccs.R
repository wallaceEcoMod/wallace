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
    if (is.null(occs.sel)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.sel
    
    # METADATA ####
    # if no removeIDs are recorded yet, make a list to record them
    # if at least one exists, add to the list
    spp[[curSp()]]$rmm$code$wallaceSettings$polySelXY <- spp[[curSp()]]$polySelXY
    
    return(occs.sel)
  })
}
