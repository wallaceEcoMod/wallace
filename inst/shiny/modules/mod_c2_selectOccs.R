selectOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

selectOccs_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.sel <- c2_selectOccs(spp[[curSp()]]$occs, spp[[curSp()]]$polySelXY,
                              spp[[curSp()]]$polySelID, logs, shiny = TRUE)
    if (is.null(occs.sel)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.sel
    
    # RMD VALUES ####
    # add to vector of IDs removed
    # if(is.null(spp[[curSp()]]$rmd$c2)) {
    #   spp[[curSp()]]$rmd$c2 <- list(removedIDs = input$removeID)
    # }else{
    #   spp[[curSp()]]$rmd$c2$removedIDs <- c(spp[[curSp()]]$rmd$c2$removedIDs, input$removeID)
    # }
    
    return(occs.sel)
  })
}
