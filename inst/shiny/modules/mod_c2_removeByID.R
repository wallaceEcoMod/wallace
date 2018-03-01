
removeByID_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("removeID"), label="Enter the record ID to be removed", value = 0)
  )
}

removeByID_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.rem <- c2_removeByID(spp[[curSp()]]$occs, 
                              input$removeID, 
                              logs, shiny = TRUE)
    if (is.null(occs.rem)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.rem
    
    # METADATA ####
    # if no removeIDs are recorded yet, make a list to record them
    # if at least one exists, add to the list
    if(is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs)) {
      spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <- list(removedIDs = input$removeID)
    } else {
      spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <- c(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs, input$removeID)
    }
    
    return(occs.rem)
  })
}
