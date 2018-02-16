
removeByID_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("removeID"), label="Enter the record ID to be removed", value = 0)
  )
}

removeByID_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.rem <- c2_removeByID(occs(), input$removeID, logs, shiny = TRUE)
    if (is.null(occs.rem)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occData$occs <- occs.rem
    
    # RMD VALUES ####
    # add to vector of IDs removed
    if(is.null(spp[[curSp()]]$rmd$c2)) {
      spp[[curSp()]]$rmd$c2 <- list(removedIDs = input$removeID)
    }else{
      spp[[curSp()]]$rmd$c2$removedIDs <- c(spp[[curSp()]]$rmd$c2$removedIDs, input$removeID)
    }
    
    return(occs.rem)
  })
}
