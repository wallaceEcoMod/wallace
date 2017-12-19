
removeByID_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("removeID"), label="Enter the record ID to be removed", value = 0)
  )
}

removeByID_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.rem <- c2_removeByID(vals$occs, input$removeID, logs, shiny = TRUE)
    
    if (is.null(occs.rem)) return()
    
    # RMD VALUES ####
    # add to vector of IDs removed
    rmd$c2$removedIDs <- c(rmd$c2$removedIDs, input$removeID)
    
    # METADATA ####
    #
    
    # MAPPING - blue pts for remove, red pts for keep
    map %>%
      clearMarkers() %>%
      map_plotLocs(occs.rem) %>%
      zoom2Occs(occs.rem)
    
    return(occs.rem)
  })
}
