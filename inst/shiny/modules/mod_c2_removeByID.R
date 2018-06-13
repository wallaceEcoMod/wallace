
removeByID_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("removeID"), label="Enter the record ID to be removed", value = 0)
  )
}

removeByID_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.rem <- c2_removeByID(occs(), 
                              input$removeID, 
                              shinyLogs)
    req(occs.rem)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.rem
    
    # METADATA ####
    # if no removeIDs are recorded yet, make a list to record them
    # if at least one exists, add to the list
    if(is.null(rmm()$code$wallaceSettings$removedIDs)) {
      spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <- input$removeID
    } else {
      spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <- c(rmm()$code$wallaceSettings$removedIDs, input$removeID)
    }
    
    return(occs.rem)
  })
}

removeByID_MAP <- function(map) {
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs()) %>%
    leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
}

removeByID_INFO <- infoGenerator(modName = "Remove Occurrences By ID",
                                 modAuts = "Jamie M. Kass, Robert P. Anderson",
                                 pkgName = NULL)



