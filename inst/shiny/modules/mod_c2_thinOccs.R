
thinOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='The minimum distance between occurrence locations (nearest neighbor distance) in km for resulting thinned dataset. Ideally based on species biology (e.g., home-range size).',
             numericInput(ns("thinDist"), label = "Thinning distance (km)", value = 0))
  )
}

thinOccs_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs.thin <- c2_thinOccs(input$occs, input$thinDist, logs, shiny=TRUE)
    
    if (is.null(occs.thin)) return()
    
    # LOAD vals ####
    vals$occs <- input$occs
    vals$thinDist <- input$thinDist
    
    # METADATA ####
    #
    return(occs.thin)
  })
}