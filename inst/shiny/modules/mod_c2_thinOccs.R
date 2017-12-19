
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
    occs.thin <- c2_thinOccs(vals$occs, input$thinDist, logs, shiny=TRUE)
    
    if (is.null(occs.thin)) return()
    
    # LOAD vals ####
    vals$thinDist <- input$thinDist
    
    # METADATA ####
    #
    
    # MAPPING - blue pts for remove, red pts for keep
    map %>% 
      addCircleMarkers(data = vals$occs, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'blue',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addCircleMarkers(data = occs.thin, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'red',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addLegend("bottomright", colors = c('red', 'blue'),
                title = "Occ Records", labels = c('retained', 'removed'),
                opacity = 1, layerId = 'leg')
    
    return(occs.thin)
  })
}