
thinOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # tags$div(checkboxInput(ns("spThinAllSp"), label = "Batch for all species?", value = TRUE),
    tags$div(
      title='The minimum distance between occurrence locations (nearest neighbor distance) in km for resulting thinned dataset. Ideally based on species biology (e.g., home-range size).',
      numericInput(ns("thinDist"), label = "Thinning distance (km)", value = 0),
      checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE))
  )
}

thinOccs_MOD <- function(input, output, session) {
  reactive({
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    for(sp in spLoop) {
      # FUNCTION CALL ####
      occs.thin <- c2_thinOccs(spp[[sp]]$occs,
                               input$thinDist,
                               shinyLogs)
      req(occs.thin)
      
      # LOAD INTO SPP ####
      # record present occs before thinning (this may be different from occData$occOrig)
      spp[[sp]]$procOccs$occsPreThin <- spp[[sp]]$occs
      spp[[sp]]$occs <- occs.thin
      spp[[sp]]$procOccs$occsThin <- occs.thin
      
      # METADATA ####
      # perhaps there should be a thinDist metadata field?
      spp[[sp]]$rmm$code$wallaceSettings$thinDistKM <- input$thinDist  
    }
  })
}

thinOccs_MAP <- function(map, session) {
  # if you've thinned already, map thinned points blue
  # and kept points red
  if(!is.null(spp[[curSp()]]$procOccs$occsThin)) {
    
    occs.preThin <- spp[[curSp()]]$procOccs$occsPreThin
    map %>% clearAll() %>% 
      addCircleMarkers(data = occs.preThin, lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "blue", 
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      addLegend("bottomright", colors = c('red', 'blue'), title = "Occ Records", 
                labels = c('retained', 'removed'), opacity = 1)  
  } else {
    # if you haven't thinned, map all points red
    map %>% clearAll() %>% 
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
  }
}



thinOccs_INFO <- infoGenerator(modName = "Spatial Thin",
                               modAuts = "Jamie M. Kass, Matthew E. Aiello-Lammens, Robert P. Anderson",
                               pkgName = "spThin")