
thinOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(checkboxInput(ns("spThinAllSp"), label = "Batch for all species?", value = TRUE),
             title='The minimum distance between occurrence locations (nearest neighbor distance) in km for resulting thinned dataset. Ideally based on species biology (e.g., home-range size).',
             numericInput(ns("thinDist"), label = "Thinning distance (km)", value = 0))
  )
}

thinOccs_MOD <- function(input, output, session) {
  reactive({
    if(input$spThinAllSp == TRUE) {
      spVec <- allSp()
    }else{
      spVec <- curSp()
    }
    
    for(i in spVec) {
      # FUNCTION CALL ####
      occs.thin <- c2_thinOccs(spp[[i]]$occs, 
                               input$thinDist, 
                               logs, shiny = TRUE)
      req(occs.thin)
      
      # LOAD INTO SPP ####
      # record present occs before thinning (this may be different from occData$occOrig)
      spp[[i]]$procOccs$occsPreThin <- spp[[i]]$occs
      spp[[i]]$occs <- occs.thin
      spp[[i]]$procOccs$occsThin <- occs.thin
      
      # METADATA ####
      # perhaps there should be a thinDist metadata field?
      spp[[i]]$rmm$code$wallaceSettings$thinDistKM <- input$thinDist
    }
    
  })
}