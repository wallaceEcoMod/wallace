
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', 
                                "Minimum convex polygon" = 'mcp',
                                "Point buffers" = 'ptbuf')),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 0.5, min = 0, step = 0.5),
             checkboxInput(ns("bgExtAllSp"), label = "Batch for all species?", value = TRUE))
  )
}

bgExtent_MOD <- function(input, output, session) {
  reactive({
    
    req(curSp(), spp[[curSp()]]$occs, spp[[curSp()]]$envs)
    
    if(input$bgExtAllSp == TRUE) {
      spVec <- allSp()
    }else{
      spVec <- curSp()
    }
    
    for(i in spVec) {
      # FUNCTION CALL ####
      bgExt <- c4_bgExtent(spp[[i]]$occs, 
                           spp[[i]]$envs, 
                           input$bgSel, 
                           input$bgBuf, 
                           logs, shiny=TRUE)  
      req(bgExt)
      
      # LOAD INTO SPP ####
      spp[[i]]$procEnvs$bgExt <- bgExt
      
      # METADATA ####
      spp[[i]]$rmm$model$maxent$backgroundSizeRule <- paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')
      
      # # MAPPING ####
      # map %>% map_occs(spp[[i]]$occs)
      # for (shp in bgShpXY()) {
      #   map %>%
      #     addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')  
      # }
      # bb <- spp[[i]]$procEnvs$bgExt@bbox
      # map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])  
    }
  })
}
