
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', 
                                "Minimum convex polygon" = 'mcp',
                                "Point buffers" = 'ptbuf')),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 0.5, min = 0, step = 0.5))
  )
}

bgExtent_MOD <- function(input, output, session) {
  reactive({
    
    req(curSp(), spp[[curSp()]]$occs, spp[[curSp()]]$envs)
    
    # observeEvent(input$bgSel, {
      # FUNCTION CALL ####
      bgExt <- c4_bgExtent(spp[[curSp()]]$occs, 
                           spp[[curSp()]]$envs, 
                           input$bgSel, 
                           input$bgBuf, 
                           logs, shiny=TRUE)  
      print("observe event")
      
      req(bgExt)
      
      # LOAD INTO SPP ####
      spp[[curSp()]]$procEnvs$bgExt <- bgExt
      
      # METADATA ####
      spp[[curSp()]]$rmm$model$maxent$backgroundSizeRule <- paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')
      
      # MAPPING ####
      map %>% map_occs(spp[[curSp()]]$occs)
      for (shp in bgShpXY()) {
        map %>%
          addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')  
      }
      bb <- spp[[curSp()]]$procEnvs$bgExt@bbox
      map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])  
    # })
    
    
    # RETURN ####
    # output the species name
    # return(bgExt)
  })
}
