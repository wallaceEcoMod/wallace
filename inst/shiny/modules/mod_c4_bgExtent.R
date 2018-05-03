
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
    
    for(sp in spIn()) {
      # FUNCTION CALL ####
      bgExt <- c4_bgExtent(spp[[sp]]$occs, 
                           spp[[sp]]$envs, 
                           input$bgSel, 
                           input$bgBuf, 
                           logs)  
      req(bgExt)
      
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- bgExt
      
      # METADATA ####
      spp[[sp]]$rmm$model$maxent$backgroundSizeRule <- paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')
      
      # # MAPPING ####
      # map %>% map_occs(spp[[sp]]$occs)
      # for (shp in bgShpXY()) {
      #   map %>%
      #     addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')  
      # }
      # bb <- spp[[sp]]$procEnvs$bgExt@bbox
      # map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])  
    }
  })
}

bgExtent_INFO <- infoGenerator(modName = "Select Study Region",
                               modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                               pkgName = c("sp", "rgeos"))
