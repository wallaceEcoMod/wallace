
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', 
                                "Minimum convex polygon" = 'mcp',
                                "Point buffers" = 'ptbuf')),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 0, min = 0, step = 0.5)),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

bgExtent_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(envs())) {
      shinyLogs %>% writeLog(type = 'error',
                             'Environmental variables missing. Obtain them in component 3.')
      return()
    }
    req(curSp(), occs(), envs())
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    for(sp in spLoop) {
      # FUNCTION CALL ####
      bgExt <- c4_bgExtent(spp[[sp]]$occs, 
                           spp[[sp]]$envs, 
                           input$bgSel, 
                           input$bgBuf, 
                           shinyLogs)  
      req(bgExt)
      
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- bgExt
      
      # METADATA ####
      spp[[sp]]$rmm$model$maxent$backgroundSizeRule <- paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')
    }
  })
}

bgExtent_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  if(is.null(bgExt())) {
    map %>% clearAll() %>%     
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
  }else{
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
    for(shp in bgShpXY()) {
      map %>%
        addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')
    }
    bb <- bgExt()@bbox
    map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
  }
}

bgExtent_INFO <- infoGenerator(modName = "Select Study Region",
                               modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                               pkgName = c("sp", "rgeos"))
