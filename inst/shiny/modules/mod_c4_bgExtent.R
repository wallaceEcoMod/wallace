
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("bounding box", 
                                "minimum convex polygon",
                                "point buffers")),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 1, min = 0, step = 0.5)), # Check default (value = 0)
    checkboxInput(ns("batch"), label = strong("Batch"), value = T) # Check default (value = FALSE)
  )
}

bgExtent_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if(is.null(envs())) {
      shinyLogs %>% writeLog(type = 'error',
                             paste0('Environmental variables missing for ', 
                                    curSp(), '. Obtain them in component 3.'))
      return()
    }
    req(curSp(), occs())
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    for(sp in spLoop) {
      # FUNCTION CALL ####
      bgExt <- c4_bgExtent(spp[[sp]]$occs, 
                           input$bgSel, 
                           input$bgBuf, 
                           shinyLogs)  
      req(bgExt)
      
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- bgExt
      
      # METADATA ####
      spp[[sp]]$rmm$model$maxent$backgroundSizeRule <- paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')
      spp[[sp]]$rmm$wallaceSettings$bgSel <- input$bgSel
      spp[[sp]]$rmm$wallaceSettings$bgBuf <- input$bgBuf
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

bgExtent_RMD <- function(sp) {
  list(bgSel = spp[[sp]]$rmm$wallaceSettings$bgSel,
       bgBuf = spp[[sp]]$rmm$wallaceSettings$bgBuf)
}
