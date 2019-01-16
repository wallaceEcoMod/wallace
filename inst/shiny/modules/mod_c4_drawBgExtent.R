drawBgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    "Draw a polygon and select buffer distance(**)", br(), br(),
    tags$div(
      title = 'Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
      numericInput(ns("drawBgBuf"), label = "Study region buffer distance (degree)",
        value = 0, min = 0, step = 0.5
      )
    ),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

drawBgExtent_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(envs())) {
      shinyLogs %>% writeLog(type = 'error',
                             'Environmental variables missing. Obtain them in component 3.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyExtXY)) {
      shinyLogs %>% writeLog(
        type = 'error', 
        "The polygon has not been drawn and finished. Please use the draw toolbar on the left-hand of the map to complete the polygon."
      )
      return()
    }
    # FUNCTION CALL ####
    drawBgExt <- c4_drawBgExtent(spp[[curSp()]]$polyExtXY, 
                                 spp[[curSp()]]$polyExtID,
                                 input$drawBgBuf, 
                                 spp[[curSp()]]$occs,
                                 shinyLogs)
    
    # loop over all species if batch is on
    if (input$batch == TRUE)
      spLoop <- allSp()
    else
      spLoop <- curSp()
    
    # PROCESSING ####
    for (sp in spLoop) {
      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- drawBgExt
      
      # METADATA ####
      polyX <- printVecAsis(round(spp[[curSp()]]$polyExtXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyExtXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallaceSettings$drawExtPolyCoords <- paste0('X: ', polyX, ', Y: ', polyY)
    }
  })
}

drawBgExtent_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw',
    polylineOptions = FALSE,
    rectangleOptions = FALSE,
    circleOptions = FALSE,
    markerOptions = FALSE,
    circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )
  req(spp[[curSp()]]$polyExtXY)
  polyExtXY <- spp[[curSp()]]$polyExtXY
  
  if(is.null(bgExt())) {
    map %>% clearAll() %>%     
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
  } else {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
    for(shp in bgShpXY()) {
      map %>%
        addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')
    }
  }
}

drawBgExtent_INFO <- infoGenerator(modName = "Draw-specified Study Region(**)",
                                   modAuts = "Gonzalo E. Pinilla-Buitrago, Jamie M. Kass, Bruno Vilela, Robert P. Anderson (**)",
                                   pkgName = NULL)