projectDraw_UI <- function(id) {
  ns <- NS(id)
  tagList(
    "Draw a polygon and select buffer distance(**)", br(), br(),
    tags$div(
      title = 'Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
      numericInput(ns("drawPjBuf"), label = "Study region buffer distance (degree)",
                   value = 0, min = 0, step = 0.5
      )
    ),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

projectDraw_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in component 7 before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>%
        writeLog(
          type = 'error',
          paste0("The polygon has not been drawn and finished. Please use the draw ",
                 "toolbar on the left-hand of the map to complete the polygon."))
      return()
    }
    # FUNCTION CALL ####
    drawPjExt <- c8_projectDraw(spp[[curSp()]]$polyPjXY,
                                spp[[curSp()]]$polyPjID,
                                input$drawPjBuf,
                                shinyLogs)
    if (input$drawPjBuf == 0 ) {
      shinyLogs %>% writeLog(em(spName(occs())), ' : Draw polygon without buffer(**).')
    } else {
      shinyLogs %>% writeLog(em(spName(occs())), ' : Draw polygon with buffer of ',
                             input$drawPjBuf, ' degrees (**).')
    }

    # loop over all species if batch is on
    if (input$batch == TRUE)
      spLoop <- allSp()
    else
      spLoop <- curSp()

    # PROCESSING ####
    for (sp in spLoop) {
      # LOAD INTO SPP ####
      spp[[curSp()]]$project$pjExt <- drawPjExt

      # METADATA ####
      polyX <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyPjXY[, 2], digits = 4))
      spp[[curSp()]]$rmm$code$wallaceSettings$drawExtPolyPjCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
    }
  })
}

projectDraw_MAP <- function(map, session) {
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
  req(spp[[curSp()]]$project$pjExt)
  polyPjXY <- spp[[curSp()]]$project$pjExt@polygons[[1]]@Polygons
  if(length(polyPjXY) == 1) {
    shp <- polyPjXY[[1]]@coords
  } else {
    shp <- lapply(polyPjXY, function(x) x@coords)
  }
  map %>% clearAll() %>%
    addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "gray",
                group = 'bgShp')

}

projectDraw_INFO <-
  infoGenerator(modName = "Draw-specified Study Region(**)",
                modAuts = paste0("Gonzalo E. Pinilla-Buitrago, Jamie M. Kass, ",
                                 "Bruno Vilela, Robert P. Anderson (**)"),
                pkgName = NULL)
