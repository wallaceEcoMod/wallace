envSimilarity_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

envSimilarity_MOD <- function(input, output, session) {
  
  reactive({
    # ERRORS ####
    if (is.null(mapProj())) {
      shinyLogs %>% writeLog(type = 'error', 'Project to new area or time first.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. Please 
                                    use the draw toolbar on the left-hand of the map to complete the polygon.")
      return()
    }
    
    # FUNCTION CALL ####
    projYr <- spp[[curSp()]]$rmm$data$transfer$environment1$yearMax
    time <- ifelse(projYr == "1990", "present-day", projYr)
    mss <- c8_mess(occs(), bg(), bgMask(), spp[[curSp()]]$project$pjEnvs, time, shinyLogs)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$project$mess <- mss
    spp[[curSp()]]$project$messVals <- getRasterVals(mss)
  })
}

envSimilarity_MAP <- function(map, session) {
  req(spp[[curSp()]]$project$mess)
  mess <- spp[[curSp()]]$project$mess
  mapProjVals <- spp[[curSp()]]$project$messVals
  rasCols <- RColorBrewer::brewer.pal(n=11, name='Reds')
  legendPal <- colorNumeric(rev(rasCols), mapProjVals, na.color='transparent')
  rasPal <- colorNumeric(rasCols, mapProjVals, na.color='transparent')
  map %>% removeControl("proj") %>%
    addLegend("bottomright", pal = legendPal, title = "MESS Values",
              values = mapProjVals, layerId = 'proj', labFormat = reverseLabels(2, reverse_order=TRUE))
  # map model prediction raster and projection polygon
  sharedExt <- rbind(polyPjXY, occs()[c("longitude", "latitude")])
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    map_occs(occs(), customZoom = sharedExt) %>%
    addRasterImage(mess, colors = rasPal, opacity = 0.7,
                   layerId = 'projRas', group = 'proj', method = "ngb") %>%
    addPolygons(lng = polyPjXY[,1], lat = polyPjXY[,2], layerId = "projExt", 
                fill = FALSE, weight = 4, color = "red", group = 'proj') %>%
    # add background polygon
    mapBgPolys(bgShpXY())
}

envSimilarity_INFO <- infoGenerator(modName = "Calculate Environmental Similarity",
                                    modAuts = "Jamie M. Kass, Bruno Vilela, 
                                    Gonzalo E. Pinilla-Buitrago, Robert P. 
                                    Anderson", 
                                    pkgName = "dismo")
