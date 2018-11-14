projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. 
             Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'none',
                                        "Minimum Training Presence" = 'mtp', 
                                        "10 Percentile Training Presence" = 'p10')))
  )
}

projectArea_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                             before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', "The polygon has not been drawn and
                              finished. Please use the draw toolbar on the 
                              left-hand of the map to complete the polygon.")
      return()
    }
    
    # FUNCTION CALL ####
    predType <- rmm()$output$prediction$notes
    projArea.out <- c8_projectArea(results(), curModel(), envs(), predType, spp[[curSp()]]$polyPjXY, 
                                   spp[[curSp()]]$polyPjID, shinyLogs)
    
    projExt <- projArea.out$projExt
    projArea <- projArea.out$projArea
    
    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    if(!(input$threshold == 'none')) {
      # use threshold from present-day model training area
      thr <- spp[[curSp()]]$visualization$thresholds[[input$threshold]]
      projAreaThr <- projArea > thr
      shinyLogs %>% writeLog("Projection of model to new area for ", curSp(), 
                             ' with threshold ', input$threshold, ': ', thr, '.')
    } else {
      projAreaThr <- projArea
      shinyLogs %>% writeLog("Projection of model to new area for ", curSp(), 
                             ' with ', predType, ' output.')
    }
    # rename
    names(projAreaThr) <- paste0(curModel(), '_thresh_', predType)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projAreaThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projAreaThr, predType)
    
    # METADATA ####
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <- printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <- printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- 1960
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- 1990
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <- paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <- printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <- "project to user-selected new area"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"
    
    spp[[curSp()]]$rmm$output$transfer$environment1$units <- ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$output$transfer$environment1$minVal <- printVecAsis(raster::cellStats(projAreaThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$output$transfer$environment1$maxVal <- printVecAsis(raster::cellStats(projAreaThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$transfer$environment1$thresholdRule <- input$threshold
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
  })
}

projectArea_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  req(results())
  map %>% leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                         rectangleOptions = FALSE, circleOptions = FALSE,
                                         markerOptions = FALSE, circleMarkerOptions = FALSE,
                                         editOptions = leaflet.extras::editToolbarOptions())
  req(spp[[curSp()]]$polyPjXY, spp[[curSp()]]$project)  
  polyPjXY <- spp[[curSp()]]$polyPjXY
  mapProjVals <- spp[[curSp()]]$project$mapProjVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if no threshold specified
  if(rmm()$output$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'blue')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", colors = c('gray', 'blue'), title = "Thresholded Suitability<br>(Projected)",
                labels = c("predicted absence", "predicted presence"), opacity = 1, layerId = 'proj')
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapProjVals, na.color='transparent')
    rasPal <- colorNumeric(rasCols, mapProjVals, na.color='transparent')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", pal = legendPal, title = "Predicted Suitability<br>(Projected)",
                values = mapProjVals, layerId = 'proj', labFormat = reverseLabels(2, reverse_order=TRUE))
    
  }
  # map model prediction raster and projection polygon
  sharedExt <- rbind(polyPjXY, occs()[c("longitude", "latitude")])
  map %>% 
    clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    map_occs(occs(), customZoom = sharedExt) %>%
    addRasterImage(mapProj(), colors = rasPal, opacity = 0.7,
                   layerId = 'projRas', group = 'proj', method = "ngb") %>%
    addPolygons(lng=polyPjXY[,1], lat=polyPjXY[,2], layerId="projExt", fill = FALSE,
                weight=4, color="blue", group='proj') %>%
    # add background polygon
    mapBgPolys(bgShpXY())
}

projectArea_INFO <- infoGenerator(modName = "Project to New Extent",
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                  pkgName = "dismo")
