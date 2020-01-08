projectArea_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title =
               paste0('Create binary map of predicted presence/absence assuming ',
                      'all values above threshold value represent presence. Also ',
                      'can be interpreted as a "potential distribution" (see ',
                      'guidance).'),
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'none',
                                        "Minimum Training Presence" = 'mtp',
                                        "10 Percentile Training Presence" = 'p10',
                                        "Quantile of Training Presences" = 'qtp'))),
    conditionalPanel(sprintf("input['%s'] == 'qtp'", ns("threshold")),
                     sliderInput(ns("trainPresQuantile"), "Set quantile",
                                 min = 0, max = 1, value = .05)),
    conditionalPanel(sprintf("input.modelSel == 'Maxent' & input['%s'] == 'none'",
                                         ns("threshold")),
                     h5("Prediction output is the same than Visualize component (**)"))
  )
}

projectArea_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in component 7 before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$project$pjExt)) {
      shinyLogs %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }

    # FUNCTION CALL ####
    predType <- rmm()$output$prediction$notes
    projArea.out <- c8_projectArea(evalOut(), curModel(), envs(),
                                   outputType = predType,
                                   alg = rmm()$model$algorithm,
                                   clamp = rmm()$model$maxent$clamping,
                                   spp[[curSp()]]$project$pjExt,
                                   shinyLogs)

    projExt <- projArea.out$projExt
    projArea <- projArea.out$projArea

    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    occPredVals <- spp[[curSp()]]$visualization$occPredVals

    if(!(input$threshold == 'none')) {
      if (input$threshold == 'mtp') {
        thr <- quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr <- quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr <- quantile(occPredVals, probs = input$trainPresQuantile)
      }
      projAreaThr <- projArea > thr
      shinyLogs %>% writeLog("Projection of model to new area for ", em(spName(curSp())),
                             ' with threshold ', input$threshold, ' (',
                             formatC(thr, format = "e", 2), ').')
    } else {
      projAreaThr <- projArea
      shinyLogs %>% writeLog("Projection of model to new area for ", em(spName(curSp())),
                             ' with ', predType, ' output.')
    }
    raster::crs(projAreaThr) <- raster::crs(envs())
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
  # Reset draw polygon
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw', polylineOptions = FALSE, rectangleOptions = FALSE,
    circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )
  # Add just projection Polygon
  req(spp[[curSp()]]$project$pjExt)
  polyPjXY <- spp[[curSp()]]$project$pjExt@polygons[[1]]@Polygons
  if(length(polyPjXY) == 1) {
    shp <- polyPjXY[[1]]@coords
  } else {
    shp <- lapply(polyPjXY, function(x) x@coords)
  }
  bb <- spp[[curSp()]]$project$pjExt@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>% clearAll() %>% removeImage('projRas') %>%
    addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "red",
                group = 'bgShp') %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  req(evalOut(), spp[[curSp()]]$project$pjEnvs)
  mapProjVals <- spp[[curSp()]]$project$mapProjVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if no threshold specified
  if(rmm()$output$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'red')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", colors = c('gray', 'red'),
                title = "Thresholded Suitability<br>(Projected)",
                labels = c("predicted absence", "predicted presence"),
                opacity = 1, layerId = 'proj')
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapProjVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, mapProjVals, na.color = 'transparent')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", pal = legendPal,
                title = "Predicted Suitability<br>(Projected)",
                values = mapProjVals, layerId = 'proj',
                labFormat = reverseLabels(2, reverse_order = TRUE))
  }
  # map model prediction raster and projection polygon
  colnames(shp) <- c("longitude", "latitude")
  map %>%
    clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    addRasterImage(mapProj(), colors = rasPal, opacity = 0.7,
                   layerId = 'projRas', group = 'proj', method = "ngb") %>%
    addPolygons(lng = shp[,1], lat = shp[,2], layerId = "projExt",
                fill = FALSE, weight = 4, color = "red", group = 'proj')

}

projectArea_INFO <-
  infoGenerator(modName = "Project to New Extent",
                modAuts = paste0("Jamie M. Kass, Bruno Vilela, Gonzalo E. ',
                                 'Pinilla-Buitrago, Robert P. Anderson"),
                pkgName = "dismo")
