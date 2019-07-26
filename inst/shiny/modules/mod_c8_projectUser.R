projectUser_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("projUserNames"),
    fileInput(ns("userProjEnvs"),
              label = paste0('Input rasters in single-file format (i.e. .tif, ',
                             '.asc). All rasters must have the same extent and ',
                             'resolution (cell size). (**)'),
              accept = c(".asc", ".tif"), multiple = TRUE),
    tags$div(title = paste0('Create binary map of predicted presence/absence ',
                            'assuming all values above threshold value represent ',
                            'presence. Also can be interpreted as a "potential ',
                            'distribution" (see guidance).'),
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'none',
                                        "Minimum Training Presence" = 'mtp',
                                        "10 Percentile Training Presence" = 'p10',
                                        "Quantile of Training Presences" = 'qtp'))),
    conditionalPanel(sprintf("input['%s'] == 'qtp'", ns("threshold")),
                     sliderInput(ns("trainPresQuantile"), "Set quantile",
                                 min = 0, max = 1, value = .05)),
    conditionalPanel(
      sprintf("input.modelSel == 'Maxent' & input['%s'] == 'none'", ns("threshold")),
      h5("Prediction output is the same than Visualize component (**)"))
  )
}

projectUser_MOD <- function(input, output, session) {
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
    if (is.null(input$userProjEnvs)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    # Check the number of selected files
    if (length(input$userProjEnvs$name) !=
        length(spp[[curSp()]]$rmm$data$environment$variableNames)) {
      shinyLogs %>%
        writeLog(type = 'error', "Number of files are not the same that the ",
                 "enviromental variables (**)")
      return()
    }
    # Check if the filesnames are the same that envs()
    if (!identical(tools::file_path_sans_ext(sort(input$userProjEnvs$name)),
                   sort(spp[[curSp()]]$rmm$data$environment$variableNames))) {
      shinyLogs %>%
        writeLog(type = 'error',
                 paste0("Raster files don't have same names. You must name your",
                        " files as: (**) "),
                 em(paste(spp[[curSp()]]$rmm$data$environment$variableNames,
                                             collapse = ", ")), ".")
      return()
    }

    # Load raster ####
    userProjEnvs <- c3_userEnvs(rasPath = input$userProjEnvs$datapath,
                                rasName = input$userProjEnvs$name)

    # ERRORS ####
    # Check that the extents of raster and projection extent instersects
    if (!rgeos::gIntersects(spp[[curSp()]]$project$pjExt,
                            as(raster::extent(userProjEnvs), 'SpatialPolygons'))) {
      shinyLogs %>%
        writeLog(type = 'error', 'Extents do not overlap (**)')
      return()
    }

    # FUNCTION CALL ####
    predType <- rmm()$output$prediction$notes
    projUser.out <- c8_projectUser(evalOut(), curModel(), userProjEnvs,
                                   outputType = predType,
                                   alg = rmm()$model$algorithm,
                                   clamp = rmm()$model$maxent$clamping,
                                   spp[[curSp()]]$project$pjExt,
                                   shinyLogs)

    projExt <- projUser.out$projExt
    projUser <- projUser.out$projUser

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
      projUserThr <- projUser > thr
      shinyLogs %>% writeLog("Projection of model to user-specified files for (**)",
                             em(spName(occs())), ' with threshold ',
                             input$threshold, ' (', formatC(thr, format = "e", 2),
                             ').')
    } else {
      projUserThr <- projUser
      shinyLogs %>% writeLog("Projection of model to user-specified files for (**)",
                             em(spName(occs())), ' with ', predType, ' output.')
    }
    raster::crs(projUserThr) <- raster::crs(envs())
    # rename
    names(projUserThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projUserThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projUserThr, predType)

    # METADATA ####
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- 1960
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- 1990
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "project to user-specified files"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"

    spp[[curSp()]]$rmm$output$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$output$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projUserThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$output$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projUserThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$transfer$environment1$thresholdRule <- input$threshold
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
  })
}

projectUser_MAP <- function(map, session) {
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
    addPolygons(lng = shp[, 1], lat = shp[, 2], layerId = "projExt",
                fill = FALSE, weight = 4, color = "red", group = 'proj')

}

projectUser_INFO <-
  infoGenerator(modName = "Project to User-files (**)",
                modAuts = paste0("Gonzalo E. Pinilla-Buitrago, Jamie M. Kass, ",
                                 "Robert P. Anderson"),
                pkgName = "dismo")
