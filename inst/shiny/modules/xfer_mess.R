xfer_mess_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    strong("Calculate MESS for current extent"), br(), br(),
    actionButton(ns('goEnvSimilarity'), "Calculate MESS")
  )
}

xfer_mess_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  mapXfer <- common$mapXfer
  occs <- common$occs
  bg <- common$bg
  bgMask <- common$bgMask
  logger <- common$logger


  observeEvent(input$goEnvSimilarity, {
    # ERRORS ####
    if (is.null(mapXfer())) {
      logger %>% writeLog(type = 'error', 'Transfer to new area or time first.')
      return()
    }
    if (is.null(spp[[curSp()]]$transfer$xfExt)) {
      logger %>%
        writeLog(
          type = 'error',
          "The polygon has not been finished. Please define a polygon."
     )
      return()
    }

    # FUNCTION CALL ####
    xferYr <- spp[[curSp()]]$rmm$data$transfer$environment1$yearMax
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      mss <- xfer_mess(occs(), bg = NULL, bgMask(), spp[[curSp()]]$transfer$xfEnvs,
                       logger, spN = curSp())
    } else {
      mss <- xfer_mess(occs(), bg(), bgMask(), spp[[curSp()]]$transfer$xfEnvs,
                       logger, spN = curSp())
    }


    # LOAD INTO SPP ####
    spp[[curSp()]]$transfer$mess <- mss
    spp[[curSp()]]$transfer$messVals <- getRasterVals(mss)
    spp[[curSp()]]$rmm$code$wallace$MESS <- TRUE
    spp[[curSp()]]$rmm$code$wallace$MESSTime <- time

    # REFERENCES
    knitcitations::citep(citation("dismo"))

    # METADATA
    spp[[curSp()]]$rmm$prediction$uncertainty$extrapolation <-
      "MESS (multivariate environmental similarity surface)"

    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

xfer_mess_module_map <- function(map, common) {

  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs
  bgShpXY <- common$bgShpXY

  req(spp[[curSp()]]$transfer$mess, spp[[curSp()]]$transfer$xfExt)
  polyXfXY <- spp[[curSp()]]$transfer$xfExt@polygons[[1]]@Polygons
  if(length(polyXfXY) == 1) {
    shp <- list(polyXfXY[[1]]@coords)
  } else {
    shp <- lapply(polyXfXY, function(x) x@coords)
  }
  mess <- spp[[curSp()]]$transfer$mess
  rasVals <- spp[[curSp()]]$transfer$messVals
  # define colorRamp for mess
  if (max(rasVals) > 0 & min(rasVals) < 0) {
    rc1 <- colorRampPalette(colors = rev(RColorBrewer::brewer.pal(n = 3, name = 'Reds')),
                            space = "Lab")(abs(min(rasVals)))
    rc2 <- colorRampPalette(colors = RColorBrewer::brewer.pal(n = 3, name = 'Blues'),
                            space = "Lab")(max(rasVals))
    rasCols <- c(rc1, rc2)
  } else if (max(rasVals) < 0 & min(rasVals) < 0) {
    rasCols <- colorRampPalette(colors = rev(RColorBrewer::brewer.pal(n = 3, name = 'Reds')),
                                space = "Lab")(abs(min(rasVals)))
  } else if (max(rasVals) > 0 & min(rasVals) > 0) {
    rasCols <- colorRampPalette(colors = RColorBrewer::brewer.pal(n = 3, name = 'Blues'),
                                space = "Lab")(max(rasVals))
  }
  legendPal <- colorNumeric(rev(rasCols), rasVals, na.color='transparent')
  rasPal <- colorNumeric(rasCols, rasVals, na.color='transparent')
  map %>% removeControl("xfer") %>%
    addLegend("bottomright", pal = legendPal, title = "MESS Values",
              values = rasVals, layerId = 'xfer',
              labFormat = reverseLabel(2, reverse_order=TRUE))
  # map model prediction raster and transfer polygon
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('xferRas') %>%
    addRasterImage(mess, colors = rasPal, opacity = 0.9,
                   layerId = 'xferRas', group = 'xfer', method = "ngb")
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", group = 'xfer', fill = FALSE)
  }
}

xfer_mess_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    xfer_mess_knit = !is.null(species$rmm$code$wallace$MESS),
    time_rmd = species$rmm$code$wallace$MESSTime
   # polyXfXY_rmd <- printVecAsis(species$transfer$xfExt@polygons[[1]]@Polygons)
    # xfer_mess_knit = species$rmm$code$wallace$someFlag,
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

