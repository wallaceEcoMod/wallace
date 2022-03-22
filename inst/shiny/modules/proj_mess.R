proj_mess_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    strong("Calculate MESS for current extent"), br(), br(),
    actionButton(ns('goEnvSimilarity'), "Calculate MESS")
  )
}

proj_mess_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  mapProj <- common$mapProj
  occs <- common$occs
  bg <- common$bg
  bgMask <- common$bgMask
  logger <- common$logger


  observeEvent(input$goEnvSimilarity, {
    # ERRORS ####
    if (is.null(mapProj())) {
      logger %>% alfred.writeLog(type = 'error', 'Transfer to new area or time first.')
      return()
    }
    if (is.null(spp[[curSp()]]$project$pjExt)) {
      logger %>%
        alfred.writeLog(
          type = 'error',
          "The polygon has not been finished. Please define a polygon."
     )
      return()
    }

    # FUNCTION CALL ####
    projYr <- spp[[curSp()]]$rmm$data$transfer$environment1$yearMax
    time <- ifelse(projYr == "1990", "present-day", projYr)
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      mss <- proj_mess(occs(), bg = NULL, bgMask(), spp[[curSp()]]$project$pjEnvs,
                       time, logger, spN = curSp())
    } else {
      mss <- proj_mess(occs(), bg(), bgMask(), spp[[curSp()]]$project$pjEnvs,
                       time, logger, spN = curSp())
    }


    # LOAD INTO SPP ####
    spp[[curSp()]]$project$mess <- mss
    spp[[curSp()]]$project$messVals <- alfred.getRasterVals(mss)
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

proj_mess_module_map <- function(map, common) {

  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs
  bgShpXY <- common$bgShpXY

  req(spp[[curSp()]]$project$mess, spp[[curSp()]]$project$pjExt)
  polyPjXY <- spp[[curSp()]]$project$pjExt@polygons[[1]]@Polygons
  if(length(polyPjXY) == 1) {
    shp <- list(polyPjXY[[1]]@coords)
  } else {
    shp <- lapply(polyPjXY, function(x) x@coords)
  }
  mess <- spp[[curSp()]]$project$mess
  rasVals <- spp[[curSp()]]$project$messVals
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
  map %>% removeControl("proj") %>%
    addLegend("bottomright", pal = legendPal, title = "MESS Values",
              values = rasVals, layerId = 'proj',
              labFormat = alfred.reverseLabel(2, reverse_order=TRUE))
  # map model prediction raster and projection polygon
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    addRasterImage(mess, colors = rasPal, opacity = 0.9,
                   layerId = 'projRas', group = 'proj', method = "ngb")
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", group = 'proj', fill = FALSE)
  }
}

proj_mess_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    proj_mess_knit = !is.null(species$rmm$code$wallace$MESS),
    time_rmd = species$rmm$code$wallace$MESSTime
   # polyPjXY_rmd <- alfred.printVecAsis(species$project$pjExt@polygons[[1]]@Polygons)
    # proj_mess_knit = species$rmm$code$wallace$someFlag,
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

