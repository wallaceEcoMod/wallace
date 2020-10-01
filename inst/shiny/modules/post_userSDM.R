post_userSDM_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(ns("sdmFile"), label = "Upload distribution map (**)",
              multiple = TRUE, accept = c(".tif", ".asc")),
    actionButton(ns("goUserSDM"), "Load SDM")
  )
}

post_userSDM_module_server <- function(input, output, session, common) {
  spp <- common$spp
  logger <- common$logger

  observeEvent(input$goUserSDM, {
    # WARNING ####
    if (is.null(input$sdmFile)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded (**)")
      return()
    }
    # FUNCTION CALL ####
    for (i in 1:length(input$sdmFile$name)) {
      ###########################
      newSppName <- fileNameNoExt(formatSpName(input$sdmFile$name[i]))

      if (!(newSppName %in% names(spp))) {
        spp[[newSppName]] <- list()
        userSDMs <- post_userSDM(rasPath = input$sdmFile$datapath[i],
                                 rasName = input$sdmFile$name[i],
                                 logger)
        logger %>% writeLog(hlSpp(newSppName), "User SDM prediction loaded (**)")
        # LOAD INTO SPP ####
        spp[[newSppName]]$postProc$prediction <- userSDMs$sdm
        spp[[newSppName]]$postProc$OrigPred <- userSDMs$sdm
        spp[[newSppName]]$procEnvs$bgExt <- userSDMs$extSdm
      } else {
        if (!is.null(spp[[newSppName]]$visualization$mapPred)) {
          logger %>% writeLog(
            type = "error", hlSpp(newSppName),
            "Prediction already available on Wallace. You cannot upload a user raster.")
          return()
        } else { # If occs exists
          userSDMs <- post_userSDM(rasPath = input$sdmFile$datapath[i],
                                   rasName = input$sdmFile$name[i],
                                   logger)
          logger %>% writeLog(hlSpp(newSppName), "User SDM prediction loaded (**)")
          # LOAD INTO SPP ####
          spp[[newSppName]]$postProc$prediction <- userSDMs$sdm
          spp[[newSppName]]$postProc$OrigPred <- userSDMs$sdm
          spp[[newSppName]]$procEnvs$bgExt <- userSDMs$extSdm
        }
      }


      # METADATA ####

    }
    common$update_component(tab = "Map")
  })
}

post_userSDM_module_map <- function(map, common) {

  curSp <- common$curSp
  spp <- common$spp
  bgShpXY <- common$bgShpXY

  # Map logic
  req(spp[[curSp()]]$postProc$prediction, spp[[curSp()]]$procEnvs$bgExt)
  # Zoom
  userRaster <- spp[[curSp()]]$postProc$prediction
  userValues <- raster::values(userRaster)
  zoomExt <- raster::extent(userRaster)
  map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                    lat1 = zoomExt[3], lat2 = zoomExt[4])

  map %>% clearMarkers() %>%
    clearShapes() %>% clearAll() %>%
    # add background polygon
    mapBgPolys(bgShpXY(), color = 'green', group = 'post')

  # Define raster colors and shiny legend
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if it is threshold specified
  if (length(unique(userValues)) == 3 |
      length(unique(userValues)) == 2) {
    map %>%
      addLegend("bottomright", colors = c('gray', 'darkgreen'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert') %>%
      addRasterImage(userRaster, colors = c('gray', 'darkgreen'),
                     opacity = 0.7, group = 'mask', layerId = 'postPred',
                     method = "ngb")
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), userValues, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, userValues, na.color = 'transparent')
    map %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = userValues, layerId = "expert",
                labFormat = reverseLabels(2, reverse_order=TRUE)) %>%
      addRasterImage(userRaster, colors = rasPal,
                     opacity = 0.7, group = 'mask', layerId = 'postPred',
                     method = "ngb")
  }
}

post_userSDM_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    post_userSDM_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

