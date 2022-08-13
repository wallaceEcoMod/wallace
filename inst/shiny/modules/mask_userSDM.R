mask_userSDM_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(ns("sdmFile"), label = "Upload distribution map (**)",
              multiple = TRUE, accept = c(".tif", ".asc")),
    actionButton(ns("goUserSDM"), "Load SDM")
  )
}

mask_userSDM_module_server <- function(input, output, session, common) {
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
      newSppName <- fileNameNoExt(fmtSpN(input$sdmFile$name[i]))

      if (!(newSppName %in% names(spp))) {
        userSDMs <- mask_userSDM(rasPath = input$sdmFile$datapath[i],
                                 rasName = input$sdmFile$name[i],
                                 logger)
        if (!is.null(userSDMs)) {
          logger %>% writeLog(hlSpp(newSppName), "User SDM prediction loaded (**)")
          # LOAD INTO SPP ####
          spp[[newSppName]] <- list()
          spp[[newSppName]]$mask$userSDM <- userSDMs$sdm
          spp[[newSppName]]$mask$userPolyExt <- userSDMs$extSdm
        }
      } else {
        if (!is.null(spp[[newSppName]]$visualization$mapPred)) {
          logger %>% writeLog(
            type = "error", hlSpp(newSppName),
            "Prediction already available on Wallace. You cannot upload a user raster.")
          return()
        } else { # If occs exists
          userSDMs <- mask_userSDM(rasPath = input$sdmFile$datapath[i],
                                   rasName = input$sdmFile$name[i],
                                   logger)
          logger %>% writeLog(hlSpp(newSppName), "User SDM prediction loaded (**)")
          # LOAD INTO SPP ####
          spp[[newSppName]]$mask$userSDM <- userSDMs$sdm
          spp[[newSppName]]$mask$userPolyExt <- userSDMs$extSdm
        }
      }

      # METADATA ####
      spp[[newSppName]]$rmm$code$wallace$userSDM <- TRUE
    }
    common$update_component(tab = "Map")
  })
}

mask_userSDM_module_map <- function(map, common) {

  curSp <- common$curSp
  spp <- common$spp

  # Map logic
  req(spp[[curSp()]]$mask$userSDM)
  # Zoom
  userRaster <- spp[[curSp()]]$mask$userSDM
  userValues <- terra::spatSample(x = terra::rast(userRaster),
                                  size = 100, na.rm = TRUE)[, 1]
  userPolyExt <- spp[[curSp()]]$mask$userPolyExt
  polys <- userPolyExt@polygons[[1]]@Polygons
  if (length(polys) == 1) {
    xy <- list(polys[[1]]@coords)
  } else{
    xy <- lapply(polys, function(x) x@coords)
  }
  zoomExt <- raster::extent(userRaster)
  map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                    lat1 = zoomExt[3], lat2 = zoomExt[4])

  map %>%
    clearAll() %>%
    # add background polygon
    mapBgPolys(xy,
               color = 'green', group = 'mask')

  # Define raster colors and shiny legend
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if it is threshold specified
  if (!any(userValues > 0 & userValues < 1)) {
    map %>%
      addLegend("bottomright", colors = c('gray', 'darkgreen'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert') %>%
      leafem::addGeoRaster(spp[[curSp()]]$mask$userSDM,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = c('gray', 'darkgreen'))),
                           opacity = 0.7, group = 'mask', layerId = 'maskPred')
  } else {
    # if threshold specified
    quanRas <- quantile(c(raster::minValue(userRaster),
                          raster::maxValue(userRaster)),
                        probs = seq(0, 1, 0.1))
    legendPal <- colorNumeric(rev(rasCols), quanRas, na.color = 'transparent')
    map %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = quanRas, layerId = "expert",
                labFormat = reverseLabel(2, reverse_order = TRUE)) %>%
      leafem::addGeoRaster(spp[[curSp()]]$mask$userSDM,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = rasCols)),
                           opacity = 0.7, group = 'mask', layerId = 'maskPred')
  }
}

mask_userSDM_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_userSDM_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

