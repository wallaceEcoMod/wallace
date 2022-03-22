mask_spatial_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Upload Spatial Data(**)", class = "stepText"), br(), br(),
    fileInput(ns("maskShp"),
              label = 'Upload polygon in shapefile (.shp, .shx, .dbf)',
              accept = c(".dbf", ".shx", ".shp"), multiple = TRUE),
    actionButton(ns("goMaskShp"), "Load (**)"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Select fields (**)", class = "stepText"), br(), br(),
    uiOutput(ns("maskFieldsUI")),
    uiOutput(ns("maskAttributeUI")),
    actionButton(ns("goSpatialMask"), "Mask (**)"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goReset_mask"), "Reset", class = 'butReset'),
    strong(" prediction")
  )
}

mask_spatial_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  logger <- common$logger
  maskFields <- common$maskFields
  maskAttribute <- common$maskAttribute

  observeEvent(input$goMaskShp, {
    # WARNING ####
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      logger %>% alfred.writeLog(
        type = 'error', alfred.hlSpp(curSp()), 'Calculate/Upload a model prediction (**)')
      return()
    }
    if (is.null(input$maskShp$datapath)) {
      logger %>% alfred.writeLog(
        type = 'error', alfred.hlSpp(curSp()), "Specified filepath(s) (**)")
      return()
    }
    # FUNCTION CALL ####
    spatialMask <- mask_spatialPoly(input$maskShp$datapath, input$maskShp$name,
                                    spp[[curSp()]]$postProc$prediction,
                                    logger, spN = curSp())
    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$spatialMask <- spatialMask

    logger %>% alfred.writeLog(alfred.hlSpp(curSp()), "Spatial data uploaded.")
    # METADATA ####

  })

  output$maskFieldsUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$spatialMask)
    if(!is.null(spp[[curSp()]]$mask$spatialMask)) {
      polMask <- spp[[curSp()]]$mask$spatialMask
      n <- names(polMask)
    } else {
      n <- NULL
    }
    spatialMaskNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("maskFields",
                              label = "Select field",
                              choices = spatialMaskNameList,
                              multiple = FALSE,
                              selected = spatialMaskNameList)
  })

  output$maskAttributeUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$spatialMask, maskFields())
    if(!is.null(maskFields())) {
      polMask <- spp[[curSp()]]$mask$spatialMask
      n <- unique(polMask[[maskFields()]])
    } else {
      n <- NULL
    }
    attributeMaskNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("maskAttribute",
                              label = "Select attribute",
                              choices = attributeMaskNameList,
                              multiple = TRUE,
                              selected = attributeMaskNameList,
                              options = list(`actions-box` = TRUE))
  })

  observeEvent(input$goSpatialMask, {
    req(spp[[curSp()]]$mask$spatialMask, maskFields(), maskAttribute())
    spatialMask <- spp[[curSp()]]$mask$spatialMask
    # WARNING ####

    # FUNCTION CALL ####
    selectedPoly <- subset(spatialMask,
                           spatialMask[[maskFields()]] %in% maskAttribute())
    dissPoly <- rgeos::gUnaryUnion(selectedPoly)
    maskPred <- raster::crop(spp[[curSp()]]$postProc$prediction, dissPoly)
    maskPred <- raster::mask(maskPred, dissPoly)
    newPred <- raster::trim(maskPred)
    extPoly <- raster::extent(maskPred)
    bgExt <- as(extPoly, 'SpatialPolygons')
    raster::crs(bgExt) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    raster::crs(maskPred) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$spatialMask <- selectedPoly
    spp[[curSp()]]$postProc$prediction <- maskPred
    spp[[curSp()]]$mask$prediction <- maskPred
    spp[[curSp()]]$procEnvs$bgExt <- bgExt
    logger %>% alfred.writeLog(
      alfred.hlSpp(curSp()), "Spatial Masked (**)")

    # METADATA ####
  })

  # Reset prediction
  observeEvent(input$goReset_mask, {
    req(curSp())
    spp[[curSp()]]$postProc$prediction <- spp[[curSp()]]$postProc$OrigPred
    spp[[curSp()]]$procEnvs$bgExt <- spp[[curSp()]]$postProc$origBgExt
    spp[[curSp()]]$mask$prediction <- NULL
    spp[[curSp()]]$mask$spatialMask <- NULL
    logger %>% alfred.writeLog(
      alfred.hlSpp(curSp()), "Reset prediction (**).")
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

mask_spatial_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

mask_spatial_module_map <- function(map, common) {

  spp <- common$spp
  curSp <- common$curSp
  bgShpXY <- common$bgShpXY
  maskFields <- common$maskFields
  maskAttribute <- common$maskAttribute

  req(spp[[curSp()]]$postProc$prediction)

  userRaster <- spp[[curSp()]]$postProc$prediction
  userValues <- terra::spatSample(x = terra::rast(userRaster),
                                  size = 100, na.rm = TRUE)[, 1]
  shinyjs::delay(1000,
                 map %>%
                   alfred.clearAll() %>%
                   mapPNG(curSp()) %>%
                   # add background polygon
                   mapBgPolys(bgShpXY(), color = 'green', group = 'postBg')
  )

  if (!any(userValues > 0 & userValues < 1)) {
    map %>%
      leafem::addGeoRaster(spp[[curSp()]]$postProc$prediction,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = c('gray', 'darkgreen'))),
                           opacity = 0.7, layerId = 'postPred') %>%
      addLegend("bottomright", colors = c('gray', 'darkgreen'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert')
  } else {
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    quanRas <- quantile(c(raster::minValue(userRaster),
                          raster::maxValue(userRaster)),
                        probs = seq(0, 1, 0.1))
    legendPal <- colorNumeric(rev(rasCols), quanRas, na.color = 'transparent')
    map %>%
      leafem::addGeoRaster(spp[[curSp()]]$postProc$prediction,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = rasCols)),
                           opacity = 0.7, layerId = 'postPred') %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = quanRas, layerId = "expert",
                labFormat = alfred.reverseLabel(2, reverse_order = TRUE))
  }
  # Plot Polygon
  req(maskFields(), maskAttribute())
  req(spp[[curSp()]]$mask$spatialMask)
  spatialMask <- spp[[curSp()]]$mask$spatialMask
  selAtt <- subset(spatialMask,
                   spatialMask[[maskFields()]] %in% maskAttribute())
  noSelAtt <- subset(spatialMask,
                   !spatialMask[[maskFields()]] %in% maskAttribute())
  map %>% clearGroup('maskSpatial') %>%
    addPolygons(data = noSelAtt,
                weight = 4, color = "blue", group = 'maskSpatial') %>%
    addPolygons(data = selAtt,
                weight = 4, color = "yellow", group = 'maskSpatial') %>%
    addLayersControl(overlayGroups = 'maskSpatial', position = "bottomleft",
                     options = layersControlOptions(collapsed = FALSE))

}

mask_spatial_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_spatial_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

