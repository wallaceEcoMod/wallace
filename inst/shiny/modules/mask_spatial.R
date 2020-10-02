mask_spatial_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Upload Spatial Data(**)", class = "stepText"), br(), br(),
    fileInput(ns("maskShp"),
              label = 'Upload polygon in shapefile (.shp, .shx, .dbf)',
              accept = c(".dbf", ".shx", ".shp"), multiple = TRUE),
    actionButton(ns("goMaskShp"), "Load (**)"), br(),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Select fields (**)", class = "stepText"), br(), br(),
    uiOutput(ns("maskFieldsUI")),
    uiOutput(ns("maskAttributeUI")),
    actionButton(ns("goSpatialMask"), "Mask (**)"), br(),
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
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), 'Calculate/Upload a model prediction (**)')
      return()
    }
    if (is.null(input$maskShp$datapath)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), "Specified filepath(s) (**)")
      return()
    }
    # FUNCTION CALL ####
    spatialMask <- mask_spatialPoly(input$maskShp$datapath, input$maskShp$name,
                                    spp[[curSp()]]$procEnvs$bgExt,
                                    logger, spN = curSp())
    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$spatialMask <- spatialMask

    logger %>% writeLog(hlSpp(curSp()), "Spatial data uploaded.")
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
    e <- maskPred > -Inf
    bgExt <- raster::rasterToPolygons(e, dissolve = TRUE)

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$prediction <- maskPred
    spp[[curSp()]]$mask$prediction <- maskPred
    spp[[curSp()]]$procEnvs$bgExt <- bgExt
    spp[[curSp()]]$mask$spatialFlag <- TRUE
    logger %>% writeLog(
      hlSpp(curSp()), "Spatial Masked (**)")

    # METADATA ####
  })

  output$result <- renderText({
    # Result
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

  req(spp[[curSp()]]$mask$spatialMask, maskFields(), maskAttribute())
  # Plot Polygon
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
  req(spp[[curSp()]]$mask$spatialFlag)
  userRaster <- spp[[curSp()]]$postProc$prediction
  userValues <- raster::values(userRaster)

  map %>% clearMarkers() %>%
    clearShapes() %>%
    clearAll() %>%
    # add background polygon
    mapBgPolys(bgShpXY(), color = 'green', group = 'post')

  if (length(unique(userValues)) == 3 |
      length(unique(userValues)) == 2) {
    map %>%
      addRasterImage(spp[[curSp()]]$postProc$prediction,
                     colors = c('gray', 'darkgreen'), opacity = 0.7, group = 'mask',
                     layerId = 'postPred', method = "ngb") %>%
      addLegend("bottomright", colors = c('gray', 'darkgreen'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert')
  } else {
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    legendPal <- colorNumeric(rev(rasCols), userValues, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, userValues, na.color = 'transparent')
    map %>%
      addRasterImage(spp[[curSp()]]$postProc$prediction,
                     colors = rasPal, opacity = 0.7, group = 'mask',
                     layerId = 'postPred', method = "ngb") %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = userValues, layerId = "expert",
                labFormat = reverseLabels(2, reverse_order = TRUE))
  }
}

mask_spatial_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_spatial_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

