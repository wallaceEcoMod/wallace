mask_spatial_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Select prediction to mask (**)", class = "stepText"), br(),
    uiOutput(ns("maskSpaUI")),
    actionButton(ns('goSelMaskPrSpa'), "Select (**)"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Upload Spatial Data(**)", class = "stepText"), br(), br(),
    fileInput(ns("maskShp"),
              label = 'Upload polygon in shapefile (.shp, .shx, .dbf)',
              accept = c(".dbf", ".shx", ".shp"), multiple = TRUE),
    actionButton(ns("goMaskShp"), "Load (**)"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 3:", class = "step"),
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
  selMaskPrSpa <- common$selMaskPrSpa

  output$maskSpaUI <- renderUI({
    req(curSp())
    n <- c()
    if (!is.null(spp[[curSp()]]$visualization$mapPred)) {
      n <- c(n, "Wallace SDM")
    }
    if (!is.null(spp[[curSp()]]$transfer$mapXfer)) {
      n <- c(n, "Transferred SDM")
    }
    if (!is.null(spp[[curSp()]]$mask$userSDM)) {
      n <- c(n, "User-specified SDM")
    }
    if (is.null(n)) {
      p("Perform or upload model prediction (**)")
    } else {
      maskPredList <- setNames(as.list(n), n)
      shinyWidgets::pickerInput("selMaskPrSpa",
                                label = "",
                                choices = maskPredList,
                                multiple = FALSE,
                                selected = NULL)
    }
  })

  observeEvent(input$goSelMaskPrSpa, {
    if (selMaskPrSpa() == "Wallace SDM") {
      spp[[curSp()]]$mask$origPred <- spp[[curSp()]]$visualization$mapPred
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$procEnvs$bgExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "Wallace SDM selected for masking (**).")
    }
    if (selMaskPrSpa() == "Transferred SDM") {
      spp[[curSp()]]$mask$origPred <-  spp[[curSp()]]$transfer$mapXfer
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$transfer$xfExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "Transferred SDM selected for masking (**).")

    }
    if (selMaskPrSpa() == "User-specified SDM") {
      spp[[curSp()]]$mask$origPred <-  spp[[curSp()]]$mask$userSDM
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$mask$userPolyExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "User-specified SDM selected for masking (**).")
    }
  })

  observeEvent(input$goMaskShp, {
    # WARNING ####
    if (is.null(spp[[curSp()]]$mask$origPred)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), 'Select a model prediction (**)')
      return()
    }
    if (is.null(input$maskShp$datapath)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), "Specified filepath(s) (**)")
      return()
    }
    # FUNCTION CALL ####
    if (is.null(spp[[curSp()]]$mask$prediction)) {
      maskPred <- spp[[curSp()]]$mask$origPred
    } else {
      maskPred <- spp[[curSp()]]$mask$prediction
    }
    spatialMask <- mask_spatialPoly(input$maskShp$datapath, input$maskShp$name,
                                    maskPred,
                                    logger, spN = curSp())
    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$spatialMask <- spatialMask
    if (is.null(spp[[curSp()]]$mask$polyExt)) {
      spp[[curSp()]]$mask$polyExt <- spp[[curSp()]]$mask$origPolyExt
    }
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

    if (is.null(spp[[curSp()]]$mask$prediction)) {
      maskPred <- spp[[curSp()]]$mask$origPred
    } else {
      maskPred <- spp[[curSp()]]$mask$prediction
    }
    maskPred <- raster::crop(maskPred, dissPoly)
    maskPred <- raster::mask(maskPred, dissPoly)
    newPred <- raster::trim(maskPred)
    extPoly <- raster::extent(maskPred)
    polyExt <- methods::as(extPoly, 'SpatialPolygons')
    raster::crs(polyExt) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    raster::crs(maskPred) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$spatialMask <- selectedPoly
    spp[[curSp()]]$mask$prediction <- maskPred
    spp[[curSp()]]$mask$polyExt <- polyExt
    logger %>% writeLog(
      hlSpp(curSp()), "Spatial Masked (**)")

    # METADATA ####
  })

  # Reset prediction
  observeEvent(input$goReset_mask, {
    req(curSp())
    spp[[curSp()]]$mask$prediction <- NULL
    spp[[curSp()]]$mask$polyExt <- NULL
    spp[[curSp()]]$mask$spatialMask <- NULL
    logger %>% writeLog(
      hlSpp(curSp()), "Reset prediction (**).")
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
  bgPostXY <- common$bgPostXY
  maskFields <- common$maskFields
  maskAttribute <- common$maskAttribute

  req(spp[[curSp()]]$mask$origPred)

  if (is.null(spp[[curSp()]]$mask$prediction)) {
    maskPred <- spp[[curSp()]]$mask$origPred
  } else {
    maskPred <- spp[[curSp()]]$mask$prediction
  }

  if (is.null(spp[[curSp()]]$mask$polyExt)) {
    xyPoly <- spp[[curSp()]]$mask$origPolyExt
    polys <- xyPoly@polygons[[1]]@Polygons
    if (length(polys) == 1) {
      xyPoly <- list(polys[[1]]@coords)
    } else{
      xyPoly <- lapply(polys, function(x) x@coords)
    }
  } else {
    xyPoly <- bgPostXY()
  }

  map %>%
    clearAll() %>%
    # add background polygon
    mapBgPolys(xyPoly, color = 'green', group = 'mask')


  maskValues <- terra::spatSample(x = terra::rast(maskPred),
                                  size = 100, na.rm = TRUE)[, 1]

  if (!any(maskValues > 0 & maskValues < 1)) {
    map %>%
      leafem::addGeoRaster(maskPred,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = c('gray', 'darkgreen'))),
                           opacity = 0.7, group = 'mask', layerId = 'postPred') %>%
      addLegend("bottomright", colors = c('gray', 'darkgreen'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert')
  } else {
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    quanRas <- quantile(c(raster::minValue(maskPred),
                          raster::maxValue(maskPred)),
                        probs = seq(0, 1, 0.1))
    legendPal <- colorNumeric(rev(rasCols), quanRas, na.color = 'transparent')
    map %>%
      leafem::addGeoRaster(maskPred,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = rasCols)),
                           opacity = 0.7, group = 'mask', layerId = 'postPred') %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(Mask) (**)",
                values = quanRas, layerId = "expert",
                labFormat = reverseLabel(2, reverse_order = TRUE))
  }

  req(maskFields(), maskAttribute())
  req(spp[[curSp()]]$mask$spatialMask)
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
}

mask_spatial_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_spatial_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

