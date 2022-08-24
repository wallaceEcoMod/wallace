indic_overlap_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Range Map", class = "stepText"), br(), br(),
    uiOutput(ns("indicOverlapSourceUI")),
    actionButton(ns("goInputRaster"), "Select"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Choose Overlap Feature", class = "stepText"), br(), br(),
    selectInput(ns('indicOverlap'), label = "Select type",
                choices = list("Shapefile" = 'shapefile',
                               "Raster" = 'raster')),
    conditionalPanel(sprintf("input['%s'] == 'shapefile'", ns("indicOverlap")),
                     fileInput(
                       ns("indicOverlapShp"),
                       label = 'Upload polygon in shapefile (.shp, .shx, .dbf) format',
                       accept = c(".dbf", ".shx", ".shp"),
                       multiple = TRUE),
    ),
    conditionalPanel(sprintf("input['%s'] == 'raster'", ns("indicOverlap")),
                     fileInput(
                       ns("indicOverlapRaster"),
                       label = "Upload raster file to overlap",
                       accept = c(".tif", ".asc"))
    ),
    actionButton(ns("goInputOver"), "Load"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 3:", class = "step"),
    span("Run Overlap", class = "stepText"), br(), br(),
    #Add a conditional panel showing the fields in the shapefile
    uiOutput(ns('overlapFieldUI')),
    uiOutput(ns('overlapCatUI')),
    # GEPB: Uncomment when subfield in changeRangeR::ratioOverlap is working
    # checkboxInput(ns("doSubfield"),
    #               label = 'Results per subfield',
    #               value = FALSE),
    actionButton(ns("goOverlap"), "Overlap"), br(),
  )
}

indic_overlap_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapXfer <- common$mapXfer
  selOverlapSource <- common$selOverlapSource
  overlapField <- common$overlapField
  overlapCat <- common$overlapCat

  # STEP 1 #####
  output$indicOverlapSourceUI <- renderUI({
    req(curSp())
    n <- c()
    l <- c()
    if (!is.null(spp[[curSp()]]$visualization$mapPred)) {
      n <- c("wallace", n)
      l <- c("Wallace SDM", l)
    }
    if (!is.null(spp[[curSp()]]$transfer$mapXfer)) {
      n <- c("xfer", n)
      l <- c("Transferred SDM", l)
    }
    if (!is.null(spp[[curSp()]]$mask$userSDM)) {
      n <- c("user", n)
      l <- c("User-specified SDM", l)
    }
    if (!is.null(spp[[curSp()]]$mask$prediction)) {
      n <- c("mask", n)
      l <- c("Masked SDM", l)
    }
    if (!is.null(spp[[curSp()]]$indic$EOOpoly)) {
      n <- c("eoo", n)
      l <- c("Extent of Occurrence (EOO)", l)
    }
    if (!is.null(spp[[curSp()]]$indic$AOOraster)) {
      n <- c("aoo", n)
      l <- c("Area of Ocupancy (AOO)", l)
    }
    indicOverlapSourceList <- setNames(as.list(n), l)
    shinyWidgets::pickerInput("selOverlapSource",
                              label = "Select source for calculations",
                              choices = indicOverlapSourceList,
                              multiple = FALSE)
  })

  observeEvent(input$goInputRaster, {
    # WARNING ####
    if (is.null(selOverlapSource())) {
      logger %>%
        writeLog(type = 'warning',
                 hlSpp(curSp()),
                 "No source available for calculations (**).")
      return()
    }
    # ERRORS ####
    if (selOverlapSource() == "wallace") {
      if (is.null(spp[[curSp()]]$visualization$thresholds)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Generate a thresholded model before doing calculations.')
        return()
      }
    }
    if (selOverlapSource() == "xfer") {
      if (is.null(spp[[curSp()]]$rmm$prediction$transfer$environment1$thresholdSet)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Generate a thresholded prediction before doing calculations')
        return()
      }
    }
    if (selOverlapSource() == "user") {
      if (!shiny::isTruthy(spp[[curSp()]]$mask$userThr)) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Load a user thresholded prediction before doing calculations.')
        return()
      }
    }
    if (selOverlapSource() == "mask") {
      if (!spp[[curSp()]]$mask$maskThr) {
        logger %>%
          writeLog(type = 'error',
                   hlSpp(curSp()),
                   'Mask a thresholded prediction before doing calculations.')
        return()
      }
    }
    if (selOverlapSource() == "eoo") {
      if (is.null(spp[[curSp()]]$indic$EOOpoly)) {
        logger %>%
          writeLog(
            type = 'error',
            'Do an EOO calculation in the area module before doing range calculations')
        return()
      }
    }
    if (selOverlapSource() == "aoo") {
      if (is.null(spp[[curSp()]]$indic$AOOraster)) {
        logger %>%
          writeLog(
            type = 'error',
            'Do an AOO calculation in the area module before doing range calculations')
        return()
      }
    }
    rangeMap <- switch (selOverlapSource(),
                        wallace = spp[[curSp()]]$visualization$mapPred,
                        xfer = spp[[curSp()]]$transfer$mapXfer,
                        user = spp[[curSp()]]$mask$userSDM,
                        mask = spp[[curSp()]]$mask$prediction,
                        eoo = sf::st_as_sf(spp[[curSp()]]$indic$EOOpoly),
                        aoo = spp[[curSp()]]$indic$AOOraster)
    # Transform to polygon
    if ("RasterLayer" %in% class(rangeMap)) {
      r <- terra::rast(rangeMap)
      r[r == 0] <- NA
      rangeMap <- terra::as.polygons(r)
      # Project AOO raster to WGS84
      if (selOverlapSource() == "aoo") {
        rangeMap <- terra::project(rangeMap, getWKT("wgs84"))
      }
      rangeMap <- sf::st_as_sf(rangeMap)
    }
    req(rangeMap)
    logger %>%
      writeLog(hlSpp(curSp()),
               paste0(switch (selOverlapSource(),
                              wallace = "Wallace SDM",
                              xfer = "Transferred SDM",
                              user = "User-specified SDM",
                              mask = "Masked SDM",
                              eoo = "Extent of Occurrence (EOO)",
                              aoo = "Area of Ocupancy (AOO)"),
                      " selected for overlap analysis (**)"))
    # LOAD TO SPP
    spp[[curSp()]]$indic$overlapSourcePoly <- rangeMap
    spp[[curSp()]]$indic$overlapSource <- selOverlapSource()
    # Create/Reset after selecting a new source
    spp[[curSp()]]$indic$inputOverlap <- NULL
    spp[[curSp()]]$indic$inputOverlapSel <- NULL
    spp[[curSp()]]$indic$overlapRaster <- NULL
    spp[[curSp()]]$indic$overlapRatio <- NULL
    spp[[curSp()]]$indic$overlapFields <- NULL
    spp[[curSp()]]$indic$overlapCat <- NULL
  })

  # STEP 2 #####
  observeEvent(input$goInputOver, {
    if (is.null(spp[[curSp()]]$indic$overlapSourcePoly)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), 'Calculate/Upload a species distribution')
      return()
    }
    if (input$indicOverlap == 'shapefile') {
      # ERROR
      if (is.null(input$indicOverlapShp$datapath)) {
        logger %>% writeLog(type = 'error', "Specified filepath(s).")
        return()
      }
      inputOverlap <- indic_inputPoly(input$indicOverlapShp$datapath,
                                      input$indicOverlapShp$name,
                                      spp[[curSp()]]$indic$overlapSourcePoly,
                                      logger, spN = curSp())
      spp[[curSp()]]$indic$inputOverlap <- inputOverlap
    }
    if (input$indicOverlap == 'raster') {
      userRaster <- indic_raster(input$indicOverlapRaster$datapath,
                                 input$indicOverlapRaster$name,
                                 spp[[curSp()]]$indic$overlapSourcePoly,
                                 logger, spN = curSp())
      spp[[curSp()]]$indic$inputOverlap <- userRaster
    }
    if (!is.null(spp[[curSp()]]$indic$inputOverlap)) {
      spp[[curSp()]]$indic$inputOverlapSel <- input$indicOverlap
    }
    # Reset after selecting a new input
    spp[[curSp()]]$indic$overlapRaster <- NULL
    spp[[curSp()]]$indic$overlapRatio <- NULL
    spp[[curSp()]]$indic$overlapFields <- NULL
    spp[[curSp()]]$indic$overlapCat <- NULL
  })

  # STEP 3 #####
  # Add this if we want to include field selection
  output$overlapFieldUI <- renderUI({
    #add a conditional on providing a file
    req(curSp())
    req(!is.null(spp[[curSp()]]$indic$inputOverlap))
    if (!("sf" %in% class(spp[[curSp()]]$indic$inputOverlap))) {
      p('The overlap raster is loaded. Click "Overlap" (**).')
    } else {
      fields <- names(spp[[curSp()]]$indic$inputOverlap)
      fields <- fields[!(fields %in% "geometry")]
      fields <- setNames(as.list(fields), fields)
      shinyWidgets::pickerInput("overlapField",
                                label = "Select field",
                                choices = fields,
                                multiple = FALSE,
                                selected = fields)
    }
  })

  output$overlapCatUI <- renderUI({
    #add a conditional on providing a file
    req(curSp(), spp[[curSp()]]$indic$inputOverlap, overlapField())
    req("sf" %in% class(spp[[curSp()]]$indic$inputOverlap))
    if (!is.null(overlapField())) {
      category <- unique(spp[[curSp()]]$indic$inputOverlap[[overlapField()]])
    } else {
      category <- NULL
    }
    category <- setNames(as.list(category), category)
    shinyWidgets::pickerInput("overlapCat",
                              label = "Select Category",
                              choices = category,
                              multiple = TRUE,
                              selected = category,
                              options = list(`actions-box` = TRUE))
  })

  observeEvent(input$goOverlap, {
    if (is.null(spp[[curSp()]]$indic$inputOverlap)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()),
        'Upload an overlap feature (raster/shapefile). (**)')
      return()
    }
    if ("sf" %in% class(spp[[curSp()]]$indic$inputOverlap)) {
      if (is.null(overlapCat()) & is.null(overlapField())) {
        logger %>% writeLog(
          type = 'warning', hlSpp(curSp()),
          'Wait until fields and categories menus are loaded. (**)')
        return()
      }
      if (is.null(overlapCat())) {
        logger %>% writeLog(
          type = 'warning', hlSpp(curSp()),
          'Select at least one category/attribute for ', overlapField(), '. (**)')
        return()
      }
    }
    rangeMap <- spp[[curSp()]]$indic$overlapSourcePoly
    inputOverlap <- spp[[curSp()]]$indic$inputOverlap

    if ("sf" %in% class(inputOverlap)) {
      overlapF <- overlapField()
      overlapC <- overlapCat()
    } else {
      overlapF <- NULL
      overlapC <- NULL
    }

    overlap <- indic_overlap(rangeMap, inputOverlap, field = overlapF,
                             category = overlapC, logger, spN = curSp())

    logger %>% writeLog(
      hlSpp(curSp()),
      "Proportion of range area calculated (check results) (**).")

    # LOAD INTO SPP ####
    spp[[curSp()]]$indic$overlapRaster <- overlap$overlapRaster
    spp[[curSp()]]$indic$overlapRatio <- overlap$overlapRatio
    spp[[curSp()]]$indic$overlapFields <- overlapF
    spp[[curSp()]]$indic$overlapCat <- overlapC
    common$update_component(tab = "Results")
  })

  output$result <- renderUI({
    req(spp[[curSp()]]$indic$overlapRatio)
    # Result
    if ("sf" %in% class(spp[[curSp()]]$indic$inputOverlap)) {
      msg <- paste("-----------------------------------------------------",
                   paste0("OVERLAP FOR ", curSp()),
                   "-----------------------------------------------------",
                   paste0("Range is ", spp[[curSp()]]$indic$overlapSource, "."),
                   paste0("Shape is a ", spp[[curSp()]]$indic$inputOverlapSel, "."),
                   paste0("Selected shapefile field: ",
                          spp[[curSp()]]$indic$overlapFields),
                   paste0("Selected field categories/attributtes: ",
                          paste(spp[[curSp()]]$indic$overlapCat,
                                collapse = ", "), "."),
                   "-----------------------------------------------------",
                   "RESULTS",
                   "-----------------------------------------------------",
                   spp[[curSp()]]$indic$overlapRatio,
                   sep = "<br/>")
    } else {
      msg <- paste("-----------------------------------------------------",
                   paste0("OVERLAP FOR ", curSp()),
                   "-----------------------------------------------------",
                   paste0("Range is ", spp[[curSp()]]$indic$overlapSource, "."),
                   paste0("Shape is a ", spp[[curSp()]]$indic$inputOverlapSel, "."),
                   "-----------------------------------------------------",
                   "RESULTS",
                   "-----------------------------------------------------",
                   paste(spp[[curSp()]]$indic$overlapRatio, collapse = "<br/>"),
                   sep = "<br/>")
    }
    HTML(msg)
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
    }
  ))

}

indic_overlap_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  htmlOutput(ns("result"))
}

indic_overlap_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  overlapField <- common$overlapField
  overlapCat <- common$overlapCat

  req(spp[[curSp()]]$indic$overlapSourcePoly)

  if (is.null(spp[[curSp()]]$indic$overlapRaster)) {
    map %>% clearAll()
    # Step 1 #
    sourcePoly <- spp[[curSp()]]$indic$overlapSourcePoly
    # Zoom
    bb <- sf::st_bbox(sourcePoly) %>% as.vector()
    bbZoom <- polyZoom(bb[1], bb[2], bb[3], bb[4], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])

    map %>%
      ##Add legend
      addLegend("bottomright", colors = "darkgrey",
                labels = "Range Map",
                opacity = 1, layerId = 'sourceLegend') %>%
      ##ADD polygon
      leafem::addFeatures(sourcePoly, fillColor = 'darkgrey', fillOpacity = 0.7,
                          opacity = 0, group = 'indic', layerId = 'indicSource')
    # Step 2 #
    if (!is.null(spp[[curSp()]]$indic$inputOverlap)) {
      inputOverlap <- spp[[curSp()]]$indic$inputOverlap
      # Step 2a: Shapefile #
      if ("sf" %in% class(inputOverlap)) {
        req(overlapField(), overlapCat())
        # Plot Polygon
        selCate <- subset(inputOverlap,
                         inputOverlap[[overlapField()]] %in% overlapCat())
        noSelCate <- subset(inputOverlap,
                           !inputOverlap[[overlapField()]] %in% overlapCat())
        map %>% clearGroup('inputOverlapGr') %>%
          addPolygons(data = noSelCate,
                      weight = 4, color = "blue", group = 'inputOverlapGr') %>%
          addPolygons(data = selCate,
                      weight = 4, color = "yellow", group = 'inputOverlapGr') %>%
          addLayersControl(overlayGroups = 'inputOverlapGr',
                           position = "bottomleft",
                           options = layersControlOptions(collapsed = FALSE))
      # Step 2b: Raster #
      } else if ("RasterLayer" %in% class(inputOverlap)) {
        overlapValues <- terra::spatSample(x = terra::rast(inputOverlap),
                                           size = 100, na.rm = TRUE)[, 1]
        # Continues raster
        if (any(overlapValues != 0 & overlapValues != 1)) {
          rasCols <- c("#E5F5F9", "#99D8C9", "#2CA25F")
          quanRas <- quantile(c(raster::minValue(inputOverlap),
                                raster::maxValue(inputOverlap)),
                              probs = seq(0, 1, 0.1))
          legendPal <- colorNumeric(rev(rasCols), quanRas,
                                    na.color = 'transparent')
          map %>%
            addLegend("bottomright", pal = legendPal,
                      title = "Input feature (**)",
                      values = quanRas, layerId = "expert",
                      labFormat = reverseLabel(2, reverse_order = TRUE))

        } else {
          if (any(overlapValues != 1)) {
            rasCols <- c("grey", "yellow")
          } else {
            rasCols <- "yellow"
          }
          map %>%
            addLegend("bottomright", colors = rasCol, labels = "Overlap",
                      opacity = 1, layerId = 'prediction')
        }
        map %>%
          leafem::addGeoRaster(inputOverlap,
                               colorOptions = leafem::colorOptions(
                                 palette = colorRampPalette(colors = rasCols)),
                               opacity = 0.7, group = 'indic',
                               layerId = 'rasterOv')
      }
    }
    # Step 3 #
  } else {
    map %>% clearAll() %>%
      ##Add legend
      addLegend("bottomright", colors = "darkred",
                labels = "Overlap Map",
                opacity = 1, layerId = 'overlapLegend') %>%
      ##ADD polygon
      leafem::addFeatures(spp[[curSp()]]$indic$overlapRaster, fillColor = 'darkred', fillOpacity = 0.7,
                          opacity = 0, group = 'indic', layerId = 'indicOverlap')
  }
}

indic_overlap_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  # list(
  #   indic_overlap_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  # )
}

