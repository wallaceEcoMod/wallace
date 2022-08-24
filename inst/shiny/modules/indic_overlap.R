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
  map %>% clearAll()
  #if EOO is selected plot the polygon
  if (!is.null(spp[[curSp()]]$indic$overlapSourcePoly)) {

    polyEOO <- spp[[curSp()]]$indic$EOOpoly@polygons[[1]]@Polygons
    bb <- spp[[curSp()]]$indic$EOOpoly@bbox
    bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = "gray",
                title = "EOO", labels = "EOO",
                opacity = 1)
    ##ADD polygon
    if (length(polyEOO) == 1) {
      xy <- list(polyEOO[[1]]@coords)
    } else {
      xy <- lapply(polyEOO, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "gray",
                    group = 'indic')
    }
  }

  #plot SDM to use
  if (is.null(spp[[curSp()]]$indic$overlapSourcePoly)) {
    req(spp[[curSp()]]$indic$overlapSourcePoly)
    sdm <-  spp[[curSp()]]$indic$overlapSourcePoly
    raster::crs(sdm) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    SDMVals <- getRasterVals(sdm)
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    legendPal <- colorNumeric(rev(rasCols), SDMVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, SDMVals, na.color = 'transparent')
    zoomExt <- raster::extent(sdm)
    map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                      lat1 = zoomExt[3], lat2 = zoomExt[4])
    if (length(unique(SDMVals)) == 3 | length(unique(SDMVals)) == 2) {
      map %>%
        addLegend("bottomright", colors = c('red', 'grey'),
                  title = "SDM",
                  labels = c("Presence", "Absence"),
                  opacity = 1, layerId = 'sdm') %>%
        addRasterImage(sdm, colors = c('grey', 'red'),
                       opacity = 0.7, group = 'indic', layerId = 'sdm',
                       method = "ngb")
    } else if (length(unique(SDMVals)) == 1) {
      map %>%
        addLegend("bottomright", colors = 'red',
                  title = "AOO",
                  labels = "Presence",
                  opacity = 1, layerId = 'expert') %>%
        addRasterImage(sdm, colors = 'red',
                       opacity = 0.7, group = 'indic', layerId = 'Overlap',
                       method = "ngb")
    } else {
      # if no threshold specified
      legendPal <- colorNumeric(rev(rasCols), SDMVals, na.color = 'transparent')
      rasPal <- colorNumeric(rasCols, SDMVals, na.color = 'transparent')
      map %>%
        addLegend("bottomright", pal = legendPal, title = "SDM",
                  values = SDMVals, layerId = "sdm",
                  labFormat = reverseLabel(2, reverse_order=TRUE)) %>%
        addRasterImage(sdm, colors = rasPal,
                       opacity = 0.7, group = 'indic', layerId = 'sdm',
                       method = "ngb")
    }
  }
  # Add just projection Polygon
  req(spp[[curSp()]]$indic$polyOverlap)
  polyOvXY <- spp[[curSp()]]$indic$polyOverlap@polygons
  if(length(polyOvXY) == 1) {
    shp <- list(polyOvXY[[1]]@Polygons[[1]]@coords)
  } else {
    shp <- lapply(polyOvXY, function(x) x@Polygons[[1]]@coords)
  }
  bb <- spp[[curSp()]]$indic$polyOverlap@bbox
  bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
  map %>%
    fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
  for (poly in shp) {
    map %>% addPolygons(lng = poly[, 1], lat = poly[, 2], weight = 4,
                        color = "red", fill=FALSE, group = 'indic')
  }

  ##Plot overlap of polygons (EOO case)
  if(!is.null(spp[[curSp()]]$indic$overlapPoly)){
    req(spp[[curSp()]]$indic$overlapPoly)
    polyOver <- as_Spatial(spp[[curSp()]]$indic$overlapPoly)
    bb <- polyOver@bbox
    polyOver <- polyOver@polygons[[1]]@Polygons

    bbZoom <- polyZoom(bb[1, 1], bb[2, 1], bb[1, 2], bb[2, 2], fraction = 0.05)
    map %>%
      fitBounds(bbZoom[1], bbZoom[2], bbZoom[3], bbZoom[4])
    map %>%
      ##Add legend
      addLegend("bottomright", colors = "red",
                title = "Overlap", labels = "Overlap",
                opacity = 1)
    ##ADD polygon
    if (length(polyOver) == 1) {
      xy <- list(polyOver[[1]]@coords)
    } else {
      xy <- lapply(polyOver, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "red",
                    group = 'indic')
    }
  }
  ##Plot overlap of raster vs raster (code to get unclear)
  ##Plot overlap of poly and raster (SDM vs. polygon case)
  if (!is.null(spp[[curSp()]]$indic$overlapRaster)) {
    req(spp[[curSp()]]$indic$overlapRaster)
    Overlap <-  spp[[curSp()]]$indic$overlapRaster
    #  if(is.list(Overlap)){
    # Overlap$fun <- mean
    #Overlap$na.rm <- TRUE

    #Overlap <- do.call(raster::mosaic, Overlap)
    #  }
    raster::crs(Overlap) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    OverlapVals <- spp[[curSp()]]$indic$overlapvalues
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    legendPal <- colorNumeric(rev(rasCols), OverlapVals, na.color = 'transparent')
    rasPal <- colorNumeric(rasCols, OverlapVals, na.color = 'transparent')
    zoomExt <- raster::extent(Overlap)
    map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                      lat1 = zoomExt[3], lat2 = zoomExt[4])
    # Create legend
    map %>% clearAll()
    if (length(unique(OverlapVals)) == 3 |
        length(unique(OverlapVals)) == 2) {
      map %>%
        addLegend("bottomright", colors = c('red', 'grey'),
                  title = "Range Overlap",
                  labels = c("Presence", "Absence"),
                  opacity = 1, layerId = 'expert') %>%
        addRasterImage(Overlap, colors = c('gray', 'red'),
                       opacity = 0.7, group = 'indic', layerId = 'Overlap',
                       method = "ngb")
    } else if (length(unique(OverlapVals)) == 1) {
      map %>%
        addLegend("bottomright", colors = 'red',
                  title = "Range Overlap",
                  labels = "Presence",
                  opacity = 1, layerId = 'expert') %>%
        addRasterImage(Overlap, colors = 'red',
                       opacity = 0.7, group = 'indic', layerId = 'Overlap',
                       method = "ngb")
    } else {
      # if threshold specified
      legendPal <- colorNumeric(rev(rasCols), OverlapVals, na.color = 'transparent')
      rasPal <- colorNumeric(rasCols, OverlapVals, na.color = 'transparent')
      map %>%
        addLegend("bottomright", pal = legendPal, title = "Range Overlap",
                  values = OverlapVals, layerId = "overlap",
                  labFormat = reverseLabel(2, reverse_order=TRUE)) %>%
        addRasterImage(Overlap, colors = rasPal,
                       opacity = 0.7, group = 'indic', layerId = 'Overlap',
                       method = "ngb")
    }
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

