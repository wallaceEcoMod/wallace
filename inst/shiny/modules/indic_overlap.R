indic_overlap_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    uiOutput(ns("indicOverlapSourceUI")),
    actionButton(ns("goInputRaster"), "Select"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Choose Overlap Map", class = "stepText"), br(), br(),
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
    span("Choose field of interest (if input is a shapefile, else go to step 4)",
         class = "stepText"),
    br(),
    #Add a conditional panel showing the fields in the shapefile
    uiOutput(ns('overlapFieldUI')),
    uiOutput(ns('overlapCatUI')),
    checkboxInput(ns("doSubfield"),
                  label = 'Results per subfield',
                  value = FALSE),
    actionButton(ns("goSelField"), "Select"), br(),
    #ADD this to be able to select category
    span("Step 4:", class = "step"),
    span("Do range overlap", class = "stepText"), br(),
    actionButton(ns("goOverlap"), "Overlap"), br(),
  )
}

indic_overlap_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapXfer <- common$mapXfer
  overlapField <- common$overlapField
  overlapCat <- common$overlapCat

  # STEP 1 #####
  output$indicOverlapSourceUI <- renderUI({
    req(curSp())
    if (input$indicOverlapSel != "") {
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
    }
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
                        eoo = spp[[curSp()]]$indic$EOOpoly,
                        aoo = spp[[curSp()]]$indic$AOOraster)
    # Transform to polygon
    if (class(rangeMap) == "Raster") {
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
      polyOverlap <- xfer_userExtent(input$polyExpShp$datapath,
                                     input$polyExpShp$name,
                                     0, logger, spN = curSp())
      spp[[curSp()]]$indic$polyOverlap <- polyOverlap
    }
    if (input$indicOverlap == 'raster') {
      userRaster <- mask_userSDM(rasPath = input$indicOverlapRaster$datapath,
                                 rasName = input$indicOverlapRaster$name,
                                 logger)
      if (!is.null(userRaster)) {
        userValues <- terra::spatSample(x = terra::rast(userRaster$sdm),
                                        size = 100, na.rm = TRUE)[, 1]
        if (any(userValues > 0 & userValues < 1)) {
          logger %>% writeLog(type = 'error',
                              hlSpp(curSp()),
                              "Upload a binary raster (0 and 1 values) (**).")
          return()
        }
        # Transform to polygon
        r <- terra::rast(userRaster$sdm)
        polyOverlap <- terra::as.polygons(r) %>% sf::st_as_sf()
        spp[[curSp()]]$indic$polyOverlap <- polyOverlap
        logger %>% writeLog(hlSpp(curSp()), "User raster file loaded.")
      }
    }
  })

  # STEP 3 #####
  # Add this if we want to include field selection
  output$overlapFieldUI <- renderUI({
    #add a conditional on providing a file
    req(curSp(), spp[[curSp()]]$indic$polyOverlap)
    if (!is.null(spp[[curSp()]]$indic$polyOverlap)) {
      fields <- colnames(spp[[curSp()]]$indic$polyOverlap@data)
    } else {
      fields <- NULL
    }
    fields <- setNames(as.list(fields), fields)
    shinyWidgets::pickerInput("overlapField",
                              label = "Select field",
                              choices = fields,
                              multiple = FALSE,
                              selected = fields)
  })

  output$overlapCatUI <- renderUI({
    #add a conditional on providing a file
    req(curSp(), spp[[curSp()]]$indic$polyOverlap, overlapField())
    if (!is.null(overlapField())) {
      category <- as.character(unique(spp[[curSp()]]$indic$polyOverlap[[overlapField()]]))
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

  observeEvent(input$goOverlap,{
    # Condition on which overlap if shp do this if raster then not, stored
    # variables would be different though
    if (input$indicOverlap == 'shapefile') {
      spp[[curSp()]]$indic$ShpCat <- overlapCat()
      spp[[curSp()]]$indic$ShpField <- overlapField()
      spp[[curSp()]]$indic$subfield <- input$doSubfield
      category<-overlapCat()
      shp = spp[[curSp()]]$indic$polyOverlap
    } else if(input$indicOverlap=='raster') {
      shp = spp[[curSp()]]$indic$polyOverlap
      spp[[curSp()]]$indic$ShpCat <-   NULL
      spp[[curSp()]]$indic$ShpField <-    NULL
      category<-NULL
      spp[[curSp()]]$indic$subfield <- FALSE
    }
    if (input$selSource == "wallace") {
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$visualization$mapPred
          #shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          raster::crs(r) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          ratio.Overlap <- changeRangeR::ratioOverlap(
            r = r, shp = shp, field = spp[[curSp()]]$indic$ShpField,
            category = category, subfield = spp[[curSp()]]$indic$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of range area that is contained by ',
                          'landcover categories calculated ")
      # LOAD INTO SPP ####
      if (length(ratio.Overlap$maskedRange) > 1) {
        names(ratio.Overlap$maskedRange) <- NULL
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE
        ratio.Overlap$maskedRange <- do.call(raster::mosaic,
                                             ratio.Overlap$maskedRange)
      } else {
        ratio.Overlap$maskedRange<-ratio.Overlap$maskedRange[[1]]
      }
      spp[[curSp()]]$indic$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$indic$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$indic$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)
    }
    if (input$selSource == "xfer") {
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$transfer$mapXfer
          # shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
          raster::crs(r) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          ratio.Overlap <- changeRangeR::ratioOverlap(
            r = r, shp = shp, field = spp[[curSp()]]$indic$ShpField,
            category = category, subfield = spp[[curSp()]]$indic$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of projected range area that is ",
                          "contained by landcover categories calculated ")
      # LOAD INTO SPP ####
      if (length(ratio.Overlap$maskedRange) > 1) {
        names(ratio.Overlap$maskedRange) <- NULL
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE
        ratio.Overlap$maskedRange <- do.call(raster::mosaic,
                                             ratio.Overlap$maskedRange)
      } else {
        ratio.Overlap$maskedRange <- ratio.Overlap$maskedRange[[1]]
      }
      spp[[curSp()]]$indic$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$indic$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$indic$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)
      common$update_component(tab = "Map")
    }
    if (input$selSource == "sdm") {
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$indic$overlapSourcePoly
          #  shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- sf::st_crs(4326)$wkt
          raster::crs(r) <- sf::st_crs(4326)$wkt
          ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp = shp, field = spp[[curSp()]]$indic$ShpField, category = category,   subfield= spp[[curSp()]]$indic$subfield)
        })
      # LOAD INTO SPP ####
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of user provided range area that is ",
                          "contained by landcover categories calculated ")
       if (input$indicOverlap == 'shapefile') {


      if (length(ratio.Overlap$maskedRange) > 1) {
        names(ratio.Overlap$maskedRange) <- NULL
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE
        ratio.Overlap$maskedRange <- do.call(raster::mosaic,
                                             ratio.Overlap$maskedRange)
      } else {
        ratio.Overlap$maskedRange <- ratio.Overlap$maskedRange[[1]]
      }
      }
      else if(input$indicOverlap=='raster') {
      ratio.Overlap$maskedRange <- ratio.Overlap$maskedRange[[1]]
      }
      spp[[curSp()]]$indic$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$indic$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$indic$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }
    if (input$selSource == "masked") {
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r =     spp[[curSp()]]$mask$prediction
          #  shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          ratio.Overlap <- changeRangeR::ratioOverlap(
            r = r , shp = shp,
            field = spp[[curSp()]]$indic$ShpField,
            category = category, subfield = spp[[curSp()]]$indic$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of masked range area that is contained ",
                          "by landcover categories calculated ")
      # LOAD INTO SPP ####
      if (length(ratio.Overlap$maskedRange) > 1) {
        names(ratio.Overlap$maskedRange) <- NULL
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE
        ratio.Overlap$maskedRange <- do.call(raster::mosaic, ratio.Overlap$maskedRange)
      } else {
        ratio.Overlap$maskedRange<-ratio.Overlap$maskedRange[[1]]
      }
      spp[[curSp()]]$indic$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$indic$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$indic$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)
    }
    if (input$selSource == "eoo") {
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$indic$EOOpoly
          #      shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          ratio.Overlap <- changeRangeR::ratioOverlap(
            r = r, shp = shp, field = spp[[curSp()]]$indic$ShpField,
            category = category, subfield = spp[[curSp()]]$indic$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of EOO that is contained by landcover ",
                          "categories calculated ")
      # LOAD INTO SPP ####
      spp[[curSp()]]$indic$overlapPoly <- ratio.Overlap$maskedRange
      spp[[curSp()]]$indic$overlapvalue <- ratio.Overlap$ratio
      # spp[[curSp()]]$indic$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)

    }
    if (input$selSource == "aoo") {
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$indic$AOOraster
          #     shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          ratio.Overlap <- changeRangeR::ratioOverlap(
            r = r, shp = shp, field = spp[[curSp()]]$indic$ShpField,
            category = category, subfield = spp[[curSp()]]$indic$subfield)
        })
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of AOO that is contained by landcover ",
                          "categories calculated ")
      # LOAD INTO SPP ####
      if (length(ratio.Overlap$maskedRange) > 1) {
        names(ratio.Overlap$maskedRange) <- NULL
        ratio.Overlap$maskedRange$fun <- mean
        ratio.Overlap$maskedRange$na.rm <- TRUE
        ratio.Overlap$maskedRange <- do.call(raster::mosaic,
                                             ratio.Overlap$maskedRange)
      } else {
        ratio.Overlap$maskedRange <- ratio.Overlap$maskedRange[[1]]
      }
      spp[[curSp()]]$indic$overlapRaster <- ratio.Overlap$maskedRange
      spp[[curSp()]]$indic$overlapvalue <- ratio.Overlap$ratio
      spp[[curSp()]]$indic$overlapvalues <- getRasterVals(ratio.Overlap$maskedRange)
    }
  })

  output$result <- renderText({
    # Result
    #   spp[[curSp()]]$indic$overlapvalue)
    gsub("%","%\n",  spp[[curSp()]]$indic$overlapvalue)
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
  verbatimTextOutput(ns("result"))
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

