indic_overlap_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Input range map", class = "stepText"), br(), br(),
    selectInput(ns("selSource") , label = "Select raster for calculations",
                choices = list("Wallace SDM" = "wallace",
                               "Projected SDM" = "proj",
                               "User SDM" = "sdm",
                               "Masked SDM" = "masked",
                               "EOO" = "eoo",
                               "AOO" = "aoo"
                )),

    actionButton(ns("goInputRaster"), "Select"),
    tags$hr(),
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
    tags$hr(),
    span("Step 3:", class = "step"),
    span("Choose field of interest (if input is a shapefile, else go to step 4)",
         class = "stepText"),
    br(),
    #Add a conditional panel showing the fields in the shapefile
    uiOutput(ns('selFieldui')),
    uiOutput(ns('selCatdui')),
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
  mapProj <- common$mapProj
  indicField <- common$indicField
  indicCategory <- common$indicCategory

  observeEvent(input$goInputRaster, {
    if(input$selSource == "wallace"){
      if (is.null(spp[[curSp()]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Visualize your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$indic$Plot <- spp[[curSp()]]$visualization$mapPred
    }
    if(input$selSource == "proj"){
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$indic$Plot <-  spp[[curSp()]]$project$mapProj
    }
    if(input$selSource == "sdm"){
      if (is.null(spp[[curSp()]]$postProc$OrigPred)) {
        logger %>%
          writeLog(
            type = 'error',
            'Load you model in component User SDM before doing range calculations')
        return()
      }
      spp[[curSp()]]$indic$Plot <- spp[[curSp()]]$postProc$OrigPred
    }
    if(input$selSource == "masked"){

      if (is.null(    spp[[curSp()]]$mask$prediction)) {
        logger %>%
          writeLog(type = 'error',
                   'Do a maskRangeR analysis before doing range calculations')
        return()
      }
      spp[[curSp()]]$indic$Plot <- spp[[curSp()]]$postProc$prediction
    }
    if(input$selSource == "eoo"){

      if (is.null(spp[[curSp()]]$rmm$data$indic$EOO)) {
        logger %>%
          writeLog(
            type = 'error',
            'Do an EOO calculation in the area module before doing range calculations')
        return()
      }
      spp[[curSp()]]$indic$Plot1 <- spp[[curSp()]]$rmm$data$indic$EOO
    }
    if(input$selSource == "aoo"){
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly

      if (is.null(spp[[curSp()]]$rmm$data$indic$AOO)) {
        logger %>%
          writeLog(
            type = 'error',
            'Do an AOO calculation in the area module before doing range calculations')
        return()
      }
      spp[[curSp()]]$indic$Plot <- spp[[curSp()]]$rmm$data$indic$AOO
    }
  })

  observeEvent(input$goInputOver, {
    if (is.null(spp[[curSp()]]$indic$Plot)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()), 'Calculate/Upload a species distribution')
      return()
    }
    if(input$indicOverlap == 'shapefile'){
      pathdir <- dirname(input$indicOverlapShp$datapath)
      pathfile <- basename(input$indicOverlapShp$datapath)
      # get extensions of all input files
      exts <- sapply(strsplit(input$indicOverlapShp$name, '\\.'),
                     FUN = function(x) x[2])
      if ('shp' %in% exts) {
        if (length(exts) < 3) {
          logger %>%
            writeLog(type = 'error',
                     paste0('If entering a shapefile, please select all the ',
                            'following files: .shp, .shx, .dbf.'))
          return()
        }
        smartProgress(
          logger,
          message = "Uploading user provided shapefile ", {
            # get index of .shp
            i <- which(exts == 'shp')
            if (!file.exists(file.path(pathdir, input$indicOverlapShp$name)[i])) {
              file.rename(input$indicOverlapShp$datapath,
                          file.path(pathdir, input$indicOverlapShp$name))
            }
            # read in shapefile and extract coords
            polyOverlap  <- rgdal::readOGR(file.path(pathdir, input$indicOverlapShp$name)[i])
            logger %>% writeLog( "User shapefile loaded ")
          })
      } else {
        logger %>%
          writeLog(type = 'error',
                   paste0('Please enter a ',
                          'shapefile (.shp, .shx, .dbf).'))
        return()
      }
      shpcrop <- rgeos::gBuffer(polyOverlap, byid = TRUE, width = 0)
      ##crop polygon for visualization if range is a raster
      if(!is.null(spp[[curSp()]]$indic$Plot)){
        shpcrop <- raster::crop(shpcrop,
                                raster::extent(spp[[curSp()]]$indic$Plot))}

      spp[[curSp()]]$indic$polyOverlap <- polyOverlap
      spp[[curSp()]]$indic$polyOverlapCrop <- shpcrop
    }
    if(input$indicOverlap == 'raster'){
      userRaster <- indic_raster(rasPath = input$indicOverlapRaster$datapath,
                                 rasName = input$indicOverlapRaster$name,
                                 logger)
      if (!is.null(userRaster)) {
        logger %>% writeLog("User raster file loaded ")
      }
      spp[[curSp()]]$indic$RasOverlap <- userRaster$sdm
    }
    sameRes <- identical(res(spp[[curSp()]]$indic$RasOverlap),
                         res(spp[[curSp()]]$indic$Plot))
    if (!sameRes) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()),
        'Raster resolution must be the same as species distribtuion resolution')
      return()
    }
  })

  ###add this if we want to include field selection
  output$selFieldui <- renderUI({
    #add a conditional on providing a file
    req(curSp())
    if (!is.null(spp[[curSp()]]$indic$polyOverlap)) {
      fields <- colnames(spp[[curSp()]]$indic$polyOverlap@data)
    } else {
      fields <- c("load shapefile first")
    }
    fields <- setNames(as.list(fields), fields)
    shinyWidgets::pickerInput("selField",
                              label = "Select field",
                              choices =   fields ,
                              multiple = FALSE,
                              selected =   fields )
  })

  output$selCatdui <- renderUI({
    #add a conditional on providing a file
    req(curSp(), req(spp[[curSp()]]$indic$polyOverlap), req(indicField()))
    if (!is.null(indicField())) {
      field <- indicField()
      category <- as.character(unique(spp[[curSp()]]$indic$polyOverlap[[field]]))
      category <- c("All",category)
    } else {
      category <- c("load shapefile first")
    }
    category <- setNames(as.list(category), category)
    shinyWidgets::pickerInput("selCat",
                              label = "Select category",
                              choices = category,
                              multiple = TRUE,
                              selected = "All",
                              options = list(`actions-box` = TRUE))

  })

  observeEvent(input$goOverlap,{
    ##condition on which overlap if shp do this if raster then not , stored variables would be different though
    if (input$indicOverlap == 'shapefile') {
      spp[[curSp()]]$indic$ShpCat <- indicCategory()
      spp[[curSp()]]$indic$ShpField <- indicField()
      spp[[curSp()]]$indic$subfield <- input$doSubfield
      category<-indicCategory()
      shp = spp[[curSp()]]$indic$polyOverlap
    } else if(input$indicOverlap=='raster') {
      shp = spp[[curSp()]]$indic$RasOverlap
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
    if (input$selSource == "proj") {
      smartProgress(
        logger,
        message = "Calculating range overlap ", {
          r = spp[[curSp()]]$project$mapProj
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
          r = spp[[curSp()]]$postProc$OrigPred
          #  shp = spp[[curSp()]]$indic$polyOverlap
          raster::crs(shp) <- sf::st_crs(4326)$wkt
          raster::crs(r) <- sf::st_crs(4326)$wkt
          ratio.Overlap <- changeRangeR::ratioOverlap(r = r , shp = shp, field = spp[[curSp()]]$indic$ShpField, category = category,   subfield= spp[[curSp()]]$indic$subfield)
        })
      # LOAD INTO SPP ####
      req(ratio.Overlap)
      logger %>% writeLog("Proportion of user provided range area that is ",
                          "contained by landcover categories calculated ")
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
          r = spp[[curSp()]]$rmm$data$indic$EOO
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
          r = spp[[curSp()]]$rmm$data$indic$AOO
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
      list(
        indicRangeSel = input$indicRangeSel,
        selSource = input$selSource,
        selField = input$selField,
        selCat = input$selCat
      )
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'indicRangeSel', selected = state$indicRangeSel)
      updateSelectInput(session, ' selSource', selected = state$selSource)
      updateSelectInput(session, ' selField', selected = state$selField)
      updateSelectInput(session, ' selCat', selected = state$selCat)
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
  if(!is.null(spp[[curSp()]]$indic$Plot1)){

    polyEOO <- spp[[curSp()]]$rmm$data$indic$EOO@polygons[[1]]@Polygons
    bb <- spp[[curSp()]]$rmm$data$indic$EOO@bbox
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
  if (is.null(spp[[curSp()]]$indic$Plot1)) {
    req(spp[[curSp()]]$indic$Plot)
    sdm <-  spp[[curSp()]]$indic$Plot
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
  req(spp[[curSp()]]$indic$polyOverlapCrop)
  #raster::crs(spp[[curSp()]]$indic$polyOverlap) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
  polyOvXY <- spp[[curSp()]]$indic$polyOverlapCrop@polygons
  if(length(polyOvXY) == 1) {
    shp <- list(polyOvXY[[1]]@Polygons[[1]]@coords)
  } else {
    shp <- lapply(polyOvXY, function(x) x@Polygons[[1]]@coords)
  }
  bb <- spp[[curSp()]]$indic$polyOverlapCrop@bbox
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
  list(
    indic_overlap_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

