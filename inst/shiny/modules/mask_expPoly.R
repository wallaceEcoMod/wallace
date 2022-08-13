mask_expPoly_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Select prediction to mask (**)", class = "stepText"), br(),
    uiOutput(ns("maskExpUI")),
    actionButton(ns('goSelMaskPrExp'), "Select (**)"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Choose Input Polygon (**)", class = "stepText"), br(), br(),
    selectInput(ns('polyExpSel'), label = "Select method (**)",
                choices = list("Draw polygon(**)" = 'expDraw',
                               "User-specified polygon(**)" = 'expUser')),
    conditionalPanel(sprintf("input['%s'] == 'expUser'", ns("polyExpSel")),
                     fileInput(ns("polyExpShp"),
                               label = paste0('Upload polygon in shapefile (.shp, .shx, .dbf) or ',
                                              'CSV file with field order (longitude, latitude)'),
                               accept = c(".csv", ".dbf", ".shx", ".shp"), multiple = TRUE)),
    conditionalPanel(sprintf("input['%s'] == 'expDraw'", ns("polyExpSel")),
                     p("Draw a polygon (**)")),
    actionButton(ns("goInputPoly"), "Create(**)"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 3:", class = "step"),
    span("Choose action (**)", class = "stepText"), br(),
    p("Add or remove polygon (**)"),
    tags$div(
      title = "Add guidance text here (**)",
      radioButtons(ns("actExpPoly"), "",
                   choices = list("Add polygon" = 'addPoly',
                                  "Remove polygon" = 'remPoly'), inline = TRUE),
      actionButton(ns('goActionPoly'), "Mask (**)")),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goReset_mask"), "Reset", class = 'butReset'),
    strong(" prediction")
  )
}

mask_expPoly_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  logger <- common$logger
  selMaskPrExp<- common$selMaskPrExp

  output$maskExpUI <- renderUI({
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
      shinyWidgets::pickerInput("selMaskPrExp",
                                label = "",
                                choices = maskPredList,
                                multiple = FALSE,
                                selected = NULL)
    }
  })

  observeEvent(input$goSelMaskPrExp, {
    if (selMaskPrExp() == "Wallace SDM") {
      spp[[curSp()]]$mask$origPred <- spp[[curSp()]]$visualization$mapPred
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$procEnvs$bgExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "Wallace SDM selected for masking (**).")
    }
    if (selMaskPrExp() == "Transferred SDM") {
      spp[[curSp()]]$mask$origPred <-  spp[[curSp()]]$transfer$mapXfer
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$transfer$xfExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "Transferred SDM selected for masking (**).")

    }
    if (selMaskPrExp() == "User-specified SDM") {
      spp[[curSp()]]$mask$origPred <-  spp[[curSp()]]$mask$userSDM
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$mask$userPolyExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "User-specified SDM selected for masking (**).")
    }
  })

  observeEvent(input$goInputPoly, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$mask$origPred)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()),
        'Select a model prediction(**)')
      return()
    }
    if (input$polyExpSel == 'expDraw') {
      if (is.null(spp[[curSp()]]$polyMaskXY)) {
        logger %>% writeLog(
          type = 'error', hlSpp(curSp()),
          "The polygon has not been drawn and finished. Please use the draw ",
          "toolbar on the left-hand of the map to complete the polygon.")
        return()
      }
    }
    if (input$polyExpSel == 'expUser') {
      if (is.null(input$polyExpShp$datapath)) {
        logger %>% writeLog(
          type = 'error', hlSpp(curSp()),
          "Specified filepath(s) (**)")
        return()
      }
    }

    # Define polyExt
    if (is.null(spp[[curSp()]]$mask$polyExt)) {
      polyExt <- spp[[curSp()]]$mask$origPolyExt
      spp[[curSp()]]$mask$polyExt <- spp[[curSp()]]$mask$origPolyExt
    } else {
      polyExt <- spp[[curSp()]]$mask$polyExt
    }

    # FUNCTION CALL ####
    if (input$polyExpSel == 'expDraw') {
      polyMask <- xfer_draw(spp[[curSp()]]$polyMaskXY, spp[[curSp()]]$polyMaskID,
                            0, logger, spN = curSp())
      polyX <- printVecAsis(round(spp[[curSp()]]$polyMaskXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyMaskXY[, 2], digits = 4))

      if (!rgeos::gIntersects(polyExt, polyMask)) {
        logger %>% writeLog(
          type = 'error', hlSpp(curSp()),
          "The polygon is outside the background extent. Please specify a new polygon. (**)"
        )
        return()
      }

      if (is.null(spp[[curSp()]]$mask$expertPoly)) {
        spp[[curSp()]]$mask$expertPoly <- list()
        spp[[curSp()]]$mask$removePoly <- list()
      }
      # LOAD INTO SPP ####
      # Check if polygon is already created
      if (length(spp[[curSp()]]$mask$expertPoly) == 0) {
        spp[[curSp()]]$mask$expertPoly <- c(spp[[curSp()]]$mask$expertPoly,
                                            polyMask)
        spp[[curSp()]]$mask$flagPoly <- FALSE
      } else {
        # Last polygon updated
        lastPoly <- spp[[curSp()]]$mask$expertPoly[[length(spp[[curSp()]]$mask$expertPoly)]]
        # Calculate difference between polygon. If zero, they are equal.
        diffPoly <- sf::st_difference(sf::st_as_sfc(polyMask),
                                      sf::st_as_sfc(lastPoly))
        if (length(diffPoly) != 0) {
          spp[[curSp()]]$mask$expertPoly <- c(spp[[curSp()]]$mask$expertPoly,
                                              polyMask)
          spp[[curSp()]]$mask$flagPoly <- FALSE
        } else {
          spp[[curSp()]]$mask$flagPoly <- TRUE
        }
      }
      # METADATA ####
      spp[[curSp()]]$rmm$code$wallace$drawMaskCoords <-
        paste0('X: ', polyX, ', Y: ', polyY)
    }

    if (input$polyExpSel == 'expUser') {
      polyMask <- xfer_userExtent(input$polyExpShp$datapath, input$polyExpShp$name,
                                  0, logger, spN = curSp())
      # get extensions of all input files
      exts <- sapply(strsplit(input$polyExpShp$name, '\\.'),
                     FUN = function(x) x[2])
      if ('csv' %in% exts) {
        spp[[curSp()]]$rmm$code$wallace$userMaskExt <- 'csv'
        spp[[curSp()]]$rmm$code$wallace$userMaskPath <- input$polyExpShp$datapath
      } else if ('shp' %in% exts) {
        spp[[curSp()]]$rmm$code$wallace$userMaskExt <- 'shp'
        # get index of .shp
        i <- which(exts == 'shp')
        shpName <- strsplit(input$polyExpShp$name[i], '\\.')[[1]][1]
        spp[[curSp()]]$rmm$code$wallace$userMaskShpParams <-
          list(dsn = input$polyExpShp$datapath[i], layer = shpName)
      }

      if (!rgeos::gIntersects(polyExt, polyMask)) {
        logger %>% writeLog(
          type = 'error', hlSpp(curSp()),
          "The polygon is outside the background extent. Please specify a new polygon. (**)"
        )
        return()
      }

      if (is.null(polyMask)) {
        logger %>% writeLog(
          type = 'warning', hlSpp(curSp()),
          "No polygon uploaded (**)"
        )
        return()
      }
      # LOAD INTO SPP ####
      if (is.null(spp[[curSp()]]$mask$expertPoly)) {
        spp[[curSp()]]$mask$expertPoly <- list()
        spp[[curSp()]]$mask$removePoly <- list()
      }
      # Check if polygon as already created
      if (length(spp[[curSp()]]$mask$expertPoly) == 0) {
        spp[[curSp()]]$mask$expertPoly <- c(spp[[curSp()]]$mask$expertPoly,
                                            polyMask)
        spp[[curSp()]]$mask$flagPoly <- FALSE
      } else {
        # Last polygon updated
        lastPoly <- spp[[curSp()]]$mask$expertPoly[[length(spp[[curSp()]]$mask$expertPoly)]]
        # Calculate difference between polygon. If zero, they are equal.
        diffPoly <- sf::st_difference(sf::st_as_sfc(polyMask),
                                      sf::st_as_sfc(lastPoly))
        if (sf::st_area(diffPoly) != 0) {
          spp[[curSp()]]$mask$expertPoly <- c(spp[[curSp()]]$mask$expertPoly,
                                              polyMask)
          spp[[curSp()]]$mask$flagPoly <- FALSE
        } else {
          spp[[curSp()]]$mask$flagPoly <- TRUE
        }
      }
    }
  })

  observeEvent(input$goActionPoly, {
    # WARNING ####
    req(spp[[curSp()]]$mask$expertPoly)
    if (is.null(spp[[curSp()]]$mask$prediction)) {
      maskPred <- spp[[curSp()]]$mask$origPred
    } else {
      maskPred <- spp[[curSp()]]$mask$prediction
    }

    binBool <- length(unique(raster::values(maskPred)))
    removePoly <- ifelse(input$actExpPoly == "addPoly", FALSE, TRUE)
    if (!(binBool == 3 | binBool == 2)) {
      if (removePoly == FALSE) {
        logger %>% writeLog(
          type = 'error', hlSpp(curSp()),
          "You cannot add a polygon to a continous map (**)."
        )
        return()
      }
    }
    # FUNCTION CALL ####
    if (spp[[curSp()]]$mask$flagPoly == FALSE) {
      polyMask <- spp[[curSp()]]$mask$expertPoly[[length(spp[[curSp()]]$mask$expertPoly)]]
      expertRast <- mask_expPoly(polyMask, maskPred,
                                 removePoly, polyExt = spp[[curSp()]]$mask$polyExt,
                                 logger, spN = curSp())
      spp[[curSp()]]$mask$flagPoly <- TRUE
      # LOAD INTO SPP ####
      spp[[curSp()]]$mask$prediction <- expertRast$pred
      spp[[curSp()]]$biomodelos$predExpert <- expertRast$pred
      spp[[curSp()]]$mask$expertPoly[[length(spp[[curSp()]]$mask$expertPoly)]]$removed <- removePoly
      spp[[curSp()]]$mask$removePoly <- c(spp[[curSp()]]$mask$removePoly, removePoly)
      spp[[curSp()]]$mask$polyExt <- expertRast$ext
    } else {
      logger %>% writeLog(
        type = "error", hlSpp(curSp()),
        "The polygon was already used. Define a new one (**)")
    }
  })

  # Reset prediction
  observeEvent(input$goReset_mask, {
    req(curSp())
    spp[[curSp()]]$mask$prediction <- NULL
    spp[[curSp()]]$mask$polyExt <- NULL
    spp[[curSp()]]$mask$expertPoly <- NULL
    spp[[curSp()]]$mask$removePoly <- NULL
    spp[[curSp()]]$mask$flagPoly <- NULL
    logger %>% writeLog(
      hlSpp(curSp()), "Reset prediction (**).")
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        polyExpSel = input$polyExpSel
      )
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'polyExpSel', selected = state$polyExpSel)
    }
  ))

}

mask_expPoly_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  bgPostXY <- common$bgPostXY

  req(spp[[curSp()]]$mask$origPred)
  # Map logic
  map %>%
    leaflet.extras::addDrawToolbar(
    targetGroup = 'draw', polylineOptions = FALSE, rectangleOptions = FALSE,
    circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )

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

  if (is.null(spp[[curSp()]]$mask$prediction)) {
    maskPred <- spp[[curSp()]]$mask$origPred
  } else {
    maskPred <- spp[[curSp()]]$mask$prediction
  }
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
  # Add/Remove expert polygon
  if (!is.null((spp[[curSp()]]$mask$flagPoly))) {
    if (spp[[curSp()]]$mask$flagPoly == FALSE) {
      # Plot Polygon
      expertPoly <- spp[[curSp()]]$mask$expertPoly
      xy <- ggplot2::fortify(expertPoly[[length(expertPoly)]])
      map %>%
        addPolygons(lng = xy[, 1], lat = xy[, 2],
                    weight = 4, color = "black", group = 'mask',
                    layerId = 'maskPoly')
    } else {
      map %>% removeShape('maskPoly')
    }
  }
}

mask_expPoly_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_expPoly_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  )
}

