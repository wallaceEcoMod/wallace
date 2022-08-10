mask_expPoly_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
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
    span("Step 2:", class = "step"),
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
  bgExt <- common$bgExt

  observeEvent(input$goInputPoly, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      logger %>% writeLog(
        type = 'error', hlSpp(curSp()),
        'Calculate/Upload a model prediction(**)')
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

    # FUNCTION CALL ####
    if (input$polyExpSel == 'expDraw') {
      polyMask <- xfer_draw(spp[[curSp()]]$polyMaskXY, spp[[curSp()]]$polyMaskID,
                            0, logger, spN = curSp())
      polyX <- printVecAsis(round(spp[[curSp()]]$polyMaskXY[, 1], digits = 4))
      polyY <- printVecAsis(round(spp[[curSp()]]$polyMaskXY[, 2], digits = 4))

      if (!rgeos::gIntersects(spp[[curSp()]]$procEnvs$bgExt, polyMask)) {
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

      if (!rgeos::gIntersects(spp[[curSp()]]$procEnvs$bgExt, polyMask)) {
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
    binBool <- length(unique(raster::values(spp[[curSp()]]$postProc$prediction)))
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
      expertRast <- mask_expPoly(polyMask, spp[[curSp()]]$postProc$prediction,
                                 removePoly, bgExt = bgExt(), logger, spN = curSp())
      spp[[curSp()]]$mask$flagPoly <- TRUE
      # LOAD INTO SPP ####
      spp[[curSp()]]$postProc$prediction <- expertRast$pred
      spp[[curSp()]]$mask$prediction <- expertRast$pred
      spp[[curSp()]]$biomodelos$predExpert <- expertRast$pred
      spp[[curSp()]]$mask$expertPoly[[length(spp[[curSp()]]$mask$expertPoly)]]$removed <- removePoly
      spp[[curSp()]]$mask$removePoly <- c(spp[[curSp()]]$mask$removePoly, removePoly)
      spp[[curSp()]]$procEnvs$bgExt <- expertRast$ext
    } else {
      logger %>% writeLog(
        type = "error", hlSpp(curSp()),
        "The polygon was already used. Define a new one (**)")
    }
  })

  # Reset prediction
  observeEvent(input$goReset_mask, {
    req(curSp())
    spp[[curSp()]]$postProc$prediction <- spp[[curSp()]]$postProc$OrigPred
    spp[[curSp()]]$procEnvs$bgExt <- spp[[curSp()]]$postProc$origBgExt
    spp[[curSp()]]$mask$expertPoly <- NULL
    spp[[curSp()]]$mask$prediction <- NULL
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
  bgShpXY <- common$bgShpXY

  req(spp[[curSp()]]$postProc$prediction)
  # Map logic
  map %>% leaflet.extras::addDrawToolbar(
    targetGroup = 'draw', polylineOptions = FALSE, rectangleOptions = FALSE,
    circleOptions = FALSE, markerOptions = FALSE, circleMarkerOptions = FALSE,
    editOptions = leaflet.extras::editToolbarOptions()
  )

  if (!is.null((spp[[curSp()]]$mask$flagPoly))) {
    if (spp[[curSp()]]$mask$flagPoly == FALSE) {
      # Plot Polygon
      expertPoly <- spp[[curSp()]]$mask$expertPoly
      xy <- ggplot2::fortify(expertPoly[[length(expertPoly)]])
      map %>% clearAll() %>%
        addPolygons(lng = xy[, 1], lat = xy[, 2],
                    weight = 4, color = "black", group = 'mask')
    }
    userRaster <- spp[[curSp()]]$postProc$prediction
    userValues <- terra::spatSample(x = terra::rast(userRaster),
                                    size = 100, na.rm = TRUE)[, 1]

    if (!any(userValues > 0 & userValues < 1)) {
      map %>%
        leafem::addGeoRaster(spp[[curSp()]]$postProc$prediction,
                             colorOptions = leafem::colorOptions(
                               palette = colorRampPalette(colors = c('gray', 'darkgreen'))),
                             opacity = 0.7, group = 'mask', layerId = 'postPred') %>%
        addLegend("bottomright", colors = c('gray', 'darkgreen'),
                  title = "Distribution<br>map",
                  labels = c("Unsuitable", "Suitable"),
                  opacity = 1, layerId = 'expert') %>%
        mapBgPolys(bgShpXY(), color = 'green', group = 'mask')
    } else {
      rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
      quanRas <- quantile(c(raster::minValue(userRaster),
                            raster::maxValue(userRaster)),
                          probs = seq(0, 1, 0.1))
      legendPal <- colorNumeric(rev(rasCols), quanRas, na.color = 'transparent')
      map %>% clearAll() %>%
        leafem::addGeoRaster(spp[[curSp()]]$postProc$prediction,
                             colorOptions = leafem::colorOptions(
                               palette = colorRampPalette(colors = rasCols)),
                             opacity = 0.7, group = 'mask', layerId = 'postPred') %>%
        addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                  values = quanRas, layerId = "expert",
                  labFormat = reverseLabel(2, reverse_order = TRUE)) %>%
        mapBgPolys(bgShpXY(), color = 'green', group = 'mask')
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

