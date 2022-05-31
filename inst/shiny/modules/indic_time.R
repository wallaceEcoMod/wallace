indic_time_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    ##inputs must be: SDM so step 1 remains
    #Environmental variables (e g forest thorugh time as rasters (multiple))
    #Threshold numeric input
    #years used (numeric input?)
    span("Step 1:", class = "step"),
    span("Choose Input range", class = "stepText"), br(), br(),
    selectInput(ns("selRasterSource") , label = "Select range for calculations",
                choices = list("Wallace SDM" = "wallace",
                               "Projected SDM" = "proj",
                               "User SDM" = "sdm",
                               "Masked SDM" = "masked",
                               "AOO" = "aoo",
                               "EOO" ="eoo")),
    actionButton(ns("goInputRaster"), "Select"),

    tags$hr(),
    span("Step 2:", class = "step"),
    span("Choose environmental data", class = "stepText"), br(), br(),
    fileInput(ns("indicEnvs"), label = "Upload environmental rasters",
              accept = c(".tif", ".asc"), multiple = TRUE),
    textInput(ns("EnvThrVal"), "Set threshold value",
              placeholder = "for single threshold: 40, for range: 30,60 ",value=""),
    selectInput(ns("selBound") , label = "Select bounds to be used for calculations",
                choices = list("Lower" = "lower",
                               "Upper" = "upper",
                               "Not Applicable" = "neither",
                               "Both" = "both")),
    actionButton(ns("goInputEnvs"), "Load"),

    tags$hr(),
    span("Step 3:", class = "step"),
    span("Choose years", class = "stepText"), br(), br(),
    textInput(ns("Years"), label = "Enter years to be used",
              placeholder = 'format: 2000, 2002',
              value = ""),
    actionButton(ns("goInputYears"), "Calculate")
  )
}

indic_time_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj

  observeEvent(input$goInputRaster, {
    if (input$selRasterSource == "wallace") {
      if (is.null(spp[[curSp()]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Visualize your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$indic$time <- spp[[curSp()]]$visualization$mapPred
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time will be calculated based on Wallace SDM ")
    }
    if (input$selRasterSource == "proj") {
      if (is.null(spp[[curSp()]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing overlap calculations')
        return()
      }
      spp[[curSp()]]$indic$time <-spp[[curSp()]]$project$mapProj
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time will be calculated based on Projected SDM.")

    }
    if (input$selRasterSource == "sdm") {
      if (is.null(spp[[curSp()]]$postProc$OrigPred)) {
        logger %>%
          writeLog(type = 'error',
                   'Load you model in component User SDM before doing range ',
                   'calculations')
        return()
      }
      spp[[curSp()]]$indic$time <- spp[[curSp()]]$postProc$OrigPred
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time will be calculated based on User provided SDM.")

    }
    if (input$selRasterSource == "masked") {
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly
      if (is.null(spp[[curSp()]]$mask$prediction)) {
        logger %>%
          writeLog(type = 'error',
                   'Do a maskRangeR analysis before doing range calculations')
        return()
      }
      spp[[curSp()]]$indic$time <- spp[[curSp()]]$mask$prediction
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time will be calculated based on Masked SDM ")
    }
    if (input$selRasterSource == "aoo") {
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly
      if (is.null( spp[[curSp()]]$rmm$data$indic$AOO)) {
        logger %>%
          writeLog(type = 'error',
                   'Do an AOO calculation before doing time calculations')
        return()
      }
      spp[[curSp()]]$indic$time <- spp[[curSp()]]$rmm$data$indic$AOO
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time will be calculated based on AOO")

    }
    if (input$selRasterSource == "eoo") {
      #CAREFUL: as its set up now if user doesn t do maskrangeR this object will be something else
      #(either user uploaed SDM or wallace SDM) this must be fixed in other components so it works smoothly
      if (is.null(spp[[curSp()]]$rmm$data$indic$EOO)) {
        logger %>%
          writeLog(type = 'error',
                   'Do an EOO calculation before doing time calculations')
        return()
      }
      spp[[curSp()]]$indic$time1 <- spp[[curSp()]]$rmm$data$indic$EOO
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time will be calculated based on EOO")

    }
  })
  observeEvent(input$goInputEnvs, {
    if (is.null(input$indicEnvs)) {
      logger %>% writeLog(type = 'error',
                          "Raster files not uploaded")
      return()
    }
    if (is.null(input$EnvThrVal)) {
      logger %>% writeLog(type = 'error',
                          "Please enter a threshold for environmental layers")
      return()
    }
    smartProgress(
      logger,
      message = "Loading environmental variables ", {
        rStack <- raster::stack(input$indicEnvs$datapath)
        if (raster::nlayers(rStack)==1) {
          logger %>% writeLog(type = 'error',
                              "Please upload more than one environmental variable")
          return()
        }
        spp[[curSp()]]$indic$indicEnvs <- rStack
        threshold <- as.numeric(trimws(strsplit(input$EnvThrVal, ",")[[1]]))
        spp[[curSp()]]$indic$indicEnvsThr <- threshold
      })
  })
  observeEvent(input$goInputYears, {
    req(spp[[curSp()]]$indic$indicEnvs)
    years <- trimws(strsplit(input$Years, ",")[[1]])
    if (raster::nlayers(spp[[curSp()]]$indic$indicEnvs) != length(years)) {
      logger %>% writeLog(type = 'error',
                          "Please enter the years for all inputed variables")
      return()
    }
    if (is.null(spp[[curSp()]]$indic$time1)&is.null(spp[[curSp()]]$indic$time)){
      logger %>% writeLog( type = 'error',
                           "No SDM is selected for the calculations")
    } else if (is.null(spp[[curSp()]]$indic$time1)) {
      smartProgress(
        logger,
        message = "Calculating area indic through time ", {
          SDM <- spp[[curSp()]]$indic$time
          rStack <- raster::projectRaster(spp[[curSp()]]$indic$indicEnvs, SDM, method = 'bilinear')
          threshold <- spp[[curSp()]]$indic$indicEnvsThr
          bound <- input$selBound
          ##run function
          SDM.time <- changeRangeR::envChange(rStack = rStack, binaryRange = SDM, threshold = threshold, bound=bound)
          ### Set up years for plotting
        })
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time calculation done")
      # LOAD INTO SPP ####
      spp[[curSp()]]$indic$AreaTime <-SDM.time$Area
      spp[[curSp()]]$indic$Years <- years
      common$update_component(tab = "Results")
    } else if (!is.null(spp[[curSp()]]$indic$time1)) {
      smartProgress(
        logger,
        message = "Calculating area indic through time ", {
          rStack <- spp[[curSp()]]$indic$indicEnvs
          eoo <- spp[[curSp()]]$indic$time1
          threshold <- spp[[curSp()]]$indic$indicEnvsThr
          bound <- input$selBound
          ##run function
          SDM.time <- changeRangeR::envChange(
            rStack = rStack, binaryRange = eoo, threshold = threshold,
            bound = bound)
          ### Set up years for plotting
        })
      logger %>% writeLog("SDM area after masking for environmental variables ",
                          "through time calculation done")
      # LOAD INTO SPP ####
      spp[[curSp()]]$indic$AreaTime <- SDM.time$Area
      spp[[curSp()]]$indic$Years <- years
      common$update_component(tab = "Results")
    }
  })

  output$TimeAreas <- renderUI({
    # Result
    output$areaMasked <- renderPrint({
      paste0("SDM area (in km^2) after masking for environmental variables ",
             "through time for: ",  spp[[curSp()]]$indic$Years, " ",
             spp[[curSp()]]$indic$AreaTime)})
    output$timePlot <- renderPlot({
      plot(y = spp[[curSp()]]$indic$AreaTime, x = spp[[curSp()]]$indic$Years,
           main = "SDM area indic", ylab = "area (square km)", xlab = "Time")
      lines(y = spp[[curSp()]]$indic$AreaTime, x = spp[[curSp()]]$indic$Years)
    })
    tabsetPanel(
      tabPanel("Area through time plot",
               tagList(
                 plotOutput(session$ns('timePlot'))
               )),
      tabPanel("Area through time values",
               tagList(
                 verbatimTextOutput(session$ns('areaMasked'))
               ))
    )
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        selSource = input$selRasterSource,
        EnvThrVal = input$EnvThrVal,
        Years = input$Years)
    },
    load = function(state) {
      # Load
      updateSelectInput(session, 'selRasterSource', selected = state$selRasterSource)
      updateSelectInput(session, 'EnvThrVal', selected = state$EnvThrVal)
      updateSelectInput(session, 'Years', selected = state$Years)

    }
  ))

}

indic_time_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  uiOutput(ns("TimeAreas"))
}

indic_time_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  #if EOO is selected plot the polygon
  if (!is.null(spp[[curSp()]]$indic$time1)) {
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
  if (is.null(spp[[curSp()]]$indic$time1)) {
    req(spp[[curSp()]]$indic$time)
    sdm <-  spp[[curSp()]]$indic$time
    raster::crs(sdm) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
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
        addRasterImage(sdm, colors = c('gray', 'red'),
                       opacity = 0.7, group = 'indic', layerId = 'sdm',
                       method = "ngb")
    } else {
      # if threshold specified
      legendPal <- colorNumeric(rev(rasCols), SDMVals,
                                na.color = 'transparent')
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
}

indic_time_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    indic_time_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

