mask_temp_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Upload Temporal Rasters (**)", class = "stepText"), br(), br(),
    fileInput(ns("tempRasters"),
              label = "Upload environmental rasters for masking distribution map",
              multiple = TRUE),
    actionButton(ns("goTempRasters"), "Load (**)", class = "tempRast"), br(),
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Bounds (**)", class = "stepText"), br(),
    uiOutput(ns("curTempRastersUI")),
    textInput(ns("yearInput"),
              label = paste0("Type the years to be used for extracting ",
                             "environmental data, separated by commas")),
    actionButton(ns('goAnnotate'), "Get Bounds (**)"), br(),
    tags$hr(),
    span("Step 3:", class = "step"),
    span("Mask (**)", class = "stepText"), br(),
    uiOutput(ns("curMaskRasterUI")),
    p("Provide lower and/or upper bound values for masking"),
    uiOutput(ns("sliderMaskUI")),
    actionButton(ns('goTempMask'), "Mask (**)")
  )
}

mask_temp_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  logger <- common$logger
  occs <- common$occs
  selTempRaster <- common$selTempRaster
  selTempMask <- common$selTempMask
  sliderTemp <- common$sliderTemp

  observeEvent(input$goTempRasters, {
    curSp <- common$curSp
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error', "Upload some occs or userSDM (**).")
      return()
    }

    if (is.null(input$tempRasters$datapath)) {
      logger %>% writeLog(type = 'warning',
                          "Wait until data is uploaded or specify rasters(**)")
      return()
    }

    ppRasters <- envs_userEnvs(rasPath = input$tempRasters$datapath,
                               rasName = input$tempRasters$name,
                               logger = logger)

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$rasters <- ppRasters
  })

  output$curTempRastersUI <- renderUI({
    req(curSp(), spp[[curSp()]]$postProc$rasters)
    if(!is.null(spp[[curSp()]]$postProc$rasters)) {
      n <- c(names(spp[[curSp()]]$postProc$rasters))
    } else {
      n <- NULL
    }
    ppRastersNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("selTempRaster",
                              label = "Select/deselect environmental rasters",
                              choices = ppRastersNameList,
                              multiple = TRUE,
                              selected = ppRastersNameList)
  })

  observeEvent(input$goAnnotate, {
    if (is.null(occs())) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()), "Upload occs (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$postProc$prediction)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()), "Upload SDM prediction (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$postProc$rasters)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()), "Raster files not uploaded.")
      return()
    }
    # Prepare rasters
    env <- raster::stack(spp[[curSp()]]$postProc$rasters[[selTempRaster()]])
    # crop climate data to study region
    env <- raster::crop(env, spp[[curSp()]]$postProc$prediction)
    # Prepare year vector
    dates <- trimws(strsplit(input$yearInput, ",")[[1]])
    # FUNCTION CALL

    tempExtract <- mask_tempAnnotate(occs = occs(),
                                     env = env,
                                     envDates = dates,
                                     logger)

    logger %>% writeLog(hlSpp(curSp()), "Values were extracted (**)")

    # # subset by key columns and make id and popup columns
    # cols <- c("occID", "scientific_name", "longitude", "latitude", "year",
    #           "extractedValue", "country", "state_province", "locality", "record_type",
    #           "catalog_number", "institution_code", "elevation", "uncertainty",
    #           "pop")
    # occsEnvs <- occs()
    # if (!('extractedValue' %in% names(occsEnvs))) {
    #   occsEnvs <- cbind.data.frame(occsEnvs, extractedValue = tempExtract)
    #   occsEnvs <- occsEnvs[, cols]
    # } else {
    #   occsEnvs[, 'extractedValue'] <- tempExtract
    # }

    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$bounds <- as.data.frame(tempExtract)
    common$update_component(tab = "Results")
  })

  output$boundsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$mask$bounds)
    spp[[curSp()]]$mask$bounds
  })

  output$curMaskRasterUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$bounds)
    if(!is.null(spp[[curSp()]]$postProc$rasters)) {
      n <- c(names(spp[[curSp()]]$postProc$rasters))
    } else {
      n <- NULL
    }
    ppRastersNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("selTempMask",
                              label = "Select raster for masking (**)",
                              choices = ppRastersNameList,
                              multiple = FALSE,
                              selected = ppRastersNameList)
  })

  output$sliderMaskUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$bounds)
    minV <- spp[[curSp()]]$mask$bounds$tempExtract[1]
    maxV <- spp[[curSp()]]$mask$bounds$tempExtract[length(spp[[curSp()]]$mask$bounds$tempExtract)]
    sliderInput("sliderTemp", label = "Select lower and upper bounds",
                min = 0,
                max = 100,
                value = c(minV, 100),
                step = 0.1)
  })

  observeEvent(input$goTempMask, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$mask$bounds)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                             "Get first masking values in previous step (**).")
      return()
    }
    # FUNCTION CALL
    doTempExtract <-
      mask_tempExtract(
        lowerInp = sliderTemp()[1], upperInp = sliderTemp()[2],
        maskRaster = spp[[curSp()]]$postProc$rasters[[selTempMask()]],
        pred = spp[[curSp()]]$postProc$prediction, logger)

    logger %>% writeLog(hlSpp(curSp()), "The prediction was masked (**)")

    # LOAD INTO SPP ####
    spp[[curSp()]]$postProc$prediction <- doTempExtract
    spp[[curSp()]]$mask$prediction <- doTempExtract
    spp[[curSp()]]$mask$tempLog <- TRUE

    common$update_component(tab = "Map")

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

mask_temp_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("boundsPrint"))
}

mask_temp_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  bgShpXY <- common$bgShpXY

  req(spp[[curSp()]]$mask$tempLog)
  userRaster <- spp[[curSp()]]$postProc$prediction
  xy.raster <- dismo::randomPoints(userRaster, 1000)
  userValues <- raster::extract(userRaster, as.matrix(xy.raster))

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

mask_temp_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_temp_knit = species$rmm$code$wallace$someFlag,
    var1 = species$rmm$code$wallace$someSetting1,
    var2 = species$rmm$code$wallace$someSetting2
  )
}

