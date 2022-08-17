mask_temp_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Select prediction to mask (**)", class = "stepText"), br(),
    uiOutput(ns("maskTempUI")),
    actionButton(ns('goSelMaskPrTemp'), "Select (**)"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Upload Temporal Rasters (**)", class = "stepText"), br(), br(),
    fileInput(ns("tempRasters"),
              label = "Upload environmental rasters for masking distribution map",
              multiple = TRUE),
    actionButton(ns("goTempRasters"), "Load (**)", class = "tempRast"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 3:", class = "step"),
    span("Bounds (**)", class = "stepText"), br(),
    uiOutput(ns("yearInputUI")),
    uiOutput(ns("curTempRastersUI")),
    actionButton(ns('goAnnotate'), "Get Bounds (**)"), br(),
    tags$hr(class = "hrDotted"),
    span("Step 4:", class = "step"),
    span("Mask (**)", class = "stepText"), br(),
    uiOutput(ns("curMaskRasterUI")),
    p("Provide lower and/or upper bound values for masking"),
    uiOutput(ns("sliderMaskUI")),
    actionButton(ns('goTempMask'), "Mask (**)"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goReset_mask"), "Reset", class = 'butReset'),
    strong(" prediction")
  )
}

mask_temp_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  logger <- common$logger
  occs <- common$occs
  selTempRaster <- common$selTempRaster
  yearInput <- common$yearInput
  selTempMask <- common$selTempMask
  sliderTemp <- common$sliderTemp
  selMaskPrTemp <- common$selMaskPrTemp

  output$maskTempUI <- renderUI({
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
    if (!is.null(spp[[curSp()]]$mask$prediction)) {
      n <- c("Masked SDM", n)
    }
    if (is.null(n)) {
      p("Perform or upload model prediction (**)")
    } else {
      if ("Masked SDM" %in% n) {
        defSel <- NULL
      } else {
        defSel <- "Masked SDM"
      }
      maskPredList <- setNames(as.list(n), n)
      shinyWidgets::pickerInput("selMaskPrTemp",
                                label = "",
                                choices = maskPredList,
                                multiple = FALSE,
                                selected = defSel)
    }
  })

  observeEvent(input$goSelMaskPrTemp, {
    if (selMaskPrTemp() == "Wallace SDM") {
      spp[[curSp()]]$mask$origPred <- spp[[curSp()]]$visualization$mapPred
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$procEnvs$bgExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "Wallace SDM selected for masking (**).")
    }
    if (selMaskPrTemp() == "Transferred SDM") {
      spp[[curSp()]]$mask$origPred <-  spp[[curSp()]]$transfer$mapXfer
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$transfer$xfExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "Transferred SDM selected for masking (**).")

    }
    if (selMaskPrTemp() == "User-specified SDM") {
      spp[[curSp()]]$mask$origPred <-  spp[[curSp()]]$mask$userSDM
      spp[[curSp()]]$mask$origPolyExt <- spp[[curSp()]]$mask$userPolyExt
      spp[[curSp()]]$mask$prediction <- NULL
      spp[[curSp()]]$mask$polyExt <- NULL
      logger %>% writeLog(
        hlSpp(curSp()), "User-specified SDM selected for masking (**).")
    }
  })

  observeEvent(input$goTempRasters, {
    curSp <- common$curSp
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error', "Upload some occs or upload/select prediction (**).")
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
    spp[[curSp()]]$mask$rasters <- ppRasters
  })

  output$curTempRastersUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$rasters)
    if(!is.null(spp[[curSp()]]$mask$rasters)) {
      n <- c(names(spp[[curSp()]]$mask$rasters))
    } else {
      n <- NULL
    }
    ppRastersNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("selTempRaster",
                              label = "Select/deselect environmental rasters",
                              choices = ppRastersNameList,
                              multiple = TRUE,
                              selected = NULL,
                              options = list(`actions-box` = TRUE))
  })

  observeEvent(input$goAnnotate, {
    if (is.null(occs())) {
      logger %>% writeLog(type = 'error', (curSp()), "Upload occs (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$mask$origPred)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Select/Upload SDM prediction (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$mask$rasters)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Raster files not uploaded.")
      return()
    }
    if (is.null(yearInput())) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Select occurrences and rasters years (**)")
      return()
    }
    if (is.null(selTempRaster())) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Select temporal rasters (**).")
      return()
    }
    if (length(yearInput()) != length(selTempRaster())) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                          "Numbers of selected years and raster are not equal (**).")
      return()
    }
    smartProgress(
      logger,
      message = paste0("Preparing rasters for extraction... (**)"),
      {
        # Prepare rasters
        env <- raster::stack(spp[[curSp()]]$mask$rasters[[selTempRaster()]])
        # crop climate data to study region
        env <- raster::crop(env, spp[[curSp()]]$mask$origPred)
        # Prepare year vector
        occs_subset <- occs()
        occs_subset <- occs_subset[occs_subset$year %in% as.numeric(yearInput()), ]
      }
    )

    # FUNCTION CALL
    tempExtract <- mask_tempAnnotate(occs = occs_subset,
                                     env = env,
                                     envDates = yearInput(),
                                     logger)

    logger %>% writeLog(hlSpp(curSp()), "Values were extracted (**)")

    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$bounds <- as.data.frame(tempExtract)
    common$update_component(tab = "Results")
  })

  output$yearInputUI <- renderUI({
    req(curSp(), occs())
    occs_table <- occs()
    years <- unique(occs_table$year) %>% na.omit() %>% sort()
    shinyWidgets::pickerInput("yearInput",
                              label = "Select years",
                              choices = setNames(as.list(years), years),
                              multiple = TRUE,
                              selected = NULL,
                              options = list(`actions-box` = TRUE))
  })

  output$boundsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$mask$bounds)
    spp[[curSp()]]$mask$bounds
  })

  output$curMaskRasterUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$bounds)
    if(!is.null(spp[[curSp()]]$mask$rasters)) {
      n <- c(names(spp[[curSp()]]$mask$rasters))
    } else {
      n <- NULL
    }
    ppRastersNameList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput("selTempMask",
                              label = "Select raster for masking the SDM",
                              choices = ppRastersNameList,
                              multiple = FALSE,
                              selected = NULL)
  })

  output$sliderMaskUI <- renderUI({
    req(curSp(), spp[[curSp()]]$mask$bounds)
    minV <- spp[[curSp()]]$mask$bounds$tempExtract[1]
    maxV <- spp[[curSp()]]$mask$bounds$tempExtract[length(spp[[curSp()]]$mask$bounds$tempExtract)]
    sliderInput("sliderTemp", label = "Select lower and upper bounds",
                min = 0,
                max = 100,
                value = c(minV, maxV),
                step = 0.1)
  })

  observeEvent(input$goTempMask, {
    # ERRORS ####
    if (is.null(spp[[curSp()]]$mask$bounds)) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()),
                             "Get first masking values in previous step (**).")
      return()
    }
    if (is.null(spp[[curSp()]]$mask$prediction)) {
      maskPred <- spp[[curSp()]]$mask$origPred
    } else {
      maskPred <- spp[[curSp()]]$mask$prediction
    }

    # FUNCTION CALL
    doTempExtract <-
      mask_tempExtract(
        lowerInp = sliderTemp()[1], upperInp = sliderTemp()[2],
        maskRaster = spp[[curSp()]]$mask$rasters[[selTempMask()]],
        pred = maskPred, logger)

    logger %>% writeLog(hlSpp(curSp()), "The prediction was masked (**)")
    maskThr <- terra::spatSample(x = terra::rast(maskPred),
                                 size = 100, na.rm = TRUE)[, 1]
    maskThr <- !any(maskThr > 0 & maskThr < 1)
    # LOAD INTO SPP ####
    spp[[curSp()]]$mask$maskThr <- maskThr
    spp[[curSp()]]$mask$prediction <- doTempExtract
    if (is.null(spp[[curSp()]]$mask$polyExt)) {
      spp[[curSp()]]$mask$polyExt <- spp[[curSp()]]$mask$origPolyExt
    }
    common$update_component(tab = "Map")

  })

  # Reset prediction
  observeEvent(input$goReset_mask, {
    req(curSp())
    spp[[curSp()]]$mask$prediction <- NULL
    spp[[curSp()]]$mask$polyExt <- NULL
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

mask_temp_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("boundsPrint"))
}

mask_temp_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  bgPostXY <- common$bgPostXY

  req(spp[[curSp()]]$mask$origPred)

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
                           opacity = 0.7, group = 'mask',
                           layerId = 'postPred') %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(Mask) (**)",
                values = quanRas, layerId = "expert",
                labFormat = reverseLabel(2, reverse_order = TRUE))
  }
}

mask_temp_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  # list(
  #   mask_temp_knit = FALSE
    # var1 = species$rmm$code$wallace$someSetting1,
    # var2 = species$rmm$code$wallace$someSetting2
  # )
}

