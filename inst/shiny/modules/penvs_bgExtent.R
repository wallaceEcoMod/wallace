penvs_bgExtent_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose Background Extent", class = "stepText"), br(), br(),
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("bounding box",
                                "minimum convex polygon",
                                "point buffers")),
    tags$div(title = paste0('Buffer area in degrees (1 degree = ~111 km). ',
                            'Exact length varies based on latitudinal position.'),
             numericInput(ns("bgBuf"),
                          label = "Study region buffer distance (degree)",
                          value = 0, min = 0, step = 0.5)),  # Check default (value = 0)
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch1"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goBgExt"), "Select"),
    tags$hr(class = "hrDotted"),
    span("Step 2:", class = "step"),
    span("Sample Background Points", class = "stepText"), br(), br(),
    strong(paste0('Mask predictor rasters by background extent and sample',
                  ' background points')), br(), br(),
    numericInput(ns("bgPtsNum"), label = "No. of background points",
                 value = 10000, min = 1, step = 1), # Check default (value = 10000)
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch2"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goBgMask"), "Sample"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goReset_penvs"), "Reset", class = 'butReset'),
    strong(" background")
  )
}

penvs_bgExtent_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  envs.global <- common$envs.global
  envs <- common$envs
  bgExt <- common$bgExt
  occs <- common$occs

  observeEvent(input$goBgExt, {
    common$update_component(tab = "Map")
    req(curSp(), occs())

    # loop over all species if batch is on
    if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()


    for (sp in spLoop) {
      # ERRORS ####
      if (is.null(spp[[sp]]$envs)) {
        logger %>% writeLog(
          type = 'error',
          hlSpp(sp),
          'Environmental variables missing. Obtain them in component 3.')
        return()
      }
      # FUNCTION CALL ####
      bgExt <- penvs_bgExtent(spp[[sp]]$occs, input$bgSel, input$bgBuf, logger,
                              spN = sp)
      req(bgExt)

      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- bgExt

      # REFERENCES ####
      knitcitations::citep(citation("rgeos"))
      knitcitations::citep(citation("sp"))

      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$backgroundSampleSizeRule <-
        paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')

      ##Creating these to facilitate RMD generation
      spp[[sp]]$rmm$code$wallace$bgSel <- input$bgSel
      spp[[sp]]$rmm$code$wallace$bgBuf <- input$bgBuf

      # spp[[sp]]$rmm$wallace$bgSel <- input$bgSel
      # spp[[sp]]$rmm$wallace$bgBuf <- input$bgBuf
    }
  })

  observeEvent(input$goBgMask, {
    # WARNING ####
    if (input$bgPtsNum < 1) {
      logger %>% writeLog(type = 'warning',
                          "Enter a non-zero number of background points.")
      return()
    }
    req(bgExt())
    # loop over all species if batch is on
    if (input$batch2 == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for (sp in spLoop) {
      # FUNCTION CALL ####
      bgMask <- penvs_bgMask(spp[[sp]]$occs, envs.global[[spp[[sp]]$envs]],
                             spp[[sp]]$procEnvs$bgExt, logger, spN = sp)
      req(bgMask)
      bgNonNA <- raster::ncell(bgMask) - raster::freq(bgMask, value = NA)[[1]]
      if ((bgNonNA + 1) < input$bgPtsNum) {
        logger %>%
          writeLog(
            type = "error", hlSpp(sp),
            "Number of requested background points (n = ", input$bgPtsNum, ") is ",
            "higher than the maximum points available on the background extent ",
            "(n = ", bgNonNA, "). Please reduce the number of requested points.")
        return()
      }
      bgPts <- penvs_bgSample(spp[[sp]]$occs, bgMask, input$bgPtsNum, logger,
                              spN = sp)
      req(bgPts)
      withProgress(
        message = paste0("Extracting background values for ", spName(sp), "..."), {
          bgEnvsVals <- as.data.frame(raster::extract(bgMask, bgPts))
        })

      NApoints <- sum(rowSums(is.na(raster::extract(bgMask, spp[[sp]]$occs[ , c("longitude", "latitude")]))))
      if (NApoints > 0) {
        logger %>%
          writeLog(type = "error", hlSpp(sp),
                   "One or more occurrence points have NULL raster values.",
                   " This can sometimes happen for points on the margin of the study extent.",
                   " Please increase the buffer slightly to include them.")
        return()
      }

      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgMask <- bgMask

      # add columns for env variables beginning with "envs_" to bg tbl
      spp[[sp]]$bg <- cbind(scientific_name = paste0("bg_", sp), bgPts,
                            country = NA, state_province = NA, locality = NA,
                            year = NA, record_type = NA, catalog_number = NA,
                            institution_code = NA, elevation = NA,
                            uncertainty = NA, bgEnvsVals)
      # sample background points
      spp[[sp]]$bgPts <- bgPts

      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$backgroundSampleSizeSet <- input$bgPtsNum
    }
    common$update_component(tab = "Map")
  })

  # reset background button functionality
  observeEvent(input$goReset_penvs, {
    req(curSp())
    spp[[curSp()]]$procEnvs$bgExt <- NULL
    spp[[curSp()]]$procEnvs$bgMask <- NULL
    spp[[curSp()]]$bg <- NULL
    spp[[curSp()]]$bgPts <- NULL
    spp[[curSp()]]$rmm$data$occurrence$backgroundSampleSizeSet <- NULL
    logger %>% writeLog(
      hlSpp(curSp()), "Reset background extent and background points.")
  })

  return(list(
    save = function() {
      list(
        bgSel = input$bgSel,
        bgBuf = input$bgBuf,
        bgPtsNum = input$bgPtsNum
      )
    },
    load = function(state) {
      # Load
      updateRadioButtons(session, "bgSel", selected = state$bgSel)
      updateNumericInput(session, "bgBuf", value = state$bgBuf)
      updateNumericInput(session, "bgPtsNum", value = state$bgPtsNum)
    }
  ))
  common$update_component(tab = "Map")
}

penvs_bgExtent_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- common$occs

  if (is.null(spp[[curSp()]]$procEnvs$bgExt)) {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
  } else {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop)
    polys <- spp[[curSp()]]$procEnvs$bgExt@polygons[[1]]@Polygons
    if (length(polys) == 1) {
      xy <- list(polys[[1]]@coords)
    } else {
      xy <- lapply(polys, function(x) x@coords)
    }
    for (shp in xy) {
      map %>%
        addPolygons(lng = shp[, 1], lat = shp[, 2], weight = 4, color = "gray",
                    group = 'bgShp')
    }
    bb <- spp[[curSp()]]$procEnvs$bgExt@bbox
    map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
  }
}

## Original idea for RMD
# bgExtent_RMD <- function(sp) {
#   list(bgSel = spp[[sp]]$rmm$wallace$bgSel,
#        bgBuf = spp[[sp]]$rmm$wallace$bgBuf)
# }
penvs_bgExtent_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    penvs_bgExtent_knit = !is.null(species$rmm$code$wallace$bgSel),
    # penvs_bgExtent_knit = species$rmm$code$wallace$someFlag,
    bgPtsNum_rmd = species$rmm$data$occurrence$backgroundSampleSizeSet,
    bgSel_rmd = species$rmm$code$wallace$bgSel,
    bgBuf_rmd = species$rmm$code$wallace$bgBuf
  )
}
