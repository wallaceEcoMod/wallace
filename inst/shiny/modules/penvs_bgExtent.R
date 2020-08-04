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
                          value = 1, min = 0, step = 0.5)),  # Check default (value = 0)
    tags$div(
      title = "Add Batch guidance text here (**)",
      checkboxInput(ns("batch1"), label = strong("Batch"), value = TRUE) # Check default (value = FALSE)
    ),
    actionButton(ns("goBgExt"), "Select"),  # Check default (value = FALSE)
    tags$hr(),
    span("Step 2:", class = "step"),
    span("Sample Background Points", class = "stepText"), br(), br(),
    strong(paste0('Mask predictor rasters by background extent and sample',
                  ' background points')), br(), br(),
    numericInput(ns("bgPtsNum"), label = "No. of background points",
                 value = 10000, min = 1, step = 1), # Check default (value = 10000)
    tags$div(
      title = "Add Batch guidance text here (**)",
      checkboxInput(ns("batch2"), label = strong("Batch"), value = TRUE) # Check default (value = FALSE)
    ),
    actionButton(ns("goBgMask"), "Sample")
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
    # ERRORS ####
    if (is.null(envs())) {
      logger %>% writeLog(type = 'error', hlSpp(curSp()), 'Environmental variables missing.',
                          '. Obtain them in component 3.')
      return()
    }
    req(curSp(), occs())

    # loop over all species if batch is on
    if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for (sp in spLoop) {
      # FUNCTION CALL ####
      bgExt <- penvs_bgExtent(spp[[sp]]$occs, input$bgSel, input$bgBuf, logger,
                              spN = sp)
      req(bgExt)

      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgExt <- bgExt

      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$backgroundSampleSizeRule <-
        paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')

      ##Creating these to facilitate RMD generation
      spp[[sp]]$rmd$bgSel <- input$bgSel
      spp[[sp]]$rmd$bgBuf <- input$bgBuf

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
                   "This can sometimes happen for points on the margin of the study extent.",
                   " Please increase the buffer slightly to include them.")
        return()
      }

      # LOAD INTO SPP ####
      spp[[sp]]$procEnvs$bgMask <- bgMask
      # add columns for env variables beginning with "envs_" to bg tbl
      spp[[sp]]$bg <- cbind(scientific_name = paste0("bg_", sp), bgPts,
                            occID = NA, year = NA, institution_code = NA, country = NA,
                            state_province = NA, locality = NA, elevation = NA,
                            record_type = NA, bgEnvsVals)
      # sample background points
      spp[[sp]]$bgPts <- bgPts

      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$backgroundSampleSizeSet <- input$bgPtsNum
    }
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
    penvs_bgExtent_knit = !is.null(species$rmd$bgSel),
    # penvs_bgExtent_knit = species$rmm$code$wallace$someFlag,
    bgPtsNum_rmd = species$rmm$data$occurrence$backgroundSampleSizeSet,
    bgSel_rmd = species$rmd$bgSel,
    bgBuf_rmd = species$rmd$bgBuf
  )
}
