
userBiasFile_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userBiasFile"), label = "Upload BiasFile"),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

userBiasFile_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # if (is.null(occs())) {
    #   shinyLogs %>% writeLog(type = 'error', "Before obtaining environmental variables,
    #                          obtain occurrence data in component 1.")
    #   return()
    # }
    if (is.null(input$userBiasFile)) { # CM: not sure where I specify the value of this
      shinyLogs %>% writeLog(type = 'error', "User bias raster file not uploaded. File name not valid.")
      return()
    }

    # CM: note we're using a generic raster reading function
    biasLayer <- envs_userEnvs(rasPath = input$userBiasFile$datapath,
                             rasName = input$userBiasFile$name)

    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for(sp in spLoop) {
      # And this does bias file-specific checks
      biasLayer = samp_userBiasFileCheck(biasLayer,spp[[sp]]$envs)

      # get environmental variable values per occurrence record
      withProgress(message = paste0("Extracting bias values for occurrences of ", sp, "..."), {
        occsBiasVals <- as.data.frame(raster::extract(biasLayer, spp[[sp]]$occs[c('longitude', 'latitude')]))
      })
      # remove occurrence records with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsBiasVals, curSp(), shinyLogs)
      # also remove variable value rows with NA environmental values
      occsEnvsVals <- na.omit(occsBiasVals)

      # LOAD INTO SPP ####
      spp[[sp]]$envs <- biasLayer
      # add columns for env variable values for each occurrence record
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsBiasVals)

      # METADATA ####
      spp[[sp]]$rmm$modeling$maxent$samplingBiasRule <- 'offset'
      spp[[sp]]$rmm$data$environment$sources <- 'user'
    }

  })
}


mapBias_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  req(mapPred())
  mapPredVals <- spp[[curSp()]]$visualization$mapPredVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), mapPredVals, na.color='transparent')
  rasPal <- colorNumeric(rasCols, mapPredVals, na.color='transparent')
  map %>% clearAll() %>%
    addLegend("bottomright", pal = legendPal, title = "Sampling Bias",
              values = mapPredVals, layerId = "train",
              labFormat = reverseLabels(2, reverse_order=TRUE))

  # map model prediction raster
  map %>%
    map_occs(occs()) %>%
    addRasterImage(mapPred(), colors = rasPal, opacity = 0.7,
                   group = 'vis', layerId = 'mapPred', method = "ngb") %>%
    # add background polygon(s)
    mapBgPolys(bgShpXY())
}

userOccs_INFO <- infoGenerator(modName = "User-specified Bias File",
                               modAuts = "Cory Merow",
                               pkgName = NULL)
