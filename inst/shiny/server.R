function(input, output, session) {
  ########################## #
  # REACTIVE VALUES LISTS ####
  ########################## #

  # single species list of lists
  spp <- reactiveValues()
  envs.global <- reactiveValues()

  # Variable to keep track of current log message
  initLogMsg <- function() {
    intro <- '***WELCOME TO WALLACE***'
    brk <- paste(rep('------', 14), collapse = '')
    expl <- 'Please find messages for the user in this log window.'
    logInit <- gsub('.{4}$', '', paste(intro, brk, expl, brk, '', sep = '<br>'))
    logInit
  }
  logger <- reactiveVal(initLogMsg())

  # Write out logs to the log Window
  observeEvent(logger(), {
    shinyjs::html(id = "logHeader", html = logger(), add = FALSE)
    shinyjs::js$scrollLogger()
  })

  # tab and module-level reactives
  component <- reactive({
    input$tabs
  })
  module <- reactive({
    if (component() == "intro") "intro"
    else input[[glue("{component()}Sel")]]
  })

  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #

  # UI for component guidance text
  output$gtext_component <- renderUI({
    file <- file.path('Rmd', glue("gtext_{component()}.Rmd"))
    if (!file.exists(file)) return()
    includeMarkdown(file)
  })

  # UI for module guidance text
  output$gtext_module <- renderUI({
    file <- COMPONENT_MODULES[[component()]][[module()]]$instructions
    if (is.null(file)) return()
    includeMarkdown(file)
  })

  ######################## #
  ### MAPPING LOGIC ####
  ######################## #

  # create map
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      leafem::addMouseCoordinates()
  )

  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")

  # change provider tile option
  observe({
    map %>% addProviderTiles(input$bmap)
  })

  # logic for recording the attributes of drawn polygon features
  observeEvent(input$map_draw_new_feature, {
    req(curSp())
    coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
    xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
    colnames(xy) <- c('longitude', 'latitude')
    id <- input$map_draw_new_feature$properties$`_leaflet_id`

    if(component() == 'poccs') {
      spp[[curSp()]]$polySelXY <- xy
      spp[[curSp()]]$polySelID <- id
    }
    if(component() == 'penvs') {
      spp[[curSp()]]$polyExtXY <- xy
      spp[[curSp()]]$polyExtID <- id
    }
    if(component() == 'proj') {
      spp[[curSp()]]$polyPjXY <- xy
      spp[[curSp()]]$polyPjID <- id
    }
    # UI CONTROLS - for some reason, curSp() disappears here unless input is updated
    updateSelectInput(session, "curSp", selected = curSp())
  })

  # Call the module-specific map function for the current module
  observe({
    # must have one species selected and occurrence data
    req(length(curSp()) == 1, occs(), module())
    map_fx <- COMPONENT_MODULES[[component()]][[module()]]$map_function
    if (!is.null(map_fx)) {
      do.call(map_fx, list(map, common = common))
    }
  })

  ######################## #
  ### BUTTONS LOGIC ####
  ######################## #

  # Enable/disable buttons
  observe({
    req(length(curSp()) == 1)
    shinyjs::toggleState("dlDbOccs", !is.null(occs()))
    shinyjs::toggleState("dlOccs", !is.null(occs()))
    shinyjs::toggleState("dlAllOccs", length(allSp()) > 1)
    shinyjs::toggleState("dlRMD", !is.null(occs()))
    shinyjs::toggleState("dlGlobalEnvs", !is.null(spp[[curSp()]]$envs))
    shinyjs::toggleState("dlProcOccs",
                         !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords) |
                           !is.null(spp[[curSp()]]$procOccs$occsThin) |
                           !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs))
    shinyjs::toggleState("dlMskEnvs", !is.null(spp[[curSp()]]$procEnvs$bgMask))
    shinyjs::toggleState("dlBgPts", !is.null(spp[[curSp()]]$bgPts))
    shinyjs::toggleState("dlBgShp", !is.null(spp[[curSp()]]$procEnvs$bgExt))
    shinyjs::toggleState("dlPart", ("partition" %in% colnames(spp[[curSp()]]$occs)))
    shinyjs::toggleState("dlEvalTbl", !is.null(evalOut()))
    shinyjs::toggleState("dlVisBioclim", !is.null(spp[[curSp()]]$rmm$modelFit$bioclim$notes))
    shinyjs::toggleState("dlMaxentPlots", !is.null(spp[[curSp()]]$rmm$modelFit$maxent$notes))
    shinyjs::toggleState("dlRespCurves", !is.null(spp[[curSp()]]$rmm$modelFit$maxent$notes))
    shinyjs::toggleState("dlPred", !is.null(spp[[curSp()]]$visualization$occPredVals))
    shinyjs::toggleState("dlPjShp", !is.null(spp[[curSp()]]$project$pjExt))
    shinyjs::toggleState("dlProj", !is.null(spp[[curSp()]]$project$pjEnvs))
    shinyjs::toggleState("dlMess", !is.null(spp[[curSp()]]$project$messVals))
    # shinyjs::toggleState("dlWhatever", !is.null(spp[[curSp()]]$whatever))
  })

  # # # # # # # # # # # # # # # # # #
  # OBTAIN OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #

  output$curSpUI <- renderUI({
    # check that a species is in the list already -- if not, don't proceed
    req(length(reactiveValuesToList(spp)) > 0)
    # get the species names
    n <- names(spp)[order(names(spp))]
    # remove multispecies names from list
    n <- n[!grepl(".", n, fixed = TRUE)]
    # if no current species selected, select the first name
    # NOTE: this line is necessary to retain the selection after selecting different tabs
    if(!is.null(curSp())) selected <- curSp() else selected <- n[1]
    # if espace component, allow for multiple species selection
    if(component() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    # make a named list of their names
    sppNameList <- c(list("Current species" = ""), setNames(as.list(n), n))
    # generate a selectInput ui that lists the available species
    selectizeInput('curSp', label = NULL , choices = sppNameList,
                   multiple = TRUE, selected = selected, options = options)
  })

  curSp <- reactive(input$curSp)

  # vector of all species with occurrence data loaded
  allSp <- reactive(names(reactiveValuesToList(spp))[!grepl("\\.", names(reactiveValuesToList(spp)))])

  # convenience function for occurrence table for current species
  occs <- reactive(spp[[curSp()]]$occs)
  # convenience function for metadata list for current species
  rmm <- reactive(spp[[curSp()]]$rmm)

  # TABLE
  # options <- list(autoWidth = TRUE, columnDefs = list(list(width = '40%', targets = 7)),
  #                 scrollX=TRUE, scrollY=400)
  output$occTbl <- DT::renderDataTable({
    # check if spp has species in it
    req(length(reactiveValuesToList(spp)) > 0)
    occs() %>%
      dplyr::mutate(occID = as.numeric(occID),
                    longitude = round(as.numeric(longitude), digits = 2),
                    latitude = round(as.numeric(latitude), digits = 2)) %>%
      dplyr::select(-pop) %>%
      dplyr::arrange(occID)
  }, rownames = FALSE, options = list(scrollX = TRUE))

  # DOWNLOAD: current species occurrence data table
  output$dlOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(curSp())
      source <- rmm()$data$occurrence$sources
      glue("{n}_{source}.csv")
    },
    content = function(file) {
      tbl <- occs() %>%
        dplyr::select(-c(pop, occID))
      # if bg values are present, add them to table
      if(!is.null(bg())) {
        tbl <- rbind(tbl, bg())
      }
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )

  # DOWNLOAD: all species occurrence data table
  output$dlAllOccs <- downloadHandler(
    filename = function(){"multispecies_ocurrence_table.csv"},
    content = function(file) {
      l <- lapply(allSp(), function(x) {
        data.frame(spp[[x]]$occData$occsCleaned, stringsAsFactors = FALSE)
        })
      tbl <- dplyr::bind_rows(l)
      tbl <- tbl %>% dplyr::select(-pop)
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )

  # DOWNLOAD: occsOrig
  output$dlDbOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(curSp())
      source <- rmm()$data$occurrence$sources
      glue("{n}_{source}_raw.csv")
    },
    content = function(file) {
      write_csv_robust(spp[[curSp()]]$occData$occsOrig, file, row.names = FALSE)
    }
  )

  ############################################# #
  ### COMPONENT: OBTAIN ENVIRONMENTAL DATA ####
  ############################################# #

  # # # # # # # # # # # # # # # # # #
  # OBTAIN ENVS: other controls ####
  # # # # # # # # # # # # # # # # # #

  bcSel <- reactive(input$bcSel)
  ecoClimSel <- reactive(input$ecoClimSel)
  VarSelector <- reactive(input$VarSelector)
  # shortcut to currently selected environmental variable, read from curEnvUI
  curEnv <- reactive(input$curEnv)

  # convenience function for environmental variables for current species
  envs <- reactive({
    req(curSp(), spp[[curSp()]]$envs)
    envs.global[[spp[[curSp()]]$envs]]
  })

  # map center coordinates for 30 arcsec download
  mapCntr <- reactive({
    round(as.numeric(input$map_center), digits = 3)
  })

  # CONSOLE PRINT
  output$envsPrint <- renderPrint({
    req(envs())
    envs()
  })

  output$dlGlobalEnvs <- downloadHandler(
    filename = function() paste0(spp[[curSp()]]$envs, '_envs.zip'),
    content = function(file) {
      withProgress(
        message = paste0("Preparing ", paste0(spp[[curSp()]]$envs, '_envs.zip ...')), {
        tmpdir <- tempdir()
        owd <- setwd(tmpdir)
        on.exit(setwd(owd))
        type <- input$globalEnvsFileType
        nm <- names(envs.global[[spp[[curSp()]]$envs]])

        raster::writeRaster(envs.global[[spp[[curSp()]]$envs]], nm, bylayer = TRUE,
                            format = type, overwrite = TRUE)
        ext <- switch(type, raster = 'grd', ascii = 'asc', GTiff = 'tif')

        fs <- paste0(nm, '.', ext)
        if (ext == 'grd') {
          fs <- c(fs, paste0(nm, '.gri'))
        }
        zip::zipr(zipfile = file, files = fs)
        if (file.exists(paste0(file, ".zip"))) file.rename(paste0(file, ".zip"), file)
      })
    },
    contentType = "application/zip"
  )

  ########################################### #
  ### COMPONENT: PROCESS OCCURRENCE DATA ####
  ########################################### #

  # # # # # # # # # # # # # # # # # # # #
  # module Profile Occurrences ####
  # # # # # # # # # # # # # # # # # # # #
  # CM: start comment
  # observeEvent(input$goProfileOccs, {
  #   profileOccs <- callModule(profileOccs_MOD, 'poccs_profileOccs_uiID')
  #   profileOccs()
  # })
  #
  # observeEvent(input$goProfileOccsClean, {
  #   profileOccsClean <- callModule(profileOccsClean_MOD, 'poccs_profileOccsClean_uiID')
  #   profileOccsClean()
  # })
  # CM: end comment

  # # # # # # # # # # # # # # # # # #
  # PROCESS OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #

  # reset occurrences button functionality
  observeEvent(input$goResetOccs, {
    req(curSp())
    spp[[curSp()]]$occs <- spp[[curSp()]]$occData$occsCleaned
    spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords <- NULL
    spp[[curSp()]]$procOccs$occsThin <- NULL
    spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs <- NULL
    shinyLogs %>% writeLog("Reset occurrences for ",
                           em(spName(curSp())), ".")
    # MAPPING
    map %>%
      map_occs(occs()) %>%
      zoom2Occs(occs())
  })

  # DOWNLOAD: current processed occurrence data table
  output$dlProcOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(spName(spp[[curSp()]]))
      glue("{n}_processed_occs.csv")
    },
    content = function(file) {
      tbl <- occs() %>% dplyr::select(-pop)
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )

  ############################################## #
  ### COMPONENT: PROCESS ENVIRONMENTAL DATA ####
  ############################################## #

  # # # # # # # # # # # # # # # # # #
  # PROCESS ENVS: other controls ####
  # # # # # # # # # # # # # # # # # #

  # convenience function for background points table for current species
  bg <- reactive(spp[[curSp()]]$bg)
  # convenience function for background polygon for current species
  bgExt <- reactive(spp[[curSp()]]$procEnvs$bgExt)
  # convenience function for environmental variable rasters masked to background for current species
  bgMask <- reactive(spp[[curSp()]]$procEnvs$bgMask)

  # get the coordinates of the current background extent shape
  bgShpXY <- reactive({
    req(bgExt())
    polys <- bgExt()@polygons[[1]]@Polygons
    if(length(polys) == 1) {
      xy <- list(polys[[1]]@coords)
    }else{
      xy <- lapply(polys, function(x) x@coords)
    }
    return(xy)
  })

  # DOWNLOAD: masked environmental variable rasters
  output$dlBgShp <- downloadHandler(
    filename = function() glue("{formatSpName(curSp())}_bgShp.zip"),
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      n <- spName(spp[[curSp()]])

      rgdal::writeOGR(obj = bgExt(),
                      dsn = tmpdir,
                      layer = paste0(n, '_bgShp'),
                      driver = "ESRI Shapefile",
                      overwrite_layer = TRUE)

      exts <- c('dbf', 'shp', 'shx')
      fs <- paste0(n, '_bgShp.', exts)
      zip::zipr(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )

  # DOWNLOAD: masked environmental variable rasters
  output$dlMskEnvs <- downloadHandler(
    filename = function() paste0(formatSpName(curSp()), '_mskEnvs.zip'),
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      type <- input$bgMskFileType
      nm <- names(envs())

      raster::writeRaster(bgMask(), nm, bylayer = TRUE,
                          format = type, overwrite = TRUE)
      ext <- switch(type, raster = 'grd', ascii = 'asc', GTiff = 'tif')

      fs <- paste0(nm, '.', ext)
      if (ext == 'grd') {
        fs <- c(fs, paste0(nm, '.gri'))
      }
      zip::zipr(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )

  # DOWNLOAD: background points table
  output$dlBgPts <- downloadHandler(
    filename = function() {
      n <- formatSpName(spName(spp[[curSp()]]))
      paste0(n, "_bgPoints.csv")
    },
    content = function(file) {
      tbl <- as.data.frame(spp[[curSp()]]$bgPts)
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )

  ############################################## #
  ### COMPONENT: SAMPLING BIAS ####
  ############################################## #

  # # # # # # # # # # # # # # # # # #
  # module User Background Data ####
  # # # # # # # # # # # # # # # # # #
  observeEvent(input$goUserBGUpload, {
    userBGUpload <- callModule(userBG_MOD, 'samp_userBiasBg_uiID')
    userBGUpload()
  })

  # # # # # # # # # # # # # # # # # #
  # module User Bias File        ####
  # # # # # # # # # # # # # # # # # #
  observeEvent(input$goBiasFileUpload, {
    userBiasFileUpload <- callModule(userBiasFile_MOD, 'samp_biasFileUpload')
    userBiasFileUpload()
  })

  # # # # # # # # # # # # # # # # # #
  # module Make Target Group ####
  # # # # # # # # # # # # # # # # # #
  observeEvent(input$goTargetDbOccs, {
    targetQueryDB <- callModule(queryDb_MOD, 'samp_queryDb_uiID', targetGroup = TRUE)
    occsList <- targetQueryDB()
    targetGroupBG <- callModule(targetGroupBG_MOD, 'samp_targetGroupBg_uiID', occsList)
  })

  ############################################## #
  ### COMPONENT: ESPACE ####
  ############################################## #


  ################################################## #
  ### COMPONENT: PARTITION OCCURRENCE DATA ####
  ################################################# #

  # download for partitioned occurrence records csv
  output$dlPart <- downloadHandler(
    filename = function() paste0(formatSpName(curSp()), "_partitioned_occs.csv"),
    content = function(file) {
      bg.bind <- data.frame(rep('background',
                                nrow(spp[[curSp()]]$bgPts)), spp[[curSp()]]$bgPts)
      names(bg.bind) <- c('scientific_name', 'longitude', 'latitude')
      occs.bg.bind <- rbind(spp[[curSp()]]$occs[, 2:4], bg.bind)
      all.bind <- cbind(occs.bg.bind, c(spp[[curSp()]]$occs$partition,
                                        spp[[curSp()]]$bg$partition))
      names(all.bind)[4] <- "group"
      write_csv_robust(all.bind, file, row.names = FALSE)
    }
  )

  ######################### #
  ### COMPONENT: MODEL ####
  ######################### #

  # # # # # # # # # # # # # # # # # #
  # MODEL: other controls ####
  # # # # # # # # # # # # # # # # # #

  # convenience function for modeling results list for current species
  evalOut <- reactive(spp[[curSp()]]$evalOut)

  # ui that populates with the names of models that were run
  output$curModelUI <- renderUI({
    # do not display until both current species is selected and it has a model
    req(curSp(), length(curSp()) == 1, evalOut())
    # if
    if(!is.null(evalOut())) {
      n <- names(evalOut()@models)
    } else {
      n <- NULL
    }

    modsNameList <- c(list("Current model" = ""), setNames(as.list(n), n))
    options <- list(maxItems = 1)
    selectizeInput('curModel', label = NULL , choices = modsNameList,
                   multiple = TRUE, selected = n[1], options = options)
  })

  # shortcut to currently selected model, read from modSelUI
  curModel <- reactive(input$curModel)

  # picker option to select categorical variables
  output$catEnvs <- renderUI({
    req(curSp(), occs(), envs())
    if(!is.null(envs())) {
      n <- c(names(envs()))
    } else {
      n <- NULL
    }
    envList <- setNames(as.list(n), n)
    shinyWidgets::pickerInput(
      "selCatEnvs",
      label = "Select categorical variables (**)",
      choices = envList,
      multiple = TRUE)
  })

  selCatEnvs <- reactive(input$selCatEnvs)

  # download for partitioned occurrence records csv
  output$dlEvalTbl <- downloadHandler(
    filename = function() {
      if(rmm()$model$algorithm == "BIOCLIM") {
        paste0(curSp(), "_bioclim_evalTbl.csv")
      } else if(rmm()$model$algorithm == "maxent.jar") {
        paste0(curSp(), "_maxent_evalTbl.csv")
      } else if(rmm()$model$algorithm == "maxnet") {
        paste0(curSp(), "_maxnet_evalTbl.csv")
      }
    },
    content = function(file) {
      evalTbl <- evalOut()@results
      write_csv_robust(evalTbl, file, row.names = FALSE)
    }
  )


  ########################################### #
  ### COMPONENT: VISUALIZE MODEL RESULTS ####
  ########################################### #

  # # # # # # # # # # # # # # # # # #
  # VISUALIZE: other controls ####
  # # # # # # # # # # # # # # # # # #

  # convenience function for mapped model prediction raster for current species
  mapPred <- reactive(spp[[curSp()]]$visualization$mapPred)

  # handle downloads for BIOCLIM Plots png
  output$dlVisBioclim <- downloadHandler(
    filename = function() {paste0(curSp(), "_bioClimPlot.png")},
    content = function(file) {
      png(file)
      makeBioclimPlot(evalOut()@models[[curModel()]],
                      spp[[curSp()]]$rmm$code$wallaceSettings$bcPlotSettings[['bc1']],
                      spp[[curSp()]]$rmm$code$wallaceSettings$bcPlotSettings[['bc2']],
                      spp[[curSp()]]$rmm$code$wallaceSettings$bcPlotSettings[['p']])
      dev.off()
    }
  )

  # handle downloads for Maxent Plots png
  output$dlMaxentPlots <- downloadHandler(
    filename = function() {paste0(curSp(), "_evalPlots.zip")},
    content = function(file) {
      tmpdir <- tempdir()
      parEval <- c('avg.test.AUC', 'avg.diff.AUC', 'avg.test.orMTP', 'avg.test.or10pct',
                   'delta.AICc')
      for (i in parEval) {
        png(paste0(tmpdir, "\\", gsub("[[:punct:]]", "_", i), ".png"))
        makeMaxentEvalPlot(evalOut()@results, i)
        dev.off()
      }
      owd <- setwd(tmpdir)
      zip::zipr(zipfile = file,
               files = paste0(gsub("[[:punct:]]", "_", parEval), ".png"))
      setwd(owd)
    }
  )

  # handle downloads for Response Curve Plots png
  output$dlRespCurves <- downloadHandler(
    filename = function() {paste0(curSp(), "_responseCurves.zip")},
    content = function(file) {
      tmpdir <- tempdir()
      namesEnvs <- names(envs())
      for (i in namesEnvs) {
        png(paste0(tmpdir, "\\", i, ".png"))
        if (spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
          maxnet::response.plot(evalOut()@models[[curModel()]], v = i, type = "cloglog")
        } else if (spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
          dismo::response(evalOut()@models[[curModel()]], var = i)
        }
        dev.off()
      }
      owd <- setwd(tmpdir)
      zip::zipr(zipfile = file, files = paste0(namesEnvs, ".png"))
      setwd(owd)
    }
  )

  # download for model predictions (restricted to background extent)
  output$dlPred <- downloadHandler(
    filename = function() {
      ext <- switch(input$predFileType, raster = 'zip', ascii = 'asc',
                    GTiff = 'tif', png = 'png')
      thresholdRule <- rmm()$output$prediction$thresholdRule
      predType <- rmm()$output$prediction$notes
      if (thresholdRule == 'none') {
        paste0(curSp(), "_", predType, '.', ext)
      } else {
        paste0(curSp(), "_", thresholdRule, '.', ext)
      }
    },
    content = function(file) {
      if(require(rgdal)) {
        if (input$predFileType == 'png') {
          req(mapPred())
          if (rmm()$output$prediction$thresholdRule != 'none') {
            mapPredVals <- 0:1
            rasPal <- c('gray', 'blue')
            legendPal <- colorBin(rasPal, 0:1, bins = 2)
            mapTitle <- "Thresholded Suitability<br>(Training)"
            mapLabFormat <- function(type, cuts, p) {
              n = length(cuts)
              cuts[n] = "predicted presence"
              for (i in 2:(n - 1)) {
                cuts[i] = ""
              }
              cuts[1] = "predicted absence"
              paste0(cuts[-n], cuts[-1])
            }
            mapOpacity <- 1
          } else {
            rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
            mapPredVals <- spp[[curSp()]]$visualization$mapPredVals
            rasPal <- colorNumeric(rasCols, mapPredVals, na.color='transparent')
            legendPal <- colorNumeric(rev(rasCols), mapPredVals, na.color='transparent')
            mapTitle <- "Predicted Suitability<br>(Training)"
            mapLabFormat <- reverseLabels(2, reverse_order=TRUE)
            mapOpacity <- NULL
          }
          m <- leaflet() %>%
            addLegend("bottomright", pal = legendPal, title = mapTitle,
                      labFormat = mapLabFormat, opacity = mapOpacity,
                      values = mapPredVals, layerId = "train") %>%
            addProviderTiles(input$bmap) %>%
            addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                             radius = 5, color = 'red', fill = TRUE, fillColor = 'red',
                             fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
            addRasterImage(mapPred(), colors = rasPal, opacity = 0.7,
                           group = 'vis', layerId = 'mapPred', method = "ngb") %>%
            addPolygons(data = bgExt(), fill = FALSE, weight = 4, color = "blue",
                        group='proj')
          mapview::mapshot(m, file = file)
        } else if (input$predFileType == 'raster') {
          fileName <- curSp()
          tmpdir <- tempdir()
          raster::writeRaster(mapPred(), file.path(tmpdir, fileName),
                              format = input$predFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zipr(zipfile = file, files = fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(mapPred(), file, format = input$predFileType,
                                   overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        shinyLogs %>%
          writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )

  ########################################### #
  ### COMPONENT: PROJECT MODEL ####
  ########################################### #

  # # # # # # # # # # # # # # # # # #
  # PROJECT: other controls ####
  # # # # # # # # # # # # # # # # # #

  # convenience function for mapped model prediction raster for current species
  mapProj <- reactive(spp[[curSp()]]$project$mapProj)

  # Reset Projection Extent button functionality
  observeEvent(input$goResetProj, {
    map %>%
      removeShape("projExt") %>%
      removeImage("projRas") %>%
      removeControl("proj")
    spp[[curSp()]]$polyPjXY <- NULL
    spp[[curSp()]]$polyPjID <- NULL
    spp[[curSp()]]$project <- NULL
    shinyLogs %>% writeLog("Reset projection extent.")
  })

  # DOWNLOAD: Shapefile of prejection extent
  output$dlPjShp <- downloadHandler(
    filename = function() paste0(formatSpName(curSp()), '_projShp.zip'),
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      n <- spName(spp[[curSp()]])

      rgdal::writeOGR(obj = spp[[curSp()]]$project$pjExt,
                      dsn = tmpdir,
                      layer = paste0(n, '_projShp'),
                      driver = "ESRI Shapefile",
                      overwrite_layer = TRUE)

      exts <- c('dbf', 'shp', 'shx')
      fs <- paste0(n, '_projShp.', exts)
      zip::zipr(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )

  # download for model predictions (restricted to background extent)
  output$dlProj <- downloadHandler(
    filename = function() {
      ext <- switch(input$projFileType, raster = 'zip', ascii = 'asc',
                    GTiff = 'tif', png = 'png')
      thresholdRule <- rmm()$output$transfer$environment1$thresholdRule
      predType <- rmm()$output$prediction$notes
      if (thresholdRule == 'none') {
        paste0(curSp(), "_proj_", predType, '.', ext)
      } else {
        paste0(curSp(), "_proj_", thresholdRule, '.', ext)
      }
    },
    content = function(file) {
      if(require(rgdal)) {
        if (input$projFileType == 'png') {
          req(mapProj())
          if (rmm()$output$transfer$environment1$thresholdRule != 'none') {
            mapProjVals <- 0:1
            rasPal <- c('gray', 'red')
            legendPal <- colorBin(rasPal, 0:1, bins = 2)
            mapTitle <- "Thresholded Suitability<br>(Projected)"
            mapLabFormat <- function(type, cuts, p) {
              n = length(cuts)
              cuts[n] = "predicted presence"
              for (i in 2:(n - 1)) {
                cuts[i] = ""
              }
              cuts[1] = "predicted absence"
              paste0(cuts[-n], cuts[-1])
            }
            mapOpacity <- 1
          } else {
            rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
            mapProjVals <- spp[[curSp()]]$project$mapProjVals
            rasPal <- colorNumeric(rasCols, mapProjVals, na.color='transparent')
            legendPal <- colorNumeric(rev(rasCols), mapProjVals, na.color='transparent')
            mapTitle <- "Predicted Suitability<br>(Projected)"
            mapLabFormat <- reverseLabels(2, reverse_order=TRUE)
            mapOpacity <- NULL
          }
          m <- leaflet() %>%
            addLegend("bottomright", pal = legendPal, title = mapTitle,
                      labFormat = mapLabFormat, opacity = mapOpacity,
                      values = mapProjVals, layerId = "train") %>%
            addProviderTiles(input$bmap) %>%
            addRasterImage(mapProj(), colors = rasPal, opacity = 0.7,
                           group = 'vis', layerId = 'mapProj', method = "ngb") %>%
            addPolygons(lng = spp[[curSp()]]$polyPjXY[, 1],
                        lat = spp[[curSp()]]$polyPjXY[, 2], fill = FALSE,
                        weight = 4, color = "red",
                        group = 'proj')
          mapview::mapshot(m, file = file)
        } else if (input$projFileType == 'raster') {
          fileName <- curSp()
          tmpdir <- tempdir()
          raster::writeRaster(mapProj(), file.path(tmpdir, fileName),
                              format = input$projFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zipr(zipfile = file, files = fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(mapProj(), file, format = input$projFileType,
                                   overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        shinyLogs %>% writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )

  # download for mess (restricted to background extent)
  output$dlMess <- downloadHandler(
    filename = function() {
      ext <- switch(input$messFileType, raster = 'zip', ascii = 'asc',
                    GTiff = 'tif', png = 'png')
      paste0(curSp(), "_mess.", ext)
    },
    content = function(file) {
      if(require(rgdal)) {
        req(spp[[curSp()]]$project$mess, spp[[curSp()]]$polyPjXY)
        mess <- spp[[curSp()]]$project$mess
        if (input$messFileType == 'png') {
          polyPjXY <- spp[[curSp()]]$polyPjXY
          rasVals <- spp[[curSp()]]$project$messVals
          # define colorRamp for mess
          if (max(rasVals) > 0 & min(rasVals) < 0) {
            rc1 <- colorRampPalette(colors = rev(RColorBrewer::brewer.pal(n = 3, name = 'Reds')),
                                    space = "Lab")(abs(min(rasVals)))
            rc2 <- colorRampPalette(colors = RColorBrewer::brewer.pal(n = 3, name = 'Blues'),
                                    space = "Lab")(max(rasVals))
            rasCols <- c(rc1, rc2)
          } else if (max(rasVals) < 0 & min(rasVals) < 0) {
            rasCols <- colorRampPalette(colors = rev(RColorBrewer::brewer.pal(n = 3, name = 'Reds')),
                                        space = "Lab")(abs(min(rasVals)))
          } else if (max(rasVals) > 0 & min(rasVals) > 0) {
            rasCols <- colorRampPalette(colors = RColorBrewer::brewer.pal(n = 3, name = 'Blues'),
                                        space = "Lab")(max(rasVals))
          }
          legendPal <- colorNumeric(rev(rasCols), rasVals, na.color='transparent')
          rasPal <- colorNumeric(rasCols, rasVals, na.color='transparent')
          m <- leaflet() %>%
            addLegend("bottomright", pal = legendPal, title = "MESS Values",
                      labFormat = reverseLabels(2, reverse_order=TRUE),
                      values = rasVals, layerId = "train") %>%
            addProviderTiles(input$bmap) %>%
            addRasterImage(mess, colors = rasPal, opacity = 0.7,
                           group = 'vis', layerId = 'mapProj', method = "ngb") %>%
            addPolygons(lng = spp[[curSp()]]$polyPjXY[, 1],
                        lat = spp[[curSp()]]$polyPjXY[, 2], fill = FALSE,
                        weight = 4, color = "red",
                        group = 'proj')
          mapview::mapshot(m, file = file)
        } else if (input$messFileType == 'raster') {
          fileName <- curSp()
          tmpdir <- tempdir()
          raster::writeRaster(mess, file.path(tmpdir, fileName),
                              format = input$messFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zipr(zipfile = file, files = fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(mess, file, format = input$messFileType,
                                   overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        shinyLogs %>% writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )

  ########################################### #
  ### RMARKDOWN FUNCTIONALITY ####
  ########################################### #

  filetype_to_ext <- function(type = c("Rmd", "PDF", "HTML", "Word")) {
    type <- match.arg(type)
    switch(
      type,
      Rmd = '.Rmd',
      PDF = '.pdf',
      HTML = '.html',
      Word = '.docx'
    )
  }

  # handler for R Markdown download
  output$dlRMD <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), filetype_to_ext(input$rmdFileType))
    },
    content = function(file) {
      spp <- common$spp
      spAbr <-
      md_files <- c()
      md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
      rmarkdown::render("Rmd/userReport_intro.Rmd",
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = md_intro_file,
                        clean = TRUE)
      md_files <- c(md_files, md_intro_file)
      spAbr <- spAbr <- plyr::alply(abbreviate(stringr::str_replace(allSp(), "_", " "),
                                               minlength = 2),
                                    .margins = 1,
                                    function(x) {
                                      x <- as.character(x)})
      names(spAbr) <- allSp()

      for (sp in allSp()) {
        species_rmds <- NULL
        for (component in names(COMPONENT_MODULES)) {
          for (module in COMPONENT_MODULES[[component]]) {
            rmd_file <- module$rmd_file
            rmd_function <- module$rmd_function
            if (is.null(rmd_file)) next

            if (is.null(rmd_function)) {
              rmd_vars <- list()
            } else {
              rmd_vars <- do.call(rmd_function, list(species = spp[[sp]]))
            }
            knit_params <- c(
              file = rmd_file,
              spName = spName(sp),
              sp = sp,
              spAbr = spAbr[[sp]],
              rmd_vars
            )
            module_rmd <- do.call(knitr::knit_expand, knit_params)

            module_rmd_file <- tempfile(pattern = paste0(module$id, "_"),
                                        fileext = ".Rmd")
            writeLines(module_rmd, module_rmd_file)
            species_rmds <- c(species_rmds, module_rmd_file)
          }
        }

        species_md_file <- tempfile(pattern = paste0(sp, "_"),
                                    fileext = ".md")
        rmarkdown::render(input = "Rmd/userReport_species.Rmd",
                          params = list(child_rmds = species_rmds,
                                        spName = spName(sp),
                                        spAbr = spAbr[[sp]]),
                          output_format = rmarkdown::github_document(html_preview = FALSE),
                          output_file = species_md_file,
                          clean = TRUE)
        md_files <- c(md_files, species_md_file)
      }

      combined_md <-
        md_files %>%
        lapply(readLines, encoding = "UTF-8") %>%
        lapply(paste, collapse = "\n") %>%
        paste(collapse = "\n\n")

      result_file <- tempfile(pattern = "result_", fileext = filetype_to_ext(input$rmdFileType))
      if (input$rmdFileType == "Rmd") {
        combined_rmd <- gsub('``` r', '```{r}', combined_md)
        writeLines(combined_rmd, result_file, useBytes = TRUE)
      } else {
        combined_md_file <- tempfile(pattern = "combined_", fileext = ".md")
        writeLines(combined_md, combined_md_file)
        rmarkdown::render(
          input = combined_md_file,
          output_format =
            switch(
              input$rmdFileType,
              "PDF" = rmarkdown::pdf_document(),
              "HTML" = rmarkdown::html_document(),
              "Word" = rmarkdown::word_document()
            ),
          output_file = result_file,
          clean = TRUE
        )
      }

      file.rename(result_file, file)
    }
  )

  ########################################### #
  ### COMPONENT: POST-PROCESSING ####
  ########################################### #

  # user SDM
  output$userSDM_UI <- renderUI({
    req(!exists("pepe"))
    fileInput("userSDM", label = "Input SDM (**)")
    actionButton('goUserSDM', 'Load')
  })

  ################################
  ### METADATA FUNCTIONALITY ####
  ################################

  output$dlRMM <- downloadHandler(
    filename = function() {paste0("wallace-metadata-", Sys.Date(), ".zip")},
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      namesSpp <- allSp()
      for (i in namesSpp) {
        rangeModelMetadata::rmmToCSV(spp[[i]]$rmm, filename = paste0(i, "_RMM.csv"))
      }
      zip::zipr(zipfile = file, files = paste0(namesSpp, "_RMM.csv"))
      setwd(owd)
  })

  # Create a data structure that holds variables and functions used by modules
  common = list(
    # Reactive variables to pass on to modules
    logger = logger,
    spp = spp,
    curSp = curSp,
    allSp = allSp,
    curEnv = curEnv,
    curModel = curModel,
    component = component,
    module = module,
    envs.global = envs.global,
    mapCntr = mapCntr,

    # Shortcuts to values nested inside spp
    occs = occs,
    envs = envs,
    bcSel = bcSel,
    ecoClimSel = ecoClimSel,
    VarSelector = VarSelector,
    bg = bg,
    bgExt = bgExt,
    bgMask = bgMask,
    bgShpXY = bgShpXY,
    evalOut = evalOut,
    mapPred = mapPred,
    mapProj = mapProj,
    rmm = rmm,

    # Switch to a new component tab
    update_component = function(tab = c("Map", "Table", "Results", "Download")) {
      tab <- match.arg(tab)
      updateTabsetPanel(session, "main", selected = tab)
    },

    # Remove a specific module so that it will not be selectable in the UI
    remove_module = function(component = COMPONENTS, module) {
      component <- match.arg(component)
      shinyjs::js$removeModule(component = component, module = module)
    }
  )

  # Initialize all modules
  modules <- list()
  lapply(names(COMPONENT_MODULES), function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      return <- callModule(get(module$server_function), module$id, common = common)
      if (is.list(return) &&
          "save" %in% names(return) && is.function(return$save) &&
          "load" %in% names(return) && is.function(return$load)) {
        modules[[module$id]] <<- return
      }
    })
  })

  observe({
    spp_size <- as.numeric(pryr::object_size(reactiveValuesToList(spp)))
    shinyjs::toggle("save_warning", condition = (spp_size >= SAVE_SESSION_SIZE_MB_WARNING * MB))
  })

  # Save the current session to a file
  save_session <- function(file) {
    state <- list()

    spp_save <- reactiveValuesToList(spp)

    # Save general data
    state$main <- list(
      version = as.character(packageVersion("wallace")),
      spp = spp_save,
      envs_global = reactiveValuesToList(envs.global),
      cur_sp = input$curSp,
      selected_module = sapply(COMPONENTS, function(x) input[[glue("{x}Sel")]], simplify = FALSE)
    )

    # Ask each module to save whatever data it wants
    for (module_id in names(modules)) {
      state[[module_id]] <- modules[[module_id]]$save()
    }

    saveRDS(state, file)
  }

  output$save_session <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".rds")
    },
    content = function(file) {
      save_session(file)
    }
  )

  # Load a wallace session from a file
  load_session <- function(file) {
    if (tools::file_ext(file) != "rds") {
      shinyalert::shinyalert("Invalid Wallace session file", type = "error")
      return()
    }

    state <- readRDS(file)

    if (!is.list(state) || is.null(state$main) || is.null(state$main$version)) {
      shinyalert::shinyalert("Invalid Wallace session file", type = "error")
      return()
    }

    # Load general data
    new_version <- as.character(packageVersion("wallace"))
    if (state$main$version != new_version) {
      shinyalert::shinyalert(
        glue("The input file was saved using Wallace v{state$main$version}, but you are using Wallace v{new_version}"),
        type = "warning"
      )
    }

    for (spname in names(state$main$spp)) {
      spp[[spname]] <- state$main$spp[[spname]]
    }
    for (envname in names(state$main$envs_global)) {
      envs.global[[envname]] <- state$main$envs_global[[envname]]
    }
    for (component in names(state$main$selected_module)) {
      value <- state$main$selected_module[[component]]
      updateRadioButtons(session, glue("{component}Sel"), selected = value)
    }
    updateSelectInput(session, "curSp", selected = state$main$cur_sp)

    state$main <- NULL

    # Ask each module to load its own data
    for (module_id in names(state)) {
      modules[[module_id]]$load(state[[module_id]])
    }
  }

  observeEvent(input$goLoad_session, {
    load_session(input$load_session$datapath)
  })
}
