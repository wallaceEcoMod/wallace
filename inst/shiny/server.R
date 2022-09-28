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
  observe({
    if (component() == "_stopapp") {
      shinyjs::runjs("window.close();")
      stopApp()
    }
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
    req(module())
    file <- COMPONENT_MODULES[[component()]][[module()]]$instructions
    if (is.null(file)) return()
    includeMarkdown(file)
  })

  # Help Component
  help_components <- c("occs", "envs", "poccs", "penvs", "espace", "part", "model", "vis", "xfer")
  lapply(help_components, function(component) {
    btn_id <- paste0(component, "Help")
    observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Component Guidance"))
  })

  # Help Module
  observeEvent(input$occs_queryDbHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$occs_paleoDbHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$occs_userOccsHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$envs_worldclimHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$envs_ecoclimateHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$envs_userEnvsHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$poccs_selectOccsHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$poccs_removeByIDHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$poccs_thinOccsHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$penvs_bgExtentHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$penvs_drawBgExtentHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$penvs_userBgExtentHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$espace_pcaHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$espace_occDensHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$espace_nicheOvHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$part_nonSpatHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$part_spatHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$model_maxentHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$model_bioclimHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$vis_mapPredsHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$vis_maxentEvalPlotHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$vis_responsePlotHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$vis_bioclimPlotHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$xfer_areaHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$xfer_timeHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$xfer_userHelp, updateTabsetPanel(session, "main", "Module Guidance"))
  observeEvent(input$xfer_messHelp, updateTabsetPanel(session, "main", "Module Guidance"))

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
    if(component() == 'xfer') {
      spp[[curSp()]]$polyXfXY <- xy
      spp[[curSp()]]$polyXfID <- id
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
    shinyjs::toggleState("goLoad_session", !is.null(input$load_session$datapath))
    req(length(curSp()) == 1)
    shinyjs::toggleState("dlDbOccs", !is.null(occs()))
    shinyjs::toggleState("dlOccs", !is.null(occs()))
    shinyjs::toggleState("dlAllOccs", length(allSp()) > 1)
    shinyjs::toggleState("dlRMD", !is.null(occs()))
    shinyjs::toggleState("dlGlobalEnvs", !is.null(spp[[curSp()]]$envs))
    shinyjs::toggleState("dlProcOccs",
                         !is.null(spp[[curSp()]]$rmm$code$wallace$occsSelPolyCoords) |
                           !is.null(spp[[curSp()]]$procOccs$occsThin) |
                           !is.null(spp[[curSp()]]$rmm$code$wallace$removedIDs))
    shinyjs::toggleState("dlMskEnvs", !is.null(spp[[curSp()]]$procEnvs$bgMask))
    shinyjs::toggleState("dlBgPts", !is.null(spp[[curSp()]]$bgPts))
    shinyjs::toggleState("dlBgShp", !is.null(spp[[curSp()]]$procEnvs$bgExt))
    shinyjs::toggleState("dlPcaResults", !is.null(spp[[curSp()]]$pca))
    shinyjs::toggleState("dlPart", ("partition" %in% colnames(spp[[curSp()]]$occs)))
    shinyjs::toggleState("dlEvalTbl", !is.null(evalOut()))
    shinyjs::toggleState("dlEvalTblBins", !is.null(evalOut()))
    shinyjs::toggleState("dlVisBioclim", !is.null(spp[[curSp()]]$rmm$model$algorithm$bioclim$notes))
    shinyjs::toggleState("dlMaxentPlots", !is.null(spp[[curSp()]]$rmm$model$algorithm$maxent$notes))
    shinyjs::toggleState("dlRespCurves", !is.null(spp[[curSp()]]$rmm$model$algorithm$maxent$notes))
    shinyjs::toggleState("dlPred", !is.null(spp[[curSp()]]$visualization$occPredVals))
    shinyjs::toggleState("dlXfShp", !is.null(spp[[curSp()]]$transfer$xfExt))
    shinyjs::toggleState("dlXferEnvs", !is.null(spp[[curSp()]]$transfer$xfEnvsDl))
    shinyjs::toggleState("dlXfer", !is.null(spp[[curSp()]]$transfer$xfEnvs))
    shinyjs::toggleState("dlMess", !is.null(spp[[curSp()]]$transfer$messVals))
    # shinyjs::toggleState("dlWhatever", !is.null(spp[[curSp()]]$whatever))
  })

  observe({
    req(length(curSp()) == 2)
    shinyjs::toggleState("dlPcaResults", !is.null(spp[[paste0(curSp()[1],".",curSp()[2])]]$pca))
    shinyjs::toggleState("dlOccDens", !is.null(spp[[paste0(curSp()[1],".",curSp()[2])]]$occDens))
    shinyjs::toggleState("dlNicheOvPlot", !is.null(spp[[paste0(curSp()[1],".",curSp()[2])]]$nicheOv))
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
    if (is.null(module())) {
      span("...Select a module...", class = "step")
    } else {
      selectizeInput('curSp', label = "Species menu", choices = sppNameList,
                     multiple = TRUE, selected = selected, options = options)
    }
  })

  curSp <- reactive(input$curSp)

  # vector of all species with occurrence data loaded
  allSp <- reactive(sort(names(reactiveValuesToList(spp))[!grepl("\\.", names(reactiveValuesToList(spp)))]))

  # vector of all species with occurrence data loaded
  multSp <- reactive(sort(names(reactiveValuesToList(spp))[grepl("\\.", names(reactiveValuesToList(spp)))]))

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
      n <- fmtSpN(curSp())
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
      n <- fmtSpN(curSp())
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
    req(occs())
    round(c(mean(occs()$longitude), mean(occs()$latitude)), digits = 3)
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

  # # # # # # # # # # # # # # # # # #
  # PROCESS OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  # DOWNLOAD: current processed occurrence data table
  output$dlProcOccs <- downloadHandler(
    filename = function() paste0(curSp(), "_processed_occs.csv"),
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
    if (length(polys) == 1) {
      xy <- list(polys[[1]]@coords)
    } else{
      xy <- lapply(polys, function(x) x@coords)
    }
    return(xy)
  })

  # DOWNLOAD: masked environmental variable rasters
  output$dlBgShp <- downloadHandler(
    filename = function() paste0(curSp(), "_bgShp.zip"),
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      n <- curSp()

      rgdal::writeOGR(obj = bgExt(),
                      dsn = tmpdir,
                      layer = paste0(n, '_bgShp'),
                      driver = "ESRI Shapefile",
                      overwrite_layer = TRUE)

      exts <- c('dbf', 'shp', 'shx')
      fs <- paste0(n, '_bgShp.', exts)
      zip::zipr(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {
        file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )

  # DOWNLOAD: masked environmental variable rasters
  output$dlMskEnvs <- downloadHandler(
    filename = function() paste0(curSp(), '_mskEnvs.zip'),
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
      n <- curSp()
      paste0(n, "_bgPoints.csv")
    },
    content = function(file) {
      tbl <- as.data.frame(spp[[curSp()]]$bgPts)
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )

  ############################################## #
  ### COMPONENT: ESPACE ####
  ############################################## #

  output$dlPcaResults <- downloadHandler(
    filename = function() {
      mspName <- paste(curSp(), collapse = ".")
      paste0(mspName, "_PcaResults.zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
        sp1 <- curSp()[1]
        sp2 <- curSp()[2]
      } else {
        mSp <- curSp()
      }
      req(spp[[mSp]]$pca)
      png(paste0("pcaScatterOccs.png"), width = 500, height = 500)
      x <- spp[[mSp]]$pca$scores[spp[[mSp]]$pca$scores$bg == 'sp', ]
      x.f <- factor(x$sp)
      ade4::s.class(x, x.f, xax = spp[[mSp]]$pc1, yax = spp[[mSp]]$pc2,
                    col = c("red", "blue"), cstar = 0, cpoint = 0.1)
      title(xlab = paste0("PC", spp[[mSp]]$pc1),
            ylab = paste0("PC", spp[[mSp]]$pc2))
      dev.off()
      png(paste0("pcaScatterOccsBg.png"), width = 500, height = 500)
      x <- spp[[mSp]]$pca$scores[spp[[mSp]]$pca$scores$sp == 'bg', ]
      x.f <- factor(x$bg)
      ade4::s.class(x, x.f, xax = spp[[mSp]]$pc1, yax = spp[[mSp]]$pc2,
                    col = c("red", "blue"), cstar = 0, cpoint = 0.1)
      title(xlab = paste0("PC", spp[[mSp]]$pc1),
            ylab = paste0("PC", spp[[mSp]]$pc2))
      dev.off()
      png(paste0("pcaCorCircle.png"), width = 500, height = 500)
      ade4::s.corcircle(spp[[mSp]]$pca$co, xax = spp[[mSp]]$pc1,
                        yax = spp[[mSp]]$pc2, lab = input$pcaSel,
                        full = FALSE, box = TRUE)
      title(xlab = paste0("PC", spp[[mSp]]$pc1),
            ylab = paste0("PC", spp[[mSp]]$pc2))
      dev.off()
      png(paste0("pcaScree.png"), width = 500, height = 500)
      screeplot(spp[[mSp]]$pca, main = NULL)
      dev.off()
      sink(paste0("pcaOut.txt"))
      print(summary(spp[[mSp]]$pca))
      sink()

      fs <- c("pcaScatterOccs.png", "pcaScatterOccsBg.png",
            "pcaCorCircle.png","pcaScree.png","pcaOut.txt")
      zip::zipr(zipfile = file,
                files = fs)
    }
  )

  output$dlOccDens <- downloadHandler(
    filename = function() {
      mspName <- paste(curSp(), collapse = ".")
      paste0(mspName, "_occDens.png")
    },
    content = function(file) {
      png(file, width = 1000, height = 500)
      graphics::par(mfrow=c(1,2))
      if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
        sp1 <- curSp()[1]
        sp2 <- curSp()[2]
      } else {
        mSp <- curSp()
      }
      req(spp[[mSp]]$occDens)
      ecospat::ecospat.plot.niche(spp[[mSp]]$occDens[[sp1]], title = spName(sp1))
      ecospat::ecospat.plot.niche(spp[[mSp]]$occDens[[sp2]], title = spName(sp2))
      dev.off()
    }
  )

  output$dlNicheOvPlot <- downloadHandler(
    filename = function() {
      mSp <- paste(curSp(), collapse = ".")
      paste0(mSp, "_nicheOvPlot.png")
    },
    content = function(file) {
      png(file, width = 1000, height = 500)
      graphics::par(mfrow = c(1, 2))
      if (length(curSp()) == 2) {
        mSp <- paste(curSp(), collapse = ".")
        sp1 <- curSp()[1]
        sp2 <- curSp()[2]
      } else {
        mSp <- curSp()
      }
      req(spp[[mSp]]$occDens)
      ecospat::ecospat.plot.niche.dyn(
        spp[[mSp]]$occDens[[sp1]],
        spp[[mSp]]$occDens[[sp2]],
        0.5,
        title = mSp,
        col.unf = "blue",
        col.exp = "red",
        col.stab = "purple",
        colZ1 = "blue",
        colZ2 = "red",
        transparency = 25
      )
      box()
      req(spp[[mSp]]$nicheOv)
      # if (!is.null(spp[[mSp]]$nicheOv$equiv))
      #   ecospat::ecospat.plot.overlap.test(spp[[mSp]]$nicheOv$equiv,
      #                                      "D", "Equivalency test")
      if (!is.null(spp[[mSp]]$nicheOv$simil))
        ecospat::ecospat.plot.overlap.test(spp[[mSp]]$nicheOv$simil,
                                           "D", "Similarity test")
      ovMetrics <- paste(
        "Overlap D = ", round(spp[[mSp]]$nicheOv$overlap$D, 2),
        " | Sp1 only :", round(spp[[mSp]]$nicheOv$USE[3], 2),
        " | Sp2 only :", round(spp[[mSp]]$nicheOv$USE[1], 2),
        " | Both :", round(spp[[mSp]]$nicheOv$USE[2], 2)
      )
      graphics::mtext(ovMetrics, outer = TRUE, line = -1)
      dev.off()
    }
  )

  ################################################## #
  ### COMPONENT: PARTITION OCCURRENCE DATA ####
  ################################################# #

  # download for partitioned occurrence records csv
  output$dlPart <- downloadHandler(
    filename = function() paste0(curSp(), "_partitioned_occs.csv"),
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
    # NOTE: this line is necessary to retain the selection after selecting different tabs
    if(!is.null(curModel())) selected <- curModel() else selected <- n[1]
    modsNameList <- c(list("Current model" = ""), setNames(as.list(n), n))
    options <- list(maxItems = 1)
    if (!is.null(module())) {
      selectizeInput('curModel', label = "Select model: " ,
                     choices = modsNameList, multiple = TRUE,
                     selected = selected, options = options)
    }
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
      label = "Select categorical variables",
      choices = envList,
      multiple = TRUE)
  })

  selCatEnvs <- reactive(input$selCatEnvs)

  # download for partitioned occurrence records csv
  output$dlEvalTbl <- downloadHandler(
    filename = function() {
      if(spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
        paste0(curSp(), "_bioclim_evalTbl.csv")
      } else if(spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
        paste0(curSp(), "_maxent_evalTbl.csv")
      } else if(spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
        paste0(curSp(), "_maxnet_evalTbl.csv")
      }
    },
    content = function(file) {
      evalTbl <- spp[[curSp()]]$evalOut@results
      write_csv_robust(evalTbl, file, row.names = FALSE)
    }
  )

  # download for partitioned occurrence records csv
  output$dlEvalTblBins <- downloadHandler(
    filename = function() {
      if(spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
        paste0(curSp(), "_bioclim_evalTblBins.csv")
      } else if(spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
        paste0(curSp(), "_maxent_evalTblBins.csv")
      } else if(spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
        paste0(curSp(), "_maxnet_evalTblBins.csv")
      }
    },
    content = function(file) {
      evalTblBins <- spp[[curSp()]]$evalOut@results.partitions
      write_csv_robust(evalTblBins, file, row.names = FALSE)
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
      vis_bioclimPlot(evalOut()@models[[curModel()]],
                      spp[[curSp()]]$rmm$code$wallace$bcPlotSettings[['bc1']],
                      spp[[curSp()]]$rmm$code$wallace$bcPlotSettings[['bc2']],
                      spp[[curSp()]]$rmm$code$wallace$bcPlotSettings[['p']])
      dev.off()
    }
  )

  # handle downloads for Maxent Plots png
  output$dlMaxentPlots <- downloadHandler(
    filename = function() {paste0(curSp(), "_evalPlots.zip")},
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      parEval <- c('auc.val', 'auc.diff', 'or.mtp', 'or.10p', 'delta.AICc')
      for (i in parEval) {
        ENMeval::evalplot.stats(spp[[curSp()]]$evalOut, i, "rm", "fc")
        ggplot2::ggsave(paste0(gsub("[[:punct:]]", "_", i), ".png"))
        # dev.off()
      }
      fs <- paste0(gsub("[[:punct:]]", "_", parEval), ".png")
      zip::zipr(zipfile = file,
                files = fs)
    }
  )

  # handle downloads for Response Curve Plots png
  output$dlRespCurves <- downloadHandler(
    filename = function() {paste0(curSp(), "_responseCurves.zip")},
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      if (spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
        namesEnvs <- mxNonzeroCoefs(evalOut()@models[[curModel()]], "maxnet")
        for (i in namesEnvs) {
          png(paste0(i, ".png"))
          suppressWarnings(
            maxnet::response.plot(evalOut()@models[[curModel()]], v = i,
                                  type = "cloglog")
          )
          dev.off()
        }
      } else if (spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
        namesEnvs <- mxNonzeroCoefs(evalOut()@models[[curModel()]], "maxent.jar")
        for (i in namesEnvs) {
          png(paste0( i, ".png"))
          dismo::response(evalOut()@models[[curModel()]], var = i)
          dev.off()
        }
      }
      fs <- paste0(namesEnvs, ".png")
      zip::zipr(zipfile = file, files = fs)
    }
  )

  # download for model predictions (restricted to background extent)
  output$dlPred <- downloadHandler(
    filename = function() {
      ext <- switch(input$predFileType, raster = 'zip', ascii = 'asc',
                    GTiff = 'tif', png = 'png')
      thresholdRule <- rmm()$prediction$binary$thresholdRule
      predType <- rmm()$prediction$notes
      if (thresholdRule == 'none') {
        paste0(curSp(), "_", predType, '.', ext)
      } else {
        paste0(curSp(), "_", thresholdRule, '.', ext)
      }
    },
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      if(require(rgdal)) {
        if (input$predFileType == 'png') {
          req(mapPred())
          if (!webshot::is_phantomjs_installed()) {
            logger %>%
              writeLog(type = "error", "To download PNG prediction, you're required to",
                       " install PhantomJS in your machine. You can use webshot::install_phantomjs()",
                       " in you are R console.")
            return()
          }
          if (!requireNamespace("mapview")) {
            logger %>%
              writeLog(
                type = "error",
                "PNG option is available if you install the 'mapview' package ",
                "(which is a suggested package for Wallace, not a required dependency). If you ",
                "want to install it, close Wallace and run the following line in the ",
                "R Console: ", em("install.packages('mapview')")
              )
            return()
          }
          if (rmm()$prediction$binary$thresholdRule != 'none') {
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
            mapLabFormat <- reverseLabel(2, reverse_order=TRUE)
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
                        group='xfer')
          mapview::mapshot(m, file = file)
        } else if (input$predFileType == 'raster') {
          fileName <- curSp()
          raster::writeRaster(mapPred(), file.path(tmpdir, fileName),
                              format = input$predFileType, overwrite = TRUE)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zipr(zipfile = file, files = fs)
        } else {
          r <- raster::writeRaster(mapPred(), file, format = input$predFileType,
                                   overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        logger %>%
          writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )

  ########################################### #
  ### COMPONENT: MODEL TRANSFER ####
  ########################################### #

  # # # # # # # # # # # # # # # # # #
  # TRANSFER: other controls ####
  # # # # # # # # # # # # # # # # # #

  # convenience function for mapped model prediction raster for current species
  mapXfer <- reactive(spp[[curSp()]]$transfer$mapXfer)

  # DOWNLOAD: Shapefile of extent of transfer
  output$dlXfShp <- downloadHandler(
    filename = function() paste0(curSp(), '_xferShp.zip'),
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      n <- curSp()
      rgdal::writeOGR(obj = spp[[curSp()]]$transfer$xfExt,
                      dsn = tmpdir,
                      layer = paste0(n, '_xferShp'),
                      driver = "ESRI Shapefile",
                      overwrite_layer = TRUE)

      exts <- c('dbf', 'shp', 'shx')
      fs <- paste0(n, '_xferShp.', exts)
      zip::zipr(zipfile = file, files = fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )

  # DOWNLOAD: Transferred envs
  output$dlXferEnvs <- downloadHandler(
    filename = function() paste0(spp[[curSp()]]$transfer$xfEnvsDl, '_xfEnvs.zip'),
    content = function(file) {
      withProgress(
        message = paste0("Preparing ", paste0(spp[[curSp()]]$transfer$xfEnvsDl, '_xfEnvs.zip...')), {
          tmpdir <- tempdir()
          owd <- setwd(tmpdir)
          on.exit(setwd(owd))
          type <- input$xferEnvsFileType
          nm <- names(spp[[curSp()]]$transfer$xferTimeEnvs)

          raster::writeRaster(spp[[curSp()]]$transfer$xferTimeEnvs, nm, bylayer = TRUE,
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

  # download for model predictions (restricted to background extent)
  output$dlXfer <- downloadHandler(
    filename = function() {
      ext <- switch(input$xferFileType, raster = 'zip', ascii = 'asc',
                    GTiff = 'tif', png = 'png')
      thresholdRule <- rmm()$prediction$transfer$environment1$thresholdRule
      predType <- rmm()$prediction$notes
      if (thresholdRule == 'none') {
        paste0(curSp(), "_xfer_", predType, '.', ext)
      } else {
        paste0(curSp(), "_xfer_", thresholdRule, '.', ext)
      }
    },
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      if(require(rgdal)) {
        if (input$xferFileType == 'png') {
          req(mapXfer())
          if (!webshot::is_phantomjs_installed()) {
            logger %>%
              writeLog(type = "error", "To download PNG prediction, you're required to",
                       " install PhantomJS in your machine. You can use webshot::install_phantomjs()",
                       " in you are R console.")
            return()
          }
          if (!requireNamespace("mapview")) {
            logger %>%
              writeLog(
                type = "error",
                "PNG option is available if you install the 'mapview' package ",
                "(which is a suggested package for Wallace, not a required dependency). If you ",
                "want to install it, close Wallace and run the following line in the ",
                "R Console: ", em("install.packages('mapview')")
              )
            return()
          }
          if (rmm()$prediction$transfer$environment1$thresholdRule != 'none') {
            mapXferVals <- 0:1
            rasPal <- c('gray', 'red')
            legendPal <- colorBin(rasPal, 0:1, bins = 2)
            mapTitle <- "Thresholded Suitability<br>(Transferred)"
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
            mapXferVals <- spp[[curSp()]]$transfer$mapXferVals
            rasPal <- colorNumeric(rasCols, mapXferVals, na.color='transparent')
            legendPal <- colorNumeric(rev(rasCols), mapXferVals, na.color='transparent')
            mapTitle <- "Predicted Suitability<br>(Transferred)"
            mapLabFormat <- reverseLabel(2, reverse_order=TRUE)
            mapOpacity <- NULL
          }
          polyXfXY <- spp[[curSp()]]$transfer$xfExt@polygons[[1]]@Polygons
          if(length(polyXfXY) == 1) {
            shp <- polyXfXY[[1]]@coords
          } else {
            shp <- lapply(polyXfXY, function(x) x@coords)
          }
          m <- leaflet() %>%
            addLegend("bottomright", pal = legendPal, title = mapTitle,
                      labFormat = mapLabFormat, opacity = mapOpacity,
                      values = mapXferVals, layerId = "train") %>%
            addProviderTiles(input$bmap) %>%
            addRasterImage(mapXfer(), colors = rasPal, opacity = 0.7,
                           group = 'vis', layerId = 'mapXfer', method = "ngb") %>%
            addPolygons(lng = shp[, 1], lat = shp[, 2], fill = FALSE,
                        weight = 4, color = "red", group = 'xfer')
          mapview::mapshot(m, file = file)
        } else if (input$xferFileType == 'raster') {
          fileName <- curSp()
          raster::writeRaster(mapXfer(), file.path(tmpdir, fileName),
                              format = input$xferFileType, overwrite = TRUE)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zipr(zipfile = file, files = fs)
        } else {
          r <- raster::writeRaster(mapXfer(), file, format = input$xferFileType,
                                   overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        logger %>% writeLog("Please install the rgdal package before downloading rasters.")
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
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      if(require(rgdal)) {
        req(spp[[curSp()]]$transfer$mess, spp[[curSp()]]$transfer$xfExt)
        mess <- spp[[curSp()]]$transfer$mess
        if (input$messFileType == 'png') {
          if (!webshot::is_phantomjs_installed()) {
            logger %>%
              writeLog(type = "error", "To download PNG prediction, you're required to",
                       " install PhantomJS in your machine. You can use webshot::install_phantomjs()",
                       " in you are R console.")
            return()
          }
          if (!requireNamespace("mapview")) {
            logger %>%
              writeLog(
                type = "error",
                "PNG option is available if you install the 'mapview' package ",
                "(which is a suggested package for Wallace, not a required dependency). If you ",
                "want to install it, close Wallace and run the following line in the ",
                "R Console: ", em("install.packages('mapview')")
              )
            return()
          }
          rasVals <- spp[[curSp()]]$transfer$messVals
          polyXfXY <- spp[[curSp()]]$transfer$xfExt@polygons[[1]]@Polygons
          if(length(polyXfXY) == 1) {
            shp <- polyXfXY[[1]]@coords
          } else {
            shp <- lapply(polyXfXY, function(x) x@coords)
          }
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
                           group = 'vis', layerId = 'mapXfer', method = "ngb") %>%
            addPolygons(lng = shp[, 1], lat = shp[, 2], fill = FALSE,
                        weight = 4, color = "red", group = 'xfer')
          mapview::mapshot(m, file = file)
        } else if (input$messFileType == 'raster') {
          fileName <- curSp()
          raster::writeRaster(mess, file.path(tmpdir, fileName),
                              format = input$messFileType, overwrite = TRUE)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zipr(zipfile = file, files = fs)
        } else {
          r <- raster::writeRaster(mess, file, format = input$messFileType,
                                   overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        logger %>% writeLog("Please install the rgdal package before downloading rasters.")
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
      md_files <- c()
      md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
      rmarkdown::render("Rmd/userReport_intro.Rmd",
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = md_intro_file,
                        clean = TRUE,
                        encoding = "UTF-8")
      md_files <- c(md_files, md_intro_file)
      # Abbreviation for one species
      spAbr <- plyr::alply(abbreviate(stringr::str_replace(allSp(), "_", " "),
                                      minlength = 2),
                           .margins = 1, function(x) {x <- as.character(x)})
      names(spAbr) <- allSp()

      for (sp in allSp()) {
        species_rmds <- NULL
        for (component in names(COMPONENT_MODULES[names(COMPONENT_MODULES) != c("espace", "rep")])) {
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
                          clean = TRUE,
                          encoding = "UTF-8")
        md_files <- c(md_files, species_md_file)
      }

      if (!is.null(multSp())) {
        for (sp in multSp()) {
          namesMult <- unlist(strsplit(sp, "\\."))
          multSpecies_rmds <- NULL
          for (component in names(COMPONENT_MODULES[names(COMPONENT_MODULES) == "espace"])) {
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
                spName1 = spName(namesMult[1]),
                spName2 = spName(namesMult[2]),
                sp1 = namesMult[1],
                spAbr1 = spAbr[[namesMult[1]]],
                sp2 = namesMult[2],
                spAbr2 = spAbr[[namesMult[2]]],
                multAbr = paste0(spAbr[[namesMult[1]]], "_", spAbr[[namesMult[2]]]),
                rmd_vars
              )
              module_rmd <- do.call(knitr::knit_expand, knit_params)

              module_rmd_file <- tempfile(pattern = paste0(module$id, "_"),
                                          fileext = ".Rmd")
              writeLines(module_rmd, module_rmd_file)
              multSpecies_rmds <- c(multSpecies_rmds, module_rmd_file)
            }
          }

          multSpecies_md_file <- tempfile(pattern = paste0(sp, "_"),
                                          fileext = ".md")
          rmarkdown::render(input = "Rmd/userReport_multSpecies.Rmd",
                            params = list(child_rmds = multSpecies_rmds,
                                          spName1 = spName(namesMult[1]),
                                          spName2 = spName(namesMult[2]),
                                          multAbr = paste0(spAbr[[namesMult[1]]], "_",
                                                           spAbr[[namesMult[2]]])
                            ),
                            output_format = rmarkdown::github_document(html_preview = FALSE),
                            output_file = multSpecies_md_file,
                            clean = TRUE,
                            encoding = "UTF-8")
          md_files <- c(md_files, multSpecies_md_file)
        }
      }

      combined_md <-
        md_files %>%
        lapply(readLines) %>%
        # lapply(readLines, encoding = "UTF-8") %>%
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
          clean = TRUE,
          encoding = "UTF-8"
        )
      }

      file.rename(result_file, file)
    }
  )

  ################################
  ### METADATA FUNCTIONALITY ####
  ################################

  output$dlRMM <- downloadHandler(
    filename = function() {paste0("wallace-metadata-", Sys.Date(), ".zip")},
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      # REFERENCES ####
      knitcitations::citep(citation("rangeModelMetadata"))
      namesSpp <- allSp()
      for (i in namesSpp) {
        rangeModelMetadata::rmmToCSV(spp[[i]]$rmm, filename = paste0(i, "_RMM.csv"))
      }
      zip::zipr(zipfile = file, files = paste0(namesSpp, "_RMM.csv"))
    })

  ################################
  ### REFERENCE FUNCTIONALITY ####
  ################################

  output$dlrefPackages <- downloadHandler(
    filename = function() {paste0("ref-packages-", Sys.Date(),
                                  filetype_to_ext(input$refFileType))},
    content = function(file) {
      # Create BIB file
      bib_file <- "Rmd/references.bib"
      temp_bib_file <- tempfile(pattern = "ref_", fileext = ".bib")
      # Package always cited
      knitcitations::citep(citation("wallace"))
      knitcitations::citep(citation("knitcitations"))
      knitcitations::citep(citation("knitr"))
      knitcitations::citep(citation("rmarkdown"))
      knitcitations::citep(citation("raster"))
      # Write BIBTEX file
      knitcitations::write.bibtex(file = temp_bib_file)
      # Replace NOTE fields with VERSION when R package
      bib_ref <- readLines(temp_bib_file)
      bib_ref  <- gsub(pattern = "note = \\{R package version", replace = "version = \\{R package", x = bib_ref)
      writeLines(bib_ref, con = temp_bib_file)
      file.rename(temp_bib_file, bib_file)
      # Render reference file
      md_ref_file <- tempfile(pattern = "ref_", fileext = ".md")
      rmarkdown::render("Rmd/references.Rmd",
                        output_format =
                          switch(
                            input$refFileType,
                            "PDF" = rmarkdown::pdf_document(),
                            "HTML" = rmarkdown::html_document(),
                            "Word" = rmarkdown::word_document()
                          ),
                        output_file = file,
                        clean = TRUE,
                        encoding = "UTF-8")
    })

  ################################
  ### COMMON LIST FUNTIONALITY ####
  ################################

  # Create a data structure that holds variables and functions used by modules
  common = list(
    # Reactive variables to pass on to modules
    logger = logger,
    spp = spp,
    curSp = curSp,
    allSp = allSp,
    multSp = multSp,
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
    bg = bg,
    bgExt = bgExt,
    bgMask = bgMask,
    bgShpXY = bgShpXY,
    selCatEnvs = selCatEnvs,
    evalOut = evalOut,
    mapPred = mapPred,
    mapXfer = mapXfer,
    rmm = rmm,

    # Switch to a new component tab
    update_component = function(tab = c("Map", "Table", "Results", "Download")) {
      tab <- match.arg(tab)
      updateTabsetPanel(session, "main", selected = tab)
    },

    # Disable a specific module so that it will not be selectable in the UI
    disable_module = function(component = COMPONENTS, module) {
      component <- match.arg(component)
      shinyjs::js$disableModule(component = component, module = module)
    },

    # Enable a specific module so that it will be selectable in the UI
    enable_module = function(component = COMPONENTS, module) {
      component <- match.arg(component)
      shinyjs::js$enableModule(component = component, module = module)
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
    spp_size <- as.numeric(utils::object.size(reactiveValuesToList(spp)))
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
    # Select names of species in spp object
    sppLoad <- grep("\\.", names(spp), value = TRUE, invert = TRUE)
    # Storage species with no env data
    noEnvsSpp <- NULL
    for (i in sppLoad) {
      # Check if envs.global object exists in spp
      if (!is.null(spp[[i]]$envs)) {
        diskRast <- raster::fromDisk(envs.global[[spp[[i]]$envs]])
        if (diskRast) {
          if (class(envs.global[[spp[[i]]$envs]]) == "RasterStack") {
            diskExist <- !file.exists(envs.global[[spp[[i]]$envs]]@layers[[1]]@file@name)
          } else if (class(envs.global[[spp[[i]]$envs]]) == "RasterBrick") {
            diskExist <- !file.exists(envs.global[[spp[[i]]$envs]]@file@name)
          }
          if (diskExist) {
            noEnvsSpp <- c(noEnvsSpp, i)
          }
        }
      }
    }
    if (is.null(noEnvsSpp)) {
      shinyalert::shinyalert(title = "Session loaded", type = "success")
    } else {
      msgEnvAgain <- paste0("Load variables again for: ",
                            paste0(noEnvsSpp, collapse = ", "))
      shinyalert::shinyalert(title = "Session loaded", type = "warning",
                             text = msgEnvAgain)
    }
  })
}
