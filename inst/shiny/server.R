source("funcs/functions.R", local = TRUE)

options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {
  # disable download buttons
  shinyjs::disable("dlDbOccs")
  shinyjs::disable("dlProcOccs")
  # shinyjs::disable("dlEnvs")
  shinyjs::disable("dlMskEnvs")
  shinyjs::disable("dlPart")
  shinyjs::disable("downloadEvalcsv")
  shinyjs::disable("downloadEvalPlots")
  shinyjs::disable("dlPreds")
  shinyjs::disable("downloadPj")
  
  # initialize module parameters list
  rvs <- reactiveValues(logs = logInit(), occs = NULL, occsOrig = NULL, envs = NULL,
                        bgMsk = NULL, bgPts = NULL, grp = NULL, mods = NULL, 
                        modPreds = NULL, modRes = NULL, modSel = NULL, modPredsLog = NULL,
                        modOccVals = NULL)
  
  observeEvent(input$load, {
    f <- read.csv('/Users/musasabi/Downloads/Puma concolor_partitioned_occs(1).csv')
    rvs$occs <- f %>% dplyr::filter(name != 'background')
    rvs$occsGrp <- rvs$occs$group
    rvs$bgPts <- f %>% dplyr::filter(name == 'background')
    rvs$bgGrp <- rvs$bgPts$group
    rvs$bgPts <- rvs$bgPts %>% dplyr::select(longitude, latitude)
    rvs$bgMsk <- stack(list.files('/Users/musasabi/Downloads/mskPreds(4)', 'gri$', full.names = TRUE))  
    print('HACKING DONE')
  })
  
  
  # logs <- reactiveValues(entries=logInit())
  gtext <- reactiveValues()
  
  # load modules
  for (f in list.files('./modules')) {
    source(file.path('modules', f), local=TRUE)
  }
  
  # UI for component guidance text
  output$gtext_comp <- renderUI({
    shiny::includeMarkdown(system.file('Rmd', gtext$cur_comp, package='wallace'))
  })
  
  # UI for module guidance text
  output$gtext_mod <- renderUI({
    shiny::includeMarkdown(system.file('Rmd', gtext$cur_mod, package='wallace'))
  })
  
  ####################### #
  ### INITIALIZE ####
  ####################### #
  
  output$log <- renderUI({tags$div(id='logHeader', tags$div(id='logContent', 
                                                            HTML(paste0(rvs$logs, "<br>", collapse = ""))))})
  
  # create map
  m <- leaflet() %>% setView(0, 0, zoom = 2) %>% addProviderTiles('Esri.WorldTopoMap')
  output$map <- renderLeaflet(m)
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  # initialize provider tile option
  observe({map %>% addProviderTiles(input$bmap)})
  
  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #
  
  # guidance text and tab behavior
  observe({
    if (input$tabs == 1) {
      gtext$cur_comp <- 'gtext_comp1.Rmd'
      if (input$occSel == 'db') gtext$cur_mod <- "gtext_comp1_dbOccs.Rmd"
      if (input$occSel == 'user') gtext$cur_mod <- "gtext_comp1_userOccs.Rmd"
    }
    if (input$tabs == 2) {
      gtext$cur_comp <- "gtext_comp2.Rmd"
      # if Module: Select Localities, populate guidance text and select legend
      if (input$procOccSel == 'selpts') gtext$cur_mod <- "gtext_comp2_selectLocs.Rmd"
      if (input$procOccSel == 'spthin') gtext$cur_mod <- "gtext_comp2_spatialThin.Rmd"
    }
    if (input$tabs == 3) {
      gtext$cur_comp <- "gtext_comp3.Rmd"
      if (input$envSel == 'wcbc') gtext$cur_mod <- "gtext_comp3_worldclim.Rmd"
    }
    if (input$tabs == 4) {
      gtext$cur_comp <- "gtext_comp4.Rmd"
      if (input$envProcSel == 'backg') gtext$cur_mod <- "gtext_comp4_backg.Rmd"
      # switch to Map tab
      updateTabsetPanel(session, 'main', selected = 'Map')
    }
  })
  
  ########################################## #
  ### COMPONENT 1: OBTAIN OCCURRENCE DATA ####
  ########################################## #
  
  # module Query Database
  dbOccs <- callModule(queryDB_MOD, 'c1_queryDB', rvs)
  
  # dbOccs <- eventReactive(input$goDbOccs, dbOccs.call())
  spName <- reactive(as.character(rvs$occs$name[1]))
  
  observeEvent(input$goDbOccs, {
    rvs$occs <- dbOccs()
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
    shinyjs::enable("dlDbOccs")
  })
  
  # module User Occurrence Data
  userOccs <- callModule(userOccs_MOD, 'c1_userOccs', rvs)
  
  observeEvent(input$goUserOccs, {
    rvs$occs <- userOccs()
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
    shinyjs::disable("dlDbOccs")
  })
  
  # TABLE
  options <- list(autoWidth = TRUE, columnDefs = list(list(width = '40%', targets = 7)),
                  scrollX=TRUE, scrollY=400)
  output$occTbl <- DT::renderDataTable({
    req(rvs$occs)
    rvs$occs %>% dplyr::select(-origID, -pop)
  })
  
  # handle downloading of original GBIF records after cleaning
  output$dlDbOccs <- downloadHandler(
    filename = function() {paste0(formatSpName(spName()), '_', input$occDb, ".csv")},
    content = function(file) {
      write.csv(rvs$occsOrig, file, row.names=FALSE)
    }
  )
  
  ########################################### #
  ### COMPONENT 2: PROCESS OCCURRENCE DATA ####
  ########################################### #
  
  # module Spatial Thin
  thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs', rvs)
  
  observeEvent(input$goThinOccs, {
    rvs$occs <- thinOccs()
    # MAPPING - blue pts for remove, red pts for keep
    map %>% 
      addCircleMarkers(data = dbOccs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'blue',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addCircleMarkers(data = rvs$occs, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'red',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addLegend("topright", colors = c('red', 'blue'),
                title = "Occ Records", labels = c('retained', 'removed'),
                opacity = 1, layerId = 'thinLeg')
    shinyjs::enable("dlProcOccs")
  })
  
  # handle download for thinned records csv
  output$dlProcOccs <- downloadHandler(
    filename = function() {paste0(formatSpName(spName()), "_procOccs.csv")},
    content = function(file) {
      thinned_rowNums <- as.numeric(thinOccs()$origID)
      origThinned <- rvs$occsOrig[thinned_rowNums,]
      write.csv(origThinned, file, row.names = FALSE)
    }
  )
  
  ############################################# #
  ### COMPONENT 3: OBTAIN ENVIRONMENTAL DATA ####
  ############################################# #
  
  # map center coordinates for 30 arcsec download
  mapCntr <- reactive(mapCenter(input$map_bounds))
  
  output$ctrLatLon <- renderText({
    paste('Using map center', paste(mapCntr(), collapse=', '))
  })
  
  # module WorldClim Bioclims
  wcBioclims <- callModule(wcBioclims_MOD, 'c3_wcBioclims', rvs)
  
  observeEvent(input$goEnvData, {
    # load into envs
    rvs$envs <- wcBioclims()
    # occs.naEnvRem <- remEnvsValsNA(envs, occs)
    # occs(occs.naEnvRem)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # enable download button
    # shinyjs::enable("dlEnvs")
  })
  
  userEnvs <- callModule(userEnvs_MOD, 'c3_userEnvs', rvs)
  
  observeEvent(input$goUserEnvs, {
    rvs$envs <- userEnvs()
    rvs$occs <- remEnvsValsNA(rvs)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  output$envsPrint <- renderPrint({
    req(rvs$envs)
    rvs$envs
    # mins <- sapply(envs()@layers, function(x) x@data@min)
    # maxs <- sapply(envs()@layers, function(x) x@data@max)
    # names <- sapply(strsplit(names(envs()), '[.]'), function(x) x[-2])
    # mins <- round(cellStats(envs(), stat = min), digits = 3)
    # maxs <- round(cellStats(envs(), stat = max), digits = 3)
    # DT::datatable(data.frame(name=names, min=mins, max=maxs), 
    #               rownames = FALSE, options = list(pageLength = raster::nlayers(envs())))
  })
  
  ############################################## #
  ### COMPONENT 4: PROCESS ENVIRONMENTAL DATA ####
  ############################################## #
  
  bgExt <- callModule(bgExtent_MOD, 'c4_bgExtent', rvs)
  
  observeEvent(input$goBgExt, {
    rvs$bgShp <- bgExt()
    coords <- rvs$bgShp@polygons[[1]]@Polygons[[1]]@coords
    map %>%
      addPolygons(lng=coords[,1], lat=coords[,2], layerId="bg",
                  weight=10, color="red", group='bgShp') %>%
      fitBounds(max(coords[,1]), max(coords[,2]), min(coords[,1]), min(coords[,2]))
  })
  
  userBg <- callModule(userBgExtent_MOD, 'c4_userBgExtent', rvs)
  
  observeEvent(input$goUserBg, {
    rvs$bgShp <- userBg()
    coords <- rvs$bgShp@polygons[[1]]@Polygons[[1]]@coords
    map %>%
      addPolygons(lng=coords[,1], lat=coords[,2], layerId="bg",
                  weight=10, color="red", group='bgShp') %>%
      fitBounds(max(coords[,1]), max(coords[,2]), min(coords[,1]), min(coords[,2]))
  })
  
  observeEvent(input$goBgMask, {
    bgMskPts.call <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts', rvs)
    bgMskPts <- bgMskPts.call()
    rvs$bgMsk <- bgMskPts$msk
    rvs$bgPts <- bgMskPts$pts
    shinyjs::enable('dlMskEnvs')
  })
  
  
  # handle download for masked predictors, with file type as user choice
  output$dlMskEnvs <- downloadHandler(
    filename = function() {'mskEnvs.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      type <- input$bgMskFileType
      nm <- names(rvs$bgMsk)
      
      raster::writeRaster(rvs$bgMsk, file.path(tmpdir, 'msk'), bylayer = TRUE,
                          suffix = nm, format = type, overwrite = TRUE)
      ext <- ifelse(type == 'raster', 'grd',
                    ifelse(type == 'ascii', 'asc',
                           ifelse(type == 'GTiff', 'tif', 'png')))
      
      fs <- paste0('msk_', nm, '.', ext)
      if (ext == 'grd') {
        fs <- c(fs, paste0('msk_', nm, '.gri'))
      }
      zip(zipfile=file, files=fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )
  
  
  ############################################# #
  ### COMPONENT 5: PARTITION OCCURRENCE DATA ####
  ############################################# #
  
  observeEvent(input$goPartNsp, {
    partNsp <- callModule(partNsp_MOD, 'c5_partNsp', rvs)
    rvs$occsGrp <- partNsp()[[1]]
    rvs$bgGrp <- partNsp()[[2]]
    # colors for partition symbology
    newColors <- gsub("FF$", "", rainbow(max(rvs$occsGrp)))  
    partsFill <- newColors[rvs$occsGrp]
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs, fillColor = partsFill, fillOpacity = 1) %>%
      zoom2Occs(rvs$occs)
    shinyjs::enable("dlPart")
  })
  
  observeEvent(input$goPartSp, {
    partSp <- callModule(partSp_MOD, 'c5_partSp', rvs)
    rvs$occsGrp <- partSp()[[1]]
    rvs$bgGrp <- partSp()[[2]]
    # colors for partition symbology
    newColors <- gsub("FF$", "", rainbow(max(rvs$occsGrp)))  
    partsFill <- newColors[rvs$occsGrp]
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs, fillColor = partsFill, fillOpacity = 1) %>%
      zoom2Occs(rvs$occs)
    shinyjs::enable("dlPart")
  })
  
  # handle download for partitioned occurrence records csv
  output$dlPart <- downloadHandler(
    filename = function() paste0(spName(), "_partitioned_occs.csv"),
    content = function(file) {
      bg.bind <- data.frame(rep('background', nrow(rvs$bgPts)), rvs$bgPts)
      print(head(bg.bind))
      print(names(bg.bind))
      names(bg.bind) <- c('name', 'longitude', 'latitude')
      occs.bg.bind <-rbind(rvs$occs[,1:3], bg.bind)
      all.bind <- cbind(occs.bg.bind, c(rvs$occsGrp, rvs$bgGrp))
      names(all.bind)[4] <- "group"
      write.csv(all.bind, file, row.names = FALSE)
    }
  )
  
  ######################### #
  ### COMPONENT 6: MODEL ####
  ######################### #
  
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
  output$maxentJar <- renderUI(HTML(paste('To use Maxent, make sure you download <b>maxent.jar</b> from the
                                       <a href "http://biodiversityinformatics.amnh.org/open_source/maxent/">AMNH Maxent webpage</a>
                                       and place it in this directory:<br><i>', jar, '</i>')))
  
  mod.maxent <- callModule(maxent_MOD, 'c6_maxent', rvs)
  
  observeEvent(input$goMaxent, {
    # unpack everything
    mod.maxent.call <- mod.maxent()
    e <- mod.maxent.call[[1]]
    rvs$mods <- e@models
    rvs$modPreds <- e@predictions
    rvs$modRes <- e@results
    rvs$modPredsLog <- mod.maxent.call[[2]]
    rvs$modOccVals <- mod.maxent.call[[3]]
    # rename results table fields
    rvs$modRes <- rvs$modRes %>% dplyr::rename(avg.test.AUC = Mean.AUC, var.test.AUC = Var.AUC,
                                               avg.diff.AUC = Mean.AUC.DIFF, var.diff.AUC = Var.AUC.DIFF,
                                               avg.test.orMTP = Mean.ORmin, var.test.orMTP = Var.ORmin,
                                               avg.test.or10pct = Mean.OR10, var.test.or10pct = Var.OR10,
                                               parameters = nparam)
    output$evalTbl <- DT::renderDataTable(cbind(rvs$modRes[,1:3], round(rvs$modRes[,4:16], digits=3)))
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  mod.bioclim <- callModule(bioclim_MOD, 'c6_bioclim', rvs)
  
  observeEvent(input$goBioclim, {
    e <- mod.bioclim()
    rvs$mods <- e$models
    rvs$modPreds <- e$predictions
    rvs$modRes <- e$results
    rvs$modOccVals <- e$occVals
    output$evalTbl <- DT::renderDataTable(round(rvs$modRes, digits=3))
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  ########################################### #
  ### COMPONENT 7: VISUALIZE MODEL RESULTS ####
  ########################################### #
  
  # ui that populates with the names of models that were run
  output$modSelUI <- renderUI({
    req(rvs$modPreds)
    n <- names(rvs$modPreds)
    predNameList <- setNames(as.list(n), n)
    selectInput('modSel', label = "Choose a model",
                choices = predNameList, selected = predNameList[[1]])
  })
  
  # always update the selected model in rvs
  observe({
    rvs$modSel <- input$modSel
  })
  
  mapPreds <- callModule(mapPreds_MOD, 'c7_mapPreds', rvs, map)
  
  observeEvent(input$goMapPreds, {
    mapPreds.call <- mapPreds()
    req(mapPreds.call)
    rvs$predCur <- mapPreds.call[[1]]
    predCurVals <- mapPreds.call[[2]]
    predType <- mapPreds.call[[3]]
    updateTabsetPanel(session, 'main', selected = 'Map')
    
    # MAPPING
    if (rvs$predThresh != 'noThresh') {
      rasPal <- c('gray', 'blue')
      map %>% addLegend("topright", colors = c('gray', 'blue'),
                  title = "Thresholded Suitability", labels = c(0, 1),
                  opacity = 1, layerId = 'r1LegThr')
    } else {
      rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
      nonNAvals <- predCurVals[!is.na(predCurVals)]
      legendPal <- colorNumeric(rev(rasCols), nonNAvals, na.color='transparent')
      rasPal <- colorNumeric(rasCols, nonNAvals, na.color='transparent')
      map %>% addLegend("topright", pal = legendPal, title = "Predicted Suitability",
                  values = nonNAvals, layerId = 'r1LegCon',
                  labFormat = reverseLabels(2, reverse_order=TRUE))
    }
    map %>% addRasterImage(rvs$predCur, colors = rasPal, opacity = 0.7, 
                           group = 'r1', layerId = 'r1ID')
    shinyjs::enable("dlPreds")
  })
  
  
  output$dlPreds <- downloadHandler(
    filename = function() {
      ext <- ifelse(input$predFileType == 'raster', 'zip',
                    ifelse(input$predFileType == 'ascii', 'asc',
                           ifelse(input$predFileType == 'GTiff', 'tif', 'png')))
      paste0(names(rvs$predCur), '.', ext)},
    content = function(file) {
      if (input$predFileType == 'png') {
        png(file)
        image(rvs$predCur)
        dev.off()
      } else if (input$predFileType == 'raster') {
        fileName <- names(rvs$predCur)
        tmpdir <- tempdir()
        raster::writeRaster(rvs$predCur, file.path(tmpdir, fileName), format = input$predFileType, overwrite = TRUE)
        fs <- file.path(tmpdir, paste0(fileName, c('.grd', '.gri')))
        zip(zipfile=file, files=fs, extras = '-j')
      } else {
        r <- raster::writeRaster(rvs$predCur, file, format = input$predFileType, overwrite = TRUE)
        file.rename(r@file@name, file)
      }
    }
  )
  
})