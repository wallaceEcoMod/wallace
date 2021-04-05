source("funcs/functions.R", local = TRUE)

# if (!require(librarian)){
#   install.packages("librarian")
#   library(librarian)
# }
# shelf(dismo, dplyr, DT, ENMeval, jsonlite, knitr, leaflet, leaflet.extras, raster, RColorBrewer, rmarkdown, shinyjs, sp, spocc, zip)

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
  shinyjs::disable("dlPred")
  shinyjs::disable("dlProj")
  shinyjs::disable("dlRMD")
  
  # initialize module parameters list
  rvs <- reactiveValues(logs = logInit(), comp1='', comp2='', comp3='', comp4.shp='', comp4.buf=0,
                        comp5='', comp6='', comp7.type='', comp7='', comp8.pj='', comp8.esim='')
  
  # observeEvent(input$load, {
  #   f <- read.csv('C:/Users/gepin/Desktop/maxnet_files/Canis lupus_partitioned_occs.csv')
  #   rvs$occs <- f %>% dplyr::filter(name != 'background')
  #   rvs$occs$pop <- unlist(apply(rvs$occs, 1, popUpContent))
  #   rvs$occsGrp <- rvs$occs$group
  #   rvs$bgPts <- f %>% dplyr::filter(name == 'background')
  #   rvs$bgGrp <- rvs$bgPts$group
  #   rvs$bgShp <- rgdal::readOGR('C:/Users/gepin/Desktop/maxnet_files/box.shp')
  #   rvs$bgPts <- rvs$bgPts %>% dplyr::select(longitude, latitude)
  #   rvs$envs <- raster::stack(list.files('C:/Users/gepin/Desktop/maxnet_files/bio_10m_bil', 'bil$', full.names=TRUE))
  #   rvs$bgMsk <- raster::stack(list.files('C:/Users/gepin/Desktop/maxnet_files/mskEnvs', 'tif$', full.names = TRUE))  
  #   print('HACKING DONE')
  # })
  
  # for RMD
  curWD <- getwd()
  
  # logs <- reactiveValues(entries=logInit())
  gtext <- reactiveValues()
  
  # load modules
  for (f in list.files('./modules')) {
    source(file.path('modules', f), local=TRUE)
  }
  
  # initialize log window
  output$log <- renderUI({tags$div(id='logHeader', tags$div(id='logContent', 
                                                            HTML(paste0(rvs$logs, "<br>", collapse = ""))))})
  
  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #
  
  # UI for component guidance text
  output$gtext_comp <- renderUI({
    shiny::includeMarkdown(file.path('Rmd', gtext$cur_comp))
  })
  
  # UI for module guidance text
  output$gtext_mod <- renderUI({
    shiny::includeMarkdown(file.path('Rmd', gtext$cur_mod))
  })
  
  # guidance text and tab behavior
  observe({
    if (input$tabs == 1) {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- 'gtext_comp1.Rmd'
      if (input$occSel == 'db') gtext$cur_mod <- "gtext_comp1_dbOccs.Rmd"
      if (input$occSel == 'user') gtext$cur_mod <- "gtext_comp1_userOccs.Rmd"
    }
    if (input$tabs == 2) {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp2.Rmd"
      # if Module: Select Localities, populate guidance text and select legend
      if (input$procOccSel == 'selOccs') gtext$cur_mod <- "gtext_comp2_selectOccsOnMap.Rmd"
      if (input$procOccSel == 'remID') gtext$cur_mod <- "gtext_comp2_removeByID.Rmd"
      if (input$procOccSel == 'spthin') gtext$cur_mod <- "gtext_comp2_spatialThin.Rmd"
    }
    if (input$tabs == 3) {
      updateTabsetPanel(session, 'main', selected = 'Results')
      gtext$cur_comp <- "gtext_comp3.Rmd"
      if (input$envDataSel == 'wcbc') gtext$cur_mod <- "gtext_comp3_worldclim.Rmd"
      if (input$envDataSel == 'user') gtext$cur_mod <- "gtext_comp3_userEnvs.Rmd"
    }
    if (input$tabs == 4) {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp4.Rmd"
      if (input$envProcSel == 'bgSel') gtext$cur_mod <- "gtext_comp4_backg.Rmd"
      if (input$envProcSel == 'bgUser') gtext$cur_mod <- "gtext_comp4_userBg.Rmd"
      
    }
    if (input$tabs == 5) {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp5.Rmd"
      if (input$partSel == 'sp') gtext$cur_mod <- "gtext_comp5_spatial.Rmd"
      if (input$partSel == 'nsp') gtext$cur_mod <- "gtext_comp5_nonspatial.Rmd"
    }
    if (input$tabs == 6) {
      updateTabsetPanel(session, 'main', selected = 'Results')
      gtext$cur_comp <- "gtext_comp6.Rmd"
      if (input$enmSel == 'BIOCLIM') gtext$cur_mod <- "gtext_comp6_bioclim.Rmd"
      if (input$enmSel == 'Maxent') gtext$cur_mod <- "gtext_comp6_maxent.Rmd"
    }
    if (input$tabs == 7) {
      gtext$cur_comp <- "gtext_comp7.Rmd"
      if (input$visSel == 'map') {
        updateTabsetPanel(session, 'main', selected = 'Map')
        gtext$cur_mod <- "gtext_comp7_map.Rmd"
      } else {
        updateTabsetPanel(session, 'main', selected = 'Results')
        if (input$visSel == 'bcPlots') gtext$cur_mod <- "gtext_comp7_bcPlots.Rmd"
        if (input$visSel == 'mxEval') gtext$cur_mod <- "gtext_comp7_mxEvalPlots.Rmd"
        if (input$visSel == 'response') gtext$cur_mod <- "gtext_comp7_respCurves.Rmd"
      }
    }
    if (input$tabs == 8) {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp8.Rmd"
      if (input$projSel == 'projArea') gtext$cur_mod <- "gtext_comp8_pjArea.Rmd"
      if (input$projSel == 'projTime') gtext$cur_mod <- "gtext_comp8_pjTime.Rmd"
      if (input$projSel == 'mess') gtext$cur_mod <- "gtext_comp8_mess.Rmd"
    }
  })
  
  ######################## #
  ### MAPPING ####
  ######################## #
  
  # create map
  m <- leaflet() %>% setView(0, 0, zoom = 2) %>% addProviderTiles('Esri.WorldTopoMap')
  output$map <- renderLeaflet(m)
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  # initialize provider tile option
  observe({map %>% addProviderTiles(input$bmap)})
  
  # initialize draw toolbar for c2_selectOccs and c8
  observe({
    if ((input$tabs == 2 & input$procOccSel == 'selOccs') | input$tabs == 8) {
      map %>% leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                             rectangleOptions = FALSE, circleOptions = FALSE,
                                             markerOptions = FALSE, circleMarkerOptions = FALSE,
                                             editOptions = leaflet.extras::editToolbarOptions())
    } else {
      map %>% leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
    }
  })
  
  observeEvent(input$map_draw_new_feature, {
    coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
    xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
    id <- input$map_draw_new_feature$properties$`_leaflet_id`
    if (input$tabs == 2 & input$procOccSel == 'selOccs') {
      rvs$polySelXY <- xy
      rvs$polySelID <- id
    } else if (input$tabs == 8) {
      rvs$polyPjXY <- xy
      rvs$polyPjID <- id  
    }
  })
  
  ########################################## #
  ### COMPONENT 1: OBTAIN OCCURRENCE DATA ####
  ########################################## #
  
  # module Query Database
  dbOccs <- callModule(queryDb_MOD, 'c1_queryDb', rvs)
  
  spName <- reactive(rvs$spName)
  
  observeEvent(input$goDbOccs, {
    rvs$occs <- dbOccs()
    rvs$occsPreProc <- rvs$occs
    # record for RMD
    rvs$comp1 <- 'db'
    map %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearImages() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
    shinyjs::enable("dlDbOccs")
    shinyjs::enable("dlRMD")
  })
  
  # module User Occurrence Data
  userOccs <- callModule(userOccs_MOD, 'c1_userOccs', rvs)
  
  observeEvent(input$goUserOccs, {
    rvs$occs <- userOccs()
    rvs$occsPreProc <- rvs$occs
    # record for RMD
    rvs$comp1 <- 'csv'
    map %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearImages() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
    shinyjs::disable("dlDbOccs")
    shinyjs::enable("dlRMD")
  })
  
  # TABLE
  options <- list(autoWidth = TRUE, columnDefs = list(list(width = '40%', targets = 7)),
                  scrollX=TRUE, scrollY=400)
  output$occTbl <- DT::renderDataTable({
    req(rvs$occs)
    occsDT <- rvs$occs %>% dplyr::mutate(longitude = round(as.numeric(longitude), digits = 2),
                                         latitude = round(as.numeric(latitude), digits = 2))
    occsDT %>% dplyr::select(name, occID, longitude:elevation)
  }, rownames = FALSE)
  
  # handle downloading of original GBIF records after cleaning
  output$dlDbOccs <- downloadHandler(
    filename = function() {paste0(formatSpName(spName()), '_original_', rvs$occDb, ".csv")},
    content = function(file) {
      write_csv_robust(rvs$occsOrig, file, row.names=FALSE)
    }
  )
  
  ########################################### #
  ### COMPONENT 2: PROCESS OCCURRENCE DATA ####
  ########################################### #
  
  # module Remove Occurrences By ID
  remByID <- callModule(removeByID_MOD, 'c2_removeByID', rvs)
  
  observeEvent(input$goRemoveByID, {
    rvs$occs <- remByID()
    # record for RMD
    rvs$comp2 <- c(rvs$comp2, 'rem')
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
    shinyjs::enable("dlProcOccs")
  })
  
  # module Select Occurrences on Map
  selOccs <- callModule(selectOccs_MOD, 'c2_selOccs', rvs)
  
  observeEvent(input$goSelectOccs, {
    rvs$occs <- selOccs()
    # record for RMD
    rvs$comp2 <- c(rvs$comp2, 'sel')
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs) %>%
      drawToolbarRefresh()
    shinyjs::enable("dlProcOccs")
  })
  
  # module Spatial Thin
  thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs', rvs)
  
  observeEvent(input$goThinOccs, {
    occsPreThin <- rvs$occs
    rvs$occs <- thinOccs()
    # stop if no occurrence data
    req(rvs$occs)
    # record for RMD
    rvs$comp2 <- c(rvs$comp2, 'thin')
    # MAPPING - blue pts for remove, red pts for keep
    map %>% 
      addCircleMarkers(data = occsPreThin, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'blue',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addCircleMarkers(data = rvs$occs, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'red',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addLegend("bottomright", colors = c('red', 'blue'),
                title = "Occ Records", labels = c('retained', 'removed'),
                opacity = 1, layerId = 'leg')
    shinyjs::enable("dlProcOccs")
  })
  
  # handle download for thinned records csv
  output$dlProcOccs <- downloadHandler(
    filename = function() {ifelse(rvs$comp1 == 'csv', 
                                  "user_processed_occs.csv", 
                                  paste0(formatSpName(spName()), "_processed_occs.csv"))},
    content = function(file) {
      # thinned_rowNums <- as.numeric(thinOccs()$occID)
      # origThinned <- rvs$occsOrig[thinned_rowNums,]
      write_csv_robust(rvs$occs %>% dplyr::select(-pop), file, row.names = FALSE)
    }
  )
  
  # Reset Occs button functionality
  observeEvent(input$goResetOccs, {
    rvs$occs <- rvs$occsPreProc  
    # reset for RMD
    rvs$comp2 <- NULL
    rvs %>% writeLog("Reset occurrences.")
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
  })
  
  ############################################# #
  ### COMPONENT 3: OBTAIN ENVIRONMENTAL DATA ####
  ############################################# #
  
  # map center coordinates for 30 arcsec download
  mapCntr <- reactive(mapCenter(input$map_bounds))
  
  output$ctrLatLon <- renderText({
    paste('Using map center', paste(mapCntr(), collapse=', '))
  })
  
  # module WorldClim Bioclims
  wcBioclims <- callModule(wcBioclims_MOD, 'c3_wcBioclims', rvs, mapCntr)
  
  observeEvent(input$goEnvData, {
    # load into envs
    rvs$envs <- wcBioclims()
    # stop if no occurrence data
    req(rvs$occs)
    req(rvs$envs)
    # record for RMD
    rvs$comp3 <- 'bc'
    # remove occurrences with NA values for variables
    rvs$occs <- remEnvsValsNA(rvs)
    # MAPPING - Plot the remaining occs after of removing NA
    map %>%
      clearMarkers() %>%
      clearControls() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # enable download button
    # shinyjs::enable("dlEnvs")
  })
  
  # module User-defined Environmental Predictors
  userEnvs <- callModule(userEnvs_MOD, 'c3_userEnvs', rvs)
  
  observeEvent(input$goUserEnvs, {
    rvs$envs <- userEnvs()
    # stop if no occurrence data
    req(rvs$occs)
    # record for RMD
    rvs$comp3 <- 'user'
    # remove occurrences with NA values for variables
    rvs$occs <- remEnvsValsNA(rvs)
    # make project to new time module unavailable for user envs
    updateRadioButtons(session, "projSel", 
                       choices = list("Project to New Extent" = 'projArea',
                                      "Calculate Environmental Similarity" = 'mess'))
    # MAPPING - Plot the remaining occs after of removing NA
    map %>%
      clearMarkers() %>%
      map_plotLocs(rvs$occs) %>%
      zoom2Occs(rvs$occs)
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
  
  # module Background Extent
  bgExt <- callModule(bgExtent_MOD, 'c4_bgExtent', rvs)
  
  bgShpXY <- reactive({
    polys <- rvs$bgShp@polygons[[1]]@Polygons
    if (length(polys) == 1) {
      xy <- list(rvs$bgShp@polygons[[1]]@Polygons[[1]]@coords)
    } else {
      xy <- sapply(polys, function(x) x@coords)
    }
    return(xy)
  })
  
  observeEvent(input$goBgExt, {
    rvs$bgShp <- bgExt()
    # stop if no environmental variables
    req(rvs$envs)
    map %>% clearShapes()
    for (shp in bgShpXY()) {
      map %>%
        addPolygons(lng=shp[,1], lat=shp[,2],
                    weight=4, color="gray", group='bgShp')  
    }
    map %>%
      fitBounds(rvs$bgShp@bbox[1], rvs$bgShp@bbox[2], rvs$bgShp@bbox[3], rvs$bgShp@bbox[4])
  })
  
  # module User-defined Background Extent
  userBg <- callModule(userBgExtent_MOD, 'c4_userBgExtent', rvs)
  
  observeEvent(input$goUserBg, {
    rvs$bgShp <- userBg()
    # stop if no environmental variables
    req(rvs$envs)
    req(rvs$bgShp)
    coords <- rvs$bgShp@polygons[[1]]@Polygons[[1]]@coords
    map %>% clearShapes()
    for (shp in bgShpXY()) {
      map %>%
        addPolygons(lng=shp[,1], lat=shp[,2],
                    weight=4, color="gray", group='bgShp')  
    }
    map %>%
      fitBounds(rvs$bgShp@bbox[1], rvs$bgShp@bbox[2], rvs$bgShp@bbox[3], rvs$bgShp@bbox[4])
  })
  
  # module Background Mask and Sample Points
  observeEvent(input$goBgMask, {
    bgMskPts.call <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts', rvs)
    bgMskPts <- bgMskPts.call()
    # stop if no background shape
    req(rvs$bgShp)
    rvs$bgMsk <- bgMskPts$msk
    rvs$bgPts <- bgMskPts$pts
    shinyjs::enable('dlMskEnvs')
  })
  
  # handle download for masked predictors, with file type as user choice
  output$dlMskEnvs <- downloadHandler(
    filename = function() {'mskEnvs.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      type <- input$bgMskFileType
      nm <- names(rvs$bgMsk)
      
      raster::writeRaster(rvs$bgMsk, file.path(tmpdir, 'msk'), bylayer = TRUE,
                          suffix = nm, format = type, overwrite = TRUE)
      ext <- switch(type, raster = 'grd', ascii = 'asc', GTiff = 'tif')
      
      fs <- paste0('msk_', nm, '.', ext)
      if (ext == 'grd') {
        fs <- c(fs, paste0('msk_', nm, '.gri'))
      }
      zip::zip(zipfile=file, files=fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
  )
  
  
  ############################################# #
  ### COMPONENT 5: PARTITION OCCURRENCE DATA ####
  ############################################# #
  
  # module Non-spatial Occurrence Partitions
  partNsp.call <- callModule(partNsp_MOD, 'c5_partNsp', rvs)
  observeEvent(input$goPartNsp, {
    partNsp <- partNsp.call()
    # stop if no background mask
    req(rvs$bgMsk)
    rvs$occsGrp <- partNsp[[1]]
    rvs$bgGrp <- partNsp[[2]]
    map %>% comp5_map(rvs$occs, rvs$occsGrp)
    shinyjs::enable("dlPart")
  })

  # module Spatial Occurrence Partitions
  partSp.call <- callModule(partSp_MOD, 'c5_partSp', rvs)  
  observeEvent(input$goPartSp, {
    partSp <- partSp.call()
    # stop if no background mask
    req(rvs$bgMsk)
    rvs$occsGrp <- partSp[[1]]
    rvs$bgGrp <- partSp[[2]]
    map %>% comp5_map(rvs$occs, rvs$occsGrp)
    shinyjs::enable("dlPart")
  })
  
  # download for partitioned occurrence records csv
  output$dlPart <- downloadHandler(
    filename = function() paste0(spName(), "_partitioned_occs.csv"),
    content = function(file) {
      bg.bind <- data.frame(rep('background', nrow(rvs$bgPts)), rvs$bgPts)
      names(bg.bind) <- c('name', 'longitude', 'latitude')
      occs.bg.bind <-rbind(rvs$occs[,1:3], bg.bind)
      all.bind <- cbind(occs.bg.bind, c(rvs$occsGrp, rvs$bgGrp))
      names(all.bind)[4] <- "group"
      write_csv_robust(all.bind, file, row.names = FALSE)
    }
  )
  
  ######################### #
  ### COMPONENT 6: MODEL ####
  ######################### #

  # module Maxent
  mod.maxent <- callModule(maxent_MOD, 'c6_maxent', rvs)
  
  observeEvent(input$goMaxent, {
    # stop if no occurrence partition group
    req(rvs$occsGrp)
    # get model evaluations
    e <- mod.maxent()
    # if e is NULL, let the c6_maxent module throw the proper error and stop
    req(e)
    rvs$comp6 <- 'maxent'  # record the enm selected
    rvs$mods <- e@models
    rvs$modPreds <- e@predictions
    rvs$modRes <- e@results
    rvs$modPart <- e@results.partitions
    # x <- callModule(mapPreds_MOD, 'c7_mapPreds', rvs)
    
    ncols.Res <- ncol(rvs$modRes)
    modRes.round <- cbind(rvs$modRes[, 1:3], 
                          round(rvs$modRes[, 4:ncols.Res], digits = 3))
    ncols.Part <- ncol(rvs$modPart)
    modPart.round <- cbind(rvs$modPart[, 1:2], 
                           round(rvs$modPart[, 3:ncols.Part], digits = 3))
    # render both full model and partition avg datatable, and individual partition datatable
    output$evalTbl <- DT::renderDataTable(modRes.round, 
                                          options = list(scrollX = TRUE,
                                                         sDom  = '<"top">rtp<"bottom">'))
    output$evalTblBins <- DT::renderDataTable(modPart.round, 
                                              options = list(scrollX = TRUE,
                                                             sDom  = '<"top">rtp<"bottom">'))
    output$lambdas <- renderPrint({
      modCur <- rvs$mods[[as.character(rvs$modSel)]]
      if (rvs$algMaxent == "maxnet") {
        modCur$betas
      } else if (rvs$algMaxent == "maxent.jar") {
        modCur@lambdas
      }
    })
    shinyjs::show(id = "evalTblBins")
    
    output$evalTbls <- renderUI({
      tabsetPanel(
        tabPanel("Evaluation", 
                 tagList(
                   br(),
                   div("Full model and partition bin average evaluation statistics", id="stepText"), br(), br(),
                   DT::dataTableOutput('evalTbl'), br(), 
                   div("Individual partition bin evaluation statistics", id="stepText"), br(), br(),
                   DT::dataTableOutput('evalTblBins')  
                 )),
        tabPanel("Lambdas",
                 br(),
                 div("Maxent Lambdas File", id="stepText"), br(), br(),
                 verbatimTextOutput("lambdas")
                 )
      )
      
    })
    
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # customize visualizations for maxent
    updateRadioButtons(session, "visSel", 
                       choices = list("Maxent Evaluation Plots" = 'mxEval',
                                      "Plot Response Curves" = 'response',
                                      "Map Prediction" = 'map'))
  })
  
  # module BIOCLIM
  mod.bioclim <- callModule(bioclim_MOD, 'c6_bioclim', rvs)
  
  observeEvent(input$goBioclim, {
    e <- mod.bioclim()
    # stop if no occurrence partition group
    req(rvs$occsGrp)
    rvs$comp6 <- 'bioclim'  # record the enm selected
    rvs$mods <- e@models
    rvs$modPreds <- e@predictions
    rvs$modRes <- e@results
    rvs$modPart <- e@results.partitions
    output$evalTbl <- DT::renderDataTable(round(rvs$modRes, digits = 3), 
                                          options = list(scrollX = TRUE, 
                                                         sDom  = '<"top">rtp<"bottom">'))
    output$evalTblBins <- DT::renderDataTable(
      round(rvs$modPart[, 2:ncol(rvs$modPart)], digits = 3),
      options = list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">'))
    output$evalTbls <- renderUI({
      tagList(
        br(),
        div("Full model and partition bin average evaluation statistics", 
            id = "stepText"), br(), br(),
        DT::dataTableOutput('evalTbl'), br(), 
        div("Individual partition bin evaluation statistics", 
            id = "stepText"), br(), br(),
        DT::dataTableOutput('evalTblBins') 
      )
    })
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    updateRadioButtons(session, "visSel", 
                       choices = list("BIOCLIM Envelope Plots" = 'bcPlots',
                                      "Map Prediction" = 'map'))
    shinyjs::show(id = "evalTblBins")
  })
  
  # download for partitioned occurrence records csv
  output$dlEvalTbl <- downloadHandler(
    filename = function() {
      if (rvs$comp6 == "bioclim") {
        paste0(spName(), "_bioclim_evalTbl.csv")  
      } else if (rvs$comp6 == "maxent") {
        paste0(spName(), "_maxent_evalTbl.csv")  
      }
    },
    content = function(file) {
      write_csv_robust(rvs$modRes, file, row.names = FALSE)
    }
  )
  
  ########################################### #
  ### COMPONENT 7: VISUALIZE MODEL RESULTS ####
  ########################################### #
  
  # ui that populates with the names of models that were run
  output$modSelUI <- renderUI({
    req(rvs$modPreds)
    n <- names(rvs$modPreds)
    modsNameList <- setNames(as.list(n), n)
    selectInput('modSel', label = "Current Model",
                choices = modsNameList, selected = modsNameList[[1]])
  })
  
  # ui that populates with the names of environmental predictors used
  output$envSelUI <- renderUI({
    req(rvs$modPreds)
    req(rvs$modSel)
    # for Maxent, only display the environmental predictors with non-zero beta coefficients
    # from the lambdas file (the predictors that were not removed via regularization)
    if (rvs$comp6 == "maxent") {
      modCur <- rvs$mods[[rvs$modSel]]
      if (rvs$algMaxent == "maxnet") {
        nonZeroCoefs <- mxNonzeroCoefs(modCur, alg = "maxnet")
      } else if(rvs$algMaxent == "maxent.jar") {
        nonZeroCoefs <- mxNonzeroCoefs(modCur, alg = "maxent.jar")
      }
      envsNames <- names(rvs$bgMsk[[nonZeroCoefs]])
      rvs$mxNonZeroCoefs <- envsNames
    } else {
      envsNames <- names(rvs$bgMsk)
    }
    envsNamesList <- setNames(as.list(envsNames), envsNames)
    selectInput("envSel", "Current Env Variable",
                choices = envsNamesList, selected = envsNamesList[[1]])
  })
  
  # always update the selected model and environmental predictor in rvs
  observe({
    rvs$modSel <- input$modSel
    rvs$envSel <- input$envSel
  })
  
  # module BIOCLIM Plots
  bcPlots <- callModule(bcPlots_MOD, 'c7_bcPlots', rvs)
  output$bcEnvelPlot <- renderPlot({
    bcPlots()
  })
  
  # module Maxent Evaluation Plots
  mxEvalPlots <- callModule(mxEvalPlots_MOD, 'c7_mxEvalPlots', rvs)
  output$mxEvalPlots <- renderPlot({
    mxEvalPlots()
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  # module Response Curve Plots
  respPlots <- callModule(respPlots_MOD, 'c7_respPlots', rvs)
  output$respPlots <- renderPlot({
    respPlots()
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  # module Map Prediction (restricted to background extent)
  observeEvent(input$goMapPreds, {
    if (rvs$comp6 == 'maxent') {
      mapPreds <- callModule(mapPredsMaxent_MOD, 'c7_mapPredsMaxent', rvs)
    } else {
      mapPreds <- callModule(mapPreds_MOD, 'c7_mapPreds', rvs)
    }
    # stop if mapPreds is NULL
    rvs$predCur <- mapPreds()
    req(rvs$predCur)
    
    # stop if no models
    req(rvs$mods)
    rvs$predCurVals <- getVals(rvs$predCur, rvs$comp7.type)
    updateTabsetPanel(session, 'main', selected = 'Map')
    
    # MAPPING
    if (rvs$comp7.thr != 'noThresh') {
      rasPal <- c('gray', 'blue')
      map %>% addLegend("bottomright", colors = c('gray', 'blue'),
                        title = "Thresholded Suitability", labels = c("predicted absence", "predicted presence"),
                        opacity = 1, layerId = 'leg')
    } else {
      rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
      legendPal <- colorNumeric(rev(rasCols), rvs$predCurVals, na.color='transparent')
      rasPal <- colorNumeric(rasCols, rvs$predCurVals, na.color='transparent')
      map %>% addLegend("bottomright", pal = legendPal, title = "Predicted Suitability",
                        values = rvs$predCurVals, layerId = 'leg',
                        labFormat = reverseLabels(2, reverse_order=TRUE))
    }
    map %>% 
      clearMarkers() %>% clearImages() %>% clearShapes() %>%
      map_plotLocs(rvs$occs) %>%
      addRasterImage(rvs$predCur, colors = rasPal, opacity = 0.7, 
                     group = 'c7', layerId = 'r1ID', project = TRUE)
    for (shp in bgShpXY()) {
      map %>%
        addPolygons(lng=shp[,1], lat=shp[,2], fill = FALSE,
                    weight=4, color="red", group='c7')  
    }
    shinyjs::enable("dlPred")
  })
  
  # download for model predictions (restricted to background extent)
  output$dlPred <- downloadHandler(
    filename = function() {
      ext <- switch(input$predFileType, raster = 'zip', ascii = 'asc', GTiff = 'tif', png = 'png')
      paste0(names(rvs$predCur), '.', ext)},
    content = function(file) {
      # browser()
      if (require(rgdal)) {
        if (input$predFileType == 'png') {
          png(file)
          raster::image(rvs$predCur)
          dev.off()
        } else if (input$predFileType == 'raster') {
          fileName <- names(rvs$predCur)
          tmpdir <- tempdir()
          raster::writeRaster(rvs$predCur, file.path(tmpdir, fileName), format = input$predFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zip(zipfile=file, files=fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(rvs$predCur, file, format = input$predFileType, overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        rvs %>% writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )
  
  ########################################### #
  ### COMPONENT 8: PROJECT MODEL ####
  ########################################### #
  
  # module Project to New Area
  projArea <- callModule(projectArea_MOD, 'c8_projectArea', rvs)
  
  observeEvent(input$goProjectArea, {
    projArea.call <- projArea()
    req(projArea.call)
    # stop if no model prediction
    req(rvs$predCur)
    # unpack
    rvs$projMsk <- projArea.call[[1]]
    rvs$projCur <- projArea.call[[2]]
    rvs$projCurVals <- getVals(rvs$projCur, rvs$comp7.type)
    rvs$comp8.pj <- 'area'
    
    rasVals <- c(rvs$predCurVals, rvs$projCurVals)
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    rasPal <- colorNumeric(rasCols, rvs$predCurVals, na.color='transparent')
    
    map %>% 
      addRasterImage(rvs$predCur, colors = rasPal, opacity = 0.7, 
                     group = 'c7', layerId = 'r1ID') %>%
      comp8_map(rvs$projCur, rvs$polyPjXY, bgShpXY, rasVals, 
                      rasCols, "Predicted Suitability", 
                      addID = 'rProjArea', clearID = c('rProjArea', 'rProjTime', 'rProjMESS'))
    
    map %>% drawToolbarRefresh()
    
    shinyjs::enable("dlProj")
  })
  
  # module Project to New Time
  projTime <- callModule(projectTime_MOD, 'c8_projectTime', rvs)
  
  observeEvent(input$goProjectTime, {
    projTime.call <- projTime()
    req(projTime.call)
    # stop if no model prediction
    req(rvs$predCur)
    # unpack
    rvs$projMsk <- projTime.call[[1]]
    rvs$projCur <- projTime.call[[2]]
    req(rvs$projCur)
    rvs$projCurVals <- getVals(rvs$projCur, rvs$comp7.type)
    rvs$comp8.pj <- 'time'
    raster::crs(rvs$projCur) <- raster::crs(rvs$bgMsk)
    rasVals <- c(rvs$predCurVals, rvs$projCurVals)
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    map %>% comp8_map(rvs$projCur, rvs$polyPjXY, bgShpXY, rasVals, 
                      rasCols, "Predicted Suitability", 
                      addID = 'rProjTime', clearID = c('r1ID', 'rProjArea', 'rProjTime', 'rProjMESS'))
    
    map %>% drawToolbarRefresh()
    
    shinyjs::enable("dlProj")
  })
  
  # module Environmental Similarity
  envSimilarity <- callModule(envSimilarity_MOD, 'c8_envSimilarity', rvs)
  
  observeEvent(input$goEnvSimilarity, {
    rvs$mess <- envSimilarity()
    req(rvs$mess)
    # stop if no model projection
    req(rvs$projCur)
    rvs$comp8.esim <- 'mess'
    # set infinite values to NA
    rvs$mess[is.infinite(rvs$mess)] <- NA
    # extract values
    rvs$messVals <- getVals(rvs$mess)
    rasVals <- rvs$messVals
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
    map %>% comp8_map(rvs$mess, rvs$polyPjXY, bgShpXY, rasVals, rasCols, "MESS Values",
                      addID = 'rProjMESS', clearID = c('r1ID', 'rProjArea', 'rProjTime', 'rProjMESS'))
    
    shinyjs::enable("dlProj")
  })
  
  # Reset Projection Extent button functionality
  observeEvent(input$goResetProj, {
    map %>%
      removeShape("projExt") %>%
      removeImage(c("rProjArea", "rProjTime", "rProjMESS"))
    rvs %>% writeLog("Reset projection extent.")
  })
  
  # download for model predictions (restricted to background extent)
  output$dlProj <- downloadHandler(
    filename = function() {
      ext <- switch(input$projFileType, raster = 'zip', ascii = 'asc', GTiff = 'tif', PNG = 'png')
      paste0(names(rvs$projCur), '.', ext)},
    content = function(file) {
      if (require(rgdal)) {
        if (input$projFileType == 'png') {
          png(file)
          raster::image(rvs$projCur)
          dev.off()
        } else if (input$projFileType == 'raster') {
          fileName <- names(rvs$projCur)
          tmpdir <- tempdir()
          raster::writeRaster(rvs$projCur, file.path(tmpdir, fileName), format = input$projFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zip(zipfile=file, files=fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(rvs$projCur, file, format = input$projFileType, overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        rvs %>% writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )
  
  ########################################### #
  ### MARKDOWN FUNCTIONALITY ####
  ########################################### #
  
  # handler for R Markdown download
  output$dlRMD <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".", switch(
        input$rmdFileType, Rmd = 'Rmd', PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))},
    content = function(file) {
      # convert removed occIDs to characters of vectors
      if (!is.null(rvs$removedIDs)) {
        rvs$occsRem <- printVecAsis(rvs$removedIDs)  
      }
      # convert polygon coordinates to characters of vectors
      if (!is.null(rvs$polySelXY)) {
        polySelX <- printVecAsis(round(rvs$polySelXY[,1], digits=4))
        polySelY <- printVecAsis(round(rvs$polySelXY[,2], digits=4))
      } else {
        polySelX <- polySelY <- NULL
      }
      if (!is.null(rvs$polyPjXY)) {
        polyPjX <- printVecAsis(round(rvs$polyPjXY[,1], digits=4))
        polyPjY <- printVecAsis(round(rvs$polyPjXY[,2], digits=4))
      } else {
        polyPjX <- polyPjY <- NULL
      }
      bcSels <- printVecAsis(rvs$bcSels)
      exp <- knitr::knit_expand("Rmd/userReport.Rmd", 
                                curWD = curWD, 
                                # comp 1
                                spName = spName(), 
                                dbName = rvs$occDb, 
                                occNum = rvs$occNum, 
                                occsCSV = rvs$userCSV$name,
                                # comp 2
                                thinDist = rvs$thinDist, 
                                occsRemoved = rvs$occsRem,
                                occsSelX = polySelX,
                                occsSelY = polySelY,  
                                # comp 3
                                bcRes = rvs$bcRes, 
                                bcLat = rvs$bcLat, 
                                bcLon = rvs$bcLon,
                                userEnvs = printVecAsis(rvs$userEnvs$name), 
                                bcSels = bcSels, 
                                # comp 4
                                bgSel = rvs$comp4.shp, 
                                bgBuf = rvs$comp4.buf, 
                                #bgUserCSVname = rvs$bgUserCSVname,
                                #bgUserCSVpath = rvs$bgUserCSVpath,
                                bgUserShpPath = rvs$bgUserShpPar$dsn, 
                                bgUserShpName = rvs$bgUserShpPar$layer, 
                                bgPtsNum = rvs$bgPtsNum,
                                # comp 5
                                partSel = rvs$partSel, 
                                kfolds = rvs$kfolds, 
                                aggFact = rvs$aggFact,
                                # comp 6
                                enmSel = rvs$comp6,
                                algMaxent = rvs$algMaxent,
                                clamp = rvs$clamp,
                                rms1 = rvs$rms[1],
                                rms2 = rvs$rms[2],
                                rmsStep = rvs$rmsStep,
                                fcs = printVecAsis(rvs$fcs),
                                # comp 7
                                modSel = rvs$modSel, 
                                mxNonZeroCoefs = printVecAsis(rvs$mxNonZeroCoefs),
                                envSel = rvs$envSel,
                                bcPlot1 = rvs$bcPlotsPar$bc1, 
                                bcPlot2 = rvs$bcPlotsPar$bc2,
                                bcPlotP = rvs$bcPlotsPar$p,
                                mxEvalSel = rvs$mxEvalSel,
                                predType = rvs$comp7.type,
                                comp7.thresh = rvs$comp7.thr,
                                # comp 8 
                                occsPjX = polyPjX,
                                occsPjY = polyPjY,
                                pjRCP = rvs$pjTimePar$rcp,
                                pjGCM = rvs$pjTimePar$gcm,
                                pjYear = rvs$pjTimePar$year,
                                comp8.thresh = rvs$comp8.thr)
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      writeLines(exp, 'userReport2.Rmd')
      
      if (input$rmdFileType == 'Rmd') {
        out <- rmarkdown::render('userReport2.Rmd', rmarkdown::md_document(variant="markdown_github"))
        writeLines(gsub('``` r', '```{r}', readLines(out)), 'userReport3.Rmd')
        out <- 'userReport3.Rmd'
      } else {
        out <- rmarkdown::render('userReport2.Rmd', 
                                 switch(input$rmdFileType,
                                        PDF = rmarkdown::pdf_document(latex_engine='xelatex'), 
                                        HTML = rmarkdown::html_document(), 
                                        Word = rmarkdown::word_document())
        )
      }
      file.rename(out, file)
    }
  )
})

