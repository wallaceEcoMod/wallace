source("funcs/functions.R", local = TRUE)

logs <- reactiveValues(entries=logInit())
gtext <- reactiveValues()

options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {
  # disable download buttons
  shinyjs::disable("dlDbOccs")
  shinyjs::disable("dlProcOccCsv")
  shinyjs::disable("predDnld")
  shinyjs::disable("downloadMskPreds")
  shinyjs::disable("downloadPart")
  shinyjs::disable("downloadEvalcsv")
  shinyjs::disable("downloadEvalPlots")
  shinyjs::disable("downloadPred")
  shinyjs::disable("downloadPj")

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

#########################
### INITIALIZE ####
#########################

  output$log <- renderUI({tags$div(id='logHeader', tags$div(id='logContent', 
                                            HTML(paste0(logs$entries, "<br>", collapse = ""))))})

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
  
######################## #
### COMPONENT 1 ####
######################## #
  
  # component 1 reactives
  occs <- reactiveVal()  # occs for analysis that get updated throughout 
  occsOrigDnld <- reactiveVal() # original database query table for user download
  
  # module Query Database
  dbOccs <- callModule(queryDB_MOD, 'c1_queryDB', logs)
  
  # dbOccs <- eventReactive(input$goDbOccs, dbOccs.call())
  spName <- reactive(as.character(occs()$name[1]))
  
  observeEvent(input$goDbOccs, {
    occs(dbOccs())
    map %>%
      clearMarkers() %>%
      map_plotLocs(occs()) %>%
      zoom2Occs(occs())
    shinyjs::enable("dlDbOccs")
  })

  # module User Occurrence Data
  userOccs <- callModule(userOccs_MOD, 'c1_userOccs', logs)
  
  observeEvent(input$goUserOccs, {
    occs(userOccs())
    map %>%
      clearMarkers() %>%
      map_plotLocs(occs()) %>%
      zoom2Occs(occs())
    shinyjs::disable("dlDbOccs")
  })
      
  # TABLE
  options <- list(autoWidth = TRUE, columnDefs = list(list(width = '40%', targets = 7)),
                  scrollX=TRUE, scrollY=400)
  output$occTbl <- DT::renderDataTable(occs() %>% dplyr::select(-origID, -pop))

  # handle downloading of original GBIF records after cleaning
  output$dlDbOccs <- downloadHandler(
    filename = function() {paste0(formatSpName(spName()), '_', input$occDb, ".csv")},
    content = function(file) {
      write.csv(occsOrigDnld(), file, row.names=FALSE)
    }
  )

######################## #
### COMPONENT 2 ####
######################## #
  
  # module Spatial Thin
  thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs', logs, occs)
  
  observeEvent(input$goThinOccs, {
    occs(thinOccs())
    # MAPPING - blue pts for remove, red pts for keep
    map %>% 
      addCircleMarkers(data = dbOccs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'blue',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fillColor = 'red',
                       fillOpacity = 1, weight = 2, popup = ~pop,
                       group = 'comp2') %>%
      addLegend("topright", colors = c('red', 'blue'),
                title = "Occ Records", labels = c('retained', 'removed'),
                opacity = 1, layerId = 'thinLeg')
    shinyjs::enable("dlProcOccCsv")
  })
  
  # handle download for thinned records csv
  output$dlProcOccCsv <- downloadHandler(
    filename = function() {paste0(formatSpName(spName()), "_procOccs.csv")},
    content = function(file) {
      thinned_rowNums <- as.numeric(thinOccs()$origID)
      origThinned <- occsOrigDnld()[thinned_rowNums,]
      write.csv(origThinned, file, row.names = FALSE)
    }
  )

######################## #
### COMPONENT 3 ####
######################## #

  # map center coordinates for 30 arcsec download
  mapCntr <- reactive(mapCenter(input$map_bounds))
  
  output$ctrLatLon <- renderText({
    paste('Using map center', paste(mapCntr(), collapse=', '))
    })
  
  # reactive value to hold environmental predictor variables
  envs <- reactiveVal()
  
  # module WorldClim Bioclims
  wcBioclims <- callModule(wcBioclims_MOD, 'c3_wcBioclims', logs, mapCntr)
  
  observeEvent(input$goEnvData, {
    # load into envs
    envs(wcBioclims())
    occs.naEnvRem <- remEnvsValsNA(envs, occs)
    occs(occs.naEnvRem)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # enable download button
    shinyjs::enable("predDnld")
  })
  
  userEnvs <- callModule(userEnvs_MOD, 'c3_userEnvs', logs)
  
  observeEvent(input$goUserEnvs, {
    envs(userEnvs())
    occs.naEnvRem <- remEnvsValsNA(envs, occs)
    occs(occs.naEnvRem)
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  output$envsPrint <- renderPrint({
    req(envs())
    envs()
    # mins <- sapply(envs()@layers, function(x) x@data@min)
    # maxs <- sapply(envs()@layers, function(x) x@data@max)
    # names <- sapply(strsplit(names(envs()), '[.]'), function(x) x[-2])
    # mins <- round(cellStats(envs(), stat = min), digits = 3)
    # maxs <- round(cellStats(envs(), stat = max), digits = 3)
    # DT::datatable(data.frame(name=names, min=mins, max=maxs), 
    #               rownames = FALSE, options = list(pageLength = raster::nlayers(envs())))
  })

######################## #
### COMPONENT 4 ####
######################## #

  bgExt <- callModule(bgExtent_MOD, 'c4_bgExtent', logs, occs)
  
  observeEvent(input$goBgExt, {
    shp <- bgExt()
    coords <- shp@polygons[[1]]@Polygons[[1]]@coords
    map %>%
      addPolygons(lng=coords[,1], lat=coords[,2], layerId="backext",
                  weight=10, color="red", group='backgPoly') %>%
      fitBounds(max(coords[,1]), max(coords[,2]), min(coords[,1]), min(coords[,2]))
  })
  
  bgMskPts <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts', logs, envs, bgExt)
  
  observeEvent(input$goBgMask, {
    bgMskPts()
    # bgMsk <- bgMskPts()$msk
    # bgPts <- bgMskPts()$pts
  })

    
  # handle download for masked predictors, with file type as user choice
  output$dlMskPreds <- downloadHandler(
    filename = function() {'mskPreds.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      type <- input$mskPredsFileType
      nm <- names(bgMskPts()$msk)
      
      raster::writeRaster(bgMskPts()$msk, file.path(tmpdir, 'msk'), bylayer = TRUE,
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
})
