# load helper functions
source("helper_functions.R")
wd <- getwd()
# load module functions
setwd("..")
setwd("..")
funcs <- list.files(path="R", full.names=TRUE)
sapply(funcs, source)
setwd(wd)

options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {
  # disable download buttons
  shinyjs::disable("dlDbOccs")
  shinyjs::disable("dlOccs")
  shinyjs::disable("dlAllOccs")
  shinyjs::disable("dlProcOccs")
  # shinyjs::disable("dlEnvs")
  shinyjs::disable("dlBgShp")
  shinyjs::disable("dlMskEnvs")
  shinyjs::disable("dlBgPts")
  shinyjs::disable("downloadEvalcsv")
  shinyjs::disable("downloadEvalPlots")
  shinyjs::disable("dlPred")
  shinyjs::disable("dlProj")
  # shinyjs::disable("dlRMD")
  
  ########################## #
  # REACTIVE VALUES LISTS ####
  ########################## #
  
  # single species list of lists
  spp <- reactiveValues()
  # multiple species list of lists: each entity is a species pair list which contains
  # results of multispecies analyses
  msp <- reactiveValues()
  # list with current guidance text
  gtext <- reactiveValues()
  # single reactive value for dynamic log vector
  intro <- '***WELCOME TO WALLACE***'
  brk <- paste(rep('------', 14), collapse='')
  expl <- 'Please find messages for the user in this log window.'
  logInit <- c(paste(intro, brk, expl, brk, '', sep='<br>'))
  shinyLogs <- reactiveVal(logInit)
  
  # load modules
  for (f in list.files('./modules')) source(file.path('modules', f), local=TRUE)
  
  # # FOR DEVELOPMENT PURPOSES
  # observeEvent(input$load, {
  #   f <- c1_userOccs('example_data/multispecies copy.csv', "multispecies copy.csv")
  #   # wc <- c3_worldclim(10, paste0('bio', 1:19))
  #   # wc <- raster::brick(wc)
  #   # r <- list()
  #   # ls1 <- list.files('example_data/Procyon_lotor_mskEnvs', full.names = TRUE)
  #   # ls1 <- ls1[-which(grepl("gri$",ls1))]
  #   # r[["Procyon_lotor"]] <- raster::stack(ls1)
  #   # r[["Procyon_lotor"]] <- raster::brick(r[["Procyon_lotor"]])
  #   # ls2 <- list.files('example_data/Nyctereutes_procyonoides_mskEnvs', full.names = TRUE)
  #   # ls2 <- ls2[-which(grepl("gri$",ls2))]
  #   # r[["Nyctereutes_procyonoides"]] <- raster::stack(ls2)
  #   # r[["Nyctereutes_procyonoides"]] <- raster::brick(r[["Nyctereutes_procyonoides"]])
  #   for(n in c("Procyon_lotor", "Nyctereutes_procyonoides")) {
  #     occs <- f[[n]]$occs
  #     occs$partition <- NULL
  #     spp[[n]] <- list(occs = occs, occData = list(occsCleaned = occs),
  #                      rmm = rangeModelMetadata::rmmTemplate())
  #     # spp[[n]]$envs <- wc
  #     # spp[[n]]$bg <- f[[n]]$bg
  #     # spp[[n]]$procEnvs <- list()
  #     # spp[[n]]$procEnvs$bgMask <- r[[n]]
  #     # spp[[n]]$occs$partition <- f[[n]]$occs$partition
  #   }
  #   print('SECRET DATA LOADED')
  # })
  
  # initialize log window
  output$log <- renderUI({
    tags$div(id='logHeader', tags$div(id='logContent', HTML(paste0(shinyLogs(), "<br>", collapse = ""))))
  })
  
  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #
  
  # UI for component guidance text
  output$gtext_component <- renderUI({
    shiny::includeMarkdown(file.path('Rmd', gtext$curComponent))
  })
  
  # UI for module guidance text
  output$gtext_module <- renderUI({
    shiny::includeMarkdown(file.path('Rmd', gtext$curModule))
  })
  
  # tab and module-level reactives
  component <- reactive({input$tabs})
  module <- reactive({
    if(component() == "intro") "intro"
    else if(component() == "occs") input$occsSel
    else if(component() == "poccs") input$procOccsSel
    else if(component() == "envs") input$envsSel
    else if(component() == "penvs") input$procEnvsSel
    else if(component() == "espace") input$espaceSel
    else if(component() == "part") input$partSel
    else if(component() == "model") input$modelSel
    else if(component() == "vis") input$visSel
    else if(component() == "proj") input$projSel
    #else if(component() == "rmd") ''
  })
  
  # logic to serve the selected component/module guidance text
  observe({
    gtext$curComponent <- paste0("gtext_", component(), ".Rmd")
    gtext$curModule <- paste0("gtext_", component(), "_", module(), ".Rmd")
  })
  
  ######################## #
  ### MAPPING LOGIC ####
  ######################## #
  
  # create map
  m <- leaflet() %>% 
    setView(0, 0, zoom = 2) %>% 
    addProviderTiles('Esri.WorldTopoMap') %>% 
    mapview::addMouseCoordinates()
  output$map <- renderLeaflet(m)
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  # initialize provider tile option
  observe({map %>% addProviderTiles(input$bmap)})
  
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
  
  # MAPPING LOGIC ####
  observe({
    # must have one species selected and occurrence data
    req(length(curSp()) == 1, occs())
    f <- switch(module(), 
                "dbOccs" = queryDb_MAP, 
                "userOccs" = userOccs_MAP,
                "selOccs" = selectOccs_MAP, 
                "remID" = removeByID_MAP, 
                "spthin" = thinOccs_MAP, 
                "wcbc" = wcBioclims_MAP,
                "ecoClimate" = ecoclimate_MAP,
                "userEnvs" = userEnvs_MAP,
                "bgSel" = bgExtent_MAP, 
                "bgUser" = userBgExtent_MAP,
                "bgDraw" = drawBgExtent_MAP,
                "nsp" = partitionNonSpat_MAP, 
                "sp" = partitionSpat_MAP,
                "mapPreds" = mapPreds_MAP, 
                "projArea" = projectArea_MAP, 
                "projTime" = projectTime_MAP, 
                "mess" = envSimilarity_MAP)
    req(f)
    map %>% f(session)
  })
  
  ########################################## #
  # COMPONENT: OBTAIN OCCURRENCE DATA ####
  ########################################## #
  
  # # # # # # # # # # # # # # # # # # # #
  # module Query Database (Present) ####
  # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goDbOccs, {
    queryDb <- callModule(queryDb_MOD, 'c1_queryDb_uiID')
    # return the occs table
    occsTbl <- queryDb()
    n <- formatSpName(occsTbl$scientific_name)
    # UI CONTROLS
    # assign the selected species to the present occ table's scientific name
    updateSelectInput(session, "curSp", selected = n)
    shinyjs::enable("dlDbOccs")
    shinyjs::enable("dlOccs")
    if (length(allSp()) > 1) shinyjs::enable("dlAllOccs")
    shinyjs::enable("dlRMD")
  })
  
  # # # # # # # # # # # # # # # # # # # 
  # module Query Database (Paleo) ####
  # # # # # # # # # # # # # # # # # # # 
  observeEvent(input$goPaleoDbOccs, {
    paleoDb <- callModule(queryPaleoDb_MOD, 'c1_queryPaleoDb_uiID')
    # return the occs table
    occsTbl <- paleoDb()
    n <- formatSpName(occsTbl$scientific_name)
    # UI CONTROLS
    # assign the selected species to the present occ table's scientific name
    updateSelectInput(session, "curSp", selected = n)
    shinyjs::enable("dlOccs")
    shinyjs::enable("dlRMD")
  })
  
  # # # # # # # # # # # # # # # # # #
  # module User Occurrence Data ####
  # # # # # # # # # # # # # # # # # #
  observeEvent(input$goUserOccs, {
    userOccs <- callModule(userOccs_MOD, 'c1_userOccs_uiID')
    userOccs()
    shinyjs::enable("dlOccs")
    if (length(allSp()) > 1) shinyjs::enable("dlAllOccs")
    shinyjs::enable("dlRMD")
  })
  
  # # # # # # # # # # # # # # # # # #
  # OBTAIN OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  output$curSpUI <- renderUI({
    # check that a species is in the list already -- if not, don't proceed
    # req(length(reactiveValuesToList(spp)) > 0)
    # get the species names
    n <- names(spp)
    # make a named list of their names
    sppNameList <- c(list("Current species" = ""), setNames(as.list(n), n))
    # if no current species selected, select the first name
    # NOTE: this line is necessary to retain the selection after selecting different tabs
    if(!is.null(curSp())) selected <- curSp() else selected <- n[1]
    # if espace component, allow for multiple species selection
    if(component() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    # generate a selectInput ui that lists the available species
    selectizeInput('curSp', label = NULL , choices = sppNameList,
                   multiple = TRUE, selected = selected, options = options)
  })
  
  # shortcut to currently selected species, read from curSpUI
  curSp <- reactive(input$curSp)
  # vector of all species with occurrence data loaded
  allSp <- reactive(names(reactiveValuesToList(spp)))
  # conditional species input to modules with batch option
  spIn <- reactive(if(input$batch == TRUE) allSp() else curSp())
  
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
      n <- formatSpName(spName(spp[[curSp()]]))
      source <- rmm()$data$occurrence$sources
      paste0(n, "_", source, ".csv")
    },
    content = function(file) {
      tbl <- occs() %>% 
        dplyr::select(-pop)
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
      l <- lapply(allSp(), function(x) {spp[[x]]$occData$occsCleaned})
      tbl <- dplyr::bind_rows(l)
      # inTbl <- occs() %>% dplyr::select(-pop)
      # tbl <- matrix(ncol = ncol(inTbl))
      # colnames(tbl) <- names(inTbl)
      # for(sp in allSp()) {
      #   curTbl <- spp[[sp]]$occs %>% dplyr::select(-pop)  
      #   # # if bg values are present, add them to table
      #   # if(!is.null(spp[[sp]]$bg)) {
      #   #   curTbl <- rbind(curTbl, spp[[sp]]$bg) 
      #   # }
      #   tbl <- rbind(tbl, curTbl)
      # }
      # remove first NA row
      tbl <- tbl %>% dplyr::select(-pop)
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )
  
  # DOWNLOAD: occsOrig
  output$dlDbOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(spName(spp[[curSp()]]))
      source <- rmm()$data$occurrence$sources
      paste0(n, "_", source, "_raw.csv")
    },
    content = function(file) {
      write_csv_robust(spp[[curSp()]]$occData$occsOrig, file, row.names = FALSE)
    }
  )
  
  ########################################### #
  ### COMPONENT: PROCESS OCCURRENCE DATA ####
  ########################################### #
  
  # # # # # # # # # # # # # # # # # # # #
  # module Remove Occurrences By ID ####
  # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goRemoveByID, {
    removeByID <- callModule(removeByID_MOD, 'c2_removeByID_uiID')
    removeByID()
  })
  
  # Enable/disable single processed occs
  observeEvent(input$goRemoveByID, {
    shiny::observe({
      shinyjs::toggleState("dlProcOccs", 
        !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords) |
        !is.null(spp[[curSp()]]$procOccs$occsThin) |
        !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs))
    })
  })
  
  # # # # # # # # # # # # # # # # # # # # #
  # module Select Occurrences on Map ####
  # # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goSelectOccs, {
    req(input$map_draw_new_feature)
    selOccs <- callModule(selectOccs_MOD, 'c2_selOccs_uiID')
    selOccs()
    # UI CONTROLS 
    #updateSelectInput(session, "curSp", selected = curSp())
  })
  
  # Enable/disable single processed occs
  observeEvent(input$goSelectOccs, {
    shiny::observe({
      shinyjs::toggleState("dlProcOccs", 
        !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords) |
        !is.null(spp[[curSp()]]$procOccs$occsThin) |
        !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs))
    })
  })
  
  # # # # # # # # # # # # # #
  # module Spatial Thin ####
  # # # # # # # # # # # # # # 
  
  observeEvent(input$goThinOccs, {
    thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs_uiID')
    thinOccs()
  })
  
  # Enable/disable single processed occs
  observeEvent(input$goThinOccs, {
      shiny::observe({
        shinyjs::toggleState("dlProcOccs", 
          !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords) |
          !is.null(spp[[curSp()]]$procOccs$occsThin) |
          !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs))
    })
  })
  
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
                           em(spp[[curSp()]]$occs[1, "scientific_name"]), ".")
    # MAPPING
    map %>%
      map_occs(occs()) %>%
      zoom2Occs(occs())
    # UI CONTROLS 
    # updateSelectInput(session, "curSp", selected = curSp())
    shinyjs::disable("dlProcOccs")
  })
  
  # DOWNLOAD: current processed occurrence data table
  output$dlProcOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(spName(spp[[curSp()]]))
      paste0(n, "_processed_occs.csv")
    },
    content = function(file) {
      tbl <- occs() %>% dplyr::select(-pop)
      write_csv_robust(tbl, file, row.names = FALSE)
    }
  )
  
  ############################################# #
  ### COMPONENT: OBTAIN ENVIRONMENTAL DATA ####
  ############################################# #
  
  # # # # # # # # # # # # # # # # #
  # module WorldClim Bioclims ####
  # # # # # # # # # # # # # # # # #
  observeEvent(input$goEnvData, {
    wcBioclims <- callModule(wcBioclims_MOD, 'c3_wcBioclims_uiID', spIn())
    wcBioclims()
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # UI CONTROLS 
    updateSelectInput(session, "curSp", selected = curSp())
    # enable download button
    # shinyjs::enable("dlEnvs")
  })
  
  # # # # # # # # # # # # # # # # #
  # module ecoClimate ####
  # # # # # # # # # # # # # # # # #
  # ecoClimatelayers <- callModule(ecoClimate_MOD, 'c3_ecoClimate_uiID')
  # observeEvent(input$goEcoClimData, {
  #   # load into envs
  #   vals$envs <- ecoClimatelayers()
  #   # stop if no occurrence data
  #   req(vals$occs)
  #   req(vals$envs)
  #   # record for RMD
  #   vals$envsType <- 'ecoClimate'
  #   # remove occurrences with NA values for variables
  #   vals$occs <- remEnvsValsNA(vals)
  #   # switch to Results tab
  #   updateTabsetPanel(session, 'main', selected = 'Results')
  #   # enable download button
  #   # shinyjs::enable("dlEnvs")
  # })
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # module User-defined Environmental Predictors ####
  # # # # # # # # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goUserEnvs, {
    userEnvs <- callModule(userEnvs_MOD, 'c3_userEnvs_uiID')
    userEnvs()
    # make project to new time module unavailable for user envs
    updateRadioButtons(session, "projSel", 
                       choices = list("Project to New Extent" = 'projArea',
                                      "Calculate Environmental Similarity" = 'mess'))
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
  })
  
  # # # # # # # # # # # # # # # # # #
  # OBTAIN ENVS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # ui that populates with the names of environmental predictors used
  output$curEnvUI <- renderUI({
    # for Maxent, only display the environmental predictors with non-zero beta coefficients
    # from the lambdas file (the predictors that were not removed via regularization)
    # if (rmm()$model$algorithm == "Maxent") {
    #   mod <- spp[[curSp()]]$model$models[[curModel()]]
    #   n <- mxNonzeroCoefs(mod)
    # } else {
    
    # ensure envs entity is within spp
    req(curSp(), envs())
    if(!is.null(envs())) {
      n <- c(names(envs()), "ALL")
    } else {
      n <- NULL
    }
    envsNameList <- c(list("Current variable" = ""), setNames(as.list(n), n))
    if(component() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    selectizeInput('curEnv', label = NULL , choices = envsNameList,
                   multiple = TRUE, selected = n[1], options = options)
  })
  
  # shortcut to currently selected environmental variable, read from curEnvUI
  curEnv <- reactive({
    if("ALL" %in% input$curEnv) {
      return(names(envs()))
    } else {
      return(input$curEnv)  
    }
  })
  
  # convenience function for environmental variables for current species
  envs <- reactive(spp[[curSp()]]$envs)
  
  # map center coordinates for 30 arcsec download
  mapCntr <- reactive(mapCenter(input$map_bounds))
  
  # text showing the current map center
  output$ctrLatLon <- renderText({
    paste('Using map center', paste(mapCntr(), collapse=', '))
  })
  
  # CONSOLE PRINT
  output$envsPrint <- renderPrint({
    req(envs())
    envs()
    # mins <- sapply(envs()@layers, function(x) x@data@min)
    # maxs <- sapply(envs()@layers, function(x) x@data@max)
    # names <- sapply(strsplit(names(envs()), '[.]'), function(x) x[-2])
    # DT::datatable(data.frame(name=names, min=mins, max=maxs), 
    #               rownames = FALSE, options = list(pageLength = raster::nlayers(envs())))
  })
  
  ############################################## #
  ### COMPONENT: PROCESS ENVIRONMENTAL DATA ####
  ############################################## #
  
  # # # # # # # # # # # # # # # # #
  # module Background Extent ####
  # # # # # # # # # # # # # # # # #
  
  observeEvent(input$goBgExt, {
    # initialize module
    bgExt <- callModule(bgExtent_MOD, 'c4_bgExtent_uiID')
    bgExt()
    # UI CONTROLS
    # updateSelectInput(session, "curSp", selected = curSp())
    
  })
  
  # Enable/disable download shapefile button
  observeEvent(input$goBgExt, {
    shiny::observe({
      shinyjs::toggleState("dlBgShp", !is.null(spp[[curSp()]]$procEnvs$bgExt))
    })
  })
  
  # # # # # # # # # # # # # # # # # # # # # # # 
  # module Upload Background Extent ####
  # # # # # # # # # # # # # # # # # # # # # # # 
  observeEvent(input$goUserBg, {
    userBg <- callModule(userBgExtent_MOD, 'c4_userBgExtent')
    userBg()
  })
  
  # # # # # # # # # # # # # # # # # # # # # # # 
  # module Draw Background Extent ####
  # # # # # # # # # # # # # # # # # # # # # # # 
  observeEvent(input$goDrawBg, {
    drawBg <- callModule(drawBgExtent_MOD, 'c4_drawBgExtent')
    drawBg()
    shiny::observe({
      shinyjs::toggleState("dlBgShp", !is.null(spp[[curSp()]]$procEnvs$bgExt))
    })
  })
  
  # # # # # # # # # # # # # # # # # # # # # # # # #
  # module Background Mask and Sample Points ####
  # # # # # # # # # # # # # # # # # # # # # # # # #
  
  observeEvent(input$goBgMask, {
    # stop if no background shape
    req(bgExt())
    bgMskPts <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts')
    bgMskPts()
    # UI CONTROLS 
    # updateSelectInput(session, "curSp", selected = curSp())
  })
  
  # Enable/disable download background mask and points buttons
  observeEvent(input$goBgMask, {
    shiny::observe({
      shinyjs::toggleState("dlMskEnvs", !is.null(spp[[curSp()]]$procEnvs$bgMask))
      shinyjs::toggleState("dlBgPts", !is.null(spp[[curSp()]]$bgPts))
      })
  })
  
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
    filename = function() paste0(formatSpName(curSp()), '_bgShp.zip'),
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
      zip::zip(zipfile=file, files=fs)
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
      zip::zip(zipfile=file, files=fs)
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
  ### COMPONENT: ESPACE ####
  ############################################## #
  
  curMSp <- reactive({
    if(length(curSp()) > 1) paste0(curSp(), collapse = '|')
  })
  
  # # # # # # # # # # # # # # # # # # # # # # #
  # module Principle Components Analysis ####
  # # # # # # # # # # # # # # # # # # # # # # #
  pca <- callModule(pca_MOD, 'cEspace_PCA_uiID')
  
  observeEvent(input$goPCA, {
    # stop if no environmental variables
    if(length(curSp()) != 2) {
      shinyLogs %>% writeLog(type = 'error', "Please select two species.")
    }
    req(spp[[curSp()[1]]]$procEnvs$bg.envsVals, spp[[curSp()[2]]]$procEnvs$bg.envsVals)
    # initialize module
    pca()
    # UI CONTROLS 
    updateTabsetPanel(session, 'main', selected = 'Results')
    updateSelectInput(session, "curSp", selected = curSp())
  })
  
  # # # # # # # # # # # # # # # # # # # # 
  # module Occurrence Density Grids ####
  # # # # # # # # # # # # # # # # # # # # 
  occDens <- callModule(occDens_MOD, 'cEspace_occDens_uiID')
  observeEvent(input$goOccDens, {
    # stop if no environmental variables
    req(msp[[curMSp()]]$pca)
    # initialize module
    occDens()
    # UI CONTROLS 
    updateSelectInput(session, "curSp", selected = curSp())
  })
  
  # # # # # # # # # # # # # # # 
  # module Niche Overlap ####
  # # # # # # # # # # # # # # # 
  nicheOv <- callModule(nicheOv_MOD, 'cEspace_nicheOv_uiID')
  
  observeEvent(input$goNicheOv, {
    # stop if no environmental variables
    req(msp[[curMSp()]]$occDens)
    # initialize module
    nicheOv()
    # UI CONTROLS 
    updateSelectInput(session, "curSp", selected = curSp())
  })
  
  ################################################## #
  ### COMPONENT: PARTITION OCCURRENCE DATA ####
  ################################################# #
  
  # # # # # # # # # # # # # # # # # # # # # # # # #
  # module Non-spatial Occurrence Partitions ####
  # # # # # # # # # # # # # # # # # # # # # # # # #
  partitionNonSpat <- callModule(partitionNonSpat_MOD, 'cParts_partitionNonSpat_uiID')
  observeEvent(input$goPartitionNonSpat, {
    partitionNonSpat()
    # UI CONTROLS 
    # updateSelectInput(session, "curSp", selected = curSp())
    shinyjs::enable("dlPart")
  })
  
  # # # # # # # # # # # # # # # # # # # # # # #
  # module Spatial Occurrence Partitions ####
  # # # # # # # # # # # # # # # # # # # # # # #
  partitionSpat <- callModule(partitionSpat_MOD, 'cParts_partitionSpat_uiID')
  observeEvent(input$goPartitionSpat, {
    partitionSpat()
    # UI CONTROLS 
    # updateSelectInput(session, "curSp", selected = curSp())
    shinyjs::enable("dlPart")
  })
  
  ######################### #
  ### COMPONENT: MODEL ####
  ######################### #
  
  # # # # # # # # # # # 
  # module Maxent ####
  # # # # # # # # # # # 
  observeEvent(input$goMaxent, {
    mod.maxent <- callModule(runMaxent_MOD, 'runMaxent')
    mod.maxent()
    runMaxent_TBL(input, output, session)
    # make sure the results were entered before proceeding
    req(results())
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # customize visualizations for maxent
    updateRadioButtons(session, "visSel", 
                       choices = list("Maxent Evaluation Plots" = 'maxentEval',
                                      "Plot Response Curves" = 'response',
                                      "Map Prediction" = 'mapPreds'))
  })
  
  # # # # # # # # # # # # 
  # module BIOCLIM ####
  # # # # # # # # # # # # 
  observeEvent(input$goBIOCLIM, {
    mod.bioclim <- callModule(runBIOCLIM_MOD, 'runBIOCLIM')
    mod.bioclim()
    runBIOCLIM_TBL(input, output, session)
    # make sure the results were entered before proceeding
    req(results())
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # update radio buttons for Visualization component
    updateRadioButtons(session, "visSel", choices = list("BIOCLIM Envelope Plots" = 'bioclimPlot',
                                                         "Map Prediction" = 'mapPreds'))
  })
  
  # # # # # # # # # # # # 
  # module GAM ####
  # # # # # # # # # # # # 
  observeEvent(input$goGAM, {
    mod.gam <- callModule(runGAM_MOD, 'runGAM')
    mod.gam()
    runGAM_TBL(input, output, session)
    # make sure the results were entered before proceeding
    req(results())
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # update radio buttons for Visualization component
    updateRadioButtons(session, "visSel", choices = list("Map Prediction" = 'mapPreds'))
  })
  
  # # # # # # # # # # # # # # # # # #
  # MODEL: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # convenience function for modeling results list for current species
  results <- reactive(spp[[curSp()]]$results)
  
  # ui that populates with the names of models that were run
  output$curModelUI <- renderUI({
    # do not display until both current species is selected and it has a model
    req(curSp(), results())
    # if 
    if(!is.null(results())) {
      n <- names(results()$models)  
    } else {
      n <- NULL
    }
    
    modsNameList <- c(list("Current model" = ""), setNames(as.list(n), n))
    options <- list(maxItems = 1)
    selectizeInput('curModel', label = NULL , choices = modsNameList,
                   multiple = TRUE, selected = n[1], options = options)
  })
  
  # shortcut to currently selected model, read from modSelUI
  curModel <- reactive({input$curModel})
  
  # download for partitioned occurrence records csv
  output$dlEvalTbl <- downloadHandler(
    filename = function() {
      if(rmm()$model$algorithm == "BIOCLIM") {
        paste0(curSp(), "_bioclim_evalTbl.csv")  
      } else if(rmm()$model$algorithm == "Maxent") {
        paste0(curSp(), "_maxent_evalTbl.csv")  
      }
    },
    content = function(file) {
      evalTbl <- cbind(results()$evalTbl, results()$evalTblBins)
      write_csv_robust(evalTbl, file, row.names = FALSE)
    }
  )
  
  
  ########################################### #
  ### COMPONENT: VISUALIZE MODEL RESULTS ####
  ########################################### #
  
  # # # # # # # # # # # # # #
  # module BIOCLIM Plots ####
  # # # # # # # # # # # # # #
  bioclimPlot <- callModule(bioclimPlot_MOD, 'c7_bioclimPlot')
  output$bioclimPlot <- renderPlot({
    # do not plot if missing models
    req(curSp(), results())
    bioclimPlot()
  }, width = 700, height = 700)
  
  # # # # # # # # # # # # # # # # # # #
  # module Maxent Evaluation Plots ####
  # # # # # # # # # # # # # # # # # # #
  maxentEvalPlot <- callModule(maxentEvalPlot_MOD, 'c7_maxentEvalPlot')
  output$maxentEvalPlot <- renderPlot({
    # do not plot if missing models
    req(curSp(), results())
    maxentEvalPlot()
  }, width = 700, height = 700)
  
  # # # # # # # # # # # # # # # # # #
  # module Response Curve Plots ####
  # # # # # # # # # # # # # # # # # #
  responsePlot <- callModule(responsePlot_MOD, 'c7_responsePlot')
  output$responsePlot <- renderPlot({
    # do not plot if missing models
    responsePlot()
  }, width = 700, height = 700)
  
  # # # # # # # # # # # # # # #
  # module Map Prediction ####
  # # # # # # # # # # # # # # #
  # MOTE: this prediction is restricted to the background extent
  observeEvent(input$goMapPreds, {
    mapPreds <- callModule(mapPreds_MOD, 'c7_mapPreds')
    mapPreds()
    updateTabsetPanel(session, 'main', selected = 'Map')
    shinyjs::enable("dlPred")
  })
  
  # # # # # # # # # # # # # # # # # #
  # VISUALIZE: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # convenience function for mapped model prediction raster for current species
  mapPred <- reactive(spp[[curSp()]]$visualization$mapPred)
  
  # handle downloads for BIOCLIM Plots png
  output$dlVizPlot <- downloadHandler(
    filename = function() {
      if(module() == "bioclimPlot") {
        paste0(spName(), "_bioClimPlot.png")
      } else if(module() == "maxentEvalPlot") {
        paste0(spName(), "_maxentEvalPlot.png")
      } else if(module() == "responsePlot") {
        paste0(spName(), "_responsePlot.png")
      }
    },
    content = function(file) {
      png(file)
      if(module() == "bioclimPlot") {
        bioclimPlot() 
      } else if(module() == "maxentEvalPlot") {
        maxentEvalPlot()
      } else if(module() == "responsePlot") {
        responsePlot()
      }
      dev.off()
    }
  )
  
  # download for model predictions (restricted to background extent)
  output$dlPred <- downloadHandler(
    filename = function() {
      ext <- switch(input$predFileType, raster = 'zip', ascii = 'asc', GTiff = 'tif', PNG = 'png')
      paste0(names(mapPred()), '.', ext)},
    content = function(file) {
      browser()
      if(require(rgdal)) {
        if (input$predFileType == 'png') {
          png(file)
          raster::image(mapPred())
          dev.off()
        } else if (input$predFileType == 'raster') {
          fileName <- names(mapPred())
          tmpdir <- tempdir()
          raster::writeRaster(mapPred(), file.path(tmpdir, fileName), format = input$predFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zip(zipfile = file, files = fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(mapPred(), file, format = input$predFileType, overwrite = TRUE)
          file.rename(r@file@name, file)
        }
      } else {
        shinyLogs %>% writeLog("Please install the rgdal package before downloading rasters.")
      }
    }
  )
  
  ########################################### #
  ### COMPONENT: PROJECT MODEL ####
  ########################################### #
  
  
  # # # # # # # # # # # # # # # # #
  # module Project to New Area ####
  # # # # # # # # # # # # # # # # #
  observeEvent(input$goProjectArea, {
    projArea <- callModule(projectArea_MOD, 'c8_projectArea')
    projArea()
    # MAPPING
    map %>% 
      # reset draw polygon
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE, 
                                     markerOptions = FALSE, circleMarkerOptions = FALSE) 
    shinyjs::enable("dlProj")
  })
  
  # # # # # # # # # # # # # # # # #
  # module Project to New Time ####
  # # # # # # # # # # # # # # # # #
  # needs to be outside observeEvent to trigger GCM ui
  projTime <- callModule(projectTime_MOD, 'c8_projectTime')
  observeEvent(input$goProjectTime, {
    projTime()
    # MAPPING
    map %>% 
      # reset draw polygon
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE, 
                                     markerOptions = FALSE, circleMarkerOptions = FALSE) 
    shinyjs::enable("dlProj")
  })
  
  # # # # # # # # # # # # # # # # # # # #
  # module Environmental Similarity ####
  # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goEnvSimilarity, {
    envSimilarity <- callModule(envSimilarity_MOD, 'c8_envSimilarity')
    envSimilarity()
    
    # MAPPING
    map %>% 
      # reset draw polygon
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE, 
                                     markerOptions = FALSE, circleMarkerOptions = FALSE) 
    shinyjs::enable("dlProj")
  })
  
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
  
  # download for model predictions (restricted to background extent)
  output$dlProj <- downloadHandler(
    filename = function() {
      ext <- switch(input$projFileType, raster = 'zip', ascii = 'asc', GTiff = 'tif', PNG = 'png')
      paste0(names(mapProj()), '.', ext)},
    content = function(file) {
      browser()
      if(require(rgdal)) {
        if (input$projFileType == 'png') {
          png(file)
          raster::image(mapProj())
          dev.off()
        } else if (input$projFileType == 'raster') {
          fileName <- names(mapProj())
          tmpdir <- tempdir()
          raster::writeRaster(mapProj(), file.path(tmpdir, fileName), format = input$projFileType, overwrite = TRUE)
          owd <- setwd(tmpdir)
          fs <- paste0(fileName, c('.grd', '.gri'))
          zip::zip(zipfile = file, files = fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(mapProj(), file, format = input$projFileType, overwrite = TRUE)
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
  
  # handler for R Markdown download
  output$dlRMD <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".", switch(
        input$rmdFileType, Rmd = 'Rmd', PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))},
    content = function(file) {
      exp <- knitr::knit_expand("Rmd/userReport.Rmd", 
                                spName=curSp(), 
                                occsSource=rmm()$data$occurrence$sources,
                                occsNum=rmm()$code$wallaceSettings$occsNum,  # comp 1
                                occsCSV=rmm()$code$wallaceSettings$userCSV,
                                occsRemoved=printVecAsis(rmm()$code$wallaceSettings$removedIDs)
                                
      )  # comp 8
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
  
  ################################
  ### METADATA FUNCTIONALITY ####
  ################################
  
  output$dlRMM <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".csv")
    },
    content = function(file) {
      rangeModelMetadata::rmmToCSV(rmm(), filename = file)
    })
})