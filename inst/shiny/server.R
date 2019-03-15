options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {
  ########################## #
  # REACTIVE VALUES LISTS ####
  ########################## #
  
  # single species list of lists
  spp <- reactiveValues()
  # # multiple species list of lists: each entity is a species pair list which contains
  # # results of multispecies analyses
  # msp <- reactiveValues()
  envs.global <- reactiveValues()
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
    req(length(curSp()) == 1, occs(), module())
    f <- switch(module(), 
                "dbOccs" = queryDb_MAP, 
                "userOccs" = userOccs_MAP,
                "selOccs" = selectOccs_MAP, 
                "profOccs" = occProfile_MAP,
                "remID" = removeByID_MAP, 
                "spthin" = thinOccs_MAP, 
                "wcbc" = wcBioclims_MAP,
                "ecoClimate" = ecoclimate_MAP,
                "userEnvs" = userEnvs_MAP,
                "bgSel" = bgExtent_MAP, 
                "bgUser" = userBgExtent_MAP,
                "bgDraw" = drawBgExtent_MAP,
                "biasFile" = mapBias_MAP,
                "nsp" = partitionNonSpat_MAP, 
                "sp" = partitionSpat_MAP,
                "mapPreds" = mapPreds_MAP, 
                "projArea" = projectArea_MAP, 
                "projTime" = projectTime_MAP, 
                "mess" = envSimilarity_MAP)
    req(f)
    map %>% f(session)
  })
  
  ######################## #
  ### BUTTONS LOGIC ####
  ######################## #
  
  # Disable download buttons
  shinyjs::disable("dlDbOccs")
  shinyjs::disable("dlOccs")
  shinyjs::disable("dlAllOccs")
  shinyjs::disable("dlProcOccs")
  shinyjs::disable("dlBgShp")
  shinyjs::disable("dlMskEnvs")
  shinyjs::disable("dlBgPts")
  shinyjs::disable("dlPart")
  shinyjs::disable("dlEvalTbl")
  shinyjs::disable("dlVisBioclim")
  shinyjs::disable("dlMaxentPlots")
  shinyjs::disable("dlRespCurves")
  shinyjs::disable("dlPred")
  shinyjs::disable("dlProj")
  shinyjs::disable("dlMess")
  # shinyjs::disable("dlRMD")
  
  # Enable/disable buttons
  observe({
    req(length(curSp()) == 1)
    shinyjs::toggleState("dlDbOccs", !is.null(occs()))
    shinyjs::toggleState("dlOccs", !is.null(occs()))
    shinyjs::toggleState("dlAllOccs", length(allSp()) > 1)
    shinyjs::toggleState("dlRMD", !is.null(occs()))
    shinyjs::toggleState("dlProcOccs",
                         !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$occsSelPolyCoords) |
                           !is.null(spp[[curSp()]]$procOccs$occsThin) |
                           !is.null(spp[[curSp()]]$rmm$code$wallaceSettings$removedIDs))
    shinyjs::toggleState("dlMskEnvs", !is.null(spp[[curSp()]]$procEnvs$bgMask))
    shinyjs::toggleState("dlBgPts", !is.null(spp[[curSp()]]$bgPts))
    shinyjs::toggleState("dlBgShp", !is.null(spp[[curSp()]]$procEnvs$bgExt))
    shinyjs::toggleState("dlPart", !is.null(spp[[curSp()]]$occs$partition))
    shinyjs::toggleState("dlEvalTbl", !is.null(evalOut()))
    shinyjs::toggleState("dlVisBioclim", spp[[curSp()]]$rmm$model$algorithm == "BIOCLIM")
    shinyjs::toggleState("dlMaxentPlots", (spp[[curSp()]]$rmm$model$algorithm == "maxnet" |
                                             spp[[curSp()]]$rmm$model$algorithm == "maxent.jar"))
    shinyjs::toggleState("dlRespCurves", (spp[[curSp()]]$rmm$model$algorithm == "maxnet" |
                                            spp[[curSp()]]$rmm$model$algorithm == "maxent.jar"))
    shinyjs::toggleState("dlPred", !is.null(spp[[curSp()]]$visualization$occPredVals))
    shinyjs::toggleState("dlProj", !is.null(spp[[curSp()]]$project$pjEnvs))
    shinyjs::toggleState("dlMess", !is.null(spp[[curSp()]]$project$messVals))
    # shinyjs::toggleState("dlWhatever", !is.null(spp[[curSp()]]$whatever))
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
    occsList <- queryDb()
    n <- formatSpName(occsList[[1]]$scientific_name)
  })
  
  # # # # # # # # # # # # # # # # # # # 
  # module Query Database (Paleo) ####
  # # # # # # # # # # # # # # # # # # # 
  observeEvent(input$goPaleoDbOccs, {
    paleoDb <- callModule(queryPaleoDb_MOD, 'c1_queryPaleoDb_uiID')
    # return the occs table
    occsList <- paleoDb()
    n <- formatSpName(occsList[[1]]$scientific_name)
  })
  
  # # # # # # # # # # # # # # # # # #
  # module User Occurrence Data ####
  # # # # # # # # # # # # # # # # # #
  observeEvent(input$goUserOccs, {
    userOccs <- callModule(userOccs_MOD, 'c1_userOccs_uiID')
    userOccs()
  })
  
  # # # # # # # # # # # # # # # # # #
  # OBTAIN OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # output$curSpUI <- renderUI({
  #   # check that a species is in the list already -- if not, don't proceed
  #   # req(length(reactiveValuesToList(spp)) > 0)
  #   # get the species names
  #   n <- names(spp)
  #   # if no current species selected, select the first name
  #   # if one species selected, retain it
  #   # if two species selected, 
  #   # NOTE: this line is necessary to retain the selection after selecting different tabs
  #   if(!is.null(curSp())) {
  #     curSp.split <- strsplit(curSp(), "|", fixed=TRUE)[[1]]
  #     print(curSp.split)
  #     if(length(curSp.split) == 1) {
  #       selected <- curSp()
  #     }else if(length(curSp.split) > 1) {
  #       selected <- n
  #     }
  #   }else{
  #     selected <- n[1]
  #   }
  #   # if espace component, allow for multiple species selection
  #   if(component() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
  #   # make a named list of their names
  #   sppNameList <- c(list("Current species" = ""), setNames(as.list(n), n))
  #   # generate a selectInput ui that lists the available species
  #   selectizeInput('curSp', label = NULL , choices = sppNameList,
  #                  multiple = TRUE, selected = selected, options = options)
  # })
  
  output$curSpUI <- renderUI({
    # check that a species is in the list already -- if not, don't proceed
    # req(length(reactiveValuesToList(spp)) > 0)
    # get the species names
    n <- names(spp)
    # remove multispecies names from list
    n <- n[!grepl("|", n, fixed = TRUE)]
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
  
  # shortcut to currently selected species, read from curSpUI
  # curSp <- reactive({
  #   curSp.len <- length(input$curSp)
  #   if(curSp.len == 1) {
  #     input$curSp
  #   }else if(curSp.len > 1) {
  #     paste0(input$curSp, collapse = '|')
  #   }
  # })
  
  curSp <- reactive(input$curSp)
  
  # vector of all species with occurrence data loaded
  allSp <- reactive(names(reactiveValuesToList(spp)))
  
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
  
  ############################################# #
  ### COMPONENT: OBTAIN ENVIRONMENTAL DATA ####
  ############################################# #
  
  # # # # # # # # # # # # # # # # #
  # module WorldClim Bioclims ####
  # # # # # # # # # # # # # # # # #
  observeEvent(input$goEnvData, {
    wcBioclims <- callModule(wcBioclims_MOD, 'c3_wcBioclims_uiID')
    wcBioclims()
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # UI CONTROLS 
    updateSelectInput(session, "curSp", selected = curSp())
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
      n <- c(names(envs()))
    } else {
      n <- NULL
    }
    envsNameList <- c(list("Current variable" = ""), setNames(as.list(n), n))
    if(component() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    selectizeInput('curEnv', label = "Select variable" , choices = envsNameList,
                   multiple = TRUE, selected = n[1], options = options)
  })
  
  # shortcut to currently selected environmental variable, read from curEnvUI
  curEnv <- reactive(input$curEnv)
  
  # convenience function for environmental variables for current species
  envs <- reactive(envs.global[[spp[[curSp()]]$envs]])
  
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
  
  ########################################### #
  ### COMPONENT: PROCESS OCCURRENCE DATA ####
  ########################################### #
  
  # # # # # # # # # # # # # # # # # # # #
  # module Profile Occurrences ####
  # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goProfileOccs, {
    profileOccs <- callModule(profileOccs_MOD, 'c2_profileOccs_uiID')
    profileOccs()
  })

  observeEvent(input$goProfileOccsClean, {
    profileOccsClean <- callModule(profileOccsClean_MOD, 'c2_profileOccsClean_uiID')
    profileOccsClean()
  })

  # # # # # # # # # # # # # # # # # # # #
  # module Remove Occurrences By ID ####
  # # # # # # # # # # # # # # # # # # # #
  observeEvent(input$goRemoveByID, {
    removeByID <- callModule(removeByID_MOD, 'c2_removeByID_uiID')
    removeByID()
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
  
  # # # # # # # # # # # # # #
  # module Spatial Thin ####
  # # # # # # # # # # # # # # 
  
  observeEvent(input$goThinOccs, {
    thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs_uiID')
    thinOccs()
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
  })
  
  # # # # # # # # # # # # # # # # # # # # # # # # #
  # module Background Mask and Sample Points ####
  # # # # # # # # # # # # # # # # # # # # # # # # #
  
  observeEvent(input$goBgMask, {
    # stop if no background shape
    req(bgExt())
    bgMskPts <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts')
    bgMskPts()
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
  
  # curMSp <- reactive({
  #   if(length(curSp()) > 1) paste0(curSp(), collapse = '|')
  # })
  
  # # # # # # # # # # # # # # # # # # # # # # #
  # module Principle Components Analysis ####
  # # # # # # # # # # # # # # # # # # # # # # #
  pca <- callModule(pca_MOD, 'cEspace_PCA_uiID')
  
  observeEvent(input$goPCA, {
    # stop if no environmental variables
    # if(length(curSp()) != 2) {
    #   shinyLogs %>% writeLog(type = 'error', "Please select two species.")
    # }
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
  })
  
  # # # # # # # # # # # # # # # # # # # # # # #
  # module Spatial Occurrence Partitions ####
  # # # # # # # # # # # # # # # # # # # # # # #
  partitionSpat <- callModule(partitionSpat_MOD, 'cParts_partitionSpat_uiID')
  observeEvent(input$goPartitionSpat, {
    partitionSpat()
  })
  
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
  
  # # # # # # # # # # # 
  # module Maxent ####
  # # # # # # # # # # # 
  
  mod.maxent <- callModule(runMaxent_MOD, 'runMaxent_uiID')
  observeEvent(input$goMaxent, {
    mod.maxent()
    # make sure the results were entered before proceeding
    req(evalOut())
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
    mod.bioclim <- callModule(runBIOCLIM_MOD, 'runBIOCLIM_uiID')
    mod.bioclim()
    #runBIOCLIM_TBL(input, output, session)
    # make sure the results were entered before proceeding
    req(evalOut())
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
    mod.gam <- callModule(runGAM_MOD, 'runGAM_uiID')
    mod.gam()
    runGAM_TBL(input, output, session)
    # make sure the results were entered before proceeding
    req(evalOut())
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # update radio buttons for Visualization component
    updateRadioButtons(session, "visSel", choices = list("Map Prediction" = 'mapPreds'))
  })
  
  # # # # # # # # # # # # # # # # # #
  # MODEL: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  output$evalTbls <- renderUI({
    req(evalOut())
    
    print(names(evalOut()@models))
    res <- evalOut()@results
    res.grp <- evalOut()@results.grp
    tuned.n <- ncol(evalOut()@tuned.settings)
    if(tuned.n > 0) {
      res.round <- cbind(res[,seq(1, tuned.n)], round(res[,seq(tuned.n+1, ncol(res))], digits = 3))
      res.grp.round <- cbind(res.grp[,seq(1, tuned.n+1)], round(res.grp[,seq(tuned.n+2, ncol(res.grp))], digits = 3))
    } else {
      res.round <- cbind(round(res[, 1:13], digits = 3))
      res.grp.round <- cbind(fold = res.grp[, 1], round(res.grp[, 2:5], digits = 3))
    }
    # define contents for both evaluation tables
    options <- list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">')
    output$evalTbl <- DT::renderDataTable(res.round, options = options)
    output$evalTblBins <- DT::renderDataTable(res.grp.round, options = options)
    # define contents for lambdas table
    output$lambdas <- renderPrint({
      if(spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
        evalOut()@models[[curModel()]]$betas
      } else if(spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
        evalOut()@models[[curModel()]]@lambdas
      }
    })
    
    tabsetPanel(
      tabPanel("Evaluation",
               tagList(
                 br(),
                 div("Evaluation statistics: full model and partition averages", id="stepText"), br(), br(),
                 DT::dataTableOutput('evalTbl'), br(),
                 div("Evaluation statistics: individual partitions", id="stepText"), br(), br(),
                 DT::dataTableOutput('evalTblBins')  
               )),
      tabPanel("Lambdas",
               br(),
               div("Maxent lambdas file", id = "stepText"), br(), br(),
               verbatimTextOutput("lambdas")
      )
    )
  })
  
  # convenience function for modeling results list for current species
  evalOut <- reactive(spp[[curSp()]]$evalOut)
  
  # ui that populates with the names of models that were run
  output$curModelUI <- renderUI({
    # do not display until both current species is selected and it has a model
    req(curSp(), length(curSp()) == 1, evalOut())
    # if 
    if(!is.null(evalOut())) {
      n <- names(evalOut()@models)  
      print(n)
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
  
  # # # # # # # # # # # # # #
  # module BIOCLIM Plots ####
  # # # # # # # # # # # # # #
  bioclimPlot <- callModule(bioclimPlot_MOD, 'c7_bioclimPlot')
  output$bioclimPlot <- renderPlot({
    # do not plot if missing models
    req(curSp(), evalOut())
    bioclimPlot()
  }, width = 700, height = 700)
  
  # # # # # # # # # # # # # # # # # # #
  # module Maxent Evaluation Plots ####
  # # # # # # # # # # # # # # # # # # #
  maxentEvalPlot <- callModule(maxentEvalPlot_MOD, 'c7_maxentEvalPlot')
  output$maxentEvalPlot <- renderPlot({
    # do not plot if missing models
    req(curSp(), evalOut())
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
  })
  
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
      zip::zip(zipfile = file, 
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
      zip::zip(zipfile = file, files = paste0(namesEnvs, ".png"))
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
          zip::zip(zipfile = file, files = fs)
          setwd(owd)
        } else {
          r <- raster::writeRaster(mapPred(), file, format = input$predFileType, 
                                   overwrite = TRUE)
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
          zip::zip(zipfile = file, files = fs)
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
          zip::zip(zipfile = file, files = fs)
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
  
  # handler for R Markdown download
  output$dlRMD <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".", switch(
        input$rmdFileType, Rmd = 'Rmd', PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))},
    content = function(file) {
      knit.lst <- list()
      # make RMD text for all species
      for(sp in allSp()) {
        occSource <- spp[[sp]]$rmm$data$occurrence$sources
        occsDb_knit <- occSource != "user"
        occsUser_knit <- occSource == "user"
        removeByID_knit <- !is.null(spp[[sp]]$rmm$code$wallaceSettings$removedIDs)
        selectByID_knit <- !is.null(spp[[sp]]$rmm$code$wallaceSettings$occsSelPolyCoords)
        knit.params <- c(file = "Rmd/userReport.Rmd", spName = sp, 
                         occsDb_knit = occsDb_knit, occsUser_knit = occsUser_knit,
                         removeByID_knit = removeByID_knit, selectByID_knit = selectByID_knit,
                         queryDb_RMD(), userOccs_RMD(), 
                         removeByID_RMD(), selectOccs_RMD())
        knit.lst[[sp]] <- do.call(knitr::knit_expand, knit.params)  
      }
      # remove the header text from all species' RMD past the first
      for(k in 2:length(knit.lst)) {
        knit.lst[[k]] <- gsub("---\ntitle.*Your analyses are below.", "", knit.lst[[k]])
      }
      # concatenate all species' RMDs together
      knit.out <- paste(knit.lst, collapse = "\n")
                                
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      writeLines(knit.out, 'userReport2.Rmd')
      
      if(input$rmdFileType == 'Rmd') {
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
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".csv")
    },
    content = function(file) {
      rangeModelMetadata::rmmToCSV(rmm(), filename = file)
    })
})