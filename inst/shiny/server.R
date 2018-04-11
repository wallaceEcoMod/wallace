source("funcs/functions.R")
wd <- getwd()
# load module and helper functions
setwd("..")
setwd("..")
funcs <- list.files(path="R", pattern="^c|^helper", full.names=TRUE)
sapply(funcs, source)
setwd(wd)

options(shiny.maxRequestSize=5000*1024^2)

shinyServer(function(input, output, session) {
  # disable download buttons
  shinyjs::disable("dlDbOccs")
  shinyjs::disable("dlPaleoDbOccs")
  shinyjs::disable("dlProcOccs")
  # shinyjs::disable("dlEnvs")
  shinyjs::disable("dlMskEnvs")
  shinyjs::disable("dlPart")
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
  logs <- reactiveVal(logInit())
  # legacy
  rvs <- reactiveValues()
  # legacy
  rmd <- reactiveValues()
  
  # FOR DEVELOPMENT PURPOSES
  observeEvent(input$load, {
    f <- read.csv('example_data/occs_multisp.csv')
    spp[["Puma_concolor"]]$occs <- spp[["Puma_concolor"]]$occData$occsCleaned <- f 
    spp[["Panthera_leo"]]$occs <- spp[["Panthera_leo"]]$occData$occsCleaned <- f 
    spp[["Puma_concolor"]]$occs$pop <- spp[["Puma_concolor"]]$occData$occsCleaned$pop <- unlist(apply(spp[["Puma_concolor"]]$occs, 1, popUpContent))
    spp[["Panthera_leo"]]$occs$pop <- spp[["Panthera_leo"]]$occData$occsCleaned$pop <- unlist(apply(spp[["Panthera_leo"]]$occs, 1, popUpContent))
    # rvs$occsGrp <- rvs$occs$group
    # spp[["Puma_concolor"]]$bg <- f %>% dplyr::filter(taxon_name == 'background1') %>% dplyr::select(longitude, latitude)
    # spp[["Panthera_leo"]]$bg <- f %>% dplyr::filter(taxon_name == 'background2') %>% dplyr::select(longitude, latitude)
    # # rvs$bgGrp <- rvs$bgPts$group
    # spp[["Puma_concolor"]]$occs <- cbind(spp[["Puma_concolor"]]$occs, read.csv('/Users/musasabi/Desktop/shiny_testing/Puma concolor_z.csv'))
    # spp[["Panthera_leo"]]$occs <- cbind(spp[["Panthera_leo"]]$occs, read.csv('/Users/musasabi/Desktop/shiny_testing/Panthera leo_z.csv'))
    # spp[["Puma_concolor"]]$bg <- cbind(spp[["Puma_concolor"]]$bg, read.csv('/Users/musasabi/Desktop/shiny_testing/Puma concolor_bz.csv'))
    # spp[["Panthera_leo"]]$bg <- cbind(spp[["Panthera_leo"]]$bg, read.csv('/Users/musasabi/Desktop/shiny_testing/Panthera leo_bz.csv'))
    # # rvs$bgShp <- rgdal::readOGR('/Users/musasabi/Downloads', 'mcp')
    # spp[["Puma_concolor"]]$procEnvs$bgMask <- raster::stack(list.files('/Users/musasabi/Desktop/shiny_testing/mskEnvs_puma', full.names = TRUE))
    # spp[["Panthera_leo"]]$procEnvs$bgMask <- raster::stack(list.files('/Users/musasabi/Desktop/shiny_testing/mskEnvs_leo', full.names = TRUE))
    # spp[["Puma_concolor"]]$envs <- raster::stack(list.files('/Users/musasabi/Documents/github/wallace/inst/shiny/wc10', 'bil$', full.names=TRUE))
    # spp[["Panthera_leo"]]$envs <- raster::stack(list.files('/Users/musasabi/Documents/github/wallace/inst/shiny/wc10', 'bil$', full.names=TRUE))
    # rvs$bgMsk <- raster::stack(list.files('/Users/musasabi/Downloads/mskEnvs', 'gri$', full.names = TRUE))  
    print('SECRET DATA LOADED')
  })
  
  # load modules
  for (f in list.files('./modules')) source(file.path('modules', f), local=TRUE)
  
  # initialize log window
  output$log <- renderUI({
    tags$div(id='logHeader', tags$div(id='logContent', HTML(paste0(logs(), "<br>", collapse = ""))))
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
    if(component() == "occs") input$occsSel
    else if(component() == "poccs") input$procOccsSel
    else if(component() == "envs") input$envsSel
    else if(component() == "penvs") input$procEnvsSel
    else if(component() == "espace") input$espaceSel
    else if(component() == "part") input$partSel
    else if(component() == "model") input$modelSel
    else if(component() == "vis") input$visSel
    else if(component() == "proj") input$projSel
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
  m <- leaflet() %>% setView(0, 0, zoom = 2) %>% addProviderTiles('Esri.WorldTopoMap')
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
    id <- input$map_draw_new_feature$properties$`_leaflet_id`
    
    if(component() == 'poccs') {
      spp[[curSp()]]$polySelXY <- xy
      spp[[curSp()]]$polySelID <- id
    } 
    if(component() == 'proj') {
      spp[[curSp()]]$polyPjXY <- xy
      spp[[curSp()]]$polyPjID <- id  
    }
    
    # UI CONTROLS - for some reason, curSp() disappears here unless input is updated
    updateSelectInput(session, "sppSel", selected = curSp())
  })
  
  # component-level logic
  observe({
    # must have one species selected and occurrence data
    # for mapping to be functional
    req(length(curSp()) == 1, spp[[curSp()]]$occs)
    # map the original occs for Component Obtain Occurrence Data
    if(component() == 'occs') {
      map %>% map_occs(spp[[curSp()]]$occData$occsCleaned)
    } 
    # map the analysis occs for components downstream of the first
    if(component() == 'poccs') {
      if(module() == 'spthin') {
        # if you've thinned already, map thinned points blue
        # and kept points red
        if(!is.null(spp[[curSp()]]$procOccs$occsThin)) {
          map %>% map_occs(spp[[curSp()]]$procOccs$occsPreThin, fillColor = 'blue', fillOpacity = 1) %>%
            map_occs(spp[[curSp()]]$occs, fillOpacity = 1, clear = FALSE) %>%
            addLegend("bottomright", colors = c('red', 'blue'), title = "Occ Records", 
                      labels = c('retained', 'removed'), opacity = 1)  
        } else {
          # if you haven't thinned, map all points red
          map %>% map_occs(spp[[curSp()]]$occs)
        }
      } else {
        map %>% map_occs(spp[[curSp()]]$occs)
      }
    }
    if(component() == 'penvs') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      if(is.null(spp[[curSp()]]$procEnvs$bgExt)) {
        map %>% map_occs(spp[[curSp()]]$occs)
      }else{
        map %>% map_occs(spp[[curSp()]]$occs)
        for (shp in bgShpXY()) {
          map %>%
            addPolygons(lng=shp[,1], lat=shp[,2], weight=4, color="gray", group='bgShp')
        }
        bb <- spp[[curSp()]]$procEnvs$bgExt@bbox
        map %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
      }
    }
    if(component() == 'part') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      req(spp[[curSp()]]$occs$partition)
      occsGrp <- spp[[curSp()]]$occs$partition
      # colors for partition symbology
      newColors <- gsub("FF$", "", rainbow(max(occsGrp)))  
      partsFill <- newColors[occsGrp]
      map %>%
        map_occs(spp[[curSp()]]$occs, fillColor = partsFill, fillOpacity = 1) %>%
        addLegend("bottomright", colors = newColors,
                  title = "Partition Groups", labels = sort(unique(occsGrp)),
                  opacity = 1, layerId = 'leg')
    }
    # logic for initializing or removing leaflet draw toolbar
    if ((component() == 'poccs' & module() == 'selOccs') | component() == 'proj') {
      map %>% leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                             rectangleOptions = FALSE, circleOptions = FALSE, 
                                             markerOptions = FALSE)
    } else {
      map %>% leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
    }
  })
  
  ########################################## #
  # COMPONENT: OBTAIN OCCURRENCE DATA ####
  ########################################## #
  
  # # # # # # # # # # # # # # # # # # # #
  # module Query Database (Present) ####
  # # # # # # # # # # # # # # # # # # # #
  dbOccs <- callModule(queryDb_MOD, 'c1_queryDb_uiID')
  observeEvent(input$goDbOccs, {
    # return the occs table
    occsTbl <- dbOccs()
    n <- formatSpName(occsTbl$taxon_name)
    # UI CONTROLS
    # assign the selected species to the present occ table's taxon name
    updateSelectInput(session, "sppSel", selected = n)
    shinyjs::enable("dlDbOccs")
    shinyjs::enable("dlRMD")
  })
  
  # # # # # # # # # # # # # # # # # # # 
  # module Query Database (Paleo) ####
  # # # # # # # # # # # # # # # # # # # 
  dbPaleoOccs <- callModule(queryPaleoDb_MOD, 'c1_queryPaleoDb_uiID')
  observeEvent(input$goPaleoDbOccs, {
    occsTbl <- dbPaleoOccs()
    n <- formatSpName(occsTbl$taxon_name)
    # assign the selected species to the present occ table's taxon name
    updateSelectInput(session, "sppSel", selected = n)
    shinyjs::enable("dlPaleoDbOccs")
    shinyjs::enable("dlRMD")
  })
  
  # # # # # # # # # # # # # # # # # #
  # module User Occurrence Data ####
  # # # # # # # # # # # # # # # # # #
  userOccs <- callModule(userOccs_MOD, 'c1_userOccs_uiID')
  observeEvent(input$goUserOccs, {
    # output not currently getting used
    userOccs()
    print(spp[['Puma_concolor']]$bg)
    shinyjs::disable("dlDbOccs")
    shinyjs::enable("dlRMD")
  })
  
  # # # # # # # # # # # # # # # # # #
  # OBTAIN OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  output$sppSelUI <- renderUI({
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
    selectizeInput('sppSel', label = NULL , choices = sppNameList,
                   multiple = TRUE, selected = selected, options = options)
  })
  
  # shortcut to currently selected species, read from sppSelUI
  curSp <- reactive(input$sppSel)
  # vector of all species with occurrence data loaded
  allSp <- reactive(names(reactiveValuesToList(spp)))
  # conditional species input to modules with batch option
  spIn <- reactive(if(input$batch == TRUE) allSp() else curSp())
  
  # TABLE
  options <- list(autoWidth = TRUE, 
                  columnDefs = list(list(width = '40%', targets = 7)),
                  scrollX=TRUE, scrollY=400)
  output$occTbl <- DT::renderDataTable({
    # check if spp has species in it
    req(length(reactiveValuesToList(spp)) > 0)
    spp[[curSp()]]$occs %>% dplyr::mutate(longitude = round(as.numeric(longitude), digits = 2),
                                          latitude = round(as.numeric(latitude), digits = 2)) %>% 
      dplyr::select(-pop)
  }, rownames = FALSE)
  
  # DOWNLOAD: current species occurrence data table
  output$dlOccs <- downloadHandler(
    filename = function() {
      paste0(spName(spp[[curSp()]]), ".csv")
    },
    content = function(file) {
      tbl <- spp[[curSp()]]$occs %>% 
        dplyr::select(-pop)
      # if bg values are present, add them to table
      if(!is.null(spp[[curSp()]]$bg)) {
       tbl <- rbind(tbl, spp[[curSp()]]$bg) 
      }
      write.csv(tbl, file, row.names = FALSE)
    }
  )
  
  # DOWNLOAD: all species occurrence data table
  output$dlAllOccs <- downloadHandler(
    filename = "multispecies.csv",
    content = function(file) {
      inTbl <- spp[[curSp()]]$occs %>% dplyr::select(-pop)
      tbl <- matrix(ncol = ncol(inTbl))
      colnames(tbl) <- names(inTbl)
      for(sp in allSp()) {
        curTbl <- spp[[sp]]$occs %>% dplyr::select(-pop)  
        # if bg values are present, add them to table
        if(!is.null(spp[[sp]]$bg)) {
          curTbl <- rbind(curTbl, spp[[sp]]$bg) 
        }
        tbl <- rbind(tbl, curTbl)
      }
      # remove first NA row
      tbl <- tbl[-1,]
      
      write.csv(tbl, file, row.names = FALSE)
    }
  )
  
  # DOWNLOAD: occsOrig
  output$dlDbOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(spp[[curSp()]]$occs$taxon_name[1])
      source <- spp[[curSp()]]$rmm$data$occurrence$sources
      paste0(n, "_", source, ".csv")
    },
    content = function(file) {
      write.csv(spp[[curSp()]]$occData$occsOrig, file, row.names=FALSE)
    }
  )
  
  ########################################### #
  ### COMPONENT: PROCESS OCCURRENCE DATA ####
  ########################################### #
  
  # # # # # # # # # # # # # # # # # # # #
  # module Remove Occurrences By ID ####
  # # # # # # # # # # # # # # # # # # # #
  removeByID <- callModule(removeByID_MOD, 'c2_removeByID_uiID')
  observeEvent(input$goRemoveByID, {
    removeByID()
  })
  
  # # # # # # # # # # # # # # # # # # # # #
  # module Select Occurrences on Map ####
  # # # # # # # # # # # # # # # # # # # # #
  selOccs <- callModule(selectOccs_MOD, 'c2_selOccs_uiID')
  observeEvent(input$goSelectOccs, {
    req(input$map_draw_new_feature)
    selOccs()
    # MAPPING
    map %>% leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE, 
                                     markerOptions = FALSE) %>%
      map_occs(spp[[curSp()]]$occs) %>%
      zoom2Occs(spp[[curSp()]]$occs)
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable("dlProcOccs")
  })
  
  # # # # # # # # # # # # # #
  # module Spatial Thin ####
  # # # # # # # # # # # # # # 
  
  observeEvent(input$goThinOccs, {
    thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs_uiID')
    thinOccs()
    shinyjs::enable("dlProcOccs")
  })
  
  # # # # # # # # # # # # # # # # # #
  # PROCESS OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # reset occurrences button functionality
  observeEvent(input$goResetOccs, {
    req(curSp())
    spp[[curSp()]]$occs <- spp[[curSp()]]$occData$occsOrig  
    logs %>% writeLog("Reset occurrences.")
    # MAPPING
    map %>%
      map_occs(spp[[curSp()]]$occs) %>%
      zoom2Occs(spp[[curSp()]]$occs)
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
  })
  
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
    updateSelectInput(session, "sppSel", selected = curSp())
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
  userEnvs <- callModule(userEnvs_MOD, 'c3_userEnvs_uiID')
  observeEvent(input$goUserEnvs, {
    vals$envs <- userEnvs()
    # stop if no occurrence data
    req(vals$occs)
    # remove occurrences with NA values for variables
    vals$occs <- remEnvsValsNA(vals)
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
  output$envSelUI <- renderUI({
    # for Maxent, only display the environmental predictors with non-zero beta coefficients
    # from the lambdas file (the predictors that were not removed via regularization)
    # if (spp[[curSp()]]$rmm$model$algorithm == "Maxent") {
    #   mod <- spp[[curSp()]]$model$models[[curModel()]]
    #   n <- mxNonzeroCoefs(mod)
    # } else {
    
    # ensure envs entity is within spp
    if(!is.null(curSp())) {
      n <- names(spp[[curSp()]]$envs)
    } else {
      n <- NULL
    }
    envsNameList <- c(list("Current variable" = ""), setNames(as.list(n), n))
    if(component() == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    selectizeInput('envSel', label = NULL , choices = envsNameList,
                   multiple = TRUE, selected = n[1], options = options)
  })
  
  # shortcut to currently selected environmental variable, read from envSelUI
  curEnv <- reactive({input$envSel})
  
  # map center coordinates for 30 arcsec download
  mapCntr <- reactive(mapCenter(input$map_bounds))
  
  # text showing the current map center
  output$ctrLatLon <- renderText({
    paste('Using map center', paste(mapCntr(), collapse=', '))
  })
  
  # CONSOLE PRINT
  output$envsPrint <- renderPrint({
    req(spp[[curSp()]]$envs)
    spp[[curSp()]]$envs
    # mins <- sapply(spp[[curSp()]]$envs@layers, function(x) x@data@min)
    # maxs <- sapply(spp[[curSp()]]$envs@layers, function(x) x@data@max)
    # names <- sapply(strsplit(names(spp[[curSp()]]$envs), '[.]'), function(x) x[-2])
    # DT::datatable(data.frame(name=names, min=mins, max=maxs), 
    #               rownames = FALSE, options = list(pageLength = raster::nlayers(spp[[curSp()]]$envs)))
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
    # updateSelectInput(session, "sppSel", selected = curSp())
  })
  
  # # # # # # # # # # # # # # # # # # # # # # # 
  # module User-defined Background Extent ####
  # # # # # # # # # # # # # # # # # # # # # # # 
  userBg <- callModule(userBgExtent_MOD, 'c4_userBgExtent')
  observeEvent(input$goUserBg, {
    userBg()
    # stop if no environmental variables
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
  
  # # # # # # # # # # # # # # # # # # # # # # # # #
  # module Background Mask and Sample Points ####
  # # # # # # # # # # # # # # # # # # # # # # # # #
  
  observeEvent(input$goBgMask, {
    # stop if no background shape
    req(spp[[curSp()]]$procEnvs$bgExt)
    bgMskPts <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts')
    bgMskPts()
    # UI CONTROLS 
    updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable('dlMskEnvs')
  })
  
  # # # # # # # # # # # # # # # # # #
  # PROCESS ENVS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # get the coordinates of the current background extent shape
  bgShpXY <- reactive({
    req(spp[[curSp()]]$procEnvs$bgExt)
    polys <- spp[[curSp()]]$procEnvs$bgExt@polygons[[1]]@Polygons
    if(length(polys) == 1) {
      xy <- list(polys[[1]]@coords)
    }else{
      xy <- lapply(polys, function(x) x@coords)
    }
    return(xy)
  })
  
  # DOWNLOAD: masked environmental variable rasters
  output$dlMskEnvs <- downloadHandler(
    filename = function() {'mskEnvs.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      type <- input$bgMskFileType
      nm <- names(spp[[curSp()]]$envs)
      
      raster::writeRaster(spp[[curSp()]]$procEnvs$bgMask, file.path(tmpdir, 'msk'), bylayer = TRUE,
                          suffix = nm, format = type, overwrite = TRUE)
      ext <- switch(type, raster = 'grd', ascii = 'asc', GTiff = 'tif')
      
      fs <- paste0('msk_', nm, '.', ext)
      if (ext == 'grd') {
        fs <- c(fs, paste0('msk_', nm, '.gri'))
      }
      zip(zipfile=file, files=fs)
      if (file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
    },
    contentType = "application/zip"
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
      logs %>% writeLog(type = 'error', "Please select two species.")
    }
    req(spp[[curSp()[1]]]$procEnvs$bg.envsVals, spp[[curSp()[2]]]$procEnvs$bg.envsVals)
    # initialize module
    pca()
    # UI CONTROLS 
    updateTabsetPanel(session, 'main', selected = 'Results')
    updateSelectInput(session, "sppSel", selected = curSp())
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
    updateSelectInput(session, "sppSel", selected = curSp())
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
    updateSelectInput(session, "sppSel", selected = curSp())
  })
  
  ################################################## #
  ### COMPONENT: PARTITION OCCURRENCE DATA ####
  ################################################# #
  
  # # # # # # # # # # # # # # # # # # # # # # # # #
  # module Non-spatial Occurrence Partitions ####
  # # # # # # # # # # # # # # # # # # # # # # # # #
  partNsp <- callModule(partNsp_MOD, 'cParts_partNsp_uiID')
  observeEvent(input$goPartNsp, {
    partNsp()
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable("dlPart")
  })
  
  # # # # # # # # # # # # # # # # # # # # # # #
  # module Spatial Occurrence Partitions ####
  # # # # # # # # # # # # # # # # # # # # # # #
  partSp <- callModule(partSp_MOD, 'cParts_partSp_uiID')
  observeEvent(input$goPartSp, {
    partSp()
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable("dlPart")
  })
  
  ######################### #
  ### COMPONENT: MODEL ####
  ######################### #
  
  # # # # # # # # # # # 
  # module Maxent ####
  # # # # # # # # # # # 
  mod.maxent <- callModule(maxent_MOD, 'c6_maxent', rvs)
  
  observeEvent(input$goMaxent, {
    mod.maxent()
    if(is.null(mod.maxent())) return()
    results <- spp[[curSp()]]$model$results
    results.round <- cbind(results[,1:3], round(results[,4:ncol(results)], digits=3))
    
    # full model and partition average evaluation table, and individual partition table
    output$evalTbls <- renderUI({
      tagList(
        br(),
        div("Full model and partition bin average evaluation statistics", id="stepText"), br(), br(),
        DT::dataTableOutput('evalTbl'), br(), 
        div("Individual partition bin evaluation statistics", id="stepText"), br(), br(),
        DT::dataTableOutput('evalTblBins')  
      )
    })
    output$evalTbl <- DT::renderDataTable(results.round[,1:16], 
                                          options = list(scrollX = TRUE,
                                                         sDom  = '<"top">rtp<"bottom">'))
    output$evalTblBins <- DT::renderDataTable(results.round[,17:ncol(results)], 
                                              options = list(scrollX = TRUE,
                                                             sDom  = '<"top">rtp<"bottom">'))
    shinyjs::show(id = "evalTblBins")
    
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # customize visualizations for maxent
    updateRadioButtons(session, "visSel", 
                       choices = list("Maxent Evaluation Plots" = 'mxEval',
                                      "Plot Response Curves" = 'response',
                                      "Map Prediction" = 'map'))
  })
  
  # # # # # # # # # # # # 
  # module BIOCLIM ####
  # # # # # # # # # # # # 
  mod.bioclim <- callModule(bioclim_MOD, 'c6_bioclim')
  
  observeEvent(input$goBioclim, {
    mod.bioclim()
    if(is.null(spp[[curSp()]]$model)) return()
    # evaluation table (written this way to be compatible with multiple tables, 
    # e.g. like in the Maxent module)
    output$evalTbls <- renderUI({
      tagList(
        br(), 
        div("Full model, partition bin average and individual evaluation statistics", id="stepText"), 
        br(), br(),
        DT::dataTableOutput('evalTbl')
      )
    })
    output$evalTbl <- DT::renderDataTable(round(spp[[curSp()]]$model$results, digits=3), 
                                          options = list(scrollX = TRUE, sDom  = '<"top">rtp<"bottom">'))
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # update radio buttons for Visualization component
    updateRadioButtons(session, "visSel", choices = list("BIOCLIM Envelope Plots" = 'bcPlots',
                                                         "Map Prediction" = 'map'))
    shinyjs::hide(id = "evalTblBins")
  })
  
  # # # # # # # # # # # # # # # # # #
  # MODEL: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # ui that populates with the names of models that were run
  output$modelSelUI <- renderUI({
    # ensure mod entity is within spp
    if(!is.null(curSp())) {
      if(!is.null(spp[[curSp()]]$model)) {
        n <- names(spp[[curSp()]]$model$models)  
      } else {
        n <- NULL
      }
    } else {
      n <- NULL
    }
    
    modsNameList <- c(list("Current model" = ""), setNames(as.list(n), n))
    options <- list(maxItems = 1)
    selectizeInput('modSel', label = NULL , choices = modsNameList,
                   multiple = TRUE, selected = n[1], options = options)
  })
  
  # shortcut to currently selected model, read from modSelUI
  curModel <- reactive({input$modelSel})
  
  ########################################### #
  ### COMPONENT: VISUALIZE MODEL RESULTS ####
  ########################################### #
  
  # # # # # # # # # # # # 
  # module BIOCLIM Plots ####
  # # # # # # # # # # # # 
  bcPlot <- callModule(bcPlot_MOD, 'c7_bcPlot')
  output$bcEnvelPlot <- renderPlot({
    bcPlot()
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
    req(mapPreds())
    rvs$predCur <- mapPreds()
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
                     group = 'c7', layerId = 'r1ID')
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
      ext <- switch(input$predFileType, raster = 'grd', ascii = 'asc', GTiff = 'tif', PNG = 'png')
      paste0(names(rvs$predCur), '.', ext)},
    content = function(file) {
      if (input$predFileType == 'png') {
        png(file)
        raster::image(rvs$predCur)
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
  
  # # # # # # # # # # # # # # # # # #
  # VISUALIZE: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # handle downloads for BIOCLIM Plots png
  output$dlVizPlot <- downloadHandler(
    filename = function() {paste0(spName(), "_bc_plot.png")},
    content = function(file) {
      png(file)
      if(module() == "bcPlot") bcPlot()
      dev.off()
    }
  )
  
  ########################################### #
  ### COMPONENT: PROJECT MODEL ####
  ########################################### #
  
  # module Project to New Area
  projArea <- callModule(projectArea_MOD, 'c8_projectArea', rvs)
  
  observeEvent(input$goProjectArea, {
    projArea.call <- projArea()
    # stop if no model prediction
    req(rvs$predCur)
    # unpack
    rvs$projMsk <- projArea.call[[1]]
    rvs$projCur <- projArea.call[[2]]
    rvs$projCurVals <- getVals(rvs$projCur, rvs$comp7.type)
    rvs$comp8.pj <- 'area'
    
    rasVals <- c(rvs$predCurVals, rvs$projCurVals)
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    map %>% comp8_map(rvs$projCur, rvs$polyPjXY, bgShpXY, rasVals, 
                      rasCols, "Predicted Suitability", 'rProj')
    
    map %>% drawToolbarRefresh()
    
    shinyjs::enable("dlProj")
  })
  
  # module Project to New Time
  projTime <- callModule(projectTime_MOD, 'c8_projectTime', rvs)
  
  observeEvent(input$goProjectTime, {
    projTime.call <- projTime()
    # stop if no model prediction
    req(rvs$predCur)
    # unpack
    rvs$projMsk <- projTime.call[[1]]
    rvs$projCur <- projTime.call[[2]]
    req(rvs$projCur)
    rvs$projCurVals <- getVals(rvs$projCur, rvs$comp7.type)
    rvs$comp8.pj <- 'time'
    
    rasVals <- c(rvs$predCurVals, rvs$projCurVals)
    rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    map %>% comp8_map(rvs$projCur, rvs$polyPjXY, bgShpXY, rasVals, 
                      rasCols, "Predicted Suitability", 'rProj')
    
    map %>% drawToolbarRefresh()
    
    shinyjs::enable("dlProj")
  })
  
  # module Environmental Similarity
  envSimilarity <- callModule(envSimilarity_MOD, 'c8_envSimilarity', rvs)
  
  observeEvent(input$goEnvSimilarity, {
    rvs$mess <- envSimilarity()
    # stop if no model projection
    req(rvs$projCur)
    rvs$comp8.esim <- 'mess'
    # set infinite values to NA
    rvs$mess[is.infinite(rvs$mess)] <- NA
    # extract values
    rvs$messVals <- getVals(rvs$mess)
    
    rasVals <- rvs$messVals
    rasCols <- RColorBrewer::brewer.pal(n=11, name='Reds')
    map %>% comp8_map(rvs$mess, rvs$polyPjXY, bgShpXY, rasVals, rasCols, "MESS Values")
    
    shinyjs::enable("dlProj")
  })
  
  # Reset Projection Extent button functionality
  observeEvent(input$goResetProj, {
    map %>%
      removeShape("projExt") %>%
      removeImage("rProj")
    logs %>% writeLog("Reset projection extent.")
  })
  
  # download for model predictions (restricted to background extent)
  output$dlProj <- downloadHandler(
    filename = function() {
      ext <- switch(input$projFileType, raster = 'grd', ascii = 'asc', GTiff = 'tif', PNG = 'png')
      paste0(names(rvs$projCur), '.', ext)},
    content = function(file) {
      if (input$projFileType == 'png') {
        png(file)
        raster::image(rvs$projCur)
        dev.off()
      } else if (input$projFileType == 'raster') {
        fileName <- names(rvs$projCur)
        tmpdir <- tempdir()
        raster::writeRaster(rvs$projCur, file.path(tmpdir, fileName), format = input$projFileType, overwrite = TRUE)
        fs <- file.path(tmpdir, paste0(fileName, c('.grd', '.gri')))
        zip(zipfile=file, files=fs, extras = '-j')
      } else {
        r <- raster::writeRaster(rvs$projCur, file, format = input$projFileType, overwrite = TRUE)
        file.rename(r@file@name, file)
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
      sp <- spp[[curSp()]]
      exp <- knitr::knit_expand("Rmd/userReport.Rmd", 
                                spName=sp$occs$taxon_name[1], 
                                occsSource=sp$rmm$data$occurrence$sources,
                                occsNum=sp$rmm$code$wallaceSettings$occsNum  # comp 1
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
      rangeModelMetadata::rmmToCSV(spp[[curSp()]]$rmm, filename = file)
    })
})