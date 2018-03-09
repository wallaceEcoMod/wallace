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
  shinyjs::disable("dlRMD")
  
  ##################### #
  # REACTIVE LISTS ####
  ##################### #
  
  # reactiveValues list for objects that get modified and reused throughout analysis
  spp <- reactiveValues()
  occsLastQuery <- reactiveVal()
  msp <- reactiveValues()
  # reactiveValues list for holding the current guidance text
  gtext <- reactiveValues()
  # single reactive value for log vector
  logs <- reactiveVal(logInit())
  # legacy
  rvs <- reactiveValues()
  # legacy
  rmd <- reactiveValues()
  
  # FOR DEVELOPMENT
  observeEvent(input$load, {
    f <- read.csv('example_data/occs_multisp.csv')
    spp[["Puma_concolor"]]$occs <- spp[["Puma_concolor"]]$occData$occsOrig <- f %>% dplyr::filter(taxon_name == 'Puma concolor') %>% dplyr::select(taxon_name,longitude,latitude,occID)
    spp[["Panthera_leo"]]$occs <- spp[["Panthera_leo"]]$occData$occsOrig <- f %>% dplyr::filter(taxon_name == 'Panthera leo') %>% dplyr::select(taxon_name,longitude,latitude,occID)
    spp[["Puma_concolor"]]$occs$pop <- spp[["Puma_concolor"]]$occData$occsOrig$pop <- unlist(apply(spp[["Puma_concolor"]]$occs, 1, popUpContent))
    spp[["Panthera_leo"]]$occs$pop <- spp[["Panthera_leo"]]$occData$occsOrig$pop <- unlist(apply(spp[["Panthera_leo"]]$occs, 1, popUpContent))
    # rvs$occsGrp <- rvs$occs$group
    spp[["Puma_concolor"]]$bg <- f %>% dplyr::filter(taxon_name == 'background1') %>% dplyr::select(longitude, latitude)
    spp[["Panthera_leo"]]$bg <- f %>% dplyr::filter(taxon_name == 'background2') %>% dplyr::select(longitude, latitude)
    # rvs$bgGrp <- rvs$bgPts$group
    spp[["Puma_concolor"]]$occs <- cbind(spp[["Puma_concolor"]]$occs, read.csv('/Users/musasabi/Desktop/shiny_testing/Puma concolor_z.csv'))
    spp[["Panthera_leo"]]$occs <- cbind(spp[["Panthera_leo"]]$occs, read.csv('/Users/musasabi/Desktop/shiny_testing/Panthera leo_z.csv'))
    spp[["Puma_concolor"]]$bg <- cbind(spp[["Puma_concolor"]]$bg, read.csv('/Users/musasabi/Desktop/shiny_testing/Puma concolor_bz.csv'))
    spp[["Panthera_leo"]]$bg <- cbind(spp[["Panthera_leo"]]$bg, read.csv('/Users/musasabi/Desktop/shiny_testing/Panthera leo_bz.csv'))
    # rvs$bgShp <- rgdal::readOGR('/Users/musasabi/Downloads', 'mcp')
    spp[["Puma_concolor"]]$procEnvs$bgMask <- raster::stack(list.files('/Users/musasabi/Desktop/shiny_testing/mskEnvs_puma', full.names = TRUE))
    spp[["Panthera_leo"]]$procEnvs$bgMask <- raster::stack(list.files('/Users/musasabi/Desktop/shiny_testing/mskEnvs_leo', full.names = TRUE))
    spp[["Puma_concolor"]]$envs <- raster::stack(list.files('/Users/musasabi/Documents/github/wallace/inst/shiny/wc10', 'bil$', full.names=TRUE))
    spp[["Panthera_leo"]]$envs <- raster::stack(list.files('/Users/musasabi/Documents/github/wallace/inst/shiny/wc10', 'bil$', full.names=TRUE))
    # rvs$bgMsk <- raster::stack(list.files('/Users/musasabi/Downloads/mskEnvs', 'gri$', full.names = TRUE))  
    print('HACKING DONE')
  })
  
  # for RMD
  curWD <- getwd()
  
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
  output$gtext_comp <- renderUI({
    shiny::includeMarkdown(file.path('Rmd', gtext$cur_comp))
  })
  
  # UI for module guidance text
  output$gtext_mod <- renderUI({
    shiny::includeMarkdown(file.path('Rmd', gtext$cur_mod))
  })
  
  # guidance text and tab behavior
  tabs <- reactive({input$tabs})
  
  observe({
    if (input$tabs == 'occs') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- 'gtext_comp1.Rmd'
      if (input$occSel == 'db') gtext$cur_mod <- "gtext_comp1_dbOccs.Rmd"
      if (input$occSel == 'user') gtext$cur_mod <- "gtext_comp1_userOccs.Rmd"
    }
    if (input$tabs == 'poccs') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp2.Rmd"
      # if Module: Select Localities, populate guidance text and select legend
      if (input$procOccSel == 'selOccs') gtext$cur_mod <- "gtext_comp2_selectOccsOnMap.Rmd"
      if (input$procOccSel == 'remID') gtext$cur_mod <- "gtext_comp2_removeByID.Rmd"
      if (input$procOccSel == 'spthin') gtext$cur_mod <- "gtext_comp2_spatialThin.Rmd"
    }
    if (input$tabs == 'envs') {
      updateTabsetPanel(session, 'main', selected = 'Results')
      gtext$cur_comp <- "gtext_comp3.Rmd"
      if (input$envDataSel == 'wcbc') gtext$cur_mod <- "gtext_comp3_worldclim.Rmd"
      if (input$envDataSel == 'ecoClimatelayers') gtext$cur_mod <- "gtext_comp3_ecoClimate.Rmd"
      if (input$envDataSel == 'user') gtext$cur_mod <- "gtext_comp3_userEnvs.Rmd"
    }
    if (input$tabs == 'penvs') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp4.Rmd"
      if (input$envProcSel == 'bgSel') gtext$cur_mod <- "gtext_comp4_backg.Rmd"
      if (input$envProcSel == 'bgUser') gtext$cur_mod <- "gtext_comp4_userBg.Rmd"
    }
    if (input$tabs == 'part') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp5.Rmd"
      if (input$partSel == 'sp') gtext$cur_mod <- "gtext_comp5_spatial.Rmd"
      if (input$partSel == 'nsp') gtext$cur_mod <- "gtext_comp5_nonspatial.Rmd"
    }
    if (input$tabs == 'model') {
      updateTabsetPanel(session, 'main', selected = 'Results')
      gtext$cur_comp <- "gtext_comp6.Rmd"
      if (input$enmSel == 'BIOCLIM') gtext$cur_mod <- "gtext_comp6_bioclim.Rmd"
      if (input$enmSel == 'Maxent') gtext$cur_mod <- "gtext_comp6_maxent.Rmd"
    }
    if (input$tabs == 'viz') {
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
    if (input$tabs == 'proj') {
      updateTabsetPanel(session, 'main', selected = 'Map')
      gtext$cur_comp <- "gtext_comp8.Rmd"
      if (input$projSel == 'projArea') gtext$cur_mod <- "gtext_comp8_pjArea.Rmd"
      if (input$projSel == 'projTime') gtext$cur_mod <- "gtext_comp8_pjTime.Rmd"
      if (input$projSel == 'mess') gtext$cur_mod <- "gtext_comp8_mess.Rmd"
    }
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
    
    if(input$tabs == 'poccs') {
      spp[[curSp()]]$polySelXY <- xy
      spp[[curSp()]]$polySelID <- id
    } 
    if(input$tabs == 'proj') {
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
    if(input$tabs == 'occs') {
      map %>% map_occs(spp[[curSp()]]$occData$occsCleaned)
    } 
    # map the analysis occs for components downstream of the first
    if(input$tabs == 'poccs') {
      if(input$procOccSel == 'spthin') {
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
    if(input$tabs == 'penvs') {
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
    if(input$tabs == 'part') {
      req(spp[[curSp()]]$occs$grp)
      occsGrp <- spp[[curSp()]]$occs$grp
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
    if ((input$tabs == 'poccs' & input$procOccSel == 'selOccs') | input$tabs == 'proj') {
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
  })
  
  # # # # # # # # # # # # # # # # # #
  # module User Occurrence Data ####
  # # # # # # # # # # # # # # # # # #
  userOccs <- callModule(userOccs_MOD, 'c1_userOccs_uiID')
  observeEvent(input$goUserOccs, {
    # output not currently getting used
    userOccs()
    shinyjs::disable("dlDbOccs")
  })
  
  # # # # # # # # # # # # # # # # # #
  # OBTAIN OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  output$sppSelUI <- renderUI({
    # check that a species is in the list already -- if not, don't proceed
    req(length(reactiveValuesToList(spp)) > 0)
    # get the species names
    n <- names(spp)
    # make a named list of their names
    sppNameList <- setNames(as.list(n), n)
    # if no current species selected, select the first name
    # NOTE: this line is necessary to retain the selection after selecting different tabs
    if(!is.null(curSp())) selected <- curSp() else selected <- n[1]
    # if espace component, allow for multiple species selection
    if(input$tabs == 'espace') options <- list(maxItems = 2) else options <- list(maxItems = 1)
    # generate a selectInput ui that lists the available species
    selectizeInput('sppSel', label = "Current species", choices = sppNameList,
                   multiple = TRUE, selected = selected, options = options)
  })
  
  # shortcut to current species, read from sppSelUI
  curSp <- reactive(input$sppSel)
  # vector of all species with occurrence data downloaded
  allSp <- reactive(names(reactiveValuesToList(spp)))
  
  # TABLE
  options <- list(autoWidth = TRUE, 
                  columnDefs = list(list(width = '40%', targets = 7)),
                  scrollX=TRUE, scrollY=400)
  output$occTbl <- DT::renderDataTable({
    # check if spp has species in it
    req(length(reactiveValuesToList(spp)) > 0)
    spp[[curSp()]]$occs %>% dplyr::mutate(longitude = round(as.numeric(longitude), digits = 2),
                                          latitude = round(as.numeric(latitude), digits = 2)) %>% 
      dplyr::select(taxon_name, occID, longitude:record_type)
  }, rownames = FALSE)
  
  # DOWNLOAD
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
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
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
  thinOccs <- callModule(thinOccs_MOD, 'c2_thinOccs_uiID')
  observeEvent(input$goThinOccs, {
    thinOccs()
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable("dlProcOccs")
  })
  
  # # # # # # # # # # # # # # # # # #
  # PROCESS OCCS: other controls ####
  # # # # # # # # # # # # # # # # # #
  
  # DOWNLOAD
  output$dlOccs <- downloadHandler(
    filename = function() {
      n <- formatSpName(spp[[curSp()]]$occs$taxon_name[1])
      paste0(n, ".csv")
    },
    content = function(file) {
      tbl <- spp[[curSp()]]$occs %>% dplyr::select(taxon_name, occID, longitude:record_type)
      write.csv(tbl, file, row.names = FALSE)
    }
  )
  
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
  wcBioclims <- callModule(wcBioclims_MOD, 'c3_wcBioclims_uiID')
  observeEvent(input$goEnvData, {
    # stop if no occurrence data
    req(spp[[curSp()]]$occs)
    # load into envs
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
  bgExt <- callModule(bgExtent_MOD, 'c4_bgExtent_uiID')
  observeEvent(input$goBgExt, {
    # initialize module
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
  bgMskPts <- callModule(bgMskAndSamplePts_MOD, 'c4_bgMskAndSamplePts')
  observeEvent(input$goBgMask, {
    # stop if no background shape
    req(spp[[curSp()]]$procEnvs$bgExt)
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
  
  # DOWNLOAD
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
  
  # module Occurrence Density Grids
  occDens <- callModule(occDens_MOD, 'cEspace_occDens_uiID')
  
  observeEvent(input$goOccDens, {
    # stop if no environmental variables
    req(msp[[curMSp()]]$pca)
    # initialize module
    occDens()
    # UI CONTROLS 
    updateSelectInput(session, "sppSel", selected = curSp())
  })
  
  # module Niche Overlap
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
  
  # module Non-spatial Occurrence Partitions
  partNsp <- callModule(partNsp_MOD, 'cParts_partNsp_uiID')
  
  observeEvent(input$goPartNsp, {
    partNsp()
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable("dlPart")
  })
  
  # module Spatial Occurrence Partitions
  partSp <- callModule(partSp_MOD, 'cParts_partSp_uiID')
  
  observeEvent(input$goPartSp, {
    partSp()
    # UI CONTROLS 
    # updateSelectInput(session, "sppSel", selected = curSp())
    shinyjs::enable("dlPart")
  })
  
  # download for partitioned occurrence records csv
  output$dlPart <- downloadHandler(
    filename = function() paste0(vals$spName, "_partitioned_occs.csv"),
    content = function(file) {
      bg.bind <- data.frame(rep('background', nrow(rvs$bgPts)), rvs$bgPts)
      names(bg.bind) <- c('name', 'longitude', 'latitude')
      occs.bg.bind <-rbind(rvs$occs[,1:3], bg.bind)
      all.bind <- cbind(occs.bg.bind, c(rvs$occsGrp, rvs$bgGrp))
      names(all.bind)[4] <- "group"
      write.csv(all.bind, file, row.names = FALSE)
    }
  )
  
  ######################### #
  ### COMPONENT: MODEL ####
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
    # x <- callModule(mapPreds_MOD, 'c7_mapPreds', rvs)
    
    ncols <- ncol(rvs$modRes)
    modRes.round <- cbind(rvs$modRes[,1:3], round(rvs$modRes[,4:ncols], digits=3))
    nBinsCols <- ncols - 16
    # render both full model and partition avg datatable, and individual partition datatable
    output$evalTbls <- renderUI({
      tagList(
        br(),
        div("Full model and partition bin average evaluation statistics", id="stepText"), br(), br(),
        DT::dataTableOutput('evalTbl'), br(), 
        div("Individual partition bin evaluation statistics", id="stepText"), br(), br(),
        DT::dataTableOutput('evalTblBins')  
      )
    })
    output$evalTbl <- DT::renderDataTable(modRes.round[,1:16], 
                                          options = list(scrollX = TRUE,
                                                         sDom  = '<"top">rt<"bottom">'))
    output$evalTblBins <- DT::renderDataTable(modRes.round[,17:(nBinsCols+16)], 
                                              options = list(scrollX = TRUE,
                                                             sDom  = '<"top">rt<"bottom">'))
    shinyjs::show(id = "evalTblBins")
    
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    # customize visualizations for maxent
    updateRadioButtons(session, "visSel", 
                       choices = list("Maxent Evaluation Plots" = 'mxEval',
                                      "Plot Response Curves" = 'response',
                                      "Map Prediction" = 'map'))
  })
  
  # module BIOCLIM
  mod.bioclim <- callModule(bioclim_MOD, 'c6_bioclim')
  
  observeEvent(input$goBioclim, {
    e <- mod.bioclim()
    # stop if no occurrence partition group
    req(rvs$occsGrp)
    rvs$comp6 <- 'bioclim'  # record the enm selected
    rvs$mods <- e$models
    rvs$modPreds <- e$predictions
    rvs$modRes <- e$results
    output$evalTbls <- renderUI({
      tagList(
        br(), 
        div("Full model, partition bin average and individual evaluation statistics", id="stepText"), br(), br(),
        DT::dataTableOutput('evalTbl')
      )
    })
    output$evalTbl <- DT::renderDataTable(round(rvs$modRes, digits=3), options = list(scrollX = TRUE,
                                                                                      sDom  = '<"top">rt<"bottom">'))
    # switch to Results tab
    updateTabsetPanel(session, 'main', selected = 'Results')
    updateRadioButtons(session, "visSel", 
                       choices = list("BIOCLIM Envelope Plots" = 'bcPlots',
                                      "Map Prediction" = 'map'))
    shinyjs::hide(id = "evalTblBins")
  })
  
  ########################################### #
  ### COMPONENT: VISUALIZE MODEL RESULTS ####
  ########################################### #
  
  # ui that populates with the names of models that were run
  output$modSelUI <- renderUI({
    req(rvs$modPreds)
    n <- names(rvs$modPreds)
    modsNameList <- setNames(as.list(n), n)
    selectInput('modSel', label = "Current model",
                choices = modsNameList, selected = modsNameList[[1]])
  })
  
  # ui that populates with the names of environmental predictors used
  output$envSelUI <- renderUI({
    req(rvs$modPreds)
    # for Maxent, only display the environmental predictors with non-zero beta coefficients
    # from the lambdas file (the predictors that were not removed via regularization)
    if (rvs$comp6 == "maxent") {
      modCur <- rvs$mods[[rvs$modSel]]
      nonZeroCoefs <- mxNonzeroCoefs(modCur)
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
                                curWD=curWD, spName=spName(), 
                                dbName=rvs$occDb, occNum=rvs$occNum, occsCSV=rvs$userCSV$name,  # comp 1
                                thinDist=rvs$thinDist, occsRemoved=rvs$occsRem, occsSelX=polySelX, occsSelY=polySelY,  # comp 2
                                bcRes=rvs$bcRes, bcLat=rvs$bcLat, bcLon=rvs$bcLon, # comp 3
                                userEnvs=printVecAsis(rvs$userEnvs$name), bcSels=bcSels, # comp 3
                                bgSel=rvs$comp4.shp, bgBuf=rvs$comp4.buf, bgUserCSVpath=rvs$userBgShp$datapath,  # comp 4
                                bgUserCSVname=rvs$userBgShp$name, bgUserShpPath=rvs$bgUserShpPar$dsn,  # comp 4 
                                bgUserShpName=rvs$bgUserShpPar$layer, bgPtsNum=rvs$bgPtsNum, # comp 4
                                partSel=rvs$partSel, kfolds=rvs$kfolds, aggFact=rvs$aggFact,  # comp 5
                                enmSel=rvs$comp6, rms1=rvs$rms[1], rms2=rvs$rms[2], rmsStep=rvs$rmsStep, # comp 6
                                fcs=printVecAsis(rvs$fcs),  # comp 6
                                modSel=rvs$modSel, mxNonZeroCoefs=printVecAsis(rvs$mxNonZeroCoefs), envSel=rvs$envSel,  # comp 7
                                bcPlot1=rvs$bcPlotsPar$bc1, bcPlot2=rvs$bcPlotsPar$bc2, bcPlotP=rvs$bcPlotsPar$p,  # comp 7
                                mxEvalSel=rvs$mxEvalSel, predType=rvs$comp7.type, comp7.thresh=rvs$comp7.thr, # comp 7 
                                occsPjX=polyPjX, occsPjY=polyPjY, pjRCP=rvs$pjTimePar$rcp, pjGCM=rvs$pjTimePar$gcm,  # comp 8
                                pjYear=rvs$pjTimePar$year, comp8.thresh=rvs$comp8.thr)  # comp 8
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
  
  ###############################
  ### METADATA FUNCTIONALITY ####
  
  output$dlRMM <- downloadHandler(
    filename = function() {
      paste0("wallace-session-", Sys.Date(), ".csv")
             },
    content = function(file) {
      rangeModelMetadata::rmmToCSV(spp[[curSp()]]$rmm, filename = file)
  })
})