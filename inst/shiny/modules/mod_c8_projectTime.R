projectTime_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # aliases are different for ecoclimate temporal scenarios. e.g. 
    # "lgm" is "LGM"
    # "mid" is "Holo"
    # "2.6" is "Future 2.6"
    # "4.5" is "Future 4.5"
    # "6" is "Future 6"
    # "8.5" is "Future 8.5"
    selectInput(ns("selTime"), label = "Select time period",
                choices = list("Select period" = "",
                               # "Last Glacial Maximum (~22,000 years ago)" = 'lgm',
                               # "Mid Holocene (~6000 years ago)" = 'mid',
                               "2050" = 50,
                               "2070" = 70)),
    uiOutput(ns('selGCMui')),
    selectInput(ns('selRCP'), label = "Select RCP",
                choices = list("Select RCP" = "",
                               '2.6' = 26,
                               '4.5' = 45,
                               '6.0' = 60,
                               '8.5' = 85)),
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. 
             Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'none',
                                        "Minimum Training Presence" = 'mtp', 
                                        "10 Percentile Training Presence" = 'p10')))
  )
}

projectTime_MOD <- function(input, output, session) {
  GCMlookup <- c(AC="ACCESS1-0", BC="BCC-CSM1-1", CC="CCSM4", CE="CESM1-CAM5-1-FV2",
                 CN="CNRM-CM5", GF="GFDL-CM3", GD="GFDL-ESM2G", GS="GISS-E2-R",
                 HD="HadGEM2-AO", HG="HadGEM2-CC", HE="HadGEM2-ES", IN="INMCM4",
                 IP="IPSL-CM5A-LR", ME="MPI-ESM-P", MI="MIROC-ESM-CHEM", MR="MIROC-ESM",
                 MC="MIROC5", MP="MPI-ESM-LR", MG="MRI-CGCM3", NO="NorESM1-M")
  # dynamic ui for GCM selection: choices differ depending on choice of time period
  output$selGCMui <- renderUI({
    ns <- session$ns
    
    if (input$selTime == 'lgm') {
      gcms <- c('CC', 'MR', 'MC')
    } else if (input$selTime == 'mid') {
      gcms <- c("BC", "CC", "CE", "CN", "HG", "IP", "MR", "ME", "MG")
    } else {
      gcms <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD",
                "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
    }
    names(gcms) <- GCMlookup[gcms]
    gcms <- as.list(c("Select GCM" = "", gcms))
    selectInput(ns("selGCM"), label = "Select global circulation model", choices = gcms)
  })
  
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                             before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. 
                             Please use the draw toolbar on the left-hand of the map to complete
                             the polygon.")
      return()
    }
    
    if(is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    envsRes <- raster::res(spp[[curSp()]]$envs)[1]
    if(envsRes < 0.01) {
      shinyLogs %>% writeLog(type = 'error', 'Project to New Time currently only available with resolutions >30 arc seconds.')
      return()
    }
    
    # code taken from dismo getData() function to catch if user is trying to 
    # download a missing combo of gcm / rcp
    gcms <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 
              'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
    rcps <- c(26, 45, 60, 85)
    m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,
                  0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
    i <- m[which(input$selGCM == gcms), which(input$selRCP == rcps)]
    if (!i) {
      shinyLogs %>% writeLog(type = 'error', 'This combination of GCM and RCP is not 
                       available. Please make a different selection.')
      return()
    }
    
    # DATA ####
    smartProgress(shinyLogs, message = paste("Retrieving WorldClim data for", input$selTime, input$selRCP, "..."), {
      projTimeEnvs <- raster::getData('CMIP5', var = "bio", res = envsRes * 60,
                                      rcp = input$selRCP, model = input$selGCM, year = input$selTime)
      names(projTimeEnvs) <- paste0('bio', c(paste0('0',1:9), 10:19))
      # in case user subsetted bioclims
      projTimeEnvs <- projTimeEnvs[[names(envs())]]
    })
    
    # FUNCTION CALL ####    
    predType <- rmm()$output$prediction$notes
    projTime.out <- c8_projectTime(results(),
                                   curModel(),
                                   projTimeEnvs,
                                   predType,
                                   spp[[curSp()]]$polyPjXY,
                                   spp[[curSp()]]$polyPjID,
                                   shinyLogs)
    projExt <- projTime.out$projExt
    projTime <- projTime.out$projTime
    
    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    if(!(input$threshold == 'none')) {
      # use threshold from present-day model training area
      thr <- spp[[curSp()]]$visualization$thresholds[[input$threshold]]
      projTimeThr <- projTime > thr
      shinyLogs %>% writeLog("Projection of model to", paste0('20', input$selTime), "for", 
                             curSp(), 'with threshold', input$threshold, ': ', thr,
                             "for GCM", GCMlookup[input$selGCM], 
                             "under RCP", as.numeric(input$selRCP)/10.0, ".")
    } else {
      projTimeThr <- projTime
      shinyLogs %>% writeLog("Projection of model to", paste0('20', input$selTime), "for", 
                             curSp(), 'with', predType, 'output',
                             "for GCM", GCMlookup[input$selGCM], 
                             "under RCP", as.numeric(input$selRCP)/10.0, ".")
    }
    # rename
    names(projTimeThr) <- paste0(curModel(), '_thresh_', predType)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projTimeThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projTimeThr, predType)
    
    # METADATA ####
    projYr <- paste0('20', input$selTime)
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <- printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <- printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- projYr
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- projYr
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <- paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <- printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <- "project to user-selected new time"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"
    spp[[curSp()]]$rmm$data$transfer$environment1$notes <- paste("projection to year", projYr, "for GCM", 
                                  GCMlookup[input$selGCM], "under RCP", 
                                  as.numeric(input$selRCP)/10.0)
    
    spp[[curSp()]]$rmm$output$transfer$environment1$units <- ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$output$transfer$environment1$minVal <- printVecAsis(raster::cellStats(projTimeThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$output$transfer$environment1$maxVal <- printVecAsis(raster::cellStats(projTimeThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$transfer$environment1$thresholdRule <- input$threshold
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
  })
}

projectTime_MAP <- function(map, session) {
  updateTabsetPanel(session, 'main', selected = 'Map')
  req(spp[[curSp()]]$polyPjXY, spp[[curSp()]]$project)
  polyPjXY <- spp[[curSp()]]$polyPjXY
  mapProjVals <- spp[[curSp()]]$project$mapProjVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if no threshold specified
  if(rmm()$output$transfer$environment1$thresholdRule != 'none') {
    rasPal <- c('gray', 'blue')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", colors = c('gray', 'blue'), title = "Thresholded Suitability<br>(Projected)",
                labels = c("predicted absence", "predicted presence"), opacity = 1, layerId = 'proj')
  } else {
    # if threshold specified
    legendPal <- colorNumeric(rev(rasCols), mapProjVals, na.color='transparent')
    rasPal <- colorNumeric(rasCols, mapProjVals, na.color='transparent')
    map %>% removeControl("proj") %>%
      addLegend("bottomright", pal = legendPal, title = "Predicted Suitability<br>(Projected)",
                values = mapProjVals, layerId = 'proj', labFormat = reverseLabels(2, reverse_order=TRUE))
    
  }
  # map model prediction raster and projection polygon
  sharedExt <- rbind(polyPjXY, occs()[c("longitude", "latitude")])
  map %>% clearMarkers() %>% clearShapes() %>% removeImage('projRas') %>%
    map_occs(occs(), customZoom = sharedExt) %>%
    addRasterImage(mapProj(), colors = rasPal, opacity = 0.7,
                   layerId = 'projRas', group = 'proj', method = "ngb") %>%
    addPolygons(lng=polyPjXY[,1], lat=polyPjXY[,2], layerId="projExt", fill = FALSE,
                weight=4, color="blue", group='proj') %>%
    # add background polygon
    mapBgPolys(bgShpXY())
}

projectTime_INFO <- infoGenerator(modName = "Project to New Time", 
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Gonzalo Pinilla, Robert P. Anderson", 
                                  pkgName = "dismo")
