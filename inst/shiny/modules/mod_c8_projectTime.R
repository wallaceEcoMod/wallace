projectTime_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selTime"), label = "Select time period",
                choices = list("Select period" = "",
                               # "Last Glacial Maximum (~22,000 years ago)" = 'lgm',
                               # "Mid Holocene (~7000 years ago)" = 'mid',
                               "2050" = 50,
                               "2070" = 70)),
    uiOutput(ns('selGCMui')),
    selectInput(ns('selRCP'), label = "Select RCP",
                choices = list("Select RCP" = "",
                               '2.6' = 26,
                               '4.5' = 45,
                               '6.0' = 60,
                               '8.5' = 85)),
    threshPred_UI(ns('threshPred'))
  )
}

projectTime_MOD <- function(input, output, session, rvs) {
  
  output$selGCMui <- renderUI({
    ns <- session$ns
    GCMlookup <- c(AC="ACCESS1-0", BC="BCC-CSM1-1", CC="CCSM4", CE="CESM1-CAM5-1-FV2",
                   CN="CNRM-CM5", GF="GFDL-CM3", GD="GFDL-ESM2G", GS="GISS-E2-R",
                   HD="HadGEM2-AO", HG="HadGEM2-CC", HE="HadGEM2-ES", IN="INMCM4",
                   IP="IPSL-CM5A-LR", ME="MPI-ESM-P", MI="MIROC-ESM-CHEM", MR="MIROC-ESM",
                   MC="MIROC5", MP="MPI-ESM-LR", MG="MRI-CGCM3", NO="NorESM1-M")
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
    if (is.null(rvs$predCur)) {
      rvs %>% writeLog(type = 'error', 'Calculate a model prediction in component 7 
                       before projecting.')
      return()
    }
    if (is.null(rvs$polyPjXY) | identical(rvs$polySelXY, rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', "The polygon has not been drawn and finished. 
                       Please use the draw toolbar on the left-hand of the map to complete
                       the polygon.")
      return()
    }
    
    # record for RMD
    rvs$pjTimePar <- list(rcp=input$selRCP, gcm=input$selGCM, year=input$selTime)
    
    if (is.null(rvs$polyPjXY)) {
      rvs %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    envsRes <- raster::res(rvs$envs)[1]
    if (envsRes < 0.01) {
      rvs %>% writeLog(type = 'error', 'Project to New Time currently only available with resolutions >30 arc seconds.')
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
      rvs %>% writeLog(type = 'error', 'This combination of GCM and RCP is not 
                       available. Please make a different selection.')
      return()
    }
    
    withProgress(message = paste("Retrieving WorldClim data for", input$selTime, input$selRCP, "..."), {
      projTimeEnvs <- raster::getData('CMIP5', var = "bio", res = envsRes * 60,
                                      rcp = input$selRCP, model = input$selGCM, year = input$selTime)
      names(projTimeEnvs) <- paste0('bio', c(paste0('0',1:9), 10:19))
      # in case user subsetted bioclims
      projTimeEnvs <- projTimeEnvs[[rvs$bcSels]]
    })
    
    # create new spatial polygon from coordinates
    newPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rvs$polyPjXY)), ID=rvs$polyPjID)))  
    
    # concatanate coords to a single character
    xy.round <- round(rvs$polyPjXY, digits = 2)
    coordsChar <- paste(apply(xy.round, 1, function(b) paste0('(',paste(b, collapse=', '),')')), collapse=', ')  
    rvs %>% writeLog('New time projection for model', rvs$modSel, 'with extent coordinates:', coordsChar)
    
    withProgress(message = "Clipping environmental data to current extent...", {
      pjtMsk <- raster::crop(projTimeEnvs, newPoly)
      pjtMsk <- raster::mask(pjtMsk, newPoly)
    })
    
    modCur <- rvs$mods[[rvs$modSel]]
    
    withProgress(message = ("Projecting to new time..."), {
      pargs <- paste0("outputformat=", rvs$comp7.type)
      modProjTime <- dismo::predict(modCur, pjtMsk, args = pargs)
      modProjTime.thr.call <- callModule(threshPred_MOD, "threshPred", modProjTime)
      modProjTime.thr <- modProjTime.thr.call()
      pjPred <- modProjTime.thr$pred
      rvs$comp8.thr <- modProjTime.thr$thresh
      rvs %>% writeLog("Projected to", paste0('20', input$selTime), 
                       "for GCM", GCMlookup[input$selGCM], 
                       "under RCP", as.numeric(input$selRCP)/10.0, ".")
    })
    
    return(list(pjMsk=pjtMsk, pjPred=pjPred))
  })
}
