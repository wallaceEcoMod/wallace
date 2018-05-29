projectTime_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # aliases are different for ecoclimate temporal scenarios. e.g. "lgm" is "LGM". 
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
    threshPred_UI(ns('threshPred'))
  )
}

projectTime_MOD <- function(input, output, session) {
  
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
    
    # pjTimePar <- list(rcp = input$selRCP, gcm = input$selGCM, year = input$selTime)
    
    if (is.null(spp[[curSp()]]$polyPjXY)) {
      shinyLogs %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    envsRes <- raster::res(spp[[curSp()]]$envs)[1]
    if (envsRes < 0.01) {
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
    
    smartProgress(shinyLogs, message = paste("Retrieving WorldClim data for", input$selTime, input$selRCP, "..."), {
      projTimeEnvs <- raster::getData('CMIP5', var = "bio", res = envsRes * 60,
                                      rcp = input$selRCP, model = input$selGCM, year = input$selTime)
      names(projTimeEnvs) <- paste0('bio', c(paste0('0',1:9), 10:19))
      # in case user subsetted bioclims
      projTimeEnvs <- projTimeEnvs[[names(envs())]]
    })
    
    outputType <- spp[[curSp()]]$rmm$output$prediction$notes
    
    projTime <- c8_projectTime(curModel(),
                               projTimeEnvs,
                               outputType,
                               spp[[curSp()]]$polyPjXY,
                               spp[[curSp()]]$polyPjID)
    
    projTime.thr.call <- callModule(threshPred_MOD, "threshPred", projTime)
    projTime.thr <- projTime.thr.call()
    
    shinyLogs %>% writeLog("Projected to", paste0('20', input$selTime), 
                           "for GCM", GCMlookup[input$selGCM], 
                           "under RCP", as.numeric(input$selRCP)/10.0, ".")
    # save to spp
    spp[[curSp()]]$project$projTime <- projTime.thr
    spp[[curSp()]]$project$projTimeVals <- getVals(projTime.thr, outputType)
    
    # METADATA
    spp[[curSp()]]$rmm$output$transfer <- NULL
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
    spp[[curSp()]]$rmm$output$project$thresholdRule <- input$threshPred
  })
}

projectTime_INFO <- infoGenerator(modName = "Project to New Time", 
                                  modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson", 
                                  pkgName = "dismo")
