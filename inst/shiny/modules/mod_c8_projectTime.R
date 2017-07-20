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
    # conditionalPanel("input.selTime == 50 || input.selTime == 70",
    selectInput(ns('selRCP'), label = "Select RCP",
                choices = list("Select RCP" = "",
                               '2.6' = 26,
                               '4.5' = 45,
                               '6.0' = 60,
                               '8.5' = 85))
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
    selectInput("selGCM", label = "Select global circulation model", choices = gcms)
  })
  
  reactive({
    req(rvs$envs, rvs$mods, rvs$predCur)
    
    if (is.null(rvs$polyXY)) {
      rvs %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    
    if (res(rvs$envs)[1] < 0.01) {
      rvs %>% writeLog(type = 'error', 'Project to New Time currently only available with resolutions >30 arc seconds.')
      return()
    }
    
    # code taken from dismo getData() function to catch if user is trying to download a missing combo of gcm / rcp
    gcms <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 
              'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
    rcps <- c(26, 45, 60, 85)
    m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,
                  0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
    i <- m[which(selGCM == gcms), which(selRCP == rcps)]
    if (!i) {
      writeLog('<font color="red"><b>! ERROR</b></font> : This combination of GCM and RCP is not available. Please make a different selection.')
      return()
    }
    
    withProgress(message = paste("Retrieving WorldClim data for", selTime, selRCP, "..."), {
      values$projTimeVars <- raster::getData('CMIP5', var = "bio", res = bcRes,
                                             rcp = selRCP, model = selGCM, year = selTime)
    })
    
    withProgress(message = "Clipping environmental data to current extent...", {
      msk <- raster::crop(values$projTimeVars, values$poly2)
      values$projMsk <- raster::mask(msk, values$poly2)
      names(values$projMsk) <- names(values$preds)  # make names same as original predictors
    })
    
    withProgress(message = ("Projecting to new time..."), {
      curMod <- values$evalMods[[as.numeric(modelSel)]]
      values$rasName <- names(values$evalPreds[[as.numeric(modelSel)]])
      values$pjTime <- dismo::predict(curMod, values$projMsk)
      rasVals <- raster::values(values$pjTime)
      values$projTimeMessage <- paste0(paste0('20', selTime), " for GCM ", GCMlookup[selGCM], " under RCP ", as.numeric(selRCP)/10.0, ".")
      writeLog(paste("> Projected to", values$projTimeMessage))
    })
    
    if (predForm == 'log' & enmSel == "Maxent") {
      rasVals <- c(values$pjTime@data@values, 0, 1)  # set to 0-1 scale
    }
    values$rasValsTime <- rasVals[!is.na(rasVals)]
    rng <- c(min(values$rasValsTime), max(values$rasValsTime))
    
    values$legPalTime <- colorNumeric(rev(rasCols), rng, na.color='transparent')
    values$rasPalTime <- colorNumeric(rasCols, rng, na.color='transparent')
    
    
  })
}
