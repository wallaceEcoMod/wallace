
wcBioclims_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Approximate lengths at equator: 10 arcmin = ~20 km, 5 arcmin = ~10 km, 2.5 arcmin = ~5 km, 30 arcsec = ~1 km. Exact length varies based on latitudinal position.',
             selectInput(ns("bcRes"), label = "Select WorldClim bioclimatic variable resolution",
                choices = list("Select resolution" = "",
                               "30 arcsec" = 0.5,
                               "2.5 arcmin" = 2.5,
                               "5 arcmin" = 5,
                               "10 arcmin" = 10))),
    checkboxInput(ns("bcSelChoice"), label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("bcSelChoice"), "']"),
                     checkboxGroupInput(ns("bcSels"), label = "Select",
                                        choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)), 
                                        inline=TRUE, selected = paste0('bio', 1:19)))
    
  )
}

wcBioclims_MOD <- function(input, output, session, logs, mapCntr, envs) {
  reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                       obtain occurrence data in component 1.")
      return()
    }
    if (input$bcRes == '') {
      rvs %>% writeLog(type = 'error', 'Select a raster resolution.')
      return()
    }
    
    # record for RMD
    rvs$bcRes <- input$bcRes
    rvs$bcSels <- input$bcSels
    
    withProgress(message = "Retrieving WorldClim data...", {
      if (input$bcRes == 0.5) {
        wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes, 
                                lon = mapCntr()[1], lat = mapCntr()[2])
        rvs$bcLon <- mapCntr()[1]
        rvs$bcLat <- mapCntr()[2]
      } else {
        wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes)
        wcbc <- wcbc[[input$bcSels]]
      }
    })
    
    if (raster::nlayers(wcbc) == 19) {
      bcSels <- 'bio1-19'
    } else {
      bcSels <- paste(names(wcbc), collapse = ", ")
    }
    logs %>% writeLog("Environmental predictors: WorldClim bioclimatic variables",
                      bcSels, "at", input$bcRes, " arcmin resolution.")
    
    # change names if bio01 is bio1, and so forth
    i <- grep('bio[0-9]$', names(wcbc))
    editNames <- paste('bio', sapply(strsplit(names(wcbc)[i], 'bio'), function(x) x[2]), sep='0')
    names(wcbc)[i] <- editNames
    rvs$bcSels[i] <- editNames
    
    return(wcbc)
  })
}