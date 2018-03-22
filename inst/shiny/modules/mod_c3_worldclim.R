
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
    checkboxInput(ns("bcAllSp"), label = "Batch for all species?", value = TRUE),
    checkboxInput(ns("bcSelChoice"), label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("bcSelChoice"), "']"),
                     checkboxGroupInput(ns("bcSel"), label = "Select",
                                        choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)), 
                                        inline=TRUE, selected = paste0('bio', 1:19)))
    
  )
}

wcBioclims_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$occs)) {
      logs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                        obtain occurrence data in component 1.")
      return()
    }
    
    # FUNCTION CALL ####
    envs <- c3_worldclim(input$bcRes, 
                         input$bcSelChoice, 
                         input$bcSel, 
                         logs, shiny = TRUE)
    req(envs)
    
    if(input$bcAllSp == TRUE) {
      spVec <- allSp()
    }else{
      spVec <- curSp()
    }
    
    for(i in spVec) {
      # remove occurrences with NA values for variables
      withProgress(message = paste0("Extracting occurrence values for ", i, "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(envs, spp[[i]]$occs[c('longitude', 'latitude')]))
        names(occsEnvsVals) <- paste0('env_', names(occsEnvsVals))
      })
      # remove occurrences with NA environmental values
      spp[[i]]$occs <- remEnvsValsNA(spp[[i]]$occs, occsEnvsVals, logs)
      
      # LOAD INTO SPP ####
      spp[[i]]$envs <- envs
      # add columns for env variables beginning with "envs_" to occs tbl
      spp[[i]]$occs <- cbind(spp[[i]]$occs, occsEnvsVals)
      
      # METADATA ####
      spp[[i]]$rmm$data$environment$variableNames <- names(envs)
      spp[[i]]$rmm$data$environment$yearMin <- 1960
      spp[[i]]$rmm$data$environment$yearMax <- 1990
      spp[[i]]$rmm$data$environment$resolution <- paste(input$bcRes, 'arcmin')
      spp[[i]]$rmm$data$environment$extent <- 'global'
      spp[[i]]$rmm$data$environment$sources <- 'WorldClim'
    }
    
    # RETURN ####
    # return(envs)
  })
}