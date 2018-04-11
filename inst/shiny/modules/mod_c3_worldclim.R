
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

wcBioclims_MOD <- function(input, output, session, spIn) {
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
    
    for(sp in spIn) {
      # remove occurrences with NA values for variables
      withProgress(message = paste0("Extracting environmental values for occurrences of ", sp, "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(envs, spp[[sp]]$occs[c('longitude', 'latitude')]))
        names(occsEnvsVals) <- paste0('env_', names(occsEnvsVals))
      })
      # remove occurrences with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, logs)
      
      # LOAD INTO SPP ####
      spp[[sp]]$envs <- envs
      # add columns for env variables beginning with "envs_" to occs tbl
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      
      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(envs)
      spp[[sp]]$rmm$data$environment$yearMin <- 1960
      spp[[sp]]$rmm$data$environment$yearMax <- 1990
      spp[[sp]]$rmm$data$environment$resolution <- raster::res(envs)
      spp[[sp]]$rmm$data$environment$extent <- 'global'
      spp[[sp]]$rmm$data$environment$sources <- 'WorldClim'
    }
  })
}