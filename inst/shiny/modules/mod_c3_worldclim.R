
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
                     checkboxGroupInput(ns("bcSel"), label = "Select",
                                        choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)), 
                                        inline=TRUE, selected = paste0('bio', 1:19)))
    
  )
}

wcBioclims_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    envs <- c3_worldclim(spp[[curSp()]]$occs, 
                         input$bcRes, 
                         input$bcSelChoice, 
                         input$bcSel, 
                         logs, shiny = TRUE)
    req(envs)
    # remove occurrences with NA values for variables
    withProgress(message = "Extracting values...", {
      occsEnvsVals <- raster::extract(envs, spp[[curSp()]]$occs[c('longitude', 'latitude')])
    })
    # remove occurrences with NA environmental values
    spp[[curSp()]]$occs <- remEnvsValsNA(spp[[curSp()]]$occs, 
                                         occsEnvsVals, 
                                         envs, logs)
    # now remove NA rows from occs.z
    occsEnvsVals <- na.exclude(spp[[curSp()]]$obtainEnvs$occs.envsVals)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$envs <- envs
    spp[[curSp()]]$obtainEnvs$occs.envsVals <- occsEnvsVals
    
    # METADATA ####
    spp[[curSp()]]$rmm$data$environment$variableNames <- names(envs)
    spp[[curSp()]]$rmm$data$environment$yearMin <- 1960
    spp[[curSp()]]$rmm$data$environment$yearMax <- 1990
    spp[[curSp()]]$rmm$data$environment$resolution <- paste(input$bcRes, 'arcmin')
    spp[[curSp()]]$rmm$data$environment$extent <- 'global'
    spp[[curSp()]]$rmm$data$environment$sources <- 'WorldClim'
    
    # RETURN ####
    # return(envs)
  })
}