
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
    envs <- c3_worldclim(spp[[curSp()]]$occs, input$bcRes, input$bcSelChoice, 
                         input$bcSel, logs, shiny=TRUE)
    # remove occurrences with NA values for variables
    spp[[curSp()]]$occs <- remEnvsValsNA(spp[[curSp()]]$occs, envs, logs)
    
    if (is.null(envs)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$envs <- envs
    
    # RMD VALUES ####
    spp[[curSp()]]$rmd$c3 <- list(type = 'worldclim_bioclims', envsRes = input$bcRes, envsSel = input$bcSel)
    
    # RETURN ####
    # output the species name
    return(envs)
  })
}