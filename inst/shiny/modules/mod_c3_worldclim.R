
wcBioclims_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("bcRes"), label = "Select WorldClim bioclimatic variable resolution",
                choices = list("Select resolution" = "",
                               "30 arcsec" = 0.5,
                               "2.5 arcmin" = 2.5,
                               "5 arcmin" = 5,
                               "10 arcmin" = 10),
                selected=10),
    shinyBS::bsPopover(ns('bcRes'), title = 'Tip',
                       'Approximate lengths at equator: 10 arcmin = ~20 km, 
                       5 arcmin = ~10 km, 2.5 arcmin = ~5 km, 30 arcsec = ~1 km. 
                       Exact length varies based on latitudinal position.',
                       placement = 'right', options = list(container = "body"))
  )
}

wcBioclims_MOD <- function(input, output, session, logs, mapCntr, envs) {
  reactive({
    req(input$bcRes)
    withProgress(message = "Retrieving WorldClim data...", {
      if (input$bcRes == 0.5) {
        wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes, 
                                lon = mapCntr()[1], lat = mapCntr()[2])
      } else {
        wcbc <- raster::getData(name = "worldclim", var = "bio", res = input$bcRes)
      }
    })
    
    logs %>% writeLog("Environmental predictors: WorldClim bio1-19 at", 
                      input$bcRes, " arcmin resolution.")
    
    return(wcbc)
  })
}