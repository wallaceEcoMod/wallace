
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE)
  )
}

userEnvs_MOD <- function(input, output, session, rvs) {
  reactive({
    req(input$userEnvs)
    
    # record for RMD
    rvs$userEnvsPath <- input$userEnvs$datapath
    
    withProgress(message = "Reading in rasters...", {
      uenvs <- raster::stack(input$userEnvs$datapath)
      names(uenvs) <- input$userEnvs$name
    })
    
    rvs %>% writeLog("> Environmental predictors: User input.")
    
    return(uenvs)
  })
}