
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE)
  )
}

userEnvs_MOD <- function(input, output, session, logs, envs) {
  reactive({
      withProgress(message = "Reading in rasters...", {
        uenvs <- raster::stack(input$userEnvs$datapath)
        names(uenvs) <- input$userEnvs$name
      })
    
    logs %>% writeLog("> Environmental predictors: User input.")
    
    return(uenvs)
  })
}