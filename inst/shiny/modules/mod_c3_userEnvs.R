
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE)
  )
}

userEnvs_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                       obtain occurrence data in component 1.")
      return()
    }
    if (is.null(input$userEnvs)) {
      rvs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    
    # record for RMD
    rvs$userEnvs <- input$userEnvs
    
    withProgress(message = "Reading in rasters...", {
      uenvs <- raster::stack(input$userEnvs$datapath)
      names(uenvs) <- fileNameNoExt(input$userEnvs$name)
    })
    
    rvs %>% writeLog("> Environmental predictors: User input.")
    
    return(uenvs)
  })
}