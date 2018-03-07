
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE)
  )
}

userEnvs_MOD <- function(input, output, session) {
  reactive({
    if (is.null(spp[[curSp()]]$occs)) {
      logs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                       obtain occurrence data in component 1.")
      return()
    }
    if (is.null(input$userEnvs)) {
      logs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    
    # record for RMD
    spp[[curSp()]]$rmm$code$wallaceSettings$userEnvs <- input$userEnvs
    
    withProgress(message = "Reading in rasters...", {
      uenvs <- raster::stack(input$userEnvs$datapath)
      names(uenvs) <- fileNameNoExt(input$userEnvs$name)
    })
    
    logs %>% writeLog("Environmental predictors: User input.")
    
    if (is.na(raster::crs(uenvs))) {
      logs %>% writeLog(type = "warning", "Input rasters have undefined coordinate 
                       reference system (CRS). Mapping functionality in components 
                       Visualize Model Results and Project Model will not work. If 
                       you wish to map rasters in these components, please define 
                       their projections and upload again. See guidance text in 
                       this module for more details.")
    }
    
    return(uenvs)
  })
}