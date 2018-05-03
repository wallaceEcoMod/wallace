
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE)
  )
}

userEnvs_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$occs)) {
      shinyLogs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                        obtain occurrence data in component 1.")
      return()
    }
    if (is.null(input$userEnvs)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    
      userEnvs <- c3_userEnvs(rasPath = input$userEnvs$datapath,
                              rasName = input$userEnvs$name)
      
      # remove occurrences with NA values for variables
      withProgress(message = paste0("Extracting environmental values for occurrences of ", spName(spp[[curSp()]]), "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(userEnvs, spp[[curSp()]]$occs[c('longitude', 'latitude')]))
        names(occsEnvsVals) <- paste0('env_', names(occsEnvsVals))
      })
      # remove occurrences with NA environmental values
      spp[[curSp()]]$occs <- remEnvsValsNA(spp[[curSp()]]$occs, occsEnvsVals, shinyLogs)
      
      # LOAD INTO SPP ####
      spp[[curSp()]]$envs <- userEnvs
      # add columns for env variables beginning with "envs_" to occs tbl
      spp[[curSp()]]$occs <- cbind(spp[[curSp()]]$occs, occsEnvsVals)
      
      # METADATA ####
      spp[[curSp()]]$rmm$data$environment$variableNames <- names(userEnvs)
      spp[[curSp()]]$rmm$data$environment$resolution <- raster::res(userEnvs)
      spp[[curSp()]]$rmm$data$environment$sources <- 'user'
  })
}

userEnvs_INFO <- infoGenerator(modName = "User-specified Environmental Data",
                               modAuts = "Jamie M. Kass, Robert P. Anderson",
                               pkgName = NULL)