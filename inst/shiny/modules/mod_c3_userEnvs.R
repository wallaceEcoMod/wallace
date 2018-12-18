
userEnvs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
  )
}

userEnvs_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(occs())) {
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
    
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()
    
    for(sp in spLoop) {
      # get environmental variable values per occurrence record
      withProgress(message = paste0("Extracting environmental values for occurrences of ", sp, "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(userEnvs, spp[[sp]]$occs[c('longitude', 'latitude')]))
      })
      # remove occurrence records with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, shinyLogs)
      # also remove variable value rows with NA environmental values
      occsEnvsVals <- na.omit(occsEnvsVals)
      
      # LOAD INTO SPP ####
      spp[[sp]]$envs <- userEnvs
      # add columns for env variable values for each occurrence record
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      
      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(userEnvs)
      spp[[sp]]$rmm$data$environment$resolution <- raster::res(userEnvs)
      spp[[sp]]$rmm$data$environment$sources <- 'user'
    }
    
  })
}

userEnvs_MAP <- function(map, session) {
  map %>% clearAll() %>%     
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}

userEnvs_INFO <- infoGenerator(modName = "User-specified Environmental Data",
                               modAuts = "Jamie M. Kass, Robert P. Anderson",
                               pkgName = NULL)