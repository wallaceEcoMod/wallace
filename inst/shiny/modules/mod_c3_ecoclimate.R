
ecoClimate_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Select AOGCM',
             selectInput(ns("bcAOGCM"), label = "Select the Atmospheric Oceanic General Circulation Model you want to use",
                         choices = list("Select AOGCMs" = "",
                                        "CCSM" = "CCSM",
                                        "CNRM"= "CNRM", 
                                        "MIROC"="MIROC", 
                                        "FGOALS"="FGOALS", 
                                        "GISS"="GISS", 
                                        "IPSL"="IPSL",
                                        "MRI"= "MRI", 
                                        "MPI"= "MPI"
                         ))),
    
    checkboxInput(ns("ecoClimSelChoice"), label = "Specify variables to use in analysis?"),
    conditionalPanel(paste0("input['", ns("ecoClimSelChoice"), "']"),
                     checkboxGroupInput(ns("ecoClimSel"), label = "Select",
                                        choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)), 
                                        inline=TRUE, selected = paste0('bio', 1:19)))
    
  )
}

ecoClimate_MOD <- function(input, output, session, shinyLogs) {
  reactive({
    # ERRORS ####
    if (is.null(vals$occs)) {
      shinyLogs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                        obtain occurrence data in component 1.")
      return()
    }
    
    # FUNCTION CALL ####
    timeInterval <- spp[[n]]$rmm$data$occurrence$yearMax
    ecoClims <- c3_ecoClimate(input$bcAOGCM, timeInterval, input$ecoClimSel, shinyLogs)
    
    # PROCESSING ####
    for(sp in spIn) {
      # remove occurrences with NA values for variables
      withProgress(message = paste0("Extracting environmental values for occurrences of ", sp, "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(ecoClims, spp[[sp]]$occs[c('longitude', 'latitude')]))
      })
      # remove occurrences with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, shinyLogs)
      
      # LOAD INTO SPP ####
      spp[[sp]]$envs <- ecoClims
      # add columns for env variables beginning with "envs_" to occs tbl
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      
      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(ecoClims)
      spp[[sp]]$rmm$data$environment$yearMin <- timeInterval
      spp[[sp]]$rmm$data$environment$yearMax <- timeInterval
      spp[[sp]]$rmm$data$environment$resolution <- paste(round(raster::res(ecoClims)[1] * 60, digits = 2), "degrees")
      spp[[sp]]$rmm$data$environment$extent <- 'global'
      spp[[sp]]$rmm$data$environment$sources <- 'EcoClimate'
    }
  })
}

ecoclimate_MAP <- function(map, session) {
  map %>% clearAll() %>%     
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}

ecoclimate_INFO <- infoGenerator(modName = "ecoClimate",
                                 modAuts = "Sara Varela, Jamie M. Kass, Robert P. Anderson",
                                 pkgName = "raster")