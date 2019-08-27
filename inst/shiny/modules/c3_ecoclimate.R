c3_ecoclimate_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "Not implemented"
    # tags$div(title='Select AOGCM',
    #          selectInput(ns("bcAOGCM"), label = "Select the Atmospheric Oceanic General Circulation Model you want to use",
    #                      choices = list("Select AOGCMs" = "",
    #                                     "CCSM" = "CCSM",
    #                                     "CNRM"= "CNRM",
    #                                     "MIROC"="MIROC",
    #                                     "FGOALS"="FGOALS",
    #                                     "GISS"="GISS",
    #                                     "IPSL"="IPSL",
    #                                     "MRI"= "MRI",
    #                                     "MPI"= "MPI"
    #                      ))),
    #
    # checkboxInput(ns("ecoClimSelChoice"), label = "Specify variables to use in analysis?"),
    # conditionalPanel(paste0("input['", ns("ecoClimSelChoice"), "']"),
    #                  checkboxGroupInput(ns("ecoClimSel"), label = "Select",
    #                                     choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)),
    #                                     inline=TRUE, selected = paste0('bio', 1:19))),
    # strong("ecoClimate layers have a resolution of 0.5 degrees"),
    # actionButton(ns("goEcoClimData"), "Load Env Data")
  )
}

c3_ecoclimate_module_server <- function(input, output, session, common) {

  logger <- common$logger
  occs <- common$occs
  spp <- common$spp
  curSp <- common$curSp
  envs.global <- common$envs.global

  observeEvent(input$goEcoClimData, {
    # ERRORS ####
    if (is.null(occs())) {
      logger %>% writeLog(type = 'error', "Before obtaining environmental variables,
                             obtain occurrence data in component 1.")
      return()
    }

    # FUNCTION CALL ####
    timeInterval <- spp[[n]]$rmm$data$occurrence$yearMax
    ecoClims <- c3_ecoClimate(input$bcAOGCM, timeInterval, input$ecoClimSel, logger)

    # PROCESSING ####
    for(sp in list(curSp())) {
      # remove occurrences with NA values for variables
      withProgress(message = paste0("Extracting environmental values for occurrences of ", sp, "..."), {
        occsEnvsVals <- as.data.frame(raster::extract(ecoClims, spp[[sp]]$occs[c('longitude', 'latitude')]))
      })
      # remove occurrences with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, logger)

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

    common$update_component(tab = "Results")
  })

  output$envsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$envs)
    envs.global[[spp[[curSp()]]$envs]]
  })

}

c3_ecoclimate_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("envsPrint"))
}

c3_ecoclimate_module_map <- function(map, common) {
  occs <- common$occs
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}
