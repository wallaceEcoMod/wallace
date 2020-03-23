envs_userEnvs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(ns("userEnvs"), label = "Input rasters", multiple = TRUE),
    checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE),
    actionButton(ns('goUserEnvs'), 'Load Env Data')
  )
}

envs_userEnvs_module_server <- function(input, output, session, common) {

  logger <- common$logger
  occs <- common$occs
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  envs.global <- common$envs.global

  observeEvent(input$goUserEnvs, {
    # ERRORS ####
    if (is.null(occs())) {
      logger %>% writeLog(type = 'error', "Before obtaining environmental variables,
                             obtain occurrence data in component 1.")
      return()
    }
    if (is.null(input$userEnvs)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }

    userEnvs <- envs_userEnvs(rasPath = input$userEnvs$datapath,
                              rasName = input$userEnvs$name)

    envs.global[["userEnvs"]] <- userEnvs

    # loop over all species if batch is on
    if (input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for (sp in spLoop) {
      # get environmental variable values per occurrence record
      withProgress(
        message = paste0("Extracting environmental values for occurrences of ",
                         sp, "..."), {
        occsEnvsVals <-
          as.data.frame(raster::extract(userEnvs,
                                        spp[[sp]]$occs[c('longitude', 'latitude')]))
      })
      # remove occurrence records with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, logger)
      # also remove variable value rows with NA environmental values
      occsEnvsVals <- na.omit(occsEnvsVals)

      # LOAD INTO SPP ####
      spp[[sp]]$envs <- "userEnvs"
      # add columns for env variable values for each occurrence record
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)

      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(userEnvs)
      spp[[sp]]$rmm$data$environment$resolution <- raster::res(userEnvs)
      spp[[sp]]$rmm$data$environment$sources <- 'user'

      spp[[sp]]$rmm$wallaceSettings$userRasName <- input$userEnvs$name
    }

    common$update_component(tab = "Results")
    common$remove_module(component = "proj", module = "projTime")
    common$remove_module(component = "proj", module = "projUser")
  })

  output$envsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$envs)
    envs.global[[spp[[curSp()]]$envs]]
  })

}

envs_userEnvs_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("envsPrint"))
}

# envs_userEnvs_module_map <- function(map, common) {
#   occs <- common$occs
#   map %>% clearAll() %>%
#     addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
#                      radius = 5, color = 'red', fill = TRUE, fillColor = "red",
#                      fillOpacity = 0.2, weight = 2, popup = ~pop)
# }
