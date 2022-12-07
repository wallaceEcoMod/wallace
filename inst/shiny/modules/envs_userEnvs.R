envs_userEnvs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    checkboxInput(
      ns("doBrick"),
      label = "Save to memory for faster processing and save/load option",
      value = FALSE), # Check default (value = FALSE)
    fileInput(ns("userEnvs"), label = "Input rasters",
              accept = c(".tif", ".asc"), multiple = TRUE),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
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
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error', "Before obtaining environmental variables,
                             obtain occurrence data in 'Occ Data' component.")
      return()
    }
    if (is.null(input$userEnvs)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }
    # Specify more than 2 variables
    if (length(input$userEnvs$name) < 2) {
      logger %>%
        writeLog(
          type = 'error',
          "Select more than two variables.")
      return()
    }

    userEnvs <- envs_userEnvs(rasPath = input$userEnvs$datapath,
                              rasName = input$userEnvs$name,
                              doBrick = input$doBrick,
                              logger)

    smartProgress(logger, message = "Checking NA values...", {
      checkNA <- terra::global(terra::rast(userEnvs),
                               fun = "isNA")
    })

    if (length(unique(checkNA$isNA)) != 1) {
      logger %>% writeLog(
        type = "error",
        'Input rasters have unmatching NAs pixel values.')
      return()
    }

    # loop over all species if batch is on
    if (input$batch == TRUE) {
      spLoop <- allSp()
      envs.global[["user"]] <- userEnvs
    } else {
      spLoop <- curSp()
      envs.global[[paste0("user_", curSp())]] <- userEnvs
    }

    for (sp in spLoop) {
      # get environmental variable values per occurrence record
      withProgress(
        message = paste0("Extracting environmental values for occurrences of ",
                         sp, "..."), {
        occsEnvsVals <-
          as.data.frame(
            raster::extract(userEnvs,
                            spp[[sp]]$occs[, c('longitude', 'latitude')],
                            cellnumbers = TRUE))
      })

      # remove occurrence records with NA environmental values
      remOccs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, sp, logger)
      if (!is.null(remOccs)) {
        spp[[sp]]$occs <- remOccs$occs
        occsEnvsVals <- remOccs$occsEnvsVals
      } else {
        # When remOccs is null, means that all localities have NAs
        return()
      }

      logger %>% writeLog(hlSpp(sp), "User specified variables (",
                          paste(names(userEnvs), collapse = ", "),
                          ") ready to use.")


      # LOAD INTO SPP ####
      if (input$batch == TRUE) {
        spp[[sp]]$envs <- "user"
      } else {
        spp[[sp]]$envs <- paste0("user_", sp)
      }
      # add columns for env variable values for each occurrence record
      if (!any(names(occsEnvsVals) %in% names(spp[[sp]]$occs))) {
        spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      } else {
        shaEnvNames <- names(occsEnvsVals)[names(occsEnvsVals) %in% names(spp[[sp]]$occs)]
        spp[[sp]]$occs <- spp[[sp]]$occs %>% dplyr::mutate(occsEnvsVals[shaEnvNames])
      }

      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(userEnvs)
      spp[[sp]]$rmm$data$environment$resolution <- raster::res(userEnvs)
      spp[[sp]]$rmm$data$environment$sources <- 'user'
      spp[[sp]]$rmm$data$environment$extent <- as.character(raster::extent(userEnvs))
      spp[[sp]]$rmm$data$environment$projection <- as.character(raster::crs(userEnvs))

      spp[[sp]]$rmm$code$wallace$userRasName <- input$userEnvs$name
      spp[[sp]]$rmm$code$wallace$userBrick <- input$doBrick
    }

    common$update_component(tab = "Results")
    common$disable_module(component = "xfer", module = "xferTime")
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

envs_userEnvs_module_map <- function(map, common) {
  occs <- common$occs
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}

envs_userEnvs_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    envs_userEnvs_knit = !is.null(species$rmm$code$wallace$userRasName),
    userRasName_rmd = printVecAsis(species$rmm$code$wallace$userRasName),
    userBrick_rmd = species$rmm$code$wallace$userBrick
  )
}
