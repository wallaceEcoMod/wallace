envs_ecoclimate_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    tags$div(title = 'Select AOGCM',
             selectInput(ns("bcAOGCM"),
                         label = "Select the Atmospheric Oceanic General Circulation Model you want to use",
                         choices = list("Select AOGCMs" = "",
                                        "CCSM" = "CCSM",
                                        "CNRM" = "CNRM",
                                        "MIROC" = "MIROC",
                                        "FGOALS" = "FGOALS",
                                        "GISS" = "GISS",
                                        "IPSL" = "IPSL",
                                        "MRI" = "MRI",
                                        "MPI" = "MPI")
             )),
    tags$div(title = 'Select Scenario',
             selectInput(ns("bcScenario"),
                         label = "select the temporal scenario that you want to use",
                         choices = list("Select Scenario" = "",
                                        "Present" = "Present",
                                        "Holocene (6,000 years ago)" = "Holo",
                                        "LGM (21,000 years ago)" = "LGM")
             )),
    shinyWidgets::pickerInput(
      "ecoClimSel",
      label = "Select bioclimatic variables",
      choices = setNames(as.list(1:19),
                         paste0('bio', sprintf("%02d", 1:19))),
      multiple = TRUE,
      selected = 1:19,
      options = list(`actions-box` = TRUE)),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    em("ecoClimate layers have a resolution of 0.5 degrees"), br(), br(),
    actionButton(ns("goEcoClimData"), "Load Env Data")
  )
}

envs_ecoclimate_module_server <- function(input, output, session, common) {

  logger <- common$logger
  ecoClimSel <- common$ecoClimSel
  occs <- common$occs
  spp <- common$spp
  curSp <- common$curSp
  allSp <- common$allSp
  envs.global <- common$envs.global

  observeEvent(input$goEcoClimData, {
    # WARNING ####
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error',
                          paste0("Before obtaining environmental variables, ",
                                 "obtain occurrence data in 'Occ Data' component."))
      return()
    }
    # Specify more than 2 variables
    if (length(ecoClimSel()) < 2) {
      logger %>%
        writeLog(
          type = 'error',
          "Select more than two variables.")
      return()
    }

    # FUNCTION CALL ####
    ecoClims <- envs_ecoClimate(input$bcAOGCM, input$bcScenario,
                                as.numeric(ecoClimSel()), logger)
    req(ecoClims)
    nmEcoClimate <- paste0("ecoClimate_", input$bcAOGCM, "_", input$bcScenario)
    envs.global[[nmEcoClimate]] <- ecoClims
    # LOAD INTO SPP ####
    # loop over all species if batch is on
    if (input$batch == FALSE) spLoop <- curSp() else spLoop <- allSp()

    # PROCESSING ####
    for (sp in spLoop) {
      # remove occurrences with NA values for variables
      withProgress(
        message = paste0("Extracting environmental values for occurrences of ",
                         spName(sp), "..."), {
                           occsEnvsVals <- as.data.frame(
                             raster::extract(ecoClims,
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

      logger %>% writeLog(hlSpp(sp), "EcoClimate variables ready to use.")

      # LOAD INTO SPP ####
      spp[[sp]]$envs <- nmEcoClimate

      # add columns for env variable values for each occurrence record
      if (!any(names(occsEnvsVals) %in% names(spp[[sp]]$occs))) {
        spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      } else {
        shaEnvNames <- names(occsEnvsVals)[names(occsEnvsVals) %in% names(spp[[sp]]$occs)]
        spp[[sp]]$occs <- spp[[sp]]$occs %>% dplyr::mutate(occsEnvsVals[shaEnvNames])
      }


      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(ecoClims)
      spp[[sp]]$rmm$data$environment$resolution <- paste(round(raster::res(ecoClims)[1] * 60, digits = 2), "minutes")
      spp[[sp]]$rmm$data$environment$extent <- as.character(raster::extent(ecoClims))
      spp[[sp]]$rmm$data$environment$sources <- nmEcoClimate
      spp[[sp]]$rmm$data$environment$projection <- as.character(raster::crs(ecoClims))

      spp[[sp]]$rmm$code$wallace$bcAOGCM <- input$bcAOGCM
      spp[[sp]]$rmm$code$wallace$bcScenario <- input$bcScenario
      spp[[sp]]$rmm$code$wallace$ecoClimSel <- ecoClimSel()
    }
    common$update_component(tab = "Results")
  })

  output$envsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$envs)
    envs.global[[spp[[curSp()]]$envs]]
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      list(
        bcAOGCM = input$bcAOGCM,
        bcScenario = input$bcScenario,
        ecoClimSel = ecoClimSel()
      )
    },
    load = function(state) {
      # Load
      updateSelectInput(session, "bcAOGCM", selected = state$bcAOGCM)
      updateSelectInput(session, "bcScenario", selected = state$bcScenario)
      shinyWidgets::updatePickerInput(session, "ecoClimSel",
                                      selected = state$ecoClimSel)
    }
  ))
}

envs_ecoclimate_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("envsPrint"))
}

envs_ecoclimate_module_map <- function(map, common) {
  occs <- common$occs
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}

envs_ecoclimate_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    envs_ecoclimate_knit = !is.null(species$rmm$code$wallace$bcAOGCM),
    bcAOGCM_rmd = species$rmm$code$wallace$bcAOGCM,
    bcScenario_rmd = species$rmm$code$wallace$bcScenario,
    ecoClimSel_rmd =  printVecAsis(as.numeric(species$rmm$code$wallace$ecoClimSel))
    ##Alternative using rmm instead of RMD object but not working
    #grepl("ecoClimate",species$rmm$data$environment$sources)
  )
}



