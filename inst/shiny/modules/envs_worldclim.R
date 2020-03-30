envs_worldclim_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Approximate lengths at equator: 10 arcmin = ~20 km, 5 arcmin = ~10 km, 2.5 arcmin = ~5 km, 30 arcsec = ~1 km. Exact length varies based on latitudinal position.',
             selectInput(ns("wcRes"), label = "Select WorldClim bioclimatic variable resolution",
                         choices = list("Select resolution" = "",
                                        "30 arcsec" = 0.5,
                                        "2.5 arcmin" = 2.5,
                                        "5 arcmin" = 5,
                                        "10 arcmin" = 10), selected = 10)), # Check default (No selected parameter)
    checkboxInput(ns("doBrick"), label = "Save to memory for faster processing?", value = T), # Check default (value = FALSE)
    checkboxInput(ns("bcSelChoice"), label = "Specify variables to use in analysis?",
                  value = TRUE),
    conditionalPanel(paste0("input['", ns("bcSelChoice"), "']"),
                     shinyWidgets::pickerInput(
                       "bcSel",
                       label = "Select bioclim variables (**)",
                       choices = setNames(as.list(paste0('bio', 1:19)), paste0('bio', 1:19)),
                       multiple = TRUE,
                       selected = paste0('bio', 1:19),
                       options = list(`actions-box` = TRUE))),
    checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE), # Check default (value = FALSE)
    strong("Using map center coordinates as reference for tile download."),
    textOutput(ns("ctrLatLon")), br(),
    actionButton(ns("goEnvData"), "Load Env Data")
  )
}

envs_worldclim_module_server <- function(input, output, session, common) {

  logger <- common$logger
  bcSel <- common$bcSel
  occs <- common$occs
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  envs.global <- common$envs.global
  mapCntr <- common$mapCntr

  observeEvent(input$goEnvData, {
    # ERRORS ####
    if (is.null(occs())) {
      logger %>% writeLog(type = 'error', "Before obtaining environmental variables,
                             obtain occurrence data in component 1.")
      return()
    }

    # FUNCTION CALL ####
    wcbc <- envs_worldclim(input$wcRes, bcSel(), mapCntr(), input$doBrick, logger)
    req(wcbc)

    envs.global[["wcbc"]] <- wcbc

    # loop over all species if batch is on
    if (input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for (sp in spLoop) {
      # get environmental variable values per occurrence record
      withProgress(message = paste0("Extracting environmental values for occurrences of ", spName(sp), "..."), {
        occs.xy <- spp[[sp]]$occs[c('longitude', 'latitude')]
        occsEnvsVals <- as.data.frame(raster::extract(wcbc, occs.xy))
      })
      # remove occurrence records with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, logger)
      # also remove variable value rows with NA environmental values
      occsEnvsVals <- na.omit(occsEnvsVals)

      # LOAD INTO SPP ####
      # add reference to WorldClim bioclim data
      spp[[sp]]$envs <- "wcbc"
      # add columns for env variable values for each occurrence record
      spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)

      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(wcbc)
      spp[[sp]]$rmm$data$environment$yearMin <- 1960
      spp[[sp]]$rmm$data$environment$yearMax <- 1990
      spp[[sp]]$rmm$data$environment$resolution <- paste(round(raster::res(wcbc)[1] * 60, digits = 2), "degrees")
      spp[[sp]]$rmm$data$environment$extent <- 'global'
      spp[[sp]]$rmm$data$environment$sources <- 'WorldClim 1.4'

      spp[[sp]]$rmm$wallaceSettings$wcRes <- input$wcRes
      spp[[sp]]$rmm$wallaceSettings$bcSel <- input$bcSel
      spp[[sp]]$rmm$wallaceSettings$mapCntr <- mapCntr()
      spp[[sp]]$rmm$wallaceSettings$wcBrick <- input$doBrick
    }

    common$update_component(tab = "Results")
  })

  # text showing the current map center
  output$ctrLatLon <- renderText({
    glue::glue('Using map center {join(mapCntr())}')
  })

  output$envsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$envs)
    envs.global[[spp[[curSp()]]$envs]]
  })

  return(list(
    save = function() {
      list(
        wcRes = input$wcRes
      )
    },
    load = function(state) {
      updateSelectInput(session, "wcRes", selected = state$wcRes)
    }
  ))
}

envs_worldclim_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("envsPrint"))
}

envs_worldclim_module_map <- function(map, common) {
  # Map logic
  occs <- common$occs
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}

envs_worldclim_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    envs_worldclim_knit = !is.null(species$rmm$wallaceSettings$wcRes),
    wcRes = species$rmm$wallaceSettings$wcRes,
    bcSel = printVecAsis(species$rmm$wallaceSettings$bcSel),
    mapCntr = printVecAsis(species$rmm$wallaceSettings$mapCntr),
    wcBrick = species$rmm$wallaceSettings$wcBrick
  )
}

