envs_worldclim_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      title = paste0('Approximate lengths at equator: 10 arcmin = ~20 km, ',
                     '5 arcmin = ~10 km, 2.5 arcmin = ~5 km, 30 arcsec = ~1 km. ',
                     'Exact length varies based on latitudinal position.'),
             selectInput(ns("wcRes"),
                         label = "Select WorldClim bioclimatic variable resolution",
                         choices = list("Select resolution" = "",
                                        "30 arcsec" = 0.5,
                                        "2.5 arcmin" = 2.5,
                                        "5 arcmin" = 5,
                                        "10 arcmin" = 10), selected = 10)), # Check default (No selected parameter)
    checkboxInput(
      ns("doBrick"),
      label = "Save to memory for faster processing and save/load option(**)",
      value = TRUE), # Check default (value = FALSE)
    shinyWidgets::pickerInput(
      "bcSel",
      label = "Select bioclim variables (**)",
      choices = setNames(as.list(paste0('bio', sprintf("%02d", 1:19))),
                         paste0('bio', sprintf("%02d", 1:19))),
      multiple = TRUE,
      selected = paste0('bio', sprintf("%02d", 1:19)),
      options = list(`actions-box` = TRUE)),
    conditionalPanel(
      sprintf("input['%s'] != '0.5'", ns("wcRes")),
      tags$div(
        title = "Add Batch guidance text here (**)",
        checkboxInput(ns("batch"), label = strong("Batch"), value = TRUE) # Check default (value = FALSE)
      )
    ),
    conditionalPanel(
      sprintf("input['%s'] == '0.5'", ns("wcRes")),
      em("Batch option not available for 30 arcsec resolution."), br(), br(),
      strong(
        paste0("Using map center coordinates as reference for tile download. ",
               "You can vizualize the tile in the bottomleft corner on ",
               "the map. All occurrence outside of the purple polygon will ",
               "be removed (**)")), br(), br(),
      textOutput(ns("ctrLatLon"))
    ),
    br(),
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
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error',
      "Before obtaining environmental variables, obtain occurrence data in 'Occ Data' component.")
      return()
    }

    if (input$wcRes != 0.5) {
      # FUNCTION CALL ####
      wcbc <- envs_worldclim(input$wcRes, bcSel(), doBrick = input$doBrick,
                             logger = logger)
      req(wcbc)
      envs.global[["wcbc"]] <- wcbc
    } else {
      wcbc <- envs_worldclim(input$wcRes, bcSel(), mapCntr(), input$doBrick, logger)
      req(wcbc)
      envs.global[[paste0("wcbc_", curSp())]] <- wcbc
    }

    # loop over all species if batch is on
    if (input$batch == FALSE | input$wcRes == 0.5) spLoop <- curSp() else spLoop <- allSp()

    # PROCESSING ####
    for (sp in spLoop) {
      # get environmental variable values per occurrence record
      withProgress(message = paste0("Extracting environmental values for occurrences of ", spName(sp), "..."), {
        occs.xy <- spp[[sp]]$occs[c('longitude', 'latitude')]
        occsEnvsVals <- as.data.frame(raster::extract(wcbc, occs.xy))
      })
      # remove occurrence records with NA environmental values
      spp[[sp]]$occs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, sp, logger)
      # also remove variable value rows with NA environmental values
      occsEnvsVals <- na.omit(occsEnvsVals)

      logger %>% writeLog(hlSpp(sp), "Worldclim variables ready to use. (**)")

      # LOAD INTO SPP ####
      # add reference to WorldClim bioclim data
      if (raster::res(wcbc)[1] > 0.009) {
        spp[[sp]]$envs <- "wcbc"
      } else {
        spp[[sp]]$envs <- paste0("wcbc_", sp)
      }

      # add columns for env variable values for each occurrence record
      if (!any(names(occsEnvsVals) %in% names(spp[[sp]]$occs))) {
        spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      } else {
        shaEnvNames <- names(occsEnvsVals)[names(occsEnvsVals) %in% names(spp[[sp]]$occs)]
        spp[[sp]]$occs <- spp[[sp]]$occs %>% dplyr::mutate(occsEnvsVals[shaEnvNames])
      }


      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(wcbc)
      spp[[sp]]$rmm$data$environment$yearMin <- 1960
      spp[[sp]]$rmm$data$environment$yearMax <- 1990
      spp[[sp]]$rmm$data$environment$resolution <- paste(round(raster::res(wcbc)[1] * 60, digits = 2), "minutes")
      spp[[sp]]$rmm$data$environment$extent <- as.character(raster::extent(wcbc))
      spp[[sp]]$rmm$data$environment$sources <- 'WorldClim 1.4'
      spp[[sp]]$rmm$data$environment$projection <- as.character(raster::crs(wcbc))

      spp[[sp]]$rmm$code$wallace$wcRes <- input$wcRes
      spp[[sp]]$rmm$code$wallace$bcSel <- bcSel()
      spp[[sp]]$rmm$code$wallace$mapCntr <- mapCntr()
      spp[[sp]]$rmm$code$wallace$wcBrick <- input$doBrick
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
        wcRes = input$wcRes,
        wcBrick = input$doBrick,
        bcSel = bcSel()
      )
    },
    load = function(state) {
      updateSelectInput(session, "wcRes", selected = state$wcRes)
      updateCheckboxInput(session, "doBrick", value = state$doBrick)
      shinyWidgets::updatePickerInput(session, "bcSel", selected = state$bcSel)
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
  mapCntr <- common$mapCntr
  lon_tile <- seq(-180, 180, 30)
  lat_tile <- seq(-60, 90, 30)
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    addRectangles(lng1 = lon_tile[sum(lon_tile <= mapCntr()[1])],
                  lng2 = lon_tile[sum(lon_tile <= mapCntr()[1])] + 30,
                  lat1 = lat_tile[sum(lat_tile <= mapCntr()[2])],
                  lat2 = lat_tile[sum(lat_tile <= mapCntr()[2])] + 30,
                  color = "purple", group = "30 arcsec tile") %>%
    hideGroup("30 arcsec tile") %>% zoom2Occs(occs()) %>%
    addLayersControl(overlayGroups = "30 arcsec tile", position = "bottomleft",
                     options = layersControlOptions(collapsed = FALSE))
}

envs_worldclim_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    envs_worldclim_knit = !is.null(species$rmm$code$wallace$wcRes),
    wcRes_rmd = species$rmm$code$wallace$wcRes,
    bcSel_rmd = printVecAsis(species$rmm$code$wallace$bcSel),
    mapCntr_rmd = printVecAsis(species$rmm$code$wallace$mapCntr),
    wcBrick_rmd = species$rmm$code$wallace$wcBrick
  )
}

