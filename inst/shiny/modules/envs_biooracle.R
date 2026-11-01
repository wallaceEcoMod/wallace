# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# envs_biooracle.R
# File author: Wallace EcoMod Dev Team. 2026.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
envs_biooracle_module_ui <- function(id) {
  ns <- NS(id)
  bo_layers <- envs_biooracle_layers()
  bo_choices <- list(
    "Ocean (surface or benthic)" = setNames(
      as.list(bo_layers$var[bo_layers$type == "ocean"]),
      paste0(bo_layers$label[bo_layers$type == "ocean"], " (",
             bo_layers$var[bo_layers$type == "ocean"], ")")),
    "Terrain (static)" = setNames(
      as.list(bo_layers$var[bo_layers$type == "terrain"]),
      paste0(bo_layers$label[bo_layers$type == "terrain"], " (",
             bo_layers$var[bo_layers$type == "terrain"], ")")))
  tagList(
    tags$div(
      title = paste0('Bio-ORACLE v3 native resolution is 0.05 degree (3 arcmin, ',
                     '~5.5 km at the equator). Coarser grids are sampled ',
                     'server-side (every n-th cell) to keep downloads small.'),
      selectInput(ns("boRes"),
                  label = "Select Bio-ORACLE resolution (decimal degrees)",
                  choices = list("Select resolution" = "",
                                 "0.05 deg (3 arcmin, native)" = 0.05,
                                 "0.1 deg (6 arcmin)" = 0.1,
                                 "0.25 deg (15 arcmin)" = 0.25,
                                 "0.5 deg (30 arcmin)" = 0.5,
                                 "1 deg (60 arcmin)" = 1),
                  selected = 0.25)),
    shinyWidgets::pickerInput(
      ns("boVars"),
      label = "Select Bio-ORACLE variables",
      choices = bo_choices,
      multiple = TRUE,
      selected = c("thetao", "so", "bathymetry_mean"),
      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
    tags$div(
      title = paste0('Surface = sea surface layer. Benthic layers give the value ',
                     'at the mean, minimum or maximum depth of each cell. ',
                     'Terrain and surface-only variables ignore this choice.'),
      selectInput(ns("boDepth"), label = "Select depth layer",
                  choices = list("Sea surface" = "surf",
                                 "Benthic (mean depth)" = "mean",
                                 "Benthic (min depth)" = "min",
                                 "Benthic (max depth)" = "max"),
                  selected = "surf")),
    tags$div(
      title = paste0('Long-term statistic over the 2000-2020 baseline decades. ',
                     'Terrain variables ignore this choice.'),
      selectInput(ns("boStat"), label = "Select statistic",
                  choices = list("Mean" = "mean", "Minimum" = "min",
                                 "Maximum" = "max", "Range" = "range",
                                 "Long-term minimum" = "ltmin",
                                 "Long-term maximum" = "ltmax"),
                  selected = "mean")),
    checkboxInput(
      ns("doBrick"),
      label = "Save to memory for faster processing and save/load option",
      value = FALSE),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE)
    ),
    br(),
    actionButton(ns("goEnvData"), "Load Env Data")
  )
}

envs_biooracle_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  envs.global <- common$envs.global

  observeEvent(input$goEnvData, {
    # ERRORS ####
    if (is.null(curSp())) {
      logger %>% writeLog(type = 'error',
      "Before obtaining environmental variables, obtain occurrence data in 'Occ Data' component.")
      return()
    }
    if (length(input$boVars) < 2) {
      logger %>% writeLog(type = 'error', "Select more than two variables.")
      return()
    }

    # loop over all species if batch is on
    if (input$batch == FALSE) spLoop <- curSp() else spLoop <- allSp()

    # PROCESSING ####
    for (sp in spLoop) {
      # FUNCTION CALL ####
      bo <- envs_biooracle(boVars = input$boVars, boDepth = input$boDepth,
                           boStat = input$boStat, boRes = input$boRes,
                           doBrick = input$doBrick, logger = logger)
      req(bo)
      envs.global[[paste0("bo_", sp)]] <- bo

      # get environmental variable values per occurrence record
      withProgress(message = paste0("Extracting environmental values for occurrences of ",
                                    spName(sp), "..."), {
        occs.xy <- spp[[sp]]$occs[, c('longitude', 'latitude')]
        occsEnvsVals <- as.data.frame(raster::extract(bo, occs.xy, cellnumbers = TRUE))
      })

      # remove occurrence records with NA environmental values (on land)
      remOccs <- remEnvsValsNA(spp[[sp]]$occs, occsEnvsVals, sp, logger)
      if (!is.null(remOccs)) {
        spp[[sp]]$occs <- remOccs$occs
        occsEnvsVals <- remOccs$occsEnvsVals
      } else {
        # When remOccs is null, means that all localities have NAs
        return()
      }

      logger %>% writeLog(hlSpp(sp), "Bio-ORACLE variables ready to use.")

      # LOAD INTO SPP ####
      # add reference to Bio-ORACLE data
      spp[[sp]]$envs <- paste0("bo_", sp)

      # add columns for env variable values for each occurrence record
      if (!any(names(occsEnvsVals) %in% names(spp[[sp]]$occs))) {
        spp[[sp]]$occs <- cbind(spp[[sp]]$occs, occsEnvsVals)
      } else {
        shaEnvNames <- names(occsEnvsVals)[names(occsEnvsVals) %in% names(spp[[sp]]$occs)]
        spp[[sp]]$occs <- spp[[sp]]$occs %>% dplyr::mutate(occsEnvsVals[shaEnvNames])
      }

      # METADATA ####
      spp[[sp]]$rmm$data$environment$variableNames <- names(bo)
      spp[[sp]]$rmm$data$environment$yearMin <- 2000
      spp[[sp]]$rmm$data$environment$yearMax <- 2020
      spp[[sp]]$rmm$data$environment$resolution <- paste(round(raster::res(bo)[1] * 60, digits = 2), "minutes")
      spp[[sp]]$rmm$data$environment$extent <- as.character(raster::extent(bo))
      spp[[sp]]$rmm$data$environment$sources <- 'Bio-ORACLE v3'
      spp[[sp]]$rmm$data$environment$projection <- as.character(raster::crs(bo))

      spp[[sp]]$rmm$code$wallace$boRes <- input$boRes
      spp[[sp]]$rmm$code$wallace$boVars <- input$boVars
      spp[[sp]]$rmm$code$wallace$boDepth <- input$boDepth
      spp[[sp]]$rmm$code$wallace$boStat <- input$boStat
      spp[[sp]]$rmm$code$wallace$boBrick <- input$doBrick
    }

    common$update_component(tab = "Results")
  })

  output$envsPrint <- renderPrint({
    req(curSp(), spp[[curSp()]]$envs)
    envs.global[[spp[[curSp()]]$envs]]
  })

  return(list(
    save = function() {
      list(
        boRes = input$boRes,
        boVars = input$boVars,
        boDepth = input$boDepth,
        boStat = input$boStat,
        boBrick = input$doBrick
      )
    },
    load = function(state) {
      updateSelectInput(session, "boRes", selected = state$boRes)
      shinyWidgets::updatePickerInput(session, "boVars", selected = state$boVars)
      updateSelectInput(session, "boDepth", selected = state$boDepth)
      updateSelectInput(session, "boStat", selected = state$boStat)
      updateCheckboxInput(session, "doBrick", value = state$boBrick)
    }
  ))
}

envs_biooracle_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  verbatimTextOutput(ns("envsPrint"))
}

envs_biooracle_module_map <- function(map, common) {
  # Map logic
  occs <- common$occs
  map %>% clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop)
}

envs_biooracle_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    envs_biooracle_knit = !is.null(species$rmm$code$wallace$boRes),
    boRes_rmd = species$rmm$code$wallace$boRes,
    boVars_rmd = printVecAsis(species$rmm$code$wallace$boVars),
    boDepth_rmd = species$rmm$code$wallace$boDepth,
    boStat_rmd = species$rmm$code$wallace$boStat,
    boBrick_rmd = species$rmm$code$wallace$boBrick
  )
}
