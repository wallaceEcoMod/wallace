# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# occs_userOccs.R
# File author: Wallace EcoMod Dev Team. 2023.
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
occs_userOccs_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV", accept = ".csv"),
    checkboxInput(
      ns("noCSV"), value = FALSE,
      label = "Do you want to define delimiter-separated and decimal values?"),
    conditionalPanel(
      sprintf("input['%s'] == 1", ns("noCSV")),
      textInput(ns("sepCSV"), label = "Define delimiter-separator", value = ","),
      textInput(ns("decCSV"), label = "Define decimal-separator", value = ".")
    ),
    actionButton(ns("goUserOccs"), "Load Occurrences")
  )
}

occs_userOccs_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp

  observeEvent(input$goUserOccs, {
    # FUNCTION CALL ####
    if (input$noCSV == 0 | is.null(input$noCSV)) {
      occsList <- occs_userOccs(input$userCSV$datapath, input$userCSV$name,
                                ",", ".", logger)
    } else {
      occsList <- occs_userOccs(input$userCSV$datapath, input$userCSV$name,
                                input$sepCSV, input$decCSV, logger)
    }


    if (is.null(occsList)) return()

    # LOAD INTO SPP ####
    # if species name is already in list, overwrite it
    for(sp in names(occsList)) {
      occs <- occsList[[sp]]$cleaned
      occsOrig <- occsList[[sp]]$orig
      if(!is.null(spp[[sp]])) spp[[sp]] <- NULL
      spp[[sp]] <- list(occs = occs,
                        occData = list(occsOrig = occsOrig, occsCleaned = occs),
                        rmm = rangeModelMetadata::rmmTemplate(),
                        rmd = list())
      if(!is.null(occsList[[sp]]$bg)) spp[[sp]]$bg <- occsList[[sp]]$bg

      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$taxon <- unique(occs$scientific_name)
      spp[[sp]]$rmm$data$occurrence$dataType <- "presence only"
      spp[[sp]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
      spp[[sp]]$rmm$data$occurrence$sources <- "user"
      spp[[sp]]$rmm$code$wallace$userCSV <- input$userCSV$name
      spp[[sp]]$rmm$code$wallace$occsNum <- nrow(occs)
      spp[[sp]]$rmm$code$wallace$occsRemoved <- nrow(occs) - nrow(occsOrig)
      spp[[sp]]$rmm$code$wallace$sepCSV <- ifelse(input$noCSV == 0 | is.null(input$noCSV),
                                     ",", input$sepCSV)
      spp[[sp]]$rmm$code$wallace$decCSV <- ifelse(input$noCSV == 0 | is.null(input$noCSV),
                                     ".", input$decCSV)

    }
    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        noCSV = input$noCSV,
        sepCSV = input$sepCSV,
        decCSV = input$decCSV
      )
    },
    load = function(state) {
      updateCheckboxInput(session, "noCSV", value = state$noCSV)
      updateTextInput(session, "sepCSV", value = state$sepCSV)
      updateTextInput(session, "decCSV", value = state$decCSV)
    }
  ))
}

occs_userOccs_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

occs_userOccs_module_rmd <- function(species) {
  list(
    occs_userOccs_knit = species$rmm$data$occurrence$sources == 'user',
    userCSV_rmd = species$rmm$code$wallace$userCSV,
    sepCSV_rmd = species$rmm$code$wallace$sepCSV,
    decCSV_rmd = species$rmm$code$wallace$decCSV
  )
}
