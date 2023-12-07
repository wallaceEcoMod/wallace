# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# poccs_removeByID.R
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
poccs_removeByID_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    numericInput(ns("removeID"), label = "Enter the record ID to be removed",
                 value = 0),
    actionButton(ns("goRemoveByID"), "Remove Occurrence"),
    tags$hr(class = "hrDashed"),
    actionButton(ns("goResetOccs"), "Reset", class = 'butReset'),
    strong(" to original occurrence")
  )
}

poccs_removeByID_module_server <- function(input, output, session, common) {

  logger <- common$logger
  occs <- common$occs
  spp <- common$spp
  curSp <- common$curSp

  observeEvent(input$goRemoveByID, {
    # FUNCTION CALL ####
    occs.rem <- poccs_removeByID(occs(),
                                 input$removeID,
                                 logger,
                                 spN = curSp())
    req(occs.rem)
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.rem

    # REFERENCES ####
    knitcitations::citep(citation("leaflet.extras"))

    # METADATA ####
    # if no removeIDs are recorded yet, make a list to record them
    # if at least one exists, add to the list
    if (is.null(spp[[curSp()]]$rmm$code$wallace$removedIDs)) {
      spp[[curSp()]]$rmm$code$wallace$removedIDs <- input$removeID
    } else {
      spp[[curSp()]]$rmm$code$wallace$removedIDs <-
        c(spp[[curSp()]]$rmm$code$wallace$removedIDs, input$removeID)
    }

    common$update_component(tab = "Map")
  })

  # reset occurrences button functionality
  observeEvent(input$goResetOccs, {
    req(curSp())
    spp[[curSp()]]$occs <- spp[[curSp()]]$occData$occsCleaned
    spp[[curSp()]]$rmm$code$wallace$occsSelPolyCoords <- NULL
    spp[[curSp()]]$procOccs$occsThin <- NULL
    spp[[curSp()]]$rmm$code$wallace$removedIDs <- NULL
    logger %>% writeLog(
      hlSpp(curSp()), "Reset to original occurrences (n = ",
      nrow(spp[[curSp()]]$occs), ").")
  })

  return(list(
    save = function() {
      list(
        removeID = input$removeID
      )
    },
    load = function(state) {
      updateNumericInput(session, "removeID", value = state$removeID)
    }
  ))

}

poccs_removeByID_module_map <- function(map, common) {
  occs <- common$occs
  # Map logic
  map %>% leaflet.extras::removeDrawToolbar() %>%
    clearAll() %>%
    addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs())
}

poccs_removeByID_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    poccs_removeByID_knit = !is.null(species$rmm$code$wallace$removedIDs),
    removeByID_id_rmd = printVecAsis(species$rmm$code$wallace$removedIDs)
  )
}

