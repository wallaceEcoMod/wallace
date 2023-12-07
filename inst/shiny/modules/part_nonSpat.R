# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# part_nonSpat.R
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
part_nonSpat_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("partNspSel"), "Options Available:",
                choices = list("None selected" = '',
                               "Jackknife (k = n)" = "jack",
                               "Random k-fold" = "rand")),
    conditionalPanel(sprintf("input['%s'] == 'rand'", ns("partNspSel")),
                     numericInput(ns("kfolds"), label = "Number of Folds",
                                  value = 2, min = 2)),
    tags$div(
      title = "Apply selection to ALL species loaded",
      checkboxInput(ns("batch"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goPartitionNonSpat"), "Partition")
  )
}

part_nonSpat_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  bgMask <- common$bgMask

  observeEvent(input$goPartitionNonSpat, {
    # loop over all species if batch is on
    if(input$batch == TRUE) spLoop <- allSp() else spLoop <- curSp()

    # PROCESSING ####
    for(sp in spLoop) {
      if (is.null(bgMask())) {
        logger %>% writeLog(
          type = 'error', hlSpp(sp),
          "Before partitioning occurrences, mask your ",
          "environmental variables by your background extent."
        )
        return()
      }

      #### FUNCTION CALL
      group.data <- part_partitionOccs(spp[[sp]]$occs, spp[[sp]]$bg,
                                       input$partNspSel, kfolds = input$kfolds,
                                       bgMask = NULL, aggFact = NULL, logger,
                                       spN = sp)
      req(group.data)

      # LOAD INTO SPP ####
      spp[[sp]]$occs$partition <- group.data$occs.grp
      spp[[sp]]$bg$partition <- group.data$bg.grp

      # REFERENCES
      knitcitations::citep(citation("ENMeval", auto = TRUE))

      # METADATA ####
      spp[[sp]]$rmm$code$wallace$partition_code <- input$partNspSel
      spp[[sp]]$rmm$model$partition$NonSpatial <- 'Non-spatial'
      if(input$partNspSel == 'jack') {
        spp[[sp]]$rmm$model$partition$numberFolds <- nrow(spp[[sp]]$occs)
        spp[[sp]]$rmm$model$partition$partitionRule <- 'jackknife'
      }
      if(input$partNspSel == 'rand') {
        spp[[sp]]$rmm$model$partition$numberFolds <- input$kfolds
        spp[[sp]]$rmm$model$partition$partitionRule <- 'random k-fold'
      }
    }
    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        partNspSel = input$partNspSel,
        kfolds = input$kfolds
      )
    },
    load = function(state) {
      updateSelectInput(session, "partNspSel", selected = state$partNspSel)
      updateNumericInput(session, "kfolds", value = state$kfolds)
    }
  ))

}

part_nonSpat_module_map <- function(map, common) {
  occs <- common$occs
  # Map logic
  if (!is.null(occs()$partition)) {
    occsGrp <- occs()$partition
    # colors for partition symbology
    if (max(occsGrp) < 3) {
      newColors <- RColorBrewer::brewer.pal(n = 3, "Set2")[1:max(occsGrp)]
    } else if (max(occsGrp) < 9) {
      newColors <- RColorBrewer::brewer.pal(n = max(occsGrp), "Set2")
    } else if (max(occsGrp) < 12) {
      newColors <- RColorBrewer::brewer.pal(n = max(occsGrp), "RdYlBu")
    } else {
      newColors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, "RdYlBu"))(max(occsGrp))
    }
    partsFill <- newColors[occsGrp]
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE,
                       fillColor = partsFill, fillOpacity = 1, weight = 2,
                       popup = ~pop) %>%
      addLegend("bottomright", colors = newColors,
                title = "Partition Groups", labels = sort(unique(occsGrp)),
                opacity = 1)
  } else {
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs())
  }
}

part_nonSpat_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    part_nonSpat_knit =  !is.null(species$rmm$model$partition$NonSpatial),
    k_folds_rmd = species$rmm$model$partition$numberFolds,
    method_rmd = species$rmm$model$partition$partitionRule,
    method_code_rmd = species$rmm$code$wallace$partition_code
  )
}

