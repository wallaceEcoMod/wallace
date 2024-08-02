# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# vis_bioclimPlot.R
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
vis_bioclimPlot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    "Pick a bioclimatic variable number for each axis",
    numericInput(ns("bc1"), "Axis 1", value = 1, min = 1, max = 19),
    numericInput(ns("bc2"), "Axis 2", value = 2, min = 1, max = 19),
    numericInput(ns("bcProb"), "Set threshold", value = 0.9, min = 0.75,
                 max = 1, step = 0.05),
    h5("BIOCLIM envelope plots are displayed automatically in 'Results' tab")
  )
}

vis_bioclimPlot_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  evalOut <- common$evalOut

  observe({
    req(curSp())
    if (length(curSp()) == 1) {
      req(evalOut())
      if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
        # METADATA ####
        spp[[curSp()]]$rmm$code$wallace$bcPlotSettings <-
          list(bc1 = input$bc1, bc2 = input$bc2, p = input$bcProb)
      }
    }
  })

  output$bioclimPlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms != "BIOCLIM") {
      graphics::par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      graphics::text(x = 0.25, y = 1, "Bioclim plots module requires a Bioclim model",
           cex = 1.2, col = "#641E16")
    } else if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      # FUNCTION CALL ####
      vis_bioclimPlot(evalOut()@models[[curModel()]],
                      input$bc1,
                      input$bc2,
                      input$bcProb)
    }
  }, width = 700, height = 700)


  return(list(
    save = function() {
      list(
        bc1 = input$bc1,
        bc2 = input$bc2,
        bcProb = input$bcProb
      )
    },
    load = function(state) {
      updateNumericInput(session, "bc1", value = state$bc1)
      updateNumericInput(session, "bc2", value = state$bc2)
      updateNumericInput(session, "bcProb", value = state$bcProb)
    }
  ))
}

vis_bioclimPlot_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  imageOutput(ns('bioclimPlot'))
}

vis_bioclimPlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_bioclimPlot_knit = !is.null(species$rmm$code$wallace$bcPlotSettings),
    a_rmd = unlist(species$rmm$code$wallace$bcPlotSettings)[1],
    b_rmd = unlist(species$rmm$code$wallace$bcPlotSettings)[2],
    p_rmd = unlist(species$rmm$code$wallace$bcPlotSettings)[3]
  )
}

