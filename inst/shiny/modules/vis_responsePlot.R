# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# vis_responsePlot.R
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
vis_responsePlot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("curEnvUI")),
    h5("Reponse curves are displayed automatically in 'Results' tab")
  )
}

vis_responsePlot_module_server <- function(input, output, session, common) {

  spp <- common$spp
  envs <- common$envs
  curSp <- common$curSp
  curModel <- common$curModel
  curEnv <- common$curEnv
  evalOut <- common$evalOut

  observe({
    req(curSp())
    req(curModel())
    req(evalOut())
    #for rmd

    if (spp[[curSp()]]$rmm$model$algorithms == "maxnet" | spp[[curSp()]]$rmm$model$algorithms == "maxent.jar"){
      spp[[curSp()]]$rmd$vis_curModel <- curModel()
      spp[[curSp()]]$rmd$vis_responsePlot <- TRUE
    }
  })


  # ui that populates with the names of environmental predictors used
  output$curEnvUI <- renderUI({
    # ensure envs entity is within spp
    req(curSp(), evalOut(), curModel())
    if (spp[[curSp()]]$rmm$model$algorithms != "BIOCLIM") {
      if (spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
        n <- mxNonzeroCoefs(evalOut()@models[[curModel()]], "maxnet")
      } else if (spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
        n <- mxNonzeroCoefs(evalOut()@models[[curModel()]], "maxent.jar")
      }
      envsNameList <- c(setNames(as.list(n), n))
      selectizeInput("curEnv", label = "Select variable" ,
                     choices = envsNameList, multiple = FALSE, selected = n[1],
                     options = list(maxItems = 1))
    }
  })


  output$responsePlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      graphics::par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      graphics::text(x = 0.25, y = 1, "Response curves module requires a Maxent model",
           cex = 1.2, col = "#641E16")
    } else if (spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
      req(curEnv())
      suppressWarnings(
        maxnet::response.plot(evalOut()@models[[curModel()]], v = curEnv(), type = "cloglog")
      )
    } else if (spp[[curSp()]]$rmm$model$algorithms == "maxent.jar") {
      predicts::partialResponse(evalOut()@models[[curModel()]], var = curEnv())
    }
  }, width = 700, height = 700)

   }

vis_responsePlot_module_result <- function(id) {
  ns <- NS(id)
  imageOutput(ns('responsePlot'))
}

vis_responsePlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_responsePlot_knit = !is.null(species$rmd$vis_responsePlot),
    vis_maxnet_knit = if(!is.null(species$rmm$model$algorithms)){
    species$rmm$model$algorithms == "maxnet"} else {FALSE},
    alg_rmd = if(!is.null(species$rmm$model$algorithms)){species$rmm$model$algorithms} else {NULL},
    curModel_rmd = if(!is.null(species$rmd$vis_curModel)){species$rmd$vis_curModel} else {NULL}
  )
}

