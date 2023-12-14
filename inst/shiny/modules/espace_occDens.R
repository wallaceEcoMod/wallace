# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# espace_occDens.R
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
espace_occDens_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("goOccDens"), "Run")
  )
}

espace_occDens_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp

  observeEvent(input$goOccDens, {
    # ERRORS ####
    if (length(curSp()) != 2) {
      logger %>% writeLog(
        type = "error",
        "Please select two species to run the occurrence density grid module."
      )
      return()
    }
    # if no multispecies analysis has been run yet
    mspName <- paste(curSp(), collapse = ".")
    if (is.null(spp[[mspName]])) {
      logger %>% writeLog(
        type = "error",
        "Please run PCA with two species before running the occurrence density grid module."
      )
      return()
    }
    # if a multispecies analysis has been run, but not PCA
    if (is.null(spp[[mspName]]$pca)) {
      logger %>% writeLog(
        type = "error",
        "Please run PCA with two species before running the occurrence density grid module."
      )
      return()
    }

    # FUNCTION CALL ####
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    occDens <- espace_occDens(sp1, sp2, spp[[mspName]]$pca, logger)

    # LOAD INTO SPP ####
    req(occDens)
    spp[[mspName]]$occDens <- occDens

    # REFERENCES
    knitcitations::citep(citation("adehabitatHR"))
    knitcitations::citep(citation("ecospat"))

    common$update_component(tab = "Results")
  })

  # PLOTS ####
  output$occDensPlot <- renderPlot({
    graphics::par(mfrow = c(1,2))
    if (length(curSp()) == 2) {
      mSp <- paste(curSp(), collapse = ".")
      sp1 <- curSp()[1]
      sp2 <- curSp()[2]
    } else {
      mSp <- curSp()
    }
    req(spp[[mSp]]$occDens)
    ecospat.plot.nicheDEV(spp[[mSp]]$occDens[[sp1]], title = spName(sp1))
    ecospat.plot.nicheDEV(spp[[mSp]]$occDens[[sp2]], title = spName(sp2))
  })
}

espace_occDens_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  plotOutput(ns("occDensPlot"))
}

espace_occDens_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    espace_occDens_knit = !is.null(species$occDens)
  )
}

