# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# diver_richness.R
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
diver_richness_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Choose source of range maps", class = "stepText"), br(), br(),
    selectInput(ns("selRichSource") , label = "Select source of range maps",
                choices = list("Wallace SDM" = "wallace",
                               "Transferred SDM" = "xfer",
                               "User uploaded SDM" = "user",
                               "Masked SDM" = "mask")),
    # UI
    uiOutput(ns("diverRich")),
    actionButton(ns("goRichness"), "Run")
  )
}

diver_richness_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  multi.sp <- common$multi.sp

  output$diverRich <- renderUI({
    ns <- session$ns
    req(curSp())
    if (length(curSp()) == 1) {
      shiny::tagList(
        shiny::em("Select at least two species in species menu"),
        br()
      )
    }
  })

  observeEvent(input$goRichness, {
    # ERRORS ####
    if (length(curSp()) < 2) {
      logger %>% writeLog(
        type = "error",
        "Please select at least two species to run the richness module."
      )
      return()
    }
    for (i in 1:length(curSp())) {
      sp <- curSp()[i]
      rangeSp <- switch (input$selRichSource,
                         wallace = spp[[sp]]$visualization$mapPred,
                         xfer = spp[[sp]]$transfer$mapXfer,
                         user = spp[[sp]]$mask$userSDM,
                         mask = spp[[sp]]$mask$prediction)
      if (is.null(rangeSp)) {
        logger %>%
          writeLog(type = 'error', hlSpp(sp),
                   'No range representation (', input$selRichSource, ') has been ',
                   'generated/loaded, please provided one (**).')
        return()
      }
      if (input$selRichSource == "wallace") {
        if (is.null(spp[[sp]]$visualization$thresholds)) {
          logger %>%
            writeLog(type = 'error',
                     hlSpp(sp),
                     'Generate a thresholded model before doing calculations.')
          return()
        }
      }
      if (input$selRichSource == "xfer") {
        if (is.null(spp[[sp]]$rmm$prediction$transfer$environment1$thresholdSet)) {
          logger %>%
            writeLog(type = 'error',
                     hlSpp(sp),
                     'Generate a thresholded prediction before doing calculations')
          return()
        }
      }
      if (input$selRichSource == "user") {
        if (!shiny::isTruthy(spp[[sp]]$mask$userThr)) {
          logger %>%
            writeLog(type = 'error',
                     hlSpp(sp),
                     'Load a user thresholded prediction before doing calculations.')
          return()
        }
      }
      if (input$selRichSource == "mask") {
        if (!spp[[sp]]$mask$maskThr) {
          logger %>%
            writeLog(type = 'error',
                     hlSpp(sp),
                     'Mask a thresholded prediction before doing calculations.')
          return()
        }
      }
    }
    all_resolutions <- list()
    for (i in 1:length(curSp())) {
      sp <- curSp()[i]
      rangeSp <- switch (input$selRichSource,
                         wallace = spp[[sp]]$visualization$mapPred,
                         xfer = spp[[sp]]$transfer$mapXfer,
                         user = spp[[sp]]$mask$userSDM,
                         mask = spp[[sp]]$mask$prediction)
      all_resolutions[[i]] <- round(raster::res(rangeSp), digits = 7)
    }
    if (length(unique(all_resolutions)) != 1) {
      logger %>%
        writeLog(type = 'error', hlSpp(paste0(length(curSp()), " species")),
                 "Resolutions of all rasters should be the same (**).")
      return()
    }

    #Processing
    smartProgress(
      logger,
      message = "Generating a species richness map", {
        # get all models

        all_models <- list()
        for (i in 1:length(curSp())) {
          sp <- curSp()[i]
          all_models[[i]] <- switch (input$selRichSource,
                                     wallace = spp[[sp]]$visualization$mapPred,
                                     xfer = spp[[sp]]$transfer$mapXfer,
                                     user = spp[[sp]]$mask$userSDM,
                                     mask = spp[[sp]]$mask$prediction)
        }
        all_extents <- lapply(all_models, raster::extent)
        all_extents <- lapply(all_extents, as.vector)
        xmin <- min(unlist(lapply(all_extents, function(l) l[[1]])))
        ymin <- min(unlist(lapply(all_extents, function(l) l[[3]])))
        xmax <- max(unlist(lapply(all_extents, function(l) l[[2]])))
        ymax <- max(unlist(lapply(all_extents, function(l) l[[4]])))
        new_extent <- raster::extent(c(xmin, xmax, ymin, ymax))

        all_stack <- lapply(all_models, raster::extend, y = new_extent)
        all_stack <- raster::stack(all_stack)

        # FUNCTION CALL ####
        richness <- raster::calc(all_stack, sum, na.rm = TRUE)
        richness[richness == 0] <- NA
      })

    req(richness)
    logger %>% writeLog("Species richness calculated.")

    # LOAD INTO SPP ####
    multi.sp$richness <- richness
    multi.sp$sppRichness <- curSp()
    common$update_component(tab = "Map")

    # REFERENCES ####
    knitcitations::citep(citation("raster"))

    # METADATA ####
    #add metadata
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))

}

diver_richness_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  multi.sp <- common$multi.sp

  req(multi.sp$richness)
  rich <- multi.sp$richness
  rasCols <- c("#3288BD", "#99D594", "#E6F598",
               "#FEE08B", "#FC8D59", "#D53E4F")
  minV <- raster::minValue(rich)
  maxV <- raster::maxValue(rich)
  legendPal <- colorNumeric(rev(rasCols), minV:maxV,
                            na.color = 'transparent')
  map %>% clearAll() %>%
    addLegend("bottomright", pal = legendPal,
              title = "Richness",
              values = minV:maxV, layerId = "richnessLeg",
              labFormat = reverseLabel(2, reverse_order = TRUE)) %>%
    leafem::addGeoRaster(rich,
                         colorOptions = leafem::colorOptions(
                           palette = colorRampPalette(colors = rasCols)),
                         opacity = 1, group = 'diver',
                         layerId = 'richnessRaster')
}

diver_richness_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  # list(
  #   diver_richness_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  # )
}

