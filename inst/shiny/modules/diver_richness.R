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
    # WARNING ####

    for (i in 1:length(curSp())){
      sp <- curSp()[i]
      if (is.null(spp[[sp]]$visualization$mapPred)) {
        logger %>%
          writeLog(type = 'error',
                   'No spatial representation of the model has been ',
                   'generated, please first use the visualize component ',
                   'to visualize your model')
        return()
      }
      if (is.null(spp[[sp]]$rmm$rmm$prediction$binary$thresholdSet)) {
        logger %>%
          writeLog(type = 'error',
                   'Generate a thresholded prediction before doing ',
                   'multisp. calculations')
        return()
      }
    }
    #Processing
    smartProgress(
      logger,
      message = "Generating a species richness map", {
        # get all models
        all_models <- list()
        for (i in 1:length(curSp())){
          all_models[[i]] <- spp[[curSp()[i]]]$visualization$mapPred
        }
        all_extents <- lapply(all_models,raster::extent)
        all_extents <- lapply(all_extents,as.vector)
        xmin <- min(unlist(lapply(all_extents, function(l) l[[1]])))
        ymin <- min(unlist(lapply(all_extents, function(l) l[[3]])))
        xmax <- max(unlist(lapply(all_extents, function(l) l[[2]])))
        ymax <- max(unlist(lapply(all_extents, function(l) l[[4]])))
        new_extent <- raster::extent(c(xmin,xmax,ymin,ymax))
        # get all models
        sp1 <- curSp()[1]
        all_stack <- raster::extend(spp[[sp1]]$visualization$mapPred,new_extent)


        for (i in 2:length(curSp())) {
          sp <- curSp()[i]
          #evaluate if same extent

          r1 <- raster::extend(spp[[sp]]$visualization$mapPred, new_extent)
          all_stack <- raster::stack(all_stack,r1)
        }

        req(all_stack)
        # FUNCTION CALL ####
        SR <- raster::calc(all_stack, sum, na.rm = TRUE)
      })
    req(SR)
    logger %>% writeLog("Species richness calculated ")
    # LOAD INTO SPP ####
    # this name concatenates the species names when there are two or more
    #  mspName <- paste(curSp(), collapse = ".")
    mspName <- "multisp"
    if (is.null(spp[[mspName]])) {
      spp[[mspName]] <- list(SR = SR)
    } else {
      spp[[mspName]]$SR <- SR
    }
    spp[[mspName]]$mapSRVals <- getRasterVals(SR)
    spp[[mspName]]$ListSR <- curSp()
    common$update_component(tab = "Map")
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
  # spp <- common$spp
  # curSp <- common$curSp
  # #SR <- common$SR
  # # mspName <- paste(curSp(), collapse = ".")
  # #set map parameters
  # SR <- spp[["multisp"]]$SR
  # mapSRVals <-  spp[["multisp"]]$mapSRVals
  # rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # legendPal <- colorNumeric(rev(rasCols), mapSRVals, na.color = 'transparent')
  # rasPal <- colorNumeric(rasCols, mapSRVals, na.color = 'transparent')
  # # Create legend
  # req(SR)
  # map %>% clearAll() %>%
  #   addLegend("bottomright", pal = legendPal,
  #             title = "Species richness",
  #             values = mapSRVals, layerId = "train",
  #             labFormat = reverseLabel(2, reverse_order = TRUE))
  # #MAP richness
  # map %>% addRasterImage(SR, colors = rasPal, opacity = 0.7,
  #                        layerId = 'SR', group = 'diver', method = "ngb")
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

