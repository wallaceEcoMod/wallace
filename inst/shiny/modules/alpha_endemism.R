alpha_endemism_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("alphaRich")),
    actionButton(ns("Goendemism"), "Run")
  )
}

alpha_endemism_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj
  allSp <- common$allSp

  output$alphaEnd <- renderUI({
    ns <- session$ns
    req(curSp())
    if (length(curSp()) == 1) {
      shiny::tagList(
        shiny::em("Select at least two species in species menu"),
        br()
      )
    }
  })


  observeEvent(input$Goendemism, {
    # WARNING ####
    # ERRORS ####
    if (length(curSp()) < 2) {
      logger %>% writeLog(
        type = "error",
        "Please select at least two species to run the endemism module."
      )
      return()
    }


    for (i in 1:length(curSp())){
      sp<-curSp()[i]
      if (is.null(spp[[sp]]$project$mapProj)) {
        logger %>%
          writeLog(type = 'error',
                   'Project your model before doing multisp. calculations')
        return()
      }
      if (is.null(spp[[sp]]$rmm$prediction$transfer$environment1$thresholdSet)) {
        logger %>%
          writeLog(type = 'error',
                   'Generate a thresholded prediction before doing multisp. calculations')
        return()
      }

    }
    #Processing
    smartProgress(
      logger,
      message = "Generating a species endemism map", {
        #get all models
        sp1<-curSp()[1]
        all_stack<-spp[[sp1]]$project$mapProj

        for (i in 2:length(curSp())){
          sp<-curSp()[i]
          #evaluate if same extent
          if(raster::extent(all_stack[[1]])!=raster::extent(spp[[sp]]$project$mapProj)){
            logger %>%
              writeLog(type = 'error',
                       'Please project all models to the same area')
            return()
          }
          else {
            all_stack<-raster::stack(all_stack,spp[[sp]]$project$mapProj)
          }
        }

        # FUNCTION CALL ####

        SE <- changeRangeR::SE(all_stack)

      })

    req(SE)
    # LOAD INTO SPP ####
    # this name concatenates the species names when there are two or more
    #  mspName <- paste(curSp(), collapse = ".")
    mspName<-"multisp"
    if (is.null(spp[[mspName]])) {
      spp[[mspName]] <- list(SE = SE)
    } else {
      spp[[mspName]]$SE <- SE
    }

    spp[[mspName]]$mapSEVals <- getRasterVals(SE)
    spp[[mspName]]$ListSE <- curSp()

    common$update_component(tab = "Map")
  })


  output$result <- renderPrint({
    # Result
    mspName <- "multisp"
    spp[[mspName]]$SE
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

alpha_endemism_module_result <- function(id) {
ns <- NS(id)

# Result UI
verbatimTextOutput(ns("result"))
}

alpha_endemism_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  #SR <- common$SR
  # mspName <- paste(curSp(), collapse = ".")
  #set map parameters
  SE <- spp[["multisp"]]$SE
  mapSEVals <-  spp[["multisp"]]$mapSEVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), mapSEVals, na.color = 'transparent')
  rasPal <- colorNumeric(rasCols, mapSEVals, na.color = 'transparent')
  # Create legend
  req(SE)
  map %>% clearAll() %>%
    addLegend("bottomright", pal = legendPal,
              title = "Species endemism",
              values = mapSEVals, layerId = "train",
              labFormat = reverseLabels(2, reverse_order = TRUE))
  #MAP endemism
  map %>% addRasterImage(SE, colors = rasPal, opacity = 0.7,
                         layerId = 'SE', group = 'alpha', method = "ngb")
}

alpha_endemism_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    alpha_endemism_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

