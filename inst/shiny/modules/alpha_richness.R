alpha_richness_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("alphaRich")),
    actionButton(ns("GoRichness"), "Run")
  )
}

alpha_richness_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  mapProj <- common$mapProj
  allSp <- common$allSp

  output$alphaRich <- renderUI({
    ns <- session$ns
    req(curSp())
    if (length(curSp()) == 1) {
      shiny::tagList(
        shiny::em("Select at least two species in species menu"),
        br()
      )
    }
  })


  observeEvent(input$GoRichness, {
    # WARNING ####
    # ERRORS ####
    if (length(curSp()) < 2) {
      logger %>% writeLog(
        type = "error",
        "Please select at least two species to run the richness module."
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
      message = "Generating a species richness map", {
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
        SR <-  raster::calc(all_stack, sum, na.rm = T)
                       })

    req(SR)
    logger %>% writeLog( "Species richness calculated ")
    # LOAD INTO SPP ####
    # this name concatenates the species names when there are two or more
  #  mspName <- paste(curSp(), collapse = ".")
   mspName<-"multisp"
     if (is.null(spp[[mspName]])) {
      spp[[mspName]] <- list(SR = SR)
    } else {
      spp[[mspName]]$SR <- SR
    }

     spp[[mspName]]$mapSRVals <- getRasterVals(SR)
     spp[[mspName]]$ListSR <- curSp()

    common$update_component(tab = "Map")
  })


  output$result <- renderPrint({
    # Result
curSp()
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

alpha_richness_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

alpha_richness_module_map <- function(map, common) {
  # Map logic
  spp <- common$spp
  curSp <- common$curSp
  #SR <- common$SR
 # mspName <- paste(curSp(), collapse = ".")
  #set map parameters
  SR <- spp[["multisp"]]$SR
  mapSRVals <-  spp[["multisp"]]$mapSRVals
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), mapSRVals, na.color = 'transparent')
  rasPal <- colorNumeric(rasCols, mapSRVals, na.color = 'transparent')
  # Create legend
  req(SR)
  map %>% clearAll() %>%
    addLegend("bottomright", pal = legendPal,
              title = "Species richness",
              values = mapSRVals, layerId = "train",
              labFormat = reverseLabels(2, reverse_order = TRUE))
  #MAP richness
  map %>% addRasterImage(SR, colors = rasPal, opacity = 0.7,
                   layerId = 'SR', group = 'alpha', method = "ngb")
}

alpha_richness_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    alpha_richness_knit = FALSE
    #species$rmm$code$wallace$someFlag,
    #var1 = species$rmm$code$wallace$someSetting1,
    #var2 = species$rmm$code$wallace$someSetting2
  )
}

