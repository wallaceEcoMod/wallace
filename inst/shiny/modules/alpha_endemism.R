alpha_endemism_module_ui <-  function(id) {
  ns <- shiny::NS(id)
  tagList(
    span("Step 1:", class = "step"),
    span("Choose source of range maps", class = "stepText"), br(), br(),
    selectInput(ns("selSource") , label = "Select source of range maps",
                choices = list("Wallace SDM" = "wallace",
                               "Projected SDM" = "proj",
                               "User uploaded SDM" = "sdm"
                )),

    # UI
    uiOutput(ns("alphaEnd")),
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



    #Processing

        #get all models

        if (input$selSource=='wallace'){
          for (i in 1:length(curSp())){
            sp<-curSp()[i]
            if (is.null(spp[[sp]]$visualization$mapPred)) {
              logger %>%
                writeLog(type = 'error',
                                'No spatial representation of the model has been generated, please first use the visualize component to visualize your model')
              return()
            }
            if (is.null(spp[[sp]]$rmm$rmm$prediction$binary$thresholdSet)) {
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
              all_models<-list()
              for (i in 1:length(curSp())){
                all_models[[i]]<-spp[[curSp()[i]]]$visualization$mapPred
              }
              all_extents<-lapply(all_models,raster::extent)
              all_extents<-lapply(all_extents,as.vector)
              xmin<-min(unlist(lapply(all_extents, function(l) l[[1]])))
              ymin<-min(unlist(lapply(all_extents, function(l) l[[3]])))
              xmax<-max(unlist(lapply(all_extents, function(l) l[[2]])))
              ymax<-max(unlist(lapply(all_extents, function(l) l[[4]])))
              new_extent<-raster::extent(c(xmin,xmax,ymin,ymax))
              #get all models
              sp1<-curSp()[1]
              all_stack<- raster::extend(spp[[sp1]]$visualization$mapPred,new_extent)


              for (i in 2:length(curSp())){
                sp<-curSp()[i]
                #evaluate if same extent

                r1 <- raster::extend(spp[[sp]]$visualization$mapPred, new_extent)
                all_stack<-raster::stack(all_stack,r1)
              }

              req(all_stack)
              # FUNCTION CALL ####
              SE <- changeRangeR::SE(all_stack)

            })
        }
        if (input$selSource=='proj'){
          for (i in 1:length(curSp())){
            sp<-curSp()[i]
            if (is.null(spp[[sp]]$project$mapProj)) {
              logger %>%
                writeLog(type = 'error',
                                'Projected model does not exist, please use the transfer module to transfer to same geographical space')
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
              all_models<-list()
              for (i in 1:length(curSp())){
                all_models[[i]]<-spp[[curSp()[i]]]$project$mapProj
              }
              all_extents<-lapply(all_models,raster::extent)
              all_extents<-lapply(all_extents,as.vector)
              xmin<-min(unlist(lapply(all_extents, function(l) l[[1]])))
              ymin<-min(unlist(lapply(all_extents, function(l) l[[3]])))
              xmax<-max(unlist(lapply(all_extents, function(l) l[[2]])))
              ymax<-max(unlist(lapply(all_extents, function(l) l[[4]])))
              new_extent<-raster::extent(c(xmin,xmax,ymin,ymax))
              #get all models
              sp1<-curSp()[1]
              all_stack<- raster::extend(spp[[sp1]]$project$mapProj,new_extent)


              for (i in 2:length(curSp())){
                sp<-curSp()[i]
                #evaluate if same extent

                r1 <- raster::extend(spp[[sp]]$project$mapProj, new_extent)
                all_stack<-raster::stack(all_stack,r1)
              }

              req(all_stack)
              # FUNCTION CALL ####
              SE <- changeRangeR::SE(all_stack)

            })
        }
        if(input$selSource=='sdm'){
          for (i in 1:length(curSp())){
            sp<-curSp()[i]
            if (is.null(spp[[sp]]$postProc$OrigPred)) {
              logger %>%
                writeLog(type = 'error',
                                'Please upload a model for each species')
              return()
            }
            #if(raster::res(spp[[curSp()[1]]]$postProc$OrigPred)!=raster::res(spp[[sp]]$postProc$OrigPred)){
            # logger %>%
            # writeLog(type = 'error',
            #   'Uploaded models must be of the same resolution')
            #return()
            #}
            if (length(unique(getRasterVals(spp[[sp]]$postProc$OrigPred)))>3) {
              logger %>%
                writeLog(type = 'error',
                                'Uploaded models must be thresholded (binary) before doing multisp. calculations')
              return()
            }
          }
          smartProgress(
            logger,
            message = "Generating a species endemism map", {
              ##Get the extent of all models and keep the max
              all_models<-list()
              for (i in 1:length(curSp())){
                all_models[[i]]<-spp[[curSp()[i]]]$postProc$OrigPred
              }
              all_extents<-lapply(all_models,raster::extent)
              all_extents<-lapply(all_extents,as.vector)
              xmin<-min(unlist(lapply(all_extents, function(l) l[[1]])))
              ymin<-min(unlist(lapply(all_extents, function(l) l[[3]])))
              xmax<-max(unlist(lapply(all_extents, function(l) l[[2]])))
              ymax<-max(unlist(lapply(all_extents, function(l) l[[4]])))
              new_extent<-raster::extent(c(xmin,xmax,ymin,ymax))
              #get all models
              sp1<-curSp()[1]
              all_stack<- raster::extend(spp[[sp1]]$postProc$OrigPred,new_extent)


              for (i in 2:length(curSp())){
                sp<-curSp()[i]
                #evaluate if same extent

                r1 <- raster::extend(spp[[sp]]$postProc$OrigPred, new_extent)
                all_stack<-raster::stack(all_stack,r1)
              }

              req(all_stack)
              # FUNCTION CALL ####
              SE <- changeRangeR::SE(all_stack)

            })
        }




        # FUNCTION CALL ####




    req(SE)
    logger %>% writeLog( "Species endemism calculated ")
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
              labFormat = reverseLabel(2, reverse_order = TRUE))
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

