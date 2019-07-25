projectUser_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("projUserNames"),
    fileInput(ns("userProjEnvs"),
              label = paste0('Input rasters in single-file format (i.e. .tif, ',
                             '.asc). All rasters must have the same extent and ',
                             'resolution (cell size). (**)'),
              accept = c(".asc", ".tif"), multiple = TRUE),
    tags$div(title = paste0('Create binary map of predicted presence/absence ',
                            'assuming all values above threshold value represent ',
                            'presence. Also can be interpreted as a "potential ',
                            'distribution" (see guidance).'),
             selectInput(ns('threshold'), label = "Set threshold",
                         choices = list("No threshold" = 'none',
                                        "Minimum Training Presence" = 'mtp',
                                        "10 Percentile Training Presence" = 'p10',
                                        "Quantile of Training Presences" = 'qtp'))),
    conditionalPanel(sprintf("input['%s'] == 'qtp'", ns("threshold")),
                     sliderInput(ns("trainPresQuantile"), "Set quantile",
                                 min = 0, max = 1, value = .05)),
    conditionalPanel(sprintf("input.modelSel == 'Maxent' & input['%s'] == 'none'",
                             ns("threshold")),
                     h5("Prediction output is the same than Visualize component (**)"))
  )
}

projectUser_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    # GEPB: Filenames different
    # GEPB: Raster and pjExt no match
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      shinyLogs %>%
        writeLog(type = 'error',
                 'Calculate a model prediction in component 7 before projecting.')
      return()
    }
    if (is.null(spp[[curSp()]]$project$pjExt)) {
      shinyLogs %>% writeLog(type = 'error', 'Select projection extent first.')
      return()
    }
    if (is.null(input$userProjEnvs)) {
      shinyLogs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }

    # FUNCTION CALL ####
    userProjEnvs <- c3_userEnvs(rasPath = input$userProjEnvs$datapath,
                                rasName = input$userProjEnvs$name)
    # FUNCTION CALL ####
    predType <- rmm()$output$prediction$notes
    projUser.out <- c8_projectUser(evalOut(), curModel(), envs(),
                                   outputType = predType,
                                   alg = rmm()$model$algorithm,
                                   clamp = rmm()$model$maxent$clamping,
                                   spp[[curSp()]]$project$pjExt,
                                   shinyLogs)

    projExt <- projUser.out$projExt
    projUser <- projUser.out$projUser

    # PROCESSING ####
    # generate binary prediction based on selected thresholding rule
    # (same for all Maxent prediction types because they scale the same)
    occPredVals <- spp[[curSp()]]$visualization$occPredVals

    if(!(input$threshold == 'none')) {
      if (input$threshold == 'mtp') {
        thr <- quantile(occPredVals, probs = 0)
      } else if (input$threshold == 'p10') {
        thr <- quantile(occPredVals, probs = 0.1)
      } else if (input$threshold == 'qtp'){
        thr <- quantile(occPredVals, probs = input$trainPresQuantile)
      }
      projUserThr <- projUser > thr
      shinyLogs %>% writeLog("Projection of model to user-specified files for (**)",
                             em(spName(occs())), ' with threshold ',
                             input$threshold, ' (', formatC(thr, format = "e", 2),
                             ').')
    } else {
      projUserThr <- projUser
      shinyLogs %>% writeLog("Projection of model to user-specified files for (**)",
                             em(spName(occs())), ' with ', predType, ' output.')
    }
    raster::crs(projUserThr) <- raster::crs(envs())
    # rename
    names(projUserThr) <- paste0(curModel(), '_thresh_', predType)

    # LOAD INTO SPP ####
    spp[[curSp()]]$project$pjEnvs <- projExt
    spp[[curSp()]]$project$mapProj <- projUserThr
    spp[[curSp()]]$project$mapProjVals <- getRasterVals(projUserThr, predType)

    # METADATA ####
    spp[[curSp()]]$rmm$data$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projExt, min), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projExt, max), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMin <- 1960
    spp[[curSp()]]$rmm$data$transfer$environment1$yearMax <- 1990
    spp[[curSp()]]$rmm$data$transfer$environment1$resolution <-
      paste(round(raster::res(projExt)[1] * 60, digits = 2), "degrees")
    spp[[curSp()]]$rmm$data$transfer$environment1$extentSet <-
      printVecAsis(as.vector(projExt@extent), asChar = TRUE)
    spp[[curSp()]]$rmm$data$transfer$environment1$extentRule <-
      "project to user-specified files"
    spp[[curSp()]]$rmm$data$transfer$environment1$sources <- "WorldClim 1.4"

    spp[[curSp()]]$rmm$output$transfer$environment1$units <-
      ifelse(predType == "raw", "relative occurrence rate", predType)
    spp[[curSp()]]$rmm$output$transfer$environment1$minVal <-
      printVecAsis(raster::cellStats(projUserThr, min), asChar = TRUE)
    spp[[curSp()]]$rmm$output$transfer$environment1$maxVal <-
      printVecAsis(raster::cellStats(projUserThr, max), asChar = TRUE)
    if(!(input$threshold == 'none')) {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- thr
    } else {
      spp[[curSp()]]$rmm$output$transfer$environment1$thresholdSet <- NULL
    }
    spp[[curSp()]]$rmm$output$transfer$environment1$thresholdRule <- input$threshold
    spp[[curSp()]]$rmm$output$transfer$notes <- NULL
  })
}

projectUser_INFO <-
  infoGenerator(modName = "Project to User-files (**)",
                modAuts = paste0("Gonzalo E. Pinilla-Buitrago, Jamie M. Kass, ",
                                 "Robert P. Anderson"),
                pkgName = "dismo")
