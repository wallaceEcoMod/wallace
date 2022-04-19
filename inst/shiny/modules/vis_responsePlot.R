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

  observeEvent(input,{
    req(curSp())
    req(curModel())
    req(evalOut())
    #for rmd
    spp[[curSp()]]$rmd$vis_responsePlot <- TRUE
    if (spp[[curSp()]]$rmm$model$algorithms == "maxnet" | spp[[curSp()]]$rmm$model$algorithms == "maxent.jar"){
      spp[[curSp()]]$rmd$vis_curModel <- curModel()
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
      dismo::response(evalOut()@models[[curModel()]], var = curEnv())
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

