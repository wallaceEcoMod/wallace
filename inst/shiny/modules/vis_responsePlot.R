vis_responsePlot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("curEnvUI")),
    h6("Reponse curves are displayed automatically in 'Results' tab(**)")
  )
}

vis_responsePlot_module_server <- function(input, output, session, common) {

  spp <- common$spp
  envs <- common$envs
  curSp <- common$curSp
  curModel <- common$curModel
  curEnv <- common$curEnv
  evalOut <- common$evalOut
  logger <- common$logger

  # ui that populates with the names of environmental predictors used
  output$curEnvUI <- renderUI({
    # ensure envs entity is within spp
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
      n <- mxNonzeroCoefs(evalOut()@models[[curModel()]], "maxnet")
    } else if (spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
      n <- mxNonzeroCoefs(evalOut()@models[[curModel()]], "maxent.jar")
    }
    envsNameList <- c(setNames(as.list(n), n))
    selectizeInput("curEnv", label = "Select variable" ,
                   choices = envsNameList, multiple = FALSE, selected = n[1],
                   options = list(maxItems = 1))
  })

  output$responsePlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
      req(curEnv())
      suppressWarnings(
        plot(evalOut()@models[[curModel()]], vars = curEnv(), type = "cloglog")
        )
    } else if (spp[[curSp()]]$rmm$model$algorithm == "maxent.jar") {
      dismo::response(evalOut()@models[[curModel()]], var = curEnv())
    }
  }, width = 700, height = 700)
}

vis_responsePlot_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  imageOutput(ns('responsePlot'))
}

vis_responsePlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_responsePlot_knit = FALSE
    # vis_responsePlot_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

