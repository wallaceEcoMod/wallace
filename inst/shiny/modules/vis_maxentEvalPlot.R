vis_maxentEvalPlot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns('maxentEvalSel'), label = "Select evaluation statistic",
                choices = list("Select Stat..." = '',
                               "average AUC test" = 'auc.test',
                               "average AUC diff" = 'auc.diff',
                               "average OR mtp" = 'or.mtp',
                               "average OR 10%" = 'or.10p',
                               "delta AICc" = 'delta.AICc'),
                selected = 'auc.test'),
    h6("Maxent evaluation plots display automatically in 'Results' tab(**)")
  )
}

vis_maxentEvalPlot_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  evalOut <- common$evalOut

  observe({
    req(curSp())
    if (length(curSp()) == 1) {
      req(evalOut())
      if (spp[[curSp()]]$rmm$model$algorithm == "maxent.jar" |
          spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
        # ERRORS ####
        if (is.null(input$maxentEvalSel)) {
          logger %>% writeLog(type = 'error', "Please choose a statistic to plot.")
          return()
        }
        # METADATA ####
        spp[[curSp()]]$rmm$code$wallaceSettings$maxentEvalPlotSel <- input$maxentEvalSel
      }
    }
  })

  output$maxentEvalPlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithm == "maxent.jar" |
        spp[[curSp()]]$rmm$model$algorithm == "maxnet") {
      # FUNCTION CALL ####
      if (!is.null(input$maxentEvalSel)) {
        ENMeval::plot.eval(evalOut(), input$maxentEvalSel, "rm", "fc")
      }
    }
  }, width = 700, height = 700)
}

vis_maxentEvalPlot_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  imageOutput(ns('maxentEvalPlot'))
}

vis_maxentEvalPlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    module_knit = species$rmm$code$wallaceSettings$someFlag,
    var1 = species$rmm$code$wallaceSettings$someSetting1,
    var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

