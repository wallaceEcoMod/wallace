vis_maxentEvalPlot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns('maxentEvalSel'), label = "Select evaluation statistic",
                choices = list("Select Stat..." = '',
                               "average AUC test" = 'auc.val',
                               "average AUC diff" = 'auc.diff',
                               "average OR mtp" = 'or.mtp',
                               "average OR 10%" = 'or.10p',
                               "delta AICc" = 'delta.AICc'),
                selected = 'auc.val'),
    h6("Maxent evaluation plots display automatically in 'Results' tab")
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
      if (spp[[curSp()]]$rmm$model$algorithms == "maxent.jar" |
          spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
        # ERRORS ####
        if (is.null(input$maxentEvalSel)) {
          logger %>% writeLog(type = 'error', "Please choose a statistic to plot.")
          return()
        }
        # METADATA ####
        spp[[curSp()]]$rmm$code$wallace$maxentEvalPlotSel <- input$maxentEvalSel
      }
    }
  })
  observeEvent(input$maxentEvalSel,{
    req(curSp())
    spp[[curSp()]]$rmm$code$wallace$maxentEvalPlot <- TRUE
  })
  output$maxentEvalPlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      graphics::par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      graphics::text(x = 0.25, y = 1, "Evaluation plot module requires a Maxent model",
           cex = 1.2, col = "#641E16")
    } else if (spp[[curSp()]]$rmm$model$algorithms == "maxent.jar" |
        spp[[curSp()]]$rmm$model$algorithms == "maxnet") {
      # FUNCTION CALL ####
      if (!is.null(input$maxentEvalSel)) {
        ENMeval::evalplot.stats(evalOut(), input$maxentEvalSel, "rm", "fc")
      }
    }
  }, width = 700, height = 700)

  return(list(
    save = function() {
      list(maxentEvalSel = input$maxentEvalSel)
    },
    load = function(state) {
      updateSelectInput(session, "maxentEvalSel", selected = state$maxentEvalSel)
    }
  ))
}

vis_maxentEvalPlot_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  imageOutput(ns('maxentEvalPlot'))
}

vis_maxentEvalPlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_maxentEvalPlot_knit = !is.null(species$rmm$code$wallace$maxentEvalPlot),
    evalPlot_rmd = species$rmm$code$wallace$maxentEvalPlotSel

  )
}

