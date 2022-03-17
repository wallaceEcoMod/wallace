vis_bioclimPlot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    "Pick a bioclimatic variable number for each axis",
    numericInput(ns("bc1"), "Axis 1", value = 1, min = 1, max = 19),
    numericInput(ns("bc2"), "Axis 2", value = 2, min = 1, max = 19),
    numericInput(ns("bcProb"), "Set threshold", value = 0.9, min = 0.75,
                 max = 1, step = 0.05)
  )
}

vis_bioclimPlot_module_server <- function(input, output, session, common) {

  spp <- common$spp
  curSp <- common$curSp
  curModel <- common$curModel
  evalOut <- common$evalOut

  observe({
    req(curSp())
    if (length(curSp()) == 1) {
      req(evalOut())
      if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
        # METADATA ####
        spp[[curSp()]]$rmm$code$wallace$bcPlotSettings <-
          list(bc1 = input$bc1, bc2 = input$bc2, p = input$bcProb)
      }
    }
  })

  output$bioclimPlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithms != "BIOCLIM") {
      graphics::par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      graphics::text(x = 0.25, y = 1, "Bioclim plots module requires a Bioclim model",
           cex = 1.2, col = "#641E16")
    } else if (spp[[curSp()]]$rmm$model$algorithms == "BIOCLIM") {
      # FUNCTION CALL ####
      vis_bioclimPlot(evalOut()@models[[curModel()]],
                      input$bc1,
                      input$bc2,
                      input$bcProb)
    }
  }, width = 700, height = 700)


  return(list(
    save = function() {
      list(
        bc1 = input$bc1,
        bc2 = input$bc2,
        bcProb = input$bcProb
      )
    },
    load = function(state) {
      updateNumericInput(session, "bc1", value = state$bc1)
      updateNumericInput(session, "bc2", value = state$bc2)
      updateNumericInput(session, "bcProb", value = state$bcProb)
    }
  ))
}

vis_bioclimPlot_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  imageOutput(ns('bioclimPlot'))
}

vis_bioclimPlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_bioclimPlot_knit = !is.null(species$rmm$code$wallace$bcPlotSettings),
    a_rmd = unlist(species$rmm$code$wallace$bcPlotSettings)[1],
    b_rmd = unlist(species$rmm$code$wallace$bcPlotSettings)[2],
    p_rmd = unlist(species$rmm$code$wallace$bcPlotSettings)[3]
  )
}

