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
      if (spp[[curSp()]]$rmm$model$algorithm == "BIOCLIM") {
        # METADATA ####
        spp[[curSp()]]$rmm$code$wallaceSettings$bcPlotSettings <-
          list(bc1 = input$bc1, bc2 = input$bc2, p = input$bcProb)
      }
    }
  })

  output$bioclimPlot <- renderPlot({
    req(curSp(), evalOut())
    if (spp[[curSp()]]$rmm$model$algorithm == "BIOCLIM"){
      # FUNCTION CALL ####
      vis_bioclimPlot(evalOut()@models[[curModel()]],
                      input$bc1,
                      input$bc2,
                      input$bcProb)
    }
  }, width = 700, height = 700)
}

vis_bioclimPlot_module_result <- function(id) {
  ns <- NS(id)
  # Result UI
  imageOutput(ns('bioclimPlot'))
}

vis_bioclimPlot_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    vis_bioclimPlot_knit = FALSE
    # vis_bioclimPlot_knit = species$rmm$code$wallaceSettings$someFlag,
    # var1 = species$rmm$code$wallaceSettings$someSetting1,
    # var2 = species$rmm$code$wallaceSettings$someSetting2
  )
}

