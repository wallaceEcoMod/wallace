
bcPlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    "Pick a bioclimatic variable number for each axis",
    numericInput(ns("bc1"), "Axis 1", value = 1, min = 1, max = 19),
    numericInput(ns("bc2"), "Axis 2", value = 2, min = 1, max = 19),
    numericInput(ns("bcProb"), "Set threshold", value = 0.9, min = 0.75, max = 1, step = 0.05)
  )
}

bcPlot_MOD <- function(input, output, session, spIn) {
  
  reactive({
    # ERRORS ####
    if (is.null(spp[[curSp()]]$model)) {
      logs %>% writeLog(type = 'error', "Models must first be run in component Model.")
      return()
    }
    
    # FUNCTION CALL ####
    bcPlot <- bioclimPlot(spp[[curSp()]]$mod[[curModel()]],
                  input$bc1,
                  input$bc2,
                  input$bcProb)
    
    # METADATA ####
    spp[[curSp()]]$rmm$code$wallaceSettings$bcPlotSettings <- list(bc1=input$bc1, bc2=input$bc2, p=input$bcProb)
    
    return(bcPlot)
  })
}
