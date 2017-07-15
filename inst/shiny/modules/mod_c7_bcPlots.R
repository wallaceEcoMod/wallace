
bcPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    "Pick a bioclimatic variable number for each axis",
    numericInput(ns("bc1"), "Axis 1", value = 1, min = 1, max = 19),
    numericInput(ns("bc2"), "Axis 2", value = 2, min = 1, max = 19),
    numericInput(ns("bcProb"), "Set threshold", value = 0.9, min = 0.75, max = 1, step = 0.05)
  )
}

bcPlots_MOD <- function(input, output, session, rvs) {
  reactive({
    req(input$fcs, rvs$occs, rvs$bgPts, rvs$bgMsk, rvs$occsGrp, rvs$bgGrp)

    validate(need(values$evalMods[[1]], message = FALSE))
    values$bcEnvelPlot <- TRUE
    bc.plot(values$evalMods[[1]], a = input$bc1, b = input$bc2, p = input$bcProb)
  })
}
