
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
    req(rvs$mods)
    return(list(a = input$bc1, b = input$bc2, p = input$bcProb))
  })
}
