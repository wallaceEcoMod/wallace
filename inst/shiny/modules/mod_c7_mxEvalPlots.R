
mxEvalPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('mxEvalSel'), label = "Select Evaluation Plot",
                choices = list("Select Stat..." = '', "average AUC test" = 'avg.test.AUC', 
                               "average AUC diff" = 'avg.diff.AUC', "average OR mtp" = 'avg.test.orMTP',
                               "average OR 10%" = 'avg.test.or10pct', "delta AICc" = 'delta.AICc'))
  )
}

mxEvalPlots_MOD <- function(input, output, session, rvs) {
  reactive({
    req(rvs$mods)

    evalPlot(rvs$modRes, input$mxEvalSel)
  })
}
