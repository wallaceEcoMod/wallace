
mxEvalPlots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('mxEvalSel'), label = "Select Evaluation Plot",
                choices = list("Select Stat..." = '', "average AUC test" = 'avg.test.AUC', 
                               "average AUC diff" = 'avg.diff.AUC', "average OR mtp" = 'avg.test.orMTP',
                               "average OR 10%" = 'avg.test.or10pct', "delta AICc" = 'delta.AICc')),
    strong("Download Maxent evaluation plot (.png)"), br(), br(),
    downloadButton(ns('dlMxEvalPlot'), "Download")
  )
}

mxEvalPlots_MOD <- function(input, output, session, rvs) {
  reactive({
    req(input$mxEvalSel, rvs$mods)

    # handle downloads for Maxent Evaluation Plots png
    output$dlMxEvalPlot <- downloadHandler(
      filename = function() {paste0(spName(), "_maxent_eval_plot.png")},
      content = function(file) {
        png(file)
        evalPlot(rvs$modRes, input$mxEvalSel)
        dev.off()
      }
    )
    
    evalPlot(rvs$modRes, input$mxEvalSel)
  })
}
