
maxentEvalPlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('maxentEvalSel'), label = "Select evaluation statistic",
                choices = list("Select Stat..." = '', "average AUC test" = 'avg.test.AUC', 
                               "average AUC diff" = 'avg.diff.AUC', "average OR mtp" = 'avg.test.orMTP',
                               "average OR 10%" = 'avg.test.or10pct', "delta AICc" = 'delta.AICc'), 
                selected = 'avg.test.AUC'),
    h6("Maxent evaluation plots display automatically in 'Results' tab(**)")
  )
}

maxentEvalPlot_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if (is.null(input$maxentEvalSel)) {
      shinyLogs %>% writeLog(type = 'error', "Please choose a statistic to plot.")
      return()
    }
    
    # FUNCTION CALL ####
    makeMaxentEvalPlot(evalOut()@results, input$maxentEvalSel)
    
    # METADATA ####
    spp[[curSp()]]$rmm$code$wallaceSettings$maxentEvalPlotSel <- input$maxentEvalSel
  })
}

maxentEvalPlot_INFO <- infoGenerator(modName = "Maxent Evaluation Plots", 
                                     modAuts = "Jamie M. Kass, Robert Muscarella,
                                     Bruno Vilela, Gonzalo E. Pinilla-Buitrago,
                                     Robert P. Anderson", 
                                     pkgName = c("ENMeval", "dismo"))
