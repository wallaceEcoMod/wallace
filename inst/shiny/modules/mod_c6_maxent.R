
maxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Select feature classes "), strong(em("(flexibility of modeled response)")), br(),
    "key: ", strong("L"), "inear, ", strong("Q"), "uadratic, ", strong("H"), "inge, ", strong("P"), "roduct, ", strong("T"), "hreshold",
    tags$div(title='Feature combinations to be explored. Features are constructed using different relationships within and among the environmental predictors, and are used to constrain the computed probability distribution. In short, more features = more potential model complexity.',
             checkboxGroupInput(ns("fcs"), label='',
                       choices = list("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), inline = TRUE)),
    strong("Select regularization multipliers "), strong(em("(penalty against complexity)")),
    tags$div(title='Range of regularization multipliers to explore. Greater values of the regularization multiplier lead to increased penalty against overly complex and/or overfit models. A value of 0 results in no regularization.',
             sliderInput(ns("rms"), label = "",
                min = 0, max = 10, value = c(1, 2))),
    tags$div(title='Value used to step through regularization multiplier range (e.g. range of 1-3 with step 0.5 results in [1, 1.5, 2, 2.5, 3]).',
             numericInput(ns("rmsStep"), label = "Multiplier step value", value = 1)),
    tags$div(title='If checked, the response will resist extrapolation to environmental values outside those used to build the model. See guidance for details.',
             checkboxInput(ns('clamp'), label = 'Clamp predictions?'))
  )
}

maxent_MOD <- function(input, output, session, rvs) {
  reactive({
    if (is.null(rvs$occsGrp)) {
      rvs %>% writeLog(type = 'error', "Before building a model, partition 
                       occurrences in component 5.")
      return()
    }
    if (is.null(input$fcs)) {
      rvs %>% writeLog(type = 'error', "No feature classes selected.")
      return()
    }
    if (!require('rJava')) {
      rvs %>% writeLog(type = "error", 'Package rJava cannot load. 
               Please download the latest version of Java, and make sure it is the 
               correct version (e.g. 64-bit for a 64-bit system). After installing, 
               try "library(rJava)". If it loads properly, restart Wallace and try again.
               If it does not, please consult www.github.com/wallaceecomod/wallace for
               more tips on getting rJava to work.')
      return()
    }
    
    if (is.null(input$fcs)) {
      rvs %>% writeLog(type = 'error', 'Select feature classes first.')
      return()
    }
    
    # record for RMD
    rvs$fcs <- input$fcs
    rvs$rms <- input$rms
    rvs$rmsStep <- input$rmsStep
    rvs$clamp <- input$clamp
    
    # define the vector of RMs to input
    rms <- seq(input$rms[1], input$rms[2], input$rmsStep)  
    # create the Progress Bar object for ENMeval
    progress <- shiny::Progress$new()
    progress$set(message = "Evaluating ENMs...", value = 0)
    on.exit(progress$close())
    n <- length(rms) * length(input$fcs)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$inc(amount = 1/n, detail = detail)
    }
    
    jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
    if (!file.exists(jar)) {
      rvs %>% writeLog(type = 'error', 'File maxent.jar missing. 
                     Please see directions to download and copy to directory on the toolbar.')
      return()
    }
    
    occs.xy <- rvs$occs %>% dplyr::select(longitude, latitude)
    
    e <- ENMeval::ENMevaluate(occs.xy, rvs$bgMsk, bg.coords = rvs$bgPts,
                              RMvalues = rms, fc = input$fcs, method = 'user', 
                              occ.grp = rvs$occsGrp, bg.grp = rvs$bgGrp, 
                              clamp = input$clamp, bin.output = TRUE,
                              progbar = FALSE, updateProgress = updateProgress)
    
    names(e@models) <- e@results$settings
    
    # rename results table fields
    e@results <- e@results %>% dplyr::rename(avg.test.AUC = Mean.AUC, var.test.AUC = Var.AUC,
                                               avg.diff.AUC = Mean.AUC.DIFF, var.diff.AUC = Var.AUC.DIFF,
                                               avg.test.orMTP = Mean.ORmin, var.test.orMTP = Var.ORmin,
                                               avg.test.or10pct = Mean.OR10, var.test.or10pct = Var.OR10,
                                               parameters = nparam)
    
    rvs %>% writeLog("Maxent ran successfully and output evaluation results for", nrow(e@results), "models.")
    
    return(e)
  })
}
