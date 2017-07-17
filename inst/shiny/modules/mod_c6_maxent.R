
maxent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("fcs"), label = "Select feature classes (flexibility of modeled response)",
                       choices = list("L (Linear)" = "L", "LQ (Linear/Quadratic)" = "LQ", "H (Hinge)" = "H",
                                      "LQH (Linear/Quadratic/Hinge)" = "LQH", "LQHP (Linear/Quadratic/Hinge/Product)" = "LQHP",
                                      "LQHPT (Linear/Quadratic/Hinge/Threshold)" = "LQHPT")),
    shinyBS::bsPopover(ns("fcs"), title = 'Tip',
                       'Feature combinations to be explored. Features are constructed using different relationships within and among the environmental predictors, and are used to constrain the computed probability distribution. In short, more features = more potential model complexity.',
                       placement = 'right', options = list(container = "body")),
    sliderInput(ns("rms"), label = "Select regularization multipliers (penalty against complexity)",
                min = 0, max = 10, value = c(1, 2)),
    shinyBS::bsPopover(ns("rms"), title = 'Tip',
                       'Range of regularization multipliers to explore. Greater values of the regularization multiplier lead to increased penalty against overly complex and/or overfit models. A value of 0 results in no regularization.',
                       placement = 'right', options = list(container = "body")),
    numericInput(ns("rmsStep"), label = "RM step value", value = 1),
    shinyBS::bsPopover(ns("rmsStep"), title = 'Tip',
                       'Value used to step through regularization multiplier range (e.g. range of 1-3 with step 0.5 results in [1, 1.5, 2, 2.5, 3]).',
                       placement = 'right', options = list(container = "body"))
  )
}

maxent_MOD <- function(input, output, session, rvs) {
  reactive({
    req(input$fcs, rvs$occs, rvs$bgPts, rvs$bgMsk, rvs$occsGrp, rvs$bgGrp)
    
    if (!require('rJava')) {
      rvs %>% writeLog(type = "error", 'Package rJava cannot load. 
               Please download the latest version of Java, and make sure it is the 
               correct version (e.g. 64-bit for a 64-bit system). After installing, 
               try "library(rJava)". If it loads properly, restart Wallace and try again.')
      return()
    }
    
    if (is.null(input$fcs)) {
      rvs %>% writeLog(type = 'error', 'Select feature classes first.')
      return()
    }
    
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
                              occ.grp = rvs$occsGrp,
                              bg.grp = rvs$bgGrp, progbar = FALSE, 
                              updateProgress = updateProgress)
    
    names(e@models) <- e@results$settings
    
    # rename results table fields
    e@results <- e@results %>% dplyr::rename(avg.test.AUC = Mean.AUC, var.test.AUC = Var.AUC,
                                               avg.diff.AUC = Mean.AUC.DIFF, var.diff.AUC = Var.AUC.DIFF,
                                               avg.test.orMTP = Mean.ORmin, var.test.orMTP = Var.ORmin,
                                               avg.test.or10pct = Mean.OR10, var.test.or10pct = Var.OR10,
                                               parameters = nparam)
    
    # generate logistic predictions for each model
    withProgress(message = "Generating logistic predictions...", {
      logPredsList <- sapply(e@models, function(x) dismo::predict(x, rvs$bgMsk))
      logPreds <- raster::stack(logPredsList)
      names(logPreds) <- names(e@predictions)
    })
    
    # extract the suitability values for all occurrences
    modOccVals <- raster::extract(e@predictions, occs.xy)
    rvs %>% writeLog("Maxent ran successfully and output evaluation results for", nrow(e@results), "models.")
    
    return(list(e, logPreds, modOccVals))
  })
}
