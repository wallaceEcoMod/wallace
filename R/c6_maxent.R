c6_maxent  <- function(occs, bgPts, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, 
                        clamp, logs = NULL, shiny = FALSE) {
  if (is.null(occsGrp)) {
    logs %>% writeLog(type = 'error', "Before building a model, please partition 
                        occurrences for cross-validation.")
    return()
  }
  
  # error for no maxent.jar in dismo directory
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
  if (!file.exists(jar)) {
    logs %>% writeLog(type = 'error', 'File maxent.jar missing. Please see directions 
                      on the toolbar to download and copy to the appropriate directory.')
    return()
  }
  
  if (is.null(fcs)) {
    logs %>% writeLog(type = 'error', "No feature classes selected.")
    return()
  }
  if (!require('rJava')) {
    logs %>% writeLog(type = "error", 'Package rJava cannot load. 
               Please download the latest version of Java, and make sure it is the 
               correct version (e.g. 64-bit for a 64-bit system). After installing, 
               try "library(rJava)". If it loads properly, restart Wallace and try again.
               If it does not, please consult www.github.com/wallaceecomod/wallace for
               more tips on getting rJava to work.')
    return()
  }
  
  if (is.null(fcs)) {
    logs %>% writeLog(type = 'error', 'Select feature classes first.')
    return()
  }
  
  # define the vector of RMs to input
  rms.interval <- seq(rms[1], rms[2], rmsStep)  
  # create the Progress Bar object for ENMeval
  if (shiny == TRUE) {
    progress <- shiny::Progress$new()
    progress$set(message = "Building and Evaluating ENMs...", value = 0)
    on.exit(progress$close())
    n <- length(rms.interval) * length(fcs)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$inc(amount = 1/n, detail = detail)
    }
  } else {
    n <- length(rms.interval) * length(fcs)
    updateProgress <- FALSE
    }

  # get just coordinates
  occs.xy <- occs %>% dplyr::select(longitude, latitude)
  
  e <- ENMeval::ENMevaluate(occs.xy, bgMsk, bg.coords = bgPts,
                            RMvalues = rms.interval, fc = fcs, method = 'user', 
                            occ.grp = occsGrp, bg.grp = bgGrp, 
                            clamp = clamp, bin.output = TRUE,
                            progbar = FALSE, updateProgress = updateProgress)
  
  # name the output models in the model list
  names(e@models) <- e@results$settings
  
  # rename results table fields
  e@results <- e@results %>% dplyr::rename(avg.test.AUC = Mean.AUC, var.test.AUC = Var.AUC,
                                           avg.diff.AUC = Mean.AUC.DIFF, var.diff.AUC = Var.AUC.DIFF,
                                           avg.test.orMTP = Mean.ORmin, var.test.orMTP = Var.ORmin,
                                           avg.test.or10pct = Mean.OR10, var.test.or10pct = Var.OR10,
                                           parameters = nparam)
  
  logs %>% writeLog("Maxent ran successfully and output evaluation results for", 
                    nrow(e@results), "models.")
  
  return(e)
  
}
