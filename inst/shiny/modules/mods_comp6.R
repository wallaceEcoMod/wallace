comp6_bioclimMod <- function() {
  e <- BioClim_eval(values$modParams$occ.pts, values$modParams$bg.pts,
                    values$modParams$occ.grp, values$modParams$bg.grp,
                    values$predsMsk)
  values$evalTbl <- e$results
  values$evalMods <- e$models
  names(e$predictions) <- "BIOCLIM"
  values$evalPreds <- e$predictions
  occVals <- raster::extract(e$predictions, values$modParams$occ.pts)

  values$mtps <- min(occVals)  # apply minimum training presence threshold

  # Define 10% training presence threshold
  if (length(occVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
    n90 <- floor(length(occVals) * 0.9)
  } else {  # if greater than or equal to 10 occ values, round up
    n90 <- ceiling(length(occVals) * 0.9)
  }

  values$p10s <- rev(sort(occVals))[n90]  # apply 10% training presence threshold

  # make datatable of results df
  output$evalTbl <- DT::renderDataTable({DT::datatable(round(e$results, digits=3))})
  writeLog(paste("> BIOCLIM ran successfully and output evaluation results."))
}

comp6_maxentMod <- function(rms, fcs) {
  
  if (!require('rJava')) {
    jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
    writeLog('<font color="red"><b>! ERROR</b></font> : Package rJava cannot load. 
             Please download the latest version of Java, and make sure it is the 
             correct version (e.g. 64-bit if you have a 64-bit system). After the
             download, try "library(rJava)". If it loads properly, restart Wallace
             and try again.')
    writeLog(paste('* To use Maxent, also make sure you download maxent.jar from 
                   http://www.cs.princeton.edu/~schapire/maxent/ and place it here:
                   ', jar))
    return()
  }
  
  if (is.null(input$fcs)) {
    writeLog('<font color="red"><b>! ERROR</b></font> : Select feature classes first.')
    return()
  }

  rms <- seq(input$rms[1], input$rms[2], input$rmsBy)  # define the vector of RMs to input
  progress <- shiny::Progress$new()
  progress$set(message = "Evaluating ENMs...", value = 0)
  on.exit(progress$close())
  n <- length(rms) * length(input$fcs)
  updateProgress <- function(value = NULL, detail = NULL) {
    progress$inc(amount = 1/n, detail = detail)
  }

  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
  if (!file.exists(jar)) {
    writeLog(paste('<font color="red"><b>! ERROR</b></font> : File maxent.jar missing:\n', jar, '\nPlease download it from http://www.cs.princeton.edu/~schapire/maxent/ and copy to directory above.'))
    return()
  }

  e <- ENMeval::ENMevaluate(values$modParams$occ.pts, values$predsMsk, bg.coords = values$modParams$bg.pts,
                   RMvalues = rms, fc = fcs, method = 'user', occ.grp = values$modParams$occ.grp,
                   bg.grp = values$modParams$bg.grp, progbar = FALSE, updateProgress = updateProgress)

  # Load the ENMeval results into the values list
  values$evalTbl <- e@results
  values$evalMods <- e@models
  values$evalPreds <- e@predictions

  # Generate logistic predictions for each model
  withProgress(message = "Generating logistic predictions...", {
    logPreds <- sapply(e@models, function(x) dismo::predict(x, values$predsMsk))
    values$evalPredsLog <- raster::stack(logPreds)
    names(values$evalPredsLog) <- names(values$evalPreds)
  })

  occVals <- raster::extract(e@predictions, values$modParams$occ.pts)

  values$mtps <- apply(occVals, MARGIN = 2, min)  # apply minimum training presence threshold over all models

  # Define 10% training presence threshold
  if (nrow(occVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
    n90 <- floor(nrow(occVals) * 0.9)
  } else {  # if greater than or equal to 10 occ values, round up
    n90 <- ceiling(nrow(occVals) * 0.9)
  }
  values$p10s <- apply(occVals, MARGIN = 2, function(x) rev(sort(x))[n90])  # apply 10% training presence threshold over all models

  # make datatable of results df
  res <- e@results %>% dplyr::rename(avg.test.AUC = Mean.AUC, var.test.AUC = Var.AUC, avg.diff.AUC = Mean.AUC.DIFF,
                                     var.diff.AUC = Var.AUC.DIFF, avg.test.orMTP = Mean.ORmin, var.test.orMTP = Var.ORmin,
                                     avg.test.or10pct = Mean.OR10, var.test.or10pct = Var.OR10, parameters = nparam)
  output$evalTbl <- DT::renderDataTable({DT::datatable(cbind(res[,1:3], round(res[,4:15], digits=3)))})
  writeLog(paste("> Maxent ran successfully and output evaluation results for", nrow(e@results), "models."))
}
