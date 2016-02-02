comp6_bioclimMod <- function() {
  e <- BioClim_eval(values$modParams$occ.pts, values$modParams$bg.pts,
                    values$modParams$occ.grp, values$modParams$bg.grp,
                    values$predsMsk)
  values$evalTbl <- e$results
  values$evalMods <- e$models
  names(e$predictions) <- "Classic_BIOCLIM"
  values$evalPreds <- e$predictions
  occVals <- extract(e$predictions, values$modParams$occ.pts)
  
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
  writeLog(paste("* BIOCLIM ran successfully and output evaluation results."))
}

comp6_maxentMod <- function(rms, fcs) {
  rms <- seq(input$rms[1], input$rms[2], input$rmsBy)  # define the vector of RMs to input
  progress <- shiny::Progress$new()
  progress$set(message = "Evaluating ENMs...", value = 0)
  on.exit(progress$close())
  n <- length(rms) * length(input$fcs)
  updateProgress <- function(value = NULL, detail = NULL) {
    progress$inc(amount = 1/n, detail = detail)
  }
  e <- ENMevaluate(values$modParams$occ.pts, values$predsMsk, bg.coords = values$modParams$bg.pts,
                   RMvalues = rms, fc = fcs, method = 'user', occ.grp = values$modParams$occ.grp,
                   bg.grp = values$modParams$bg.grp, updateProgress = updateProgress)
  
  # Load the ENMeval results into the values list
  values$evalTbl <- e@results
  values$evalMods <- e@models
  values$evalPreds <- e@predictions.raw
  values$evalPredsLog <- e@predictions.log
  
  occVals <- extract(e@predictions.raw, values$modParams$occ.pts)
  
  values$mtps <- apply(occVals, MARGIN = 2, min)  # apply minimum training presence threshold over all models
  
  # Define 10% training presence threshold
  if (nrow(occVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
    n90 <- floor(nrow(occVals) * 0.9)
  } else {  # if greater than or equal to 10 occ values, round up
    n90 <- ceiling(nrow(occVals) * 0.9)
  }
  values$p10s <- apply(occVals, MARGIN = 2, function(x) rev(sort(x))[n90])  # apply 10% training presence threshold over all models
  
  # make datatable of results df
  output$evalTbl <- DT::renderDataTable({DT::datatable(cbind(e@results[,1:3], round(e@results[,4:15], digits=3)))})
  writeLog(paste("* Maxent ran successfully and output evaluation results for", nrow(e@results), "models."))
}