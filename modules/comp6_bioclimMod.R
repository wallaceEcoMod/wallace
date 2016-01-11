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