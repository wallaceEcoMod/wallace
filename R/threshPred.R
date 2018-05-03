getThresh <- function(occPredVals, thresh) {
  # remove all NA
  occPredVals <- na.omit(occPredVals)
  if (thresh == 'mtp') {
    # apply minimum training presence threshold
    x <- min(occPredVals)
  } else if (thresh == 'p10') {
    # Define 10% training presence threshold
    if (length(occPredVals) < 10) {  # if less than 10 occ values, find 90% of total and round down
      n90 <- floor(length(occPredVals) * 0.9)
    } else {  # if greater than or equal to 10 occ values, round up
      n90 <- ceiling(length(occPredVals) * 0.9)
    }
    x <- rev(sort(occPredVals))[n90]  # apply 10% training presence threshold over all models
  }
  return(x)
}

threshPred  <- function(occs, results, modSel, thresh, predType, shinyLogs = NULL) {
  if (thresh != 'noThresh') {
    # find predicted values for occurrences for selected model
    # extract the suitability values for all occurrences
    occs.xy <- occs[c('longitude', 'latitude')]
    
    if(predType == 'logistic') {
      predCur <- results$predictionsLog[[modSel]]
    } else if(predType == 'cloglog') {
      predCur <- results$predictionsCLL[[modSel]]  
    } else {
      predCur <- results$predictions[[modSel]]  
    }
    
    # determine the threshold based on the current, not projected, prediction
    occPredVals <- raster::extract(predCur, occs.xy)
    # get the chosen threshold value
    x <- getThresh(occPredVals, thresh)  
    # threshold model prediction
    pred <- pred > x
    # rename
    names(pred) <- paste0(modSel, '_thresh_', thresh)
    shinyLogs %>% writeLog(thresh, 'threshold selected: value =', round(x, digits = 3), '.')
  }
  
  return(list(thresh=thresh, pred=pred))
}